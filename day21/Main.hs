module Main where

import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let boss = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show boss)

    let me = Player 100 0 0 0

    let (weapons,armor,rings) = shop
    let nullItem = Item 0 0 0
    let itemCombinations = [[w,a,r1,r2] | w  <- weapons
                                        , a  <- nullItem:armor
                                        , r1 <- nullItem:rings
                                        , r2 <- nullItem:rings
                                        , if r1 == r2 && r1 /= nullItem
                                          then False else True]

    -- Solve
    let applyItemCombination = foldr applyItemStats me
    let answer1 = head . sort . map _cCost . filter (fight boss)
                $ map applyItemCombination itemCombinations
    let answer2 = last . sort . map _cCost . filter (not . fight boss)
                $ map applyItemCombination itemCombinations

    ---- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Character = Player { cHP :: Int, cDmg :: Int, cArmor :: Int, _cCost :: Int }
               | Boss { cHP :: Int, cDmg :: Int, cArmor :: Int }
               deriving (Show)

isPlayer :: Character -> Bool
isPlayer Player {} = True
isPlayer _ = False

parse :: String -> Character
parse input = Boss (read hp) (read damage) (read armor) 
    where asLines = lines input
          hp = snd . Common.splitOnceOn ": " $ asLines !! 0
          damage = snd . Common.splitOnceOn ": " $ asLines !! 1
          armor = snd . Common.splitOnceOn ": " $ asLines !! 2

fight :: Character -> Character -> Bool
fight a b
    | cHP a <= 0 = not $ isPlayer a
    | cHP b <= 0 = not $ isPlayer b
    | otherwise  = fight b (a { cHP = hpAfterDamage })
    where hpAfterDamage = cHP a - max 1 (cDmg b - cArmor a)

data Item = Item { iCost :: Int, iDmg :: Int, iArmor :: Int }
            deriving (Show,Eq)

applyItemStats :: Item -> Character -> Character
applyItemStats _ Boss {} = error "Do you really want to make the boss stronger?!?"
applyItemStats (Item costAdd dmgAdd armorAdd)
               (Player hp dmg armor cost) =
    Player hp (dmg + dmgAdd) (armor + armorAdd) (cost + costAdd)

type Shop = ([Item],[Item],[Item])
shop :: Shop
shop = (splitByItemTypes !! 0 -- weapons
      , splitByItemTypes !! 1 -- armor
      , splitByItemTypes !! 2) -- rings
    where splitByItemTypes = map (map toItem . tail)
                           . tail . splitOn [""] $ lines shopString
          toItem xs = Item (read $ fields !! 1)
                           (read $ fields !! 2)
                           (read $ fields !! 3)
              where fields = filter (\x -> x /= "" && not ('+' `elem` x))
                           $ splitOn " " xs

shopString :: String
shopString = "\n\
\Weapons:    Cost  Damage  Armor\n\
\Dagger        8     4       0\n\
\Shortsword   10     5       0\n\
\Warhammer    25     6       0\n\
\Longsword    40     7       0\n\
\Greataxe     74     8       0\n\
\\n\
\Armor:      Cost  Damage  Armor\n\
\Leather      13     0       1\n\
\Chainmail    31     0       2\n\
\Splintmail   53     0       3\n\
\Bandedmail   75     0       4\n\
\Platemail   102     0       5\n\
\\n\
\Rings:      Cost  Damage  Armor\n\
\Damage +1    25     1       0\n\
\Damage +2    50     2       0\n\
\Damage +3   100     3       0\n\
\Defense +1   20     0       1\n\
\Defense +2   40     0       2\n\
\Defense +3   80     0       3"
