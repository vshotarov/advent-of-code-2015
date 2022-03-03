module Main where

import qualified Data.Map as M
import Data.List (sortOn)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let boss = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show boss)

    let me = Player 50 0 500 0

    -- Solve
    let answer1 = fight False 10000 $ InProgress me boss effects
    let answer2 = fight True 10000 $ InProgress me boss effects

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

fight :: Bool -> Int -> State -> State
fight _ _ s@(Win _) = s
fight _ _ Loss = Loss
fight _ best (InProgress p _ _) | _pCost p > best = Loss
fight hardMode best state =
    let state' = if hardMode then applyHardMode state else state
        state'' = tickEffects state'
        statesAfterPlayer = map ($ state'') $ M.elems spellbook
        statesAfterPlayer' = map tickEffects statesAfterPlayer
        statesAfterBoss = map bossTurn statesAfterPlayer'
        (_,wins) = foldr (\x (best',acc) -> let outcome = fight hardMode best' x
                                             in if not $ isWin outcome
                                                then (best',acc)
                                                else (min best' $ _sCost outcome,outcome:acc))
               (best,[]) statesAfterBoss
     in if length wins == 0 then Loss
        else head $ sortOn _sCost $ wins

isWin :: State -> Bool
isWin (Win _) = True
isWin _ = False

applyHardMode :: State -> State
applyHardMode (InProgress p b e)
  | cHP p <= 1 = Loss
  | otherwise = InProgress (p { cHP = cHP p - 1 }) b e
applyHardMode s = s

data Character = Player { cHP :: Int, cArmor :: Int, _pMana :: Int, _pCost :: Int }
               | Boss   { cHP :: Int, cArmor :: Int, _bDmg :: Int }
               deriving (Show)
type Effects = M.Map String Int
data State = InProgress { _sPlayer :: Character, _sBoss :: Character, _sEffects :: Effects}
           | Win { _sCost :: Int }
           | Loss
    deriving (Show)

type Spell = ((State -> State))
spellbook :: M.Map String Spell
spellbook = M.fromList
    [("Magic Missile", castSpell 53 (\(InProgress p b e) ->
                                        InProgress p (applyDamage 4 b) e))
    ,("Drain"        , castSpell 73 (\(InProgress p b e) ->
                                        InProgress (heal 2 p) (applyDamage 2 b) e))
    ,("Shield"       , castSpell 113 (\s -> addEffect "Shield" 6 s))
    ,("Poison"       , castSpell 173 (\s -> addEffect "Poison" 6 s))
    ,("Recharge"     , castSpell 229 (\s -> addEffect "Recharge" 5 s))
    ]

effects :: Effects
effects = M.fromList . zip ["Shield","Poison","Recharge"] $ repeat 0
tickEffects :: State -> State
tickEffects s@(Win _) = s
tickEffects Loss = Loss
tickEffects s 
  | cHP (_sBoss s') <= 0 = Win . _pCost $ _sPlayer s'
  | otherwise = s'
      where s' = foldr tickEffect s $ M.keys effects
            decrease k m = M.insert k ((m M.! k) - 1) m
            tickEffect k (InProgress p b e)
              | k == "Shield" && e M.! k == 0 = InProgress (p { cArmor=0 }) b e
              | e M.! k == 0 = InProgress p b e
              | otherwise = applyTick k $ decrease k e
                  where applyTick "Shield" = InProgress (p { cArmor=7 }) b
                        applyTick "Poison" = InProgress p (b { cHP=cHP b - 3 })
                        applyTick "Recharge" = InProgress (p { _pMana=_pMana p + 101 }) b
                        applyTick _ = error "Unrecognized effect"
            tickEffect _ s'' = s''

addEffect :: String -> Int -> State -> State
addEffect effect turns (InProgress p b e)
  | e M.! effect /= 0 = Loss
  | otherwise         = (InProgress p b $ M.insert effect turns e)
addEffect _ _ s = s

castSpell :: Int -> (State -> State) -> State -> State
castSpell mana spell (InProgress p b e)
  | mana > _pMana p = Loss
  | not isLoss && cHP (_sBoss s') <= 0 = Win . _pCost $ _sPlayer s'
  | otherwise       = s'
      where s' = spell $ InProgress (p { _pMana = _pMana p - mana, _pCost = _pCost p + mana }) b e
            isLoss = case s' of
                       Loss -> True
                       _ -> False
castSpell _ _ s = s

bossTurn :: State -> State
bossTurn (InProgress p b e)
  | cHP p' <= 0 = Loss
  | otherwise   = InProgress p' b e
      where p' = applyDamage (_bDmg b) p
bossTurn s = s

applyDamage :: Int -> Character -> Character
applyDamage dmg char = char { cHP = cHP char - max 1 (dmg - cArmor char) }

heal :: Int -> Character -> Character
heal n p@(Player { cHP=hp }) = p { cHP = hp + n }
heal _ _ = error "Can't heal the boss"

parse :: String -> Character
parse input = Boss (read hp) 0 (read damage)
    where asLines = lines input
          hp = snd . Common.splitOnceOn ": " $ asLines !! 0
          damage = snd . Common.splitOnceOn ": " $ asLines !! 1
