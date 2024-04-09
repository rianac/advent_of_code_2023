module Main where

import Data.Char (isDigit, isAlpha)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

type Condition = (Char, Char, Int)
type Rule = (Condition, String)
type Workflow = (String, [Rule])
type Workflows = Map.Map String [Rule]

type Part = Map.Map Char Int

type Range = (Int, Int)
type Ranges = Map.Map Char Range

-- Parsing input file --

decimalP :: ReadP Int
decimalP = read <$> munch1 isDigit

wordP :: ReadP String
wordP = munch isAlpha

conditionP :: ReadP Condition
conditionP = (,,) <$> get <*> satisfy (`elem` "<>") <*> decimalP 

ruleP :: ReadP Rule
ruleP = (,) <$> conditionP <*> (char ':' *> wordP)

defaultRuleP :: ReadP Rule
defaultRuleP = (,) ('.', '.', 0) <$> wordP

rulesP :: ReadP [Rule]
rulesP = (ruleP +++ defaultRuleP) `sepBy1` char ',' 

workflowP :: ReadP Workflow
workflowP = (,) <$> wordP <*> (char '{' *> rulesP <* char '}')

workflowsP :: ReadP (Map.Map String [Rule])
workflowsP = Map.fromList <$> workflowP `sepBy1` char '\n'

paramP :: ReadP (Char, Int)
paramP = (,) <$> get <* char '=' <*> decimalP

partP :: ReadP Part
partP = Map.fromList <$> (char '{' *> paramP `sepBy1` char ',' <* char '}')

partsP :: ReadP [Part]
partsP = partP `sepBy1` char '\n'

parseData = fst . head . readP_to_S dataP
  where
    dataP = (,) <$> workflowsP <* string "\n\n" <*> partsP <* char '\n' <* eof

-- Task 1 --

-- Find which rule of a workflow is satisfied by the given part
checkRules :: [Rule] -> Part -> String
checkRules [] _ = error "No rule applicable"
checkRules [(('.', _, _), x)] _ = x
checkRules (((var, op, lim), wf) : rest) part =
  if checkRule (part Map.! var) op lim then wf else checkRules rest part
  where
    checkRule y '<' lim = y < lim
    checkRule y '>' lim = y > lim
    checkRule _ _ _ = error "Unknown rule"

-- Check whether a part is accepted (calculate the sum of its ratings) or
-- rejected (zero)
checkWf :: Workflows -> String -> Part -> Int
checkWf wfs wfName part = case checkRules (wfs Map.! wfName) part  of
  "R" -> 0
  "A" -> sum $ Map.elems part
  newWfName  -> checkWf wfs newWfName part

-- Calculate the sum of ratings of all accepted parts based on given workflows
calculateTotal :: (Workflows, [Part]) -> Int
calculateTotal (wfs, parts) = sum $ map (checkWf wfs "in") parts

task1 filename = calculateTotal . parseData <$> readFile filename

-- Task 2 --

-- Range update if the rule is satisfied
updateIfTrue :: Range -> Char -> Int -> Range
updateIfTrue (bot, top) '<' lim = (bot, min top lim-1)
updateIfTrue (bot, top) '>' lim = (max bot lim+1, top)

-- Range update if the rule in unsatisfied
updateIfFalse :: Range -> Char -> Int -> Range
updateIfFalse (bot, top) '<' lim = (max bot lim, top)
updateIfFalse (bot, top) '>' lim = (bot, min top lim)

-- All possible range updates relevant to all rules of one workflow  
updateRange :: Ranges -> [Rule] -> [(String, Ranges)]
updateRange ranges [(('.', _, _), wf)] = [(wf, ranges)]
updateRange ranges (((var, op, lim), wf) : rest) =
  (wf, ifTrue) : updateRange ifFalse rest
  where
    range = ranges Map.! var
    ifTrue = Map.insert var (updateIfTrue range op lim) ranges
    ifFalse = Map.insert var (updateIfFalse range op lim) ranges

-- Range updates along branches of a decision tree (decision points
-- are represented by rules of relevant workflows)
updateRanges :: Workflows -> [(String, Ranges)] -> [(String, Ranges)]
updateRanges _ [] = []
updateRanges wfs ranges = finals ++ updateRanges wfs intern
  where
    expand (wf, range) = updateRange range (wfs Map.! wf)
    expanded = filter ((/="R") . fst) $ concatMap expand ranges
    finals = filter ((=="A") . fst) expanded
    intern = filter ((/="A") . fst) expanded

-- Calculate the number of combinations allowed by ranges 
rangeSize :: Ranges -> Int
rangeSize ranges = product $ map size $ Map.elems ranges
  where
    size (bot, top) = top - bot + 1

-- Calculate the number of distinct combinations of ratings accepted
-- by workflows
calculateTotal2 :: (Workflows, [Part]) -> Int
calculateTotal2 (wfs, _) =
  sum $ map (rangeSize . snd) $ updateRanges wfs [("in", fullRange)]
  where
    fullRange =
      Map.fromList [('x',(1,4000)),('m',(1,4000)),('a',(1,4000)),('s',(1,4000))]

task2 filename = calculateTotal2 . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 19114 then "OK" else "Something went wrong")
    <$> task1 "day19-example"
  
testTask2 =
  (\x -> if x == 167409079868000 then "OK" else "Something went wrong")
    <$> task2 "day19-example"

main = do
  x <- task1 "day19-input"
  y <- task2 "day19-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
