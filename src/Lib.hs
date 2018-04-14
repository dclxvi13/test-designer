module Lib where

newtype TState = TState String
newtype TCondition = TCondition String
newtype TAction = TAction String

data TCase = TCase {
  _caseState      :: TState,
  _caseConditions :: [TCondition],
  _caseAction     :: TAction
}

----------------------------------------
-- Get globals
----------------------------------------

getGlobalActions :: [TAction]
getGlobalActions = []

getGlobalConditions :: [TCondition]
getGlobalConditions = []

getStates :: [TState]
getStates = []

----------------------------------------
-- Get locals
----------------------------------------

getActionsForState :: TState -> [TAction]
getActionsForState state = []

getConditionsForState :: TState -> [TCondition]
getConditionsForState state = []

----------------------------------------
-- Cases
----------------------------------------

buildCasesForState :: TState -> [TCondition] -> [TAction] -> [TCondition] -> [TAction] -> [TCase]
buildCasesForState state globalConditions globalActions stateConditions stateActions =
  let conds =[[]] ++
             [[c] | c <- globalConditions ++ stateConditions] ++
             [[g, s] | g <- globalConditions, s <- stateConditions]
  in
  [TCase state cond action | cond <- conds, action <- globalActions ++ stateActions]

getCases :: [TCase]
getCases =
  let states = getStates
      globalActions = getGlobalActions
      globalConditions = getGlobalConditions
  in
  concatMap (\state -> buildCasesForState state globalConditions globalActions (getConditionsForState state) (getActionsForState state)) states
