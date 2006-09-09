-- Copyright 2006, Sascha Boehme.




-- | This module provides a state object used in various stages of the theorem
--   generation process. It serves to create unique indices.

module FreeTheorems.TheoremData (
    TheoremData,
    TheoremState,
    newTheoremData,
    execute,
    executeWith,
    newRelationVariable,
    newVariablesFor,
    newRelationAsFunctionVariable
) where



import FreeTheorems.Declarations (isDefined)
import FreeTheorems.Types
import Control.Monad (liftM2)
import Control.Monad.State (State, get, modify, runState)



-- | A type synonym for the state used when modifying 'TheoremData'.

type TheoremState a = State TheoremData a



-- | Stores information used to create unique indices. It is used while
--   generating theorems for types.

data TheoremData = TheoremData {
                    forbiddenVariable :: TermVariable,
                        -- ^ The name of the type, from which the theorem is
                        --   generated. Used to avoid name clashes.

                    nextRelVarIndex :: Int,
                        -- ^ The next index when creating a new relation
                        --   variable.

                    nextTypeTermVarIndex :: Int,
                        -- ^ The next index when creating a new type term
                        --   variable.

                    nextTermVarIndex  :: Int,
                        -- ^ Next index when creating a new term variable.

                    nextFunVarIndex   :: Int,
                        -- ^ Next index when creating a new term variable
                        --   representing a function.

                    nextRelFunVarIndex   :: Int
                        -- ^ Next index when creating a new term variable
                        --   representing a function specialized from a
                        --   relation.
                  }



-- | Creates new theorem data based on the name of the type from which the
--   theorem is generated and the relation variable mapping of 'applyDelta'.

newTheoremData :: TermVariable -> TheoremData
newTheoremData tv = TheoremData {
                      forbiddenVariable = tv,
                      nextRelVarIndex = 1,
                      nextTypeTermVarIndex = 1,
                      nextTermVarIndex = 1,
                      nextFunVarIndex = 1,
                      nextRelFunVarIndex = 1
                    }



-- | Executes a 'TheoremState' action and returns the result of it along with
--   the last data. The latter may be used to execute consecutive actions.

execute :: TermVariable -> TheoremState a -> (a, TheoremData)
execute tv action = executeWith (newTheoremData tv) action


-- | Executes a 'TheoremState' action using previously generated data. Returns
--   the action's result and updated data.

executeWith :: TheoremData -> TheoremState a -> (a, TheoremData)
executeWith tdata action = runState action tdata



-- | Creates a new relation variable.

newRelationVariable :: TypeVariable -> TheoremState RelationVariable
newRelationVariable v = do
  ttv1 <- newTypeTermVariable
  ttv2 <- newTypeTermVariable

  tdata <- get
  let index  = nextRelVarIndex tdata
  let rv = R index v (ttv1, ttv2)
  modify (\tdata -> tdata { nextRelVarIndex = index + 1 })
  return rv



-- | Creates a new type term variable.

newTypeTermVariable :: TheoremState TypeTermVariable
newTypeTermVariable = do
  tdata <- get
  let index = nextTypeTermVarIndex tdata
  modify (\tdata -> tdata { nextTypeTermVarIndex = index + 1 })

  -- check if the name of the new type term variable collides with an already
  -- existing type constructor name
  if isDefined ("T" ++ show index)
    then newTypeTermVariable
    else return (T index)



-- Generates a term variable for a non-function term or generates a function
-- variable for a function, even if it is hidden by abstractions.

newVariablesFor :: Relation -> TheoremState (TermVariable, TermVariable)
newVariablesFor rel =
  case rel of
    RelFun _ _ _    -> newFunctionVariables
    RelForall _ _ r -> newVariablesFor r
    otherwise       -> newTermVariables



-- | Creates a new pair of term variables (@x_i@ and @y_i@).
--   The created variables are checked against the name of the type from which
--   the theorem is generated to ensure that the variables are unique.

newTermVariables :: TheoremState (TermVariable, TermVariable)
newTermVariables = do
  tdata <- get
  let index = nextTermVarIndex tdata
  modify (\tdata -> tdata { nextTermVarIndex = index + 1 })
  let x = (TV "x" index)
  let y = (TV "y" index)
  areInUse <- liftM2 (||) (isAlreadyInUse x) (isAlreadyInUse y)
  if areInUse
    then newTermVariables
    else return (x, y)



-- | Creates a new pair of function term variables (@f_i@ and @g_i@).
--   The created variables are checked against the name of the type from which
--   the theorem is generated to ensure that the variables are unique.

newFunctionVariables :: TheoremState (TermVariable, TermVariable)
newFunctionVariables = do
  tdata <- get
  let index = nextFunVarIndex tdata
  modify (\tdata -> tdata { nextFunVarIndex = index + 1 })
  let f = (TV "f" index)
  let g = (TV "g" index)
  areInUse <- liftM2 (&&) (isAlreadyInUse f) (isAlreadyInUse g)
  if areInUse
    then newFunctionVariables
    else return (f, g)



-- | Creates a new function variable @h_i@ which is used to replace or
--   specialize a relation variable.
--   The created variable is checked against the name of the type from which
--   the theorem is generated to ensure that the variable is unique.

newRelationAsFunctionVariable :: TheoremState TermVariable
newRelationAsFunctionVariable = do
  tdata <- get
  let index = nextRelFunVarIndex tdata
  modify (\tdata -> tdata { nextRelFunVarIndex = index + 1 })
  let h = TV "h" index
  isInUse <- isAlreadyInUse h
  if isInUse
    then newRelationAsFunctionVariable
    else return h



-- | Checks if a generated variable is unique. Returns 'True' if the same name
--   is already in use.

isAlreadyInUse :: TermVariable -> TheoremState Bool
isAlreadyInUse var = do
  tdata <- get
  return (var == forbiddenVariable tdata)

