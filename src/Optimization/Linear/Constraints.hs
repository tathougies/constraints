{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Optimization.Linear.Constraints where

import           Control.Monad.State

import           Data.Bifunctor (bimap)
import           Data.FingerTree (FingerTree, Measured(..))
import qualified Data.FingerTree as FingerTree
import           Data.Foldable (toList, foldr, foldl', all)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import           Data.Word

import           Debug.Trace

import           Text.PrettyPrint.ANSI.Leijen (align, text, vsep, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as Colored

data ColumnIndex
    = ColumnIndex
    { goalVarCount, lastGoalVar                :: !Word
    , considerationCount, lastConsiderationVar :: !Word
    , slackCount, lastSlackVar                 :: !Word
    , constantSeen                             :: !Bool
    } deriving Show

instance Semigroup ColumnIndex where
    a <> b = b { goalVarCount = goalVarCount b + goalVarCount a
               , lastGoalVar = if lastGoalVar b == 0 then lastGoalVar a else lastGoalVar b
               , considerationCount = considerationCount b + considerationCount a
               , lastConsiderationVar = if lastConsiderationVar b == 0 then lastConsiderationVar a else lastConsiderationVar b
               , slackCount = slackCount b + slackCount a
               , lastSlackVar = if lastSlackVar b == 0 then lastSlackVar a else lastSlackVar b
               , constantSeen = constantSeen a || constantSeen b }

instance Monoid ColumnIndex where
    mempty = ColumnIndex 0 0 0 0 0 0 False
    mappend = (<>)

data Column = Column !ColumnPos !Double
  deriving Show

data ColumnPos
    = Solution      !Word
    | Consideration !Word
    | Slack         !Word
    | Constant
      deriving (Show, Eq, Ord)

mapColumn :: (Double -> Double) -> Column -> Column
mapColumn f (Column p d) = Column p (f d)

data Equation
    = Equation
    { eqName :: !Word
    , coefficients :: !(FingerTree ColumnIndex Column) }
    deriving Show

splitGoals :: FingerTree ColumnIndex Column -> ( FingerTree ColumnIndex Column
                                               , FingerTree ColumnIndex Column )
splitGoals =
    FingerTree.split (\ix -> considerationCount ix > 0 || slackCount ix > 0)

filterEqn :: (Column -> Bool) -> FingerTree ColumnIndex Column -> FingerTree ColumnIndex Column
filterEqn pred = foldr (\a eq -> if pred a then a FingerTree.<| eq else eq) mempty

partitionEqn :: (Column -> Bool)
             -> FingerTree ColumnIndex Column
             -> (FingerTree ColumnIndex Column, FingerTree ColumnIndex Column)
partitionEqn pred = foldr (\a (eqTrue, eqFalse) ->
                               if pred a
                               then (a FingerTree.<| eqTrue, eqFalse)
                               else (eqTrue, a FingerTree.<| eqFalse))
                          (mempty, mempty)

newtype Solution
    = Assignments { assignments :: FingerTree ColumnIndex Column }
      deriving Show

data SolveResult
    = Unbounded
      -- ^ The solution is infinite
    | Overspecified
      -- ^ There is no solution because there are too many constraints
    | Feasible !Solution
      -- ^ There is a solution, and here it is
      deriving Show

data EqnPosition
    = EqnPosition { lastEqnNumber :: !Word
                  , eqnCount      :: !Word }
      deriving Show

data System
    = System
    { goals     :: !(FingerTree EqnPosition Equation)
    , equations :: !(FingerTree EqnPosition Equation)
    } deriving Show

instance Measured EqnPosition Equation where
    measure (Equation nr _) = EqnPosition nr 1

instance Semigroup EqnPosition where
    EqnPosition _ cnt1 <> EqnPosition nr2 cnt2 =
        EqnPosition nr2 (cnt1 + cnt2)

instance Monoid EqnPosition where
    mempty = EqnPosition 0 0
    mappend = (<>)

instance Measured ColumnIndex Column where
    measure (Column p _) = measure p

instance Measured ColumnIndex ColumnPos where
    measure (Solution w) = ColumnIndex 1 w 0 0 0 0 False
    measure (Consideration w) = ColumnIndex 0 0 1 w 0 0 False
    measure (Slack w) =  ColumnIndex 0 0 0 0 1 w False
    measure Constant = ColumnIndex 0 0 0 0 0 0 True

emptySystem :: System
emptySystem = System mempty mempty

emptyEquation :: Equation
emptyEquation = Equation 0 mempty

above :: Colored.Doc -> Colored.Doc -> Colored.Doc
above = (Colored.<$>)

sampleSystem :: System
sampleSystem = System (FingerTree.fromList [ Equation 1 (FingerTree.fromList [ Column (Solution 1) 1, Column (Consideration 1) (-1), Column (Consideration 2) (-1) ]) ])
                      (FingerTree.fromList [ Equation 1 (FingerTree.fromList [ Column (Consideration 1) 0.5, Column (Consideration 2) 0.5, Column (Consideration 3) (-1), Column Constant (-2) ])
                                           , Equation 2 (FingerTree.fromList [ Column (Consideration 2) 1, Column (Slack 1) (-1), Column Constant (-100) ])
                                           , Equation 3 (FingerTree.fromList [ Column (Consideration 1) 1, Column (Slack 2) 1, Column Constant (-50) ]) ])

prettyPrint :: System -> Colored.Doc
prettyPrint (System goals equations) =
    (Colored.green (text "Maximize") <+> align (vsep goalDocs)) `above`
    (Colored.green (text "w.r.t.  ") <+> align (vsep equationDocs))
    where
      goalDocs     = map mkGoalDoc (toList goals)
      equationDocs = map mkEquationDoc (toList equations)

      mkGoalDoc (Equation nr eq) =
          let (goals, eq') = splitGoals eq
          in mkSum goals <+> Colored.yellow (text "=") <+> mkSum (mapColumn negate `FingerTree.fmap'` eq') <+>
             Colored.cyan (text ("(Goal " ++ show nr ++ ")"))

      mkEquationDoc (Equation nr eq) =
          mkSum eq <+> Colored.yellow (text "=") <+> text "0" <+> Colored.cyan (text ("(Equation " ++ show nr ++ ")"))

      mkSum goals
          | FingerTree.null goals = Colored.red (text "0")
          | otherwise = foldl1 (\term1 term2 -> term1 <+> text "+" <+> term2) (map mkTerm (toList goals))

      mkTerm (Column (Solution ix) d) = mkCoeff d <+> Colored.blue (text ("z" ++ show ix))
      mkTerm (Column (Consideration ix) d) = mkCoeff d <+> Colored.green (text ("x" ++ show ix))
      mkTerm (Column (Slack ix) d) = mkCoeff d <+> Colored.magenta (text ("s" ++ show ix))
      mkTerm (Column Constant d) = Colored.green (Colored.text (show d))

      mkCoeff d = if d == 1 then Colored.empty else if d == 0 then Colored.red (Colored.text (show d))
                  else Colored.text (show d)

mergeUsed :: FingerTree ColumnIndex ColumnPos -> FingerTree ColumnIndex ColumnPos
          -> FingerTree ColumnIndex ColumnPos
mergeUsed a b =
    FingerTree.fmap' (\(Column pos _) -> pos) $
    mergeEquation (\_ _ -> 1) (FingerTree.fmap' (\pos -> Column pos 1) a)
                              (FingerTree.fmap' (\pos -> Column pos 1) b)

-- | TODO test
mergeEquation :: (Double -> Double -> Double)
              -> FingerTree ColumnIndex Column -> FingerTree ColumnIndex Column
              -> FingerTree ColumnIndex Column
mergeEquation f as bs =
    case (FingerTree.viewl as, FingerTree.viewl bs) of
      (FingerTree.EmptyL, _) -> bs
      (_, FingerTree.EmptyL) -> as
      ( a@(Column ap ad) FingerTree.:< as',
        b@(Column bp bd) FingerTree.:< bs' ) ->
        case compare ap bp of
          LT -> a FingerTree.<| mergeEquation f as' bs
          EQ -> let d' = f ad bd
                in if d' == 0 then mergeEquation f as' bs'
                   else Column ap d' FingerTree.<| mergeEquation f as' bs'
          GT -> b FingerTree.<| mergeEquation f as bs'

removeVars :: FingerTree ColumnIndex ColumnPos
           -> FingerTree ColumnIndex Column
           -> FingerTree ColumnIndex Column
removeVars as bs =
    case (FingerTree.viewl as, FingerTree.viewl bs) of
      (FingerTree.EmptyL, _) -> bs
      (_, FingerTree.EmptyL) -> mempty
      ( ap FingerTree.:< as',
        b@(Column bp bd) FingerTree.:< bs' ) ->
        case compare ap bp of
          LT -> removeVars as' bs
          EQ -> removeVars as' bs'
          GT -> b FingerTree.<| removeVars as bs'

-- | TODO keep an actual track of these
nonGoalVariables :: System -> Word
nonGoalVariables (System _ equations) =
    let allVariables = foldr (mergeEquation const) mempty (map coefficients (toList equations))
    in nonGoalCount allVariables

nonGoalCount :: Measured ColumnIndex a => FingerTree ColumnIndex a -> Word
nonGoalCount eqn =
    let varInfo = measure eqn
    in considerationCount varInfo + slackCount varInfo

countVariables :: System -> FingerTree ColumnIndex Column
countVariables (System _ equations) =
    let -- Find n variables that only appear in one column
        varCounts = FingerTree.fmap' (FingerTree.fmap' (mapColumn (const 1)) . coefficients) equations
        basis = foldr (mergeEquation (+)) mempty varCounts
    in basis

basicVariables :: System -> (FingerTree ColumnIndex ColumnPos, FingerTree ColumnIndex ColumnPos)
basicVariables sys@(System goals _) =
    let counts = countVariables sys

        justPos = FingerTree.fmap' (\(Column pos _) -> pos)
    in bimap justPos justPos $
       partitionEqn (\(Column _ d) -> d == 1) $
       filterEqn (\(Column pos _) -> case pos of { Constant -> False; Solution _ -> False; _ -> True }) $

       foldr (mergeEquation (\_ x -> x) . coefficients) counts goals

-- | Convert a system into canonical form
-- canonical :: System -> (System, Word, FingerTree ColumnIndex ColumnPos, FingerTree ColumnIndex ColumnPos)
-- canonical system@(System _ equations) =
--     let (currentBasis, nonBasic) = basicVariables system
--         n = eqnCount (measure equations)
--     in trace ("Got " ++ show (currentBasis, nonBasic) ++ " and " ++ show n) $
--        if nonGoalCount currentBasis >= n
--        then (system, n, currentBasis, nonBasic) -- TODO be smart about which columns we choose as the basis
--        else error "TODO canonicalize"
--
-- -- | Convert the program into a basic feasible solution. This gets us to
-- -- one edge along the polytope, or results in a proof that there is no
-- -- feasible solution
-- solve :: System -> SolveResult
-- solve system = let (system', varCount, basis, nonBasis) = canonical system
--                in solveCanonical system' varCount basis nonBasis

collectBasis :: Measured ColumnIndex a
             => Word -> FingerTree ColumnIndex a -> ( FingerTree ColumnIndex a, FingerTree ColumnIndex a )
collectBasis ix =
    FingerTree.split (\pos -> considerationCount pos + slackCount pos > ix)

allNonNegative :: FingerTree ColumnIndex ColumnPos -> FingerTree ColumnIndex Column
               -> Bool
allNonNegative a b =
    case ( FingerTree.viewl a
         , FingerTree.viewl b ) of
      ( FingerTree.EmptyL, _ ) -> True
      ( _, FingerTree.EmptyL ) -> True
      ( ap FingerTree.:< a',
        Column bp bc FingerTree.:< b' ) ->
        case compare ap bp of
          EQ -> bc >= 0 && allNonNegative a' b'
          LT -> allNonNegative a' b
          GT -> allNonNegative a b'

selectPivot :: Equation -> Maybe ColumnPos
selectPivot (Equation _ cs) =
    let nextPivot = foldl' (\a (Column pos d) ->
                                let next = case a of
                                             Nothing -> Just (pos, d)
                                             Just (_, d')
                                               | d < d' -> Just (pos, d)
                                               | otherwise -> a
                                in case pos of
                                     Consideration _ -> next
                                     Slack _ -> next
                                     _ -> a) Nothing cs
    in fst <$> nextPivot

eqnSplitPred :: ColumnPos -> ColumnIndex -> Bool
eqnSplitPred (Solution i) = \p -> lastGoalVar p >= i
eqnSplitPred (Consideration i) = \p -> lastConsiderationVar p >= i
eqnSplitPred (Slack i) = \p -> lastSlackVar p >= i
eqnSplitPred Constant = \p -> constantSeen p

getCoeff :: ColumnPos -> FingerTree ColumnIndex Column -> Double
getCoeff pos eqn =
    let (before, after) = FingerTree.split (eqnSplitPred pos) eqn

    in case FingerTree.viewl after of
         Column pos' d FingerTree.:< _
             | pos == pos' -> d
         _ -> 0

getConstant :: FingerTree ColumnIndex Column -> Double
getConstant = getCoeff Constant

selectPivotRow :: ColumnPos -> FingerTree EqnPosition Equation -> Maybe (FingerTree EqnPosition Equation, Double, Equation, FingerTree EqnPosition Equation)
selectPivotRow pos equations =
    let pivotValue eqn =
            let denom = getCoeff pos eqn
                num = getConstant eqn
            in if denom == 0
               then (0, 0)
               else (denom, num / denom)

        accum a (Equation nr eqn) =
            let (coeff, val) = pivotValue eqn
            in trace ("Val for " ++ show nr ++ " is " ++ show val) $
               if val >= 0
               then a
               else case a of
                      Nothing -> Just ((nr, coeff), val)
                      Just (_, oldVal)
                          | val > oldVal -> Just ((nr, coeff), val)
                          | otherwise -> a

        choice = foldl' accum Nothing equations

        split eqNo =
            let (before, after) = FingerTree.split (\pos -> lastEqnNumber pos >= eqNo) equations
            in case FingerTree.viewl after of
                 eqn@(Equation eqNo' _) FingerTree.:< after'
                    | eqNo' == eqNo -> Just (before, eqn, after')
                 _ -> Nothing
    in do ((eqNo, coeff), _) <- choice
          (before, eqn, after) <- split eqNo
          pure (before, coeff, eqn, after)

validCorner :: Solution -> Bool
validCorner (Assignments as) = all (\(Column _ a) -> a >= 0) as

solve :: System -> SolveResult
solve system@(System goals equations) =
    trace ("Solving\n" ++ show (prettyPrint system)) $
    let (basis, nonBasis) = basicVariables system
        eqnCount' = eqnCount (measure equations)

        -- Now, using the basis, set all other columns to zero. This
        -- yields one 'feasible solution'.
        --
        -- If it's the best feasible solution, return. Otherwise,
        -- continue the algorithm.
        (solvingVars, forcedNonBasis) = collectBasis eqnCount' basis

        -- To set all other columns to zero is the same as setting
        -- their coefficients to zero, or removing them from all the
        -- equations.
        --
        -- When we do that, we'll be left with one single coefficient
        -- plus a (possible) constant term
        solution = FingerTree.fmap' (removeVars forcedNonBasis . removeVars nonBasis . coefficients) equations

        corner = readSolution solution
    in case FingerTree.viewl goals of
         FingerTree.EmptyL -> Overspecified
         goal FingerTree.:< remainingGoals
             | not (validCorner corner) -> Overspecified
             |  allNonNegative forcedNonBasis (coefficients goal) && allNonNegative nonBasis (coefficients goal) ->
                 Feasible corner
             | otherwise ->
                 fromMaybe Unbounded $ do
                   pivotCol <- selectPivot goal
                   (before, mult, eqn, after) <- trace ("Pivot is " ++ show pivotCol) $ selectPivotRow pivotCol equations

                   let goals' = FingerTree.fmap' (cancel mult pivotCol eqn) goals
                       before' = FingerTree.fmap' (cancel mult pivotCol eqn) before
                       after' = FingerTree.fmap' (cancel mult pivotCol eqn) after

                       system' = System goals' (before' <> FingerTree.singleton eqn <> after')
                   pure (trace ("One solution is " ++ show corner) (solve system'))

cancel :: Double -> ColumnPos -> Equation -> Equation -> Equation
cancel cancelCoeff var (Equation _ toCancel) (Equation no within) =
    Equation no $
    let withCoeff = getCoeff var within
        Term next = cancelCoeff .* Term within .- withCoeff .* Term toCancel

        (before, after) = FingerTree.split (eqnSplitPred var) next
    in case FingerTree.viewl after of
         Column pos' _ FingerTree.:< after'
             | pos' == var -> before <> after'
         _ -> next

readSolution :: FingerTree ColumnIndex (FingerTree ColumnIndex Column)
             -> Solution
readSolution equations =
    Assignments (foldr (mergeEquation (\_ _ -> 0)) mempty (map solve (toList equations)))
    where
      solve row = case FingerTree.viewl row of
                    FingerTree.EmptyL -> mempty
                    Column pos coeff FingerTree.:< row' ->
                        case FingerTree.viewl row' of
                          Column Constant constant FingerTree.:< row''
                              | FingerTree.null row'' ->
                                  FingerTree.singleton (Column pos ((negate constant) / coeff))
                          FingerTree.EmptyL -> FingerTree.singleton (Column pos 0)

-- * Constraints

data SystemState
    = SystemState
    { curEqns :: !(FingerTree EqnPosition Equation)
    , nextGoal, nextConsideration, nextSlack :: !Word
    }

newtype Constraints a
    = Constraints { buildConstraints :: State SystemState a }
      deriving (Monad, Applicative, Functor)

newtype Term = Term (FingerTree ColumnIndex Column)

newtype EqName = EqName Word

(.*) :: Double -> Term -> Term
scale .* Term term =
    Term (FingerTree.fmap' (\(Column p d) -> Column p (d * scale)) term)

infixl 7 .*

(.+), (.-) :: Term -> Term -> Term
Term a .+ Term b =
    Term (mergeEquation (+) a b)
a .- b = a .+ negate_ b

negate_ :: Term -> Term
negate_ (Term e) = Term (FingerTree.fmap' (\(Column p d) -> Column p (-d)) e)

infixl 6 .+, .-

infix 4 ==., <=., >=.

constant :: Double -> Term
constant d = Term (FingerTree.singleton (Column Constant d))

var :: Constraints Term
var = do
  st <- Constraints get

  let varNum = 1 + nextConsideration st

  Constraints (put (st { nextConsideration = varNum }))
  pure (Term (FingerTree.singleton (Column (Consideration varNum) 1)))

slack :: Constraints Term
slack = do
  st <- Constraints get

  let varNum = 1 + nextSlack st

  Constraints (put (st { nextSlack = varNum }))

  pure (Term (FingerTree.singleton (Column (Slack varNum) 1)))


isZero :: Term -> Constraints EqName
isZero (Term term) = do
  st <- Constraints get

  let nextNumber = 1 + lastEqnNumber (measure (curEqns st))

  Constraints $ put st { curEqns = curEqns st FingerTree.|> Equation nextNumber term }

  pure (EqName nextNumber)

(==.) :: Term -> Term -> Constraints EqName
a ==. b = isZero (a .- b)

(<=.) :: Term -> Term -> Constraints EqName
a <=. b = do s <- slack
             isZero (a .- b .+ s)

(>=.) :: Term -> Term -> Constraints EqName
a >=. b = b <=. a

maximize :: Constraints Term -> System
maximize (Constraints mk) =
  let (goal, SystemState eqns _ _ _) = runState mk (SystemState mempty 0 0 0)

      Term goalTerm = negate_ goal
      goals = FingerTree.singleton (Equation 1 (FingerTree.singleton (Column (Solution 1) 1) <> goalTerm))

  in System goals eqns

-- Example programs

maxProfit :: System
maxProfit = maximize $ do
              unitsA <- var
              unitsB <- var

              unitsA .+ unitsB <=. constant 5
              3 .* unitsA .+ 2 .* unitsB <=. constant 12

              pure (6 .* unitsA .+ 5 .* unitsB)

sample1 :: System
sample1 = maximize $ do
            xCab <- var
            yCab <- var

            10 .* xCab .+ 20 .* yCab <=. constant 140
            6 .* xCab .+ 8.* yCab <=. constant 72

            pure (8 .* xCab .+ 12 .* yCab)

-- From https://people.richland.edu/james/ictcm/2006/3dsimplex.html

sample3d :: System
sample3d = maximize $ do
             x1 <- var
             x2 <- var
             x3 <- var

             3 .* x1 .+ 2 .* x2 .+ 5 .* x3 <=. constant 55
             2 .* x1 .+ x2 .+ x3 <=. constant 26
             x1 .+ x2 .+ 3 .* x3 <=. constant 30
             5 .* x1 .+ 2 .* x2 .+ 4 .* x3 <=. constant 57

             return (20 .* x1 .+ 10 .* x2 .+ 15 .* x3)


trivialUnbounded :: System
trivialUnbounded = maximize ((20 .*) <$> var)

trivialMaximize :: System
trivialMaximize = maximize $ do
                    var1 <- var

                    var1 <=. constant 50

                    pure (2 .* var1)

trivialOverSpec :: System
trivialOverSpec = maximize $ do
                    var1 <- var

                    var1 <=. constant 50
                    var1 >=. constant 100

                    pure (2 .* var1)
