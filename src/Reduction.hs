{-# LANGUAGE Rank2Types, ConstraintKinds, FlexibleContexts #-}
module Reduction where

import HERMIT.Core (CoreDef(..), Crumb)
import HERMIT.Dictionary hiding (externals)
import HERMIT.External (External,external)
import HERMIT.Context
import HERMIT.GHC
import HERMIT.Kure
import HERMIT.Name
import System.IO.Unsafe
import HERMIT.Plugin (hermitPlugin,pass,interactive)
import HERMIT.External (External,Extern(..),external,ExternalName)
import Control.Arrow
--- we want to specialize and inline everything with arrow type

plugin :: Plugin
plugin = hermitPlugin (pass 0 . interactive externals)

externals :: [External]
externals =
  [ externC "simplifyAll" simplifyAll "Bash with normalization simplifier"
  , externC "find-type" (findTypePath :: HermitName -> TransformH LCoreTC LocalPathH) "Find the first occurrence of given type name"
  , externC "find-appty" (findAppTypePath :: HermitName -> TransformH LCoreTC LocalPathH) "Find the first occurrence of given type name"
  ]

externC name rew help = external name rew [help]

findTypePath name = oneNonEmptyPathToT $ do
    n <- findTypeT name
    dynFlags <- dynFlagsT
    return $ (unsafePerformIO $ print $ (name, showPpr dynFlags n)) `seq` True

findAppTypePath name = oneNonEmptyPathToT $ promoteExprT $ arr (matchAppType name)

-- getTyConApp (Type (TyConApp v _)) = Just (getName v)
-- getTyConApp _ = Nothing

matchAppType nm (App _ (Type (TyConApp v _))) = cmpHN2Name nm (getName v)
matchAppType nm (App _ (Type (TyVarTy v))) = cmpHN2Name nm (getName v)
matchAppType nm _ = False

simplifyAll :: RewriteH LCore
simplifyAll = bashWith mySimplifiers

bashWith = \ rs -> bashUsingR $ map fst (rs ++ bashSimplifiers)

nowatchR name rule = (promoteExprR rule, name)

-- From bashComponents.
mySimplifiers, bashSimplifiers 
  :: ( AddBindings c, ExtendPath c Crumb, HasEmptyContext c, ReadBindings c, ReadPath c Crumb
                  , MonadCatch m, MonadUnique m )
               => [(Rewrite c m LCore, String)]
bashSimplifiers =
  [ nowatchR "betaReduceR" betaReduceR
  , nowatchR "(caseReduceR True)" (caseReduceR True)
  , nowatchR "(caseReduceUnfoldR True)" (caseReduceUnfoldR True)
  , nowatchR "caseElimSeqR" caseElimSeqR
  , nowatchR "unfoldBasicCombinatorR" unfoldBasicCombinatorR
  , nowatchR "inlineCaseAlternativeR" inlineCaseAlternativeR
  , nowatchR "etaReduceR" etaReduceR
  -- letNonRecSubstSafeR was replicating some monomorphic method
  -- specializations.
  -- , nowatchR "letNonRecSubstSafeR" letNonRecSubstSafeR
  , nowatchR "caseFloatAppR" caseFloatAppR
  , nowatchR "caseFloatCaseR" caseFloatCaseR
  , nowatchR "caseFloatLetR" caseFloatLetR
  , nowatchR "caseFloatCastR" caseFloatCastR  -- Watch this one
--  , nowatchR "letFloatAppR" letFloatAppR
  , nowatchR "letFloatArgR" letFloatArgR
--  , nowatchR "letFloatArgNoDelayR" letFloatArgNoDelayR
  , nowatchR "letFloatLamR" letFloatLamR
  , nowatchR "letFloatLetR" letFloatLetR
  , nowatchR "letFloatCaseR" letFloatCaseR
  , nowatchR "letFloatCastR" letFloatCastR
  , nowatchR "castElimReflR" castElimReflR
  , nowatchR "castElimSymR" castElimSymR
  ]

mySimplifiers = []
