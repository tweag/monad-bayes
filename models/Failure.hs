{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Bayes.Class (MonadInfer, MonadSample (uniformD, bernoulli), MonadCond (condition))
import Control.Monad (when)
import Control.Monad.Bayes.Enumerator
-- Here, `ExceptT` is on top of the probability monad `m`, which allows me to throw errors. The result is that I can write a version of `condition` which results in a safe failure when I condition on something that is always false. 

model :: (MonadError String m, MonadInfer m) => m Int
model = do
    x <- uniformD [0..10]
    conditionAllowingForFailure (x < 0)
    return x

conditionAllowingForFailure :: (MonadSample m, MonadError [Char] m, MonadCond m) 
    => Bool -> m ()
conditionAllowingForFailure b = do 
    fail <- bernoulli 0.1
    when fail $ throwError "fail"
    condition b

run :: [(Either String Int, Double)]
run = enumerate $ runExceptT model
-- >>> main
-- [(Left "fail",1.0),(Right 0,0.0),(Right 1,0.0),(Right 2,0.0),(Right 3,0.0),(Right 4,0.0),(Right 5,0.0),(Right 6,0.0),(Right 7,0.0),(Right 8,0.0),(Right 9,0.0),(Right 10,0.0)]

