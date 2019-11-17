{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, FlexibleInstances,
   UndecidableInstances #-}

module IHaskell.Eval.PtGhci where

import           IHaskellPrelude
import Control.Monad.Reader
import Control.Monad.Catch (handle)
import Language.Haskell.PtGhci.Ghci
import Language.Haskell.PtGhci.Engine
import Language.Haskell.PtGhci.PtgResponse (PtgResponse(..))
import IHaskell.Types
import IHaskell.Display
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS

instance {-# OVERLAPS #-} Show a => IHaskellDisplay a where
  display s = return $ Display [DisplayData PlainText (T.pack $ show s)]

ihaskellDisplay :: IHaskellDisplay a => a -> IO ()
ihaskellDisplay val = do
  disp <- display val
  BS.putStrLn $ BSL.toStrict $ A.encode disp

evalToResult :: (MonadReader Ghci m, MonadIO m)
             => String 
             -> (EvaluationResult -> ErrorOccurred -> IO ())
             -> m EvaluationResult
evalToResult code output = do
  ghci <- ask
  -- _ <- liftIO $ execStream ghci ":set -interactive-print=IHaskell.Eval.PtGhci.ihaskellDisplay"
  (out, err) <- liftIO $ runMultiline ghci $ T.pack code
  case A.decode (BSL.fromStrict $ T.encodeUtf8 $ T.unlines out) of
    Just disp -> return $ FinalResult disp [] []
    Nothing -> do
      let allOut = T.unlines $ out++err
      return $ FinalResult (Display [DisplayData PlainText allOut]) [] []

