module Main (main) where

import           Prelude
import           System.FilePath.Glob (glob)
import           Test.DocTest         (doctest)

main :: IO ()
main = do
  sourceFiles <- glob "src/**/*.hs"
  doctest
    -- NOTE: Keep in sync with package.yaml.
    ( "-XOverloadedStrings"
      : "-XInstanceSigs"
      : "-XDataKinds"
      : "-XScopedTypeVariables"
      : "-XKindSignatures"
      : "-XLambdaCase"
      : "-XRecordWildCards"
      : "-XDeriveAnyClass"
      : "-XDerivingStrategies"
      : "-XDeriveGeneric"
      : "-XFlexibleInstances"
      : "-XTypeApplications"
      : "-XDuplicateRecordFields"
      : "-XOverloadedLabels"
      : "-XTemplateHaskell"
      : "-XTupleSections"
      : "-XNumericUnderscores"
      : "-XTypeOperators"
      : "-XGeneralizedNewtypeDeriving"
      : "-XFlexibleContexts"
      : "-XNamedFieldPuns"
      : sourceFiles
    )
