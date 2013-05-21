{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Novelty
    ( bfBinary
    , unlambdaBinary
    ) where

import Lambdabot.Config

config "bfBinary"       [t| String |] [| "bf"       |]
config "unlambdaBinary" [t| String |] [| "unlambda" |]
