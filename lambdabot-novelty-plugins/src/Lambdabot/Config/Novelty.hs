{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Novelty
    ( bfBinary
    , unlambdaBinary
    ) where

import Lambdabot.Config

config "bfBinary"       [t| String |] [| "bf"       |]
config "unlambdaBinary" [t| String |] [| "unlambda" |]
