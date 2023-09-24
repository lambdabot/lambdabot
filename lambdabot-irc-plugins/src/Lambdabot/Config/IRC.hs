{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.IRC
    ( reconnectDelay
    ) where

import Lambdabot.Config

config "reconnectDelay" [t| Int |] [| 10000000 |]
