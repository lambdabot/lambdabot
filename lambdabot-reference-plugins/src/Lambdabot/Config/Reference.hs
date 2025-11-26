{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Reference
    ( proxy
    , aspellBinary
    ) where

import Lambdabot.Config
import Network.HTTP.Client (ProxyOverride, defaultProxy)

config "proxy"              [t| ProxyOverride           |] [| defaultProxy  |]
config "aspellBinary"       [t| String                  |] [| "aspell"      |]
