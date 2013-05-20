--
-- Provides interface to messages, message pipes
--
module Lambdabot.Message
    ( Message(..)
    ) where

import Lambdabot.Nick

-- TODO: probably remove "Show a" later (used only to implement @echo)
class Show a => Message a where
    -- | extracts the tag of the server involved in a given message
    server      :: a -> String

    -- | extracts the nickname involved in a given message.
    nick        :: a -> Nick

    -- | 'fullName' extracts the full user name involved in a given message.
    fullName    :: a -> String

    -- | 'channels' extracts the channels a Message operate on.
    channels    :: a -> [Nick]

    -- TODO: there must be a better way of handling this ...
    lambdabotName :: a -> Nick
