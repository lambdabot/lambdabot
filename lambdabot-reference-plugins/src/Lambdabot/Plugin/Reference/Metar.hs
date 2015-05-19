-- | Look up METAR weather records.
--
-- Copyright (c) 2014 Bertram Felgenhauer <int-e@gmx.de>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

module Lambdabot.Plugin.Reference.Metar (metarPlugin) where

import Lambdabot.Plugin
import Lambdabot.Util.Browser (browseLB)

import Network.Browser (request)
import Network.HTTP (getRequest, rspCode, rspBody)
import Data.Char (isAlpha, toUpper)

metarPlugin :: Module ()
metarPlugin = newModule
    { moduleCmds = return
        [ (command "metar")
            { help = say "metar <ICAO airport code>\n\
                         \Look up METAR weather data for given airport."
            , process = doMetar
            }
        ]
    }

addsUri :: String
addsUri =
    "http://www.aviationweather.gov/adds/dataserver_current/httpparam"

addsSrc :: String -> String
addsSrc code = addsUri ++
    "?dataSource=metars&requestType=retrieve&format=csv&hoursBeforeNow=2\
    \&mostRecentForEachStation=true&stationString=" ++ code

doMetar :: MonadLB m => String -> Cmd m ()
doMetar code | length code == 4 && all isAlpha code = do
    msg <- browseLB $ do
        let src = addsSrc (map toUpper code)
        (uri, resp) <- request $ getRequest src
        case rspCode resp of
            (2,_,_) -> return $ extractMetar (rspBody resp)
            _ -> return $ "Request failed."
    say msg
doMetar _ = return ()

extractMetar :: String -> String
extractMetar body = case lines body of
    ls@("No errors" : _) -> case takeWhile (/= ',') (last ls) of
        "raw_text" -> "No result."
        l          -> l
    _ -> "Request failed."
