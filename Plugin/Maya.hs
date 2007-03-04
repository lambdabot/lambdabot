--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- maya, useful commands for #maya
--

--
-- TEMP:

-- current temperature at UNSW
-- 'temp' is the following script, with line breaks for readability. Should be
-- converted into a MiniHTTP call and a regex:
--

-- PAGE='http://www.bom.gov.au/products/IDN65066.shtml'
-- 
-- w3m -dump_source $PAGE | perl -naWF", " -e '
-- sub f($) {
 --        my $i = shift @_;
--         return ($i == -9999) ? "-" : sprintf "%.1f", $i;
-- }
-- 
-- if (/DATA.*Sydney Airport/) {
-- printf "now %s°, min %s°, max %s°, rain %smm, wind %dkm/h%s\n", 
--         f($F[3]),f($F[11]),f($F[13]),f($F[10]),$F[7], $F[6];
-- exit
-- }'
--
------------------------------------------------------------------------
-- 
-- FORECAST:
--
--PAGE='http://www.bom.gov.au/cgi-bin/wrap_fwo.pl?IDN10064.txt'
--
--w3m -dump_source $PAGE | perl -nW -e '
--if (/Issued at/) {
--        $_ =~ s/ on.*$/./ and print $_;
--        while (<>) {
--                next if (/^\s*$/ or /Precis/ or /Liver|Penrith|Richm/);
--                print;
--                exit if /UV Index/;
--        }
--}' | sed 's/  / /g; s/ *$//' | perl -p -e 's/\n/ / if $. == 1'
--

module Plugin.Maya where

import Plugin
import Control.Monad.Trans      ( liftIO        )

newtype MayaModule = MayaModule ()

theModule :: MODULE
theModule = MODULE $ MayaModule ()

instance Module MayaModule () where
        moduleHelp _ s = case s of
                            "temp"          -> "Local temperature"
                            "forecast"      -> "Local forecast"
                            "ring"          -> "@ring <user>, CSE phonebook"
                            _           -> "Maya module: @temp, @forecast, @ring"

        moduleCmds   _ = ["temp","forecast", "ring"]

        process_ _ "temp" s = 
         if s == "help"
            then return ["  Sydney Ap (http://www.bom.gov.au/images/syd_aws.gif)" ]
            else do (o,_,_) <- liftIO $ popen "/home/dons/bin-pc.i86.linux/temp" [] Nothing
                    return [ "  " ++ o ]

        process_  _  "ring" s =
            do (o,_,_) <- liftIO $ popen "/usr/local/bin/ring" [s] Nothing
               return ["  " ++ o]

        process_ _ "forecast" s =
         if s == "help"
            then return ["  Sydney Metro (http://www.bom.gov.au/cgi-bin/wrap_fwo.pl?IDN10064.txt)" ]
            else do (o,e,_) <- liftIO $ popen "/home/dons/bin-pc.i86.linux/forecast" [s] Nothing
                    liftIO $ print (o,e)
                    let o' = map (\t -> "  "++t) $ lines o
                    return o'
