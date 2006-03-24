--
-- | Support for quotes
--
module Plugins.Quote (theModule) where

import Plugins.Quote.Fortune      (randFortune)
import Lambdabot
import Util
import qualified Data.FastPackedString as P
import LBState
import qualified Data.Map as M
import Serial
import Data.Maybe

import Control.Monad.Trans      (liftIO)

------------------------------------------------------------------------
newtype QuoteModule = QuoteModule ()

theModule :: MODULE
theModule = MODULE $ QuoteModule ()

type Quotes = M.Map P.FastString [P.FastString]

instance Module QuoteModule Quotes where
    moduleCmds           _ = ["quote", "remember", "ghc", "fortune","yow","arr","keal","b52s"]

    moduleHelp _ "fortune" = "Provide a random fortune"
    moduleHelp _ "yow"     = "Yow!"
    moduleHelp _ "arr"     = "Talk to a pirate"
    moduleHelp _ "keal"    = "Talk like Keal"
    moduleHelp _ "ghc"     = "GHC!"
    moduleHelp _ "b52s"    = "Anyone noticed the b52s sound a lot like zippy?"
    moduleHelp _ _          = help

    moduleSerialize _       = Just mapListPackedSerial
    moduleDefState  _       = return M.empty

    process_ _ cmd s = case cmd of
          "remember" -> runRemember (dropSpace s)
          "quote"    -> runQuote    (dropSpace s)
          "ghc"      -> runQuote    "ghc"
          "fortune"  -> return `fmap` liftIO (randFortune Nothing)
          "yow"      -> return `fmap` liftIO (randFortune (Just "zippy"))
          "keal"     -> return `fmap` liftIO kealRandom
          "arr"      -> return `fmap` liftIO arrRandom
          "b52s"     -> return `fmap` liftIO b52sRandom

help :: String
help = "@quote [nick]/@remember [nick] [quote]\n" ++
   "Quote somebody, a random person, or save a memorable quote"

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

-- error handling!
runRemember :: String -> ModuleLB Quotes
runRemember str = do
    case break (== ' ') str of
        (_,[])    -> return ["Incorrect arguments to quote"]
        (name,q') -> do let q = tail q'
                        withMS $ \fm writer -> do
                        let ss  = fromMaybe [] (M.lookup (P.pack name) fm)
                            fm' = M.insert (P.pack name) (P.pack q : ss) fm
                        writer fm'
                        return ["Done."]

--
--  the @quote command, takes a user name to choose a random quote from
--
runQuote :: String -> ModuleLB Quotes
runQuote name = do
    fm <- readMS
    if M.null fm then return ["No quotes yet."] else do

        let pnm = P.pack name
            qs' = M.lookup pnm fm

        (nm,qs) <- if not (P.null pnm)
                   then return (pnm,qs') -- (FastString, Maybe [FastString])
                   else do (nm',rs') <- liftIO $ stdGetRandItem (M.toList fm) -- random person
                           return (nm', Just rs')
        case qs of
            Nothing   -> return [P.unpack nm ++ " hasn't said anything memorable"]
            Just msgs -> do msg <- liftIO $ stdGetRandItem msgs
                            return $ if not (P.null pnm)
                                then ["  " ++ (P.unpack msg)]
                                else [(P.unpack nm)++" says: " ++ (P.unpack msg)]

------------------------------------------------------------------------

-- | Return a random arr-quote
arrRandom :: IO String
arrRandom = Util.stdGetRandItem arrList

kealRandom :: IO String
kealRandom = Util.stdGetRandItem kealList

b52sRandom :: IO String
b52sRandom = Util.stdGetRandItem b52s

------------------------------------------------------------------------

-- | Some pirate quotes
arrList :: [String]
arrList = 
    ["Avast!"
    ,"Shiver me timbers!"
    ,"Yeh scurvy dog..."
    ,"I heard andersca is a pirate"
    ,"I'll keel haul ya fer that!"
    ,"I'd like to drop me anchor in her lagoon"
    ,"Well me 'earties, let's see what crawled out of the bung hole..."
    ,"I want me grog!"
    ,"Drink up, me 'earties"
    ,"Is that a hornpipe in yer pocket, or arr ya just happy ta see me?"
    ,"Get out of me way, yeh landlubber"
    ,"Smartly me lass"
    ,"Arrr!"
    ,"Ahoy mateys"
    ,"Aye"
    ,"Aye Aye Cap'n"
    ,"This is the END for you, you gutter-crawling cur!"
    ,"May the clap make ye incapable of Cracking Jenny's Tea Cup."
    ,"Eat maggoty hardtack, ye unkempt, jenny frequentin', son of a gun."
    ,"Swab the deck!"
    ,"Keelhaul the swabs!"
    ,"Yo ho ho, and a bottle of rum!"
    ,"I'll crush ye barnacles!"
    ,"Har de har har!"
    ]

--
-- Actual quotes from an asshat called Keal over Jan 12-14 2006.
--
kealList :: [String]
kealList =
    ["endian mirrors the decimal"
    ,"primary elemental assumption of integer coefficients to roots in counting sytem is wrong"
    ,"actually it bug in math"
    ,"b*(Floor[v/b^p]/b-Floor[Floor[v/b^p]/b])"
    ,"proofs are no longer sound"
    ,"my proof show math is broken right now"
    ,"doctor just give meds not fix prollem"
    ,"Keal was so happy with T, coded in basic so run on anything, and does lot"
    ,"one prollem. T broke confines of the visual basic langage and would not compile"
    ,"perhaps i just genius and never tested"
    ,"and yes that was with zero formal training in all realms"
    ,"somone would expect that trees 500gb hdds of expressions as if they were floppy dicks"
    ,"can you make a macro that builds the expression accoridng to a genetic algorithm where you decide what is good and what is bad?"
    ,"T could perform expressions 600mb and bigger"
    ,"what is the max amount of operands haskell can handle in a single expression?"
    ,"T seems to be haskell, except with a decent interface at this point"
    ,"love a black and white lower 128 from 32 up of ascii glyphs?"
    ,"evaluating expressions is ALL haskell does?????"
    ,"you think i am one of them persnipity uppity men are pig lesbian mathematicians?"
    ,"how bout i say ick no unicorn and daisy loving girl mathematician will ever enjoy this"
    ,"better be atleast 16x16 color with extended ascii set"
    ,"what the hell does Prelude > mean?"
    ,"how do i search for someone saying 'Keal' in mirc"
    ,"i have basically written a proof that shows an assumption is wrong"
    ,"they dumbified you"
    ,"antiparsimony were 100% correct..."
    ,"its because the timeline diverges and past events themselves unhappen"
    ,"all i know is i have experienced my own death unhappening..."
    ," what have you been smoking? you narrow minded Haskell user?"
    ,"i use an 8088"
    ,"my very first computer was an 80-0840"
    ,"it is very easy to go off topic"
    ,"someone needs to write a boids for haskell that emulates humans going on and off topic"
    ,"i just got banned from math because i not have good ability to convey thoughts"
    ,"i lack in verbal and social expression"
    ,"i try make program called Glyph to do it but my script lang called T too slow. i invent T"
    ,"can GMP support KealDigit? I invent KealDigit"
    ,"with KealDigit quantum crackproof encryption possible"
    ,"i show how spell triangle in less than three corners using darkmanifold"
    ,"can haskell pipe the raw irrational megaequation into an analog device"
    ,"the fractal is 5 irrationals"
    ,"99% of my book has been erased by faulty hdd's"
    ,"last day i was in my lab i had a diagram which might have removed pi"
    ,"i only trust opensource tools. where can i download haskell for windows?"
    ,"obviously you never heard of Tier. theoretically it would work using nanobots"
    ,"you need a Zh function in Haskell"
    ,"can haskell compile flash animations and java apps?"
    ,"i need math friendly compiler to compile for jvm or flash"
    ,"Cale etc already pointed out Haskell is puny to nothing to emulate using my barrage of mathematic theories"
    ,"i prove infinity never ends in both directions"
    ,"are you saying i am MegaMonad?"
    ,"Keal angry @ dons"
    ,"i can explain why something is without knowing what the rules decided by man are"
    ,"making a bot of me is highly offensive"
    ,"just seeing how offtopic i could get everyone"
    ,"intuitive != imperative"
    ,"doubles and floats cause b*(Floor[v/b^p]/b-Floor[Floor[v/b^p]/b) to fuck up"
    ,"what are epsilons?"
    ,"haskell always said undefined"
    ,"bot seems useless"
    ,"when i put what i dat recoved from that tile into a ti92. the damn thing blew up"
    ,"i think it because mathematics damage you cpu"
    ,"ithink has to do with hardcased government failsafe in chip"
    ,"i suggest you tear apart a 20q and plug it with the alg"
    ,"nsa has all the profiling info you need to come up with the correct survey answers"
    ,"write an algorthim that generates the correct responses for a phone survey based on number of rings whether answered how quickly hung up on and the mood of the receiver"
    ,"where can i find opensource schematics of Linus Torvalds' x86 clone?"
    ,"need to plan a fieldtrip to Frederick B. Mancoff of Freescale Semiconductor"
    ,"ghc need to have plugin that allow copy paste in xp"
    ,"know you know this 24 periods Keal SecretTM"
    ,"tomorrow i share next mathematical secrety"
    ,"nsa prevent me from returning to math on efnet"
    ,"nsa try kill me numerous times"
    ,"#haskell needs to take its meds"
    ,"i think i know what code does but code looks to simple to actually do it"
    ,"need 1 to do a while 0 does !a. need 1 to do a while 0 does !a"
    ,"will it return [] if map gives fpu infinite list?"
    ,"today's 24hour project was supposed to be logical overloading using plegm method"
    ,"there is no way to prove the failsafe exists"
    ,"oh btw my fpu is blown due to a hardcased failsafe i have 3 year warranty right. and then they call fads"
    ,"i aint running that on my puter"
    ,"lamadabot took 5 to 8 whole seconds to return []"
    ,"bot defective"
    ,"i changed my user od"
    ,"i cant think anymore"
    ,"i want to invent white dye"
    ,"pork steaks taste like dick"
    ,"i dont really eat vegetables unless cheese is a vegetable"
    ,"the [nsa] even make light green both ways once"
    ]

--
-- Quotes from the lyrics of B52s songs. They remind me (dons) of zippy.
-- 
b52s :: [String]
b52s = 
    [ "His ear lobe fell in the deep. Someone reached in and grabbed it. It was a rock lobster!"
    , "Watch out for that piranha. There goes a narwhale. HERE COMES A BIKINI WHALE!"
    , "She drove a Plymouth Satellite faster than the speed of light!"
    , "Some say she's from Mars, or one of the seven stars that shine after 3:30 in the morning. WELL SHE ISN'T."
    , "It's a dreary downtown day, but at the end of my 40 foot leash is my little friend quiche."
    , "Girl from Ipanema, she goes to Greenland"
    , "Hot pants explosion at the factory!"
    , "You belong in Ripley's Believe It Or Not"
    ]
