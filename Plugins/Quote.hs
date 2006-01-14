--
-- | Support for quotes
--
module Plugins.Quote (theModule) where

import Plugins.Quote.Fortune      (randFortune)
import Lambdabot
import Util                     (stdGetRandItem)

import Control.Monad.Trans      (liftIO)

------------------------------------------------------------------------
newtype QuoteModule = QuoteModule ()

theModule :: MODULE
theModule = MODULE $ QuoteModule ()

instance Module QuoteModule () where
    moduleCmds           _ = ["fortune","yow","arr","keal"]
    moduleHelp _ "fortune" = "Provide a random fortune"
    moduleHelp _ "yow"     = "Yow!"
    moduleHelp _ "arr"     = "Talk to a pirate"
    moduleHelp _ "keal"    = "Talk like Keal"

    process_ _ cmd _ = do
       quote <- liftIO $ case cmd of
                  "fortune" -> randFortune Nothing
                  "yow"     -> randFortune (Just "zippy")
                  "keal"    -> kealRandom
                  "arr"     -> arrRandom
                  _ -> error "QuoteModule: bad string"
       return [quote]

------------------------------------------------------------------------

-- | Return a random arr-quote
arrRandom :: IO String
arrRandom = Util.stdGetRandItem arrList

kealRandom :: IO String
kealRandom = Util.stdGetRandItem kealList

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
    ]
