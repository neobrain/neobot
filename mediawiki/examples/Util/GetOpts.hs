-- |
--
-- Module      : Util.GetOpts
-- Copyright   : (c) 2004-2008, Sigbjorn Finne.
--
-- Maintainer  : sof@forkIO.com
-- Stability   : provisional
-- Portability : portable
--
-- Accumulator-style command-line option parsing; minor extension
-- of @System.Console.GetOpt@ (whose interface this module re-exports).
--
-- So, to correctly attribute the source of the bulk of the functionality
-- provided, here's its module header:
--
-- Module      :  System.Console.GetOpt
-- Copyright   :  (c) Sven Panne Oct. 1996 (small changes Dec. 1997)
-- License     :  BSD-style (see the file libraries\/core\/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A Haskell port of GNU's getopt library
--

module Util.GetOpts
    ( module System.Console.GetOpt
    , getOpt2           -- :: ArgOrder (a->a)
                        -- -> [OptDescr (a->a)]
                        -- -> [String]
                        -- -> a
                        -- -> (a,[String],[String])
    , usageInfo2        -- :: String -> [OptDescr a] -> String

    , parseOptions      -- :: [OptDescr (a -> a)]
                        -- :: a
			-- -> [String]
			-- -> (a, [String], [String])
    , processOptions    -- :: [OptDescr (a -> a)]
                        -- -> a
	                -- -> IO (a, [String])
    ) where

import Prelude
import Data.List        ( isPrefixOf )
import Text.PrettyPrint
import System.Console.GetOpt

import System.IO
import System.Environment
import System.Exit

import Control.Monad ( when )

processOptions :: [OptDescr (a -> a)]
               -> a
	       -> IO (a, [String])
processOptions descriptors nullVal = do
  ls <- getArgs
  let (opts, ws, es) = parseOptions descriptors nullVal ls
  when (not $ null es) $ do
    hPutStrLn stderr (unlines es)
    hPutStrLn stderr ("(try '--help' for list of options supported.)")
    exitFailure
  return (opts, ws)

parseOptions :: [OptDescr (a -> a)] -> a -> [String] -> (a, [String], [String])
parseOptions descriptors nullVal argv = getOpt2 Permute descriptors argv nullVal

{- |
 The @getOpt@ provided @System.Console.GetOpt@ returns a list of
 values representing the options found present in an @argv@-vector.

 @getOpt2@ offers a different view -- the options are represented
 by a single value instead, i.e., you define a labelled field record
 type representing the options supported, and have the OptDescr
 elements just update the contents of that record. This has proven
 to be quite a bit less cumbersome to work with, leaving you at
 the end with a single value (the record) packaging up all the
 options setting.

 With @getOpt@, you normally end up (essentially) having to
 write a sub parser of the list of values returned, which just
 adds even more bulk to your code, for very little benefit.
-}
getOpt2 :: ArgOrder (a -> a)          -- non-option handling
        -> [OptDescr (a -> a)]        -- option descriptors
        -> [String]                   -- the commandline arguments
        -> a                          -- initial state
        -> (a,[String],[String])      -- (options,non-options,error messages)
getOpt2 _        _        []         st = (st,[],[])
getOpt2 ordering optDescr (arg:args) st = procNextOpt opt ordering
   where procNextOpt (Opt f)    _                 = (f st',xs,es)
         procNextOpt (NonOpt x) RequireOrder      = (st,x:rest,[])
         procNextOpt (NonOpt x) Permute           = (st',x:xs,es)
         procNextOpt (NonOpt x) (ReturnInOrder f) = (f x st', xs,es)
         procNextOpt EndOfOpts  RequireOrder      = (st',rest,[])
         procNextOpt EndOfOpts  Permute           = (st',rest,[])
         procNextOpt EndOfOpts  (ReturnInOrder f) = (foldr f st' rest,[],[])
         procNextOpt (OptErr e) _                 = (st',xs,e:es)

         (opt,rest)  = getNext arg args optDescr
         (st',xs,es) = getOpt2 ordering optDescr rest st

-- getOpt2 supporting code - copied verbatim from System.Console.GetOpt
-- as it isn't exported.

data OptKind a                -- kind of cmd line arg (internal use only):
   = Opt       a                --    an option
   | NonOpt    String           --    a non-option
   | EndOfOpts                  --    end-of-options marker (i.e. "--")
   | OptErr    String           --    something went wrong...

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
getNext ('-':'-':[]) rest _        = (EndOfOpts,rest)
getNext ('-':'-':xs) rest optDescr = longOpt xs rest optDescr
getNext ('-': x :xs) rest optDescr = shortOpt x xs rest optDescr
getNext a            rest _        = (NonOpt a,rest)

-- handle long option
longOpt :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
longOpt ls rs optDescr = long ads arg rs
   where (opt,arg) = break (=='=') ls
         options   = [ o  | o@(Option _ xs _ _) <- optDescr,
                            l <- xs,
                            opt `isPrefixOf` l
                     ]
         ads       = [ ad | Option _ _ ad _ <- options ]
         optStr    = ("--"++opt)

         long (_:_:_)      _        rest     = (errAmbig options optStr,rest)
         long [NoArg  a  ] []       rest     = (Opt a,rest)
         long [NoArg  _  ] ('=':_)  rest     = (errNoArg optStr,rest)
         long [ReqArg _ d] []       []       = (errReq d optStr,[])
         long [ReqArg f _] []       (r:rest) = (Opt (f r),rest)
         long [ReqArg f _] ('=':xs) rest     = (Opt (f xs),rest)
         long [OptArg f _] []       rest     = (Opt (f Nothing),rest)
         long [OptArg f _] ('=':xs) rest     = (Opt (f (Just xs)),rest)
         long _            _        rest     = (errUnrec optStr,rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a,[String])
shortOpt x ys rest1 optDescr = short ads ys rest1
  where options = [ o  | o@(Option ss _ _ _) <- optDescr, s <- ss, x == s ]
        ads     = [ ad | Option _ _ ad _ <- options ]
        optStr  = '-':[x]

        short (_:_:_)        _  rest     = (errAmbig options optStr,rest)
        short (NoArg  a  :_) [] rest     = (Opt a,rest)
        short (NoArg  a  :_) xs rest     = (Opt a,('-':xs):rest)
        short (ReqArg _ d:_) [] []       = (errReq d optStr,[])
        short (ReqArg f _:_) [] (r:rest) = (Opt (f r),rest)
        short (ReqArg f _:_) xs rest     = (Opt (f xs),rest)
        short (OptArg f _:_) [] rest     = (Opt (f Nothing),rest)
        short (OptArg f _:_) xs rest     = (Opt (f (Just xs)),rest)
        short []             [] rest     = (errUnrec optStr,rest)
        short []             xs rest     = (errUnrec optStr,('-':xs):rest)

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr =
  OptErr $ usageInfo header ods
  where header = "option '" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr =
  OptErr $ "option '" ++ optStr ++ "' requires an argument " ++ d

errUnrec :: String -> OptKind a
errUnrec optStr =
  OptErr $ "unrecognized option '" ++ optStr ++ "'"

errNoArg :: String -> OptKind a
errNoArg optStr =
  OptErr $ "option '" ++ optStr ++ "' doesn't allow an argument"

usageInfo2 :: String               -- header
           -> [OptDescr a]         -- option descriptors
           -> String               -- nicely formatted decription of options
usageInfo2 header optDescr = render $ vcat $ text header : table
  where
    (ss, ls, descs) = unzip3 $ map fmtOpt optDescr
    table           =
        let (c1, ssd) = sameLen ss
            (c2, lsd) = sameLen ls
        in
            zipWith3 (paste (74 - c1 - c2)) ssd lsd descs
    paste c3 x y z = nest 2 $ x <+> space <> y <> space <+> fitText c3 z
    sameLen xs     =
        let width = maximum $ map length xs
        in
            (width, flushLeft width xs)
    flushLeft n xs = [ text $ take n (x ++ repeat ' ') | x <- xs ]

    fmtOpt :: OptDescr a -> (String, String, String)
    fmtOpt (Option sos los argdesc descr) =
      ( render $ hcat $ punctuate comma $ map (fmtShort argdesc) sos
      , render $ hcat $ punctuate comma $ map (fmtLong  argdesc) los
      , descr
      )
      where
        fmtShort :: ArgDescr a -> Char -> Doc
        fmtShort (NoArg  _   ) so = char '-' <> char so
        fmtShort (ReqArg _ ad) so = char '-' <> char so <+> text ad
        fmtShort (OptArg _ ad) so = char '-' <> char so <+> brackets (text ad)

        fmtLong :: ArgDescr a -> String -> Doc
        fmtLong (NoArg  _   ) lo = text "--" <> text lo
        fmtLong (ReqArg _ ad) lo = text "--" <> text lo <> equals <> text ad
        fmtLong (OptArg _ ad) lo = text "--" <> text lo <> equals <>
                                   brackets (text ad)

-- | Simple line formatting of a paragraph, introducing line breaks
-- whenever length exceeds that of the first argument.
fitText :: Int -> String -> Doc
fitText width s =
   vcat $ map (hsep . (map text))
              (fit 0 [] (words s))
  where
     -- fill up lines left-to-right, introducing line breaks
     -- whenever line length exceeds 'width'.
    fit :: Int -> [String] -> [String] -> [[String]]
    fit _ acc [] = [reverse acc]
    fit n acc (w:ws)
     | (n + l) >= width = (reverse (w:acc)) : fit 0 [] ws
     | otherwise        = fit (n+l+1{-word space-}) (w:acc) ws
     where
      l = length w
