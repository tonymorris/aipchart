#!/usr/bin/env runhaskell

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

erch =
  (
    "erch"
  , [
      "erch1"
    , "erch2"
    , "erch3"
    , "erch4"
    , "erch5"
    ]
  )

ercl =
  (
    "ercl"
  , [
      "ercl1"
    , "ercl2"
    , "ercl3"
    , "ercl4"
    , "ercl5"
    , "ercl6"
    , "ercl7"
    , "ercl8"
    ]
  )

pca =
  (
    "pca"
  , [
      "PCA_back"
    , "PCA_front"
    ]
  )

tac =
  (
    "tac"
  , [
      "tac1"
    , "tac2"
    , "tac3"
    , "tac4"
    , "tac5"
    , "tac6"
    , "tac7"
    , "tac8"
    ]
  )

vnc =
  (
    "vnc"
  , [
      "Adelaide_VNC"
    , "Brisbane_VNC"
    , "Bundaberg_VNC"
    , "Cairns_VNC"
    , "Darwin_VNC"
    , "Hobart_VNC"
    , "Launceston_VNC"
    , "Melbourne_VNC"
    , "Newcastle_VNC"
    , "Perth_VNC"
    , "Rockhampton_VNC"
    , "Sydney_VNC"
    , "Tindal_VNC"
    , "Townsville_VNC"
    ]
  )

vtc =
  (
    "vtc"
  , [
      "Adelaide_VTC"
    , "Albury_VTC"
    , "AliceSprings_Uluru_VTC"
    , "Brisbane_Sunshine_VTC"
    , "Broome_VTC"
    , "Cairns_VTC"
    , "Canberra_VTC"
    , "Coffs_Harbour_VTC"
    , "Darwin_VTC"
    , "Gold_Coast_VTC"
    , "Hobart_VTC"
    , "Karratha_VTC"
    , "Launceston_VTC"
    , "Mackay_VTC"
    , "Melbourne_VTC"
    , "Newcastle_Williamtown_VTC"
    , "Oakey_Bris_VTC"
    , "perth_legend"
    , "Perth_VTC"
    , "Rockhampton_VTC"
    , "Sydney_VTC"
    , "Tamworth_VTC"
    , "Townsville_VTC"
    , "Whitsunday_VTC"
    ]
  )

aipchart =
  [
    erch
  , ercl
  , pca
  , tac
  , vnc
  , vtc
  ]

(>.>) ::
  Monad f =>
  f ExitCode
  -> f ExitCode
  -> f ExitCode
a >.> b =
  do  r <- a
      case r of
        ExitFailure n ->
          pure (ExitFailure n)
        ExitSuccess ->
          b

traverseExitCode ::
  (Foldable t, Monad f) =>
  (a -> f ExitCode)
  -> t a
  -> f ExitCode
traverseExitCode f =
  foldr ((>.>) . f) (pure ExitSuccess)

baseuri ::
  String
baseuri =
  "http://www.airservicesaustralia.com/aip/current/aipchart/"

wgetaipchart :: 
  String
  -> String
  -> String
  -> IO ExitCode
wgetaipchart d x y =
  let u = concat
            [
              baseuri
            , "/"
            , x
            , "/"
            , y
            , ".pdf"
            ]
  in  do  mapM_ (createDirectoryIfMissing True) [d, d </> x]
          rawSystem
            "wget"
            [
              "-c"
            , "--show-progress"
            , "--directory-prefix"
            , d </> x
            , u
            ]

wgetaipcharts :: 
  String
  -> String
  -> [String]
  -> IO ExitCode
wgetaipcharts d =
  traverseExitCode . wgetaipchart d

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        [] ->
          hPutStrLn stderr "pass the base directory as an argument"
        h:_ ->
          do  e <- traverseExitCode (uncurry (wgetaipcharts h)) aipchart
              exitWith e
