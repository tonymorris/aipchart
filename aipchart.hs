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
      "erch1.pdf"
    , "erch2.pdf"
    , "erch3.pdf"
    , "erch4.pdf"
    , "erch5.pdf"
    ]
  )

ercl =
  (
    "ercl"
  , [
      "ercl1.pdf"
    , "ercl2.pdf"
    , "ercl3.pdf"
    , "ercl4.pdf"
    , "ercl5.pdf"
    , "ercl6.pdf"
    , "ercl7.pdf"
    , "ercl8.pdf"
    ]
  )

pca =
  (
    "pca"
  , [
      "PCA_back.pdf"
    , "PCA_front.pdf"
    ]
  )

tac =
  (
    "tac"
  , [
      "tac1.pdf"
    , "tac2.pdf"
    , "tac3.pdf"
    , "tac4.pdf"
    , "tac5.pdf"
    , "tac6.pdf"
    , "tac7.pdf"
    , "tac8.pdf"
    ]
  )

vnc =
  (
    "vnc"
  , [
      "Adelaide_VNC.pdf"
    , "Brisbane_VNC.pdf"
    , "Bundaberg_VNC.pdf"
    , "Cairns_VNC.pdf"
    , "Darwin_VNC.pdf"
    , "Hobart_VNC.pdf"
    , "Launceston_VNC.pdf"
    , "Melbourne_VNC.pdf"
    , "Newcastle_VNC.pdf"
    , "Perth_VNC.pdf"
    , "Rockhampton_VNC.pdf"
    , "Sydney_VNC.pdf"
    , "Tindal_VNC.pdf"
    , "Townsville_VNC.pdf"
    ]
  )

vtc =
  (
    "vtc"
  , [
      "Adelaide_VTC.pdf"
    , "Albury_VTC.pdf"
    , "AliceSprings_Uluru_VTC.pdf"
    , "Brisbane_Sunshine_VTC.pdf"
    , "Broome_VTC.pdf"
    , "Cairns_VTC.pdf"
    , "Canberra_VTC.pdf"
    , "Coffs_Harbour_VTC.pdf"
    , "Darwin_VTC.pdf"
    , "Gold_Coast_VTC.pdf"
    , "Hobart_VTC.pdf"
    , "Karratha_VTC.pdf"
    , "Launceston_VTC.pdf"
    , "Mackay_VTC.pdf"
    , "Melbourne_VTC.pdf"
    , "Newcastle_Williamtown_VTC.pdf"
    , "Oakey_Bris_VTC.pdf"
    , "perth_legend.pdf"
    , "Perth_VTC.pdf"
    , "Rockhampton_VTC.pdf"
    , "Sydney_VTC.pdf"
    , "Tamworth_VTC.pdf"
    , "Townsville_VTC.pdf"
    , "Whitsunday_VTC.pdf"
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

-- rawSystem :: String -> [String] -> IO GHC.IO.Exception.ExitCode


{-
  rawSystem'
    (l </> concat [f d, ".get.err"])
    (l </> concat [f d, ".get.out"])
    "wget"
    [
      "--no-check-certificate"
    , "-c"
    , "--show-progress"
    , "--directory-prefix"
    , w
    , baseuri d ++ a
    ])

                -}

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
