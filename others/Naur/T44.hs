-- t44.hs: SuDoku reduction execution
-- 2005-Oct-26 / TN

module Main where

  import System.IO
  import System.CPUTime
  import System.Environment

  import SuDoku

  processSuDokuReduction :: String -> String -> IO ()
  processSuDokuReduction process_name result
    = do
        cpuStart <- getCPUTime
        putStr ( process_name ++ ": " ++ result ++ "\n" )
        hFlush stdout
        cpuEnd <- getCPUTime
        putStr ( process_name ++ ": CPU Used: " ++
          show ( ((fromIntegral (cpuEnd - cpuStart))::Double)/10^12 ) ++ "\n" )

  progName :: String
  progName = "SdkMSolv"

  data Args
    = ArgElements
        deriving ( Enum )

  main :: IO ()
  main
    = do
        putStr "t44: 2005-Oct-26 20.24\n";
        args<-getArgs
        let
          elements = args!!(fromEnum ArgElements)
          allElementListSets = sdkAllElementListSets elements
          sensible = filter sdkSensible allElementListSets
          reductionsDifferent = filter (not . sdkReductionsIdentical) allElementListSets
          interesting1 = filter sdkSensible reductionsDifferent
          interesting2 = filter (not . sdkReductionsIdentical) sensible
          in
            do
              -- processSuDokuReduction
                -- ( "sdkAllElementListSets " ++ show elements )
                -- ( show ( sdkAllElementListSets elements ) )
              -- processSuDokuReduction
                -- ( "length ( sdkAllElementListSets " ++ show elements ++ " )" )
                -- ( show ( genericLength ( sdkAllElementListSets elements ) ) )
              -- processSuDokuReduction
                -- "length sensible"
                -- ( show ( genericLength sensible ) )
              -- processSuDokuReduction
                -- "length reductionsDifferent"
                -- ( show ( genericLength reductionsDifferent ) )
              -- processSuDokuReduction
                -- "interesting1"
                -- ( show interesting1 )
              -- processSuDokuReduction
                -- "interesting2"
                -- ( show interesting2 )
              processSuDokuReduction
                ( "filter (not . sdkReductionsIdentical) ( filter sdkSensible ( sdkAllElementListSets " ++ show elements ++ " ) )" )
                ( show ( filter (not . sdkReductionsIdentical) ( filter sdkSensible ( sdkAllElementListSets elements ) ) ) )
