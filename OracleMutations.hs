{-# LANGUAGE PackageImports  #-}

module Main where

import Control.Monad
import System.FilePath.Posix
import Data.List
import Data.Maybe
import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Data.Function

import "safe" Safe

import HSH

import Eu.Fittest.Logging.Mutation.MyLogMutator

import Eu.Fittest.Logging.XML.EventLog
import Eu.Fittest.Logging.XML.XMLparser

import Eu.Fittest.WitnessSetParser (getRewPattern)
import Eu.Fittest.Data (lengthPattern)

main = klfaCheckMutations "./test1/mutation/klfa" "mutation_klfa_all.txt"
-- main = daikonCheckMutations "./mutation_test/log_1382696722317_new" oracleFile mutationLog violationReportFile

daikonJar :: String
daikonJar = "daikon.jar"

oracleFile :: String
oracleFile = "flexstore5.inv"

selectedVars :: String
-- selectedVars = "selectedProduct"
-- selectedVars = "selectedProduct|catalogContents"
-- selectedVars = ""
selectedVars = "numOfSelectedItems|numInShopCart|cartCurrency|cartTotal|numInCompareCart|selectedProduct|catalogContents|shoppingCartContents|compareCartContents"

violationReportFile :: String
violationReportFile = "violations"

mutationLog :: String
mutationLog = "log_1382696722317_new.xml"
-- mutationLog = "test1.xml"

daikonCheckMutations :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
daikonCheckMutations workD invF mutF vioF = do
  -- oldMuts <- glob $ workD </> "mut_*"
  -- when (not $ null oldMuts) $ runIO ("rm", oldMuts)
  -- mutLog <- parseXMLlog (workD </> mutF)
  -- let mutations = genAllMutants mutLog
  -- print ("*** Mutations generated:  #" ++ (show $ length mutations))
  -- mapM_ (\x -> print $ (fst $ head x) ++ ": " ++ (show $ length x))
  --   $ groupBy (\(mt1, _) (mt2, _) -> mt1 == mt2) $ sortBy (\(mt1, _) (mt2, _) -> mt1 `compare`  mt2) mutations
  
  -- zipWithM_ (\(mt, evs) i ->
  --             writeFile (workD </> "mut_" ++ mt ++ "_" ++ show i <.> "xml")
  --             $ ppXMLEventLog evs
  --           ) mutations [1..]
  -- curMutsXMLs <- glob $  workD </> "mut_*.xml"
  -- mapM_ (\lxml -> runIO ("haslog", ["--toraw", lxml])) curMutsXMLs
  -- curMutsLogs <- glob $  workD </> "mut_*.log"
  -- mapM_ (\llog -> runIO ("haslog", ["-c", llog])) curMutsLogs
  -- curMutLoxs <- glob $  workD </> "mut_*.lox"
  -- mapM_ (\llox -> runIO ("haslog", [ "-d"
  --                                  -- , "--varsSelect=" ++ selectedVars
  --                                  , llox]
  --                       )) curMutLoxs

  curMutDtraces <- glob $  workD </> "mut_*.dtrace"
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 100 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         runIO ("java", [ "-cp"
                                                        , daikonJar
                                                        , "daikon.tools.InvariantChecker"
                                                        , "--config_option"
                                                        , "daikon.PrintInvariants.print_inv_class=true"
                                                        , "--verbose"
                                                        , "--conf"
                                                        , "--filter" 
                                                        , "--output"
                                                        , workD </> (vioF ++ show acc) <.> "txt"
                                                        , workD </> invF ] ++ cur_traces)
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0
  
genDaikonMutReport :: FilePath -> IO ()
genDaikonMutReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                              , takeWhile (/=']') $ tail $ fromJust $ find (\x -> x!!0 == '[' && isLetter (x!!1)) v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      groupedOracles = groupBy (\x y -> let {(t1, _, _) = head x; (t2, _, _) = head y} in t1 == t2)
                       $  sortBy (\x y -> let {(t1, _, _) = head x; (t2, _, _) = head y} in t1 `compare` t2)
                       $ groupBy (\(_, _, x) (_, _, y) -> x == y)
                       $ sortBy (\(_, _, x) (_, _, y) -> x `compare` y) rawReport
  mapM_ (\x -> do let (t, _ ,_ ) = head $ head x
                  print (t ++ ": " ++ (show $ length x))
        ) groupedOracles
    
  mapM_ (\x -> mapM_  (\y -> do let (cat, otype, trace) =  head y
                                print $ cat
                                        ++ " : "
                                        ++ otype
                                        ++ " : "
                                        ++ (show $ length
                                                 $ groupBy (\(_, _, tr1) (_, _, tr2) -> tr1 == tr2)
                                                 $ sortBy (\(_, _, tr1) (_, _, tr2) -> tr1 `compare` tr2 )
                                                 y)
                      )
                      x
        )
    $ map (groupBy (\(x, _, _) (y, _, _) -> x == y) . sortBy (\(x, _, _) (y, _, _) -> x `compare` y))    
    $ groupBy (\(_, x, _) (_, y, _) -> x == y)
    $ sortBy (\(_, x, _) (_, y, _) -> x `compare` y) rawReport 
  
      
measureFPRateDaikon :: FilePath -> FilePath -> FilePath -> IO ()
measureFPRateDaikon logsDir invF vioF = do
  curMutDtraces <- glob $ logsDir </> "*.dtrace"
  print $ show $ length curMutDtraces
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 100 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         runIO ("java", [ "-cp"
                                                        , daikonJar
                                                        , "daikon.tools.InvariantChecker"
                                                        , "--config_option"
                                                        , "daikon.PrintInvariants.print_inv_class=true"
                                                        , "--verbose"
                                                        , "--conf"
                                                        , "--filter" 
                                                        , "--output"
                                                        , logsDir </> (vioF ++ show acc) <.> "txt"
                                                        , invF ] ++ cur_traces)
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0  


genDaikonFPReport :: FilePath -> IO ()
genDaikonFPReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  map (\v -> ( takeWhile (/=']')
                                $ tail
                                $ fromJust
                                $ find (\x -> x!!0 == '[' && isLetter (x!!1)) v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      violGrByTraces = groupBy (\(_, d1) (_, d2) -> d1 == d2)
                       $ sortBy (\(_, d1) (_, d2) -> d1 `compare` d2) rawReport
      violGrByOrcs   = map (groupBy (\(_, d1) (_, d2) -> d1 == d2)
                            . sortBy (\(_, d1) (_, d2) -> d1 `compare` d2))
                       $ groupBy (\(o1, _) (o2, _) -> o1 == o2)
                       $ sortBy (\(o1, _) (o2, _) -> o1 `compare` o2) rawReport
  print $ "Falsified Traces: " ++ (show $ length $ violGrByTraces)
  mapM_ (\x -> do let (or, tr) = head $ head x
                  print $ or ++ ": " ++ (show $ length x)
        ) violGrByOrcs


    
-- | invocation example: 
-- | > lopiCheckMutations "./test1/mutation" "lopi_new/flexstore_3_5_lopi_new_2.inv" "lopi_new/mutation_lopi"
lopiCheckMutations :: FilePath -> FilePath -> FilePath -> IO ()
lopiCheckMutations workD invF vioF = do
  curMutDtraces <- glob $  workD </> "mut_*.xml"
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 100 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         report <- run ("lopi", [ "--check-invs"
                                                                , "--in-logs=" ++ unwords cur_traces
                                                                , "--rew-rules=" ++ workD </>  invF]
                                                       ) :: IO String
                                         writeFile (workD </>  (vioF ++ "_" ++ show acc) <.> "txt") report
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0

test = do
  report <- run ("lopi",  [ "--check-invs"
                          , "--in-logs=./test1/mutation/mut_ABS_4539.xml ./test1/mutation/mut_ABS_4540.xml"
                          , "--rew-rules=./test1/mutation/flexstore5_new.pat"]) :: IO String
  writeFile "testLopi"  report
                          

-- | > genLopiMutReportRuleLength "./test1/mutation/lopi_new/mutation_lopi_all.txt" 26715 [(1,3),(2,2),(3,2),(4,1)]
genLopiMutReportRuleLength :: FilePath -> Int -> [(Int, Int)] -> IO Float
genLopiMutReportRuleLength vrep traceN sample_map = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  filter (\(_,patLen, patCount, _) -> maybe False (\sc -> (read patCount :: Int) >= sc)  $ lookup patLen sample_map) $
                   map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                              , lengthPattern $ getRewPattern $ v!!1 ++ v!!2 ++ v!!3
                              , tail $ fromJust $ find (\x -> x!!0 == '#') v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      groupedOracles = groupBy (\x y -> let {(t1, _,_, _) = head x; (t2, _,_,_) = head y} in t1 == t2)
                       $  sortBy (\x y -> let {(t1, _,_, _) = head x; (t2, _,_, _) = head y} in t1 `compare` t2)
                       $ groupBy (\(_, _,_, x) (_, _,_, y) -> x == y)
                       $ sortBy (\(_, _,_, x) (_, _,_, y) -> x `compare` y) rawReport
  mapM_ (\x -> do let (t, _ ,_,_ ) = head $ head x
                  print (t ++ ": " ++ (show $ length x))
        ) groupedOracles
    
  mapM_ (\x -> mapM_  (\y -> do let (cat, otype, _, trace) =  head y
                                print $ cat
                                        ++ " : "
                                        ++ show otype
                                        ++ " : "
                                        ++ (show $ length
                                                 $ groupBy (\(_, _,_, tr1) (_, _,_, tr2) -> tr1 == tr2)
                                                 $ sortBy (\(_, _,_, tr1) (_, _,_, tr2) -> tr1 `compare` tr2 )
                                                 y)
                      )
                      x
        )
    $ map (groupBy (\(x, _, _,_) (y, _, _,_) -> x == y) . sortBy (\(x, _, _,_) (y, _, _,_) -> x `compare` y))    
    $ groupBy (\(_, x, _, _) (_, y, _, _) -> x == y)
    $ sortBy (\(_, x, _,_) (_, y, _,_) -> x `compare` y) rawReport
  return $ ((/) `on` fromIntegral)  (length $ concat groupedOracles)  traceN




genLopiMutReportNoSkip :: FilePath -> IO ()
genLopiMutReportNoSkip vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                              , takeWhile (/='}') $ tail $ fromJust $ find (\x -> x!!0 == '{' && isLetter (x!!1)) v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      rawReportNoSkip = filter (\(_, r, _) -> r /= "Skip")  rawReport
      groupedOracles = groupBy (\x y -> let {(t1, _, _) = head x; (t2, _, _) = head y} in t1 == t2)
                       $  sortBy (\x y -> let {(t1, _, _) = head x; (t2, _, _) = head y} in t1 `compare` t2)
                       $ groupBy (\(_, _, x) (_, _, y) -> x == y)
                       $ sortBy (\(_, _, x) (_, _, y) -> x `compare` y) rawReportNoSkip
  mapM_ (\x -> do let (t, _ ,_ ) = head $ head x
                  print (t ++ ": " ++ (show $ length x))
        ) groupedOracles
    

synopticCheckMutations :: FilePath -> FilePath -> FilePath -> IO ()
synopticCheckMutations logsDir invF vioF = do
  curMutDtraces <- glob $ logsDir </> "*.syn"
  print $ show $ length curMutDtraces
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 1 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         report <- run ("haslog", [ "--check-synoptic-invs"
                                                                  , logsDir </> invF
                                                                  ,"-o"
                                                                  , unwords cur_traces]
                                                       ) :: IO String
                                         when (not $ null report) $ do
                                           writeFile (logsDir </> (vioF ++ show acc) <.> "txt") report
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0 


genSynopticMutReport :: FilePath -> IO ()
genSynopticMutReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ filter (\l -> head l /= '*') $ lines  vreport
  let rawReport =  map (\v -> (takeWhile (/='_') $ tail $ dropWhile (/='_') $ last v 
                              , takeBaseName $ last v)) tokenizedViol 
      violGrByTraces = groupBy (\(_, tr1) (_, tr2) -> tr1 == tr2)
                       $ sortBy (\(_, tr1) (_, tr2) -> tr1 `compare` tr2) rawReport
      violGrByOperType = groupBy (\tr1 tr2 -> (fst $ head tr1) == (fst $ head tr2))
                       $ sortBy (\tr1 tr2 -> (fst $ head tr1) `compare` (fst $ head tr2)) violGrByTraces
  mapM_ (\v -> print ((fst $ head $ head v) ++ " : " ++ (show $ length v))) violGrByOperType

genKLFAMutReport :: FilePath -> IO ()
genKLFAMutReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  lines $ vreport
  let rawReport =  map (takeWhile (/='_') . tail . dropWhile (/='_')) tokenizedViol
      groupedOracles = groupBy (\x y -> x == y) $ sortBy (\x y -> x `compare` y) rawReport
  mapM_ (\x -> print ( head x ++ ": " ++ (show $ length x))) groupedOracles
    

klfaCheckMutations :: FilePath  -> FilePath -> IO ()
klfaCheckMutations logsDir vioF = do
  curMutDtraces <- glob $ logsDir </> "*.csv"
  print $ show $ length curMutDtraces
  writeFile (logsDir </> vioF) "Results of KLFA violation analysis"       
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 1 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         checkOutFolder <- glob "klfaoutput"      
                                         when (not $ null checkOutFolder) $ do 
                                           runIO("rm", ("-r":checkOutFolder))
                                         runIO ("java"
                                               , [ "-cp"
                                                 , "/home/alex/tools/testing/KLFA/klfa-201211191358/klfa.jar"
                                                 , "it.unimib.disco.lta.alfa.klfa.LogTraceAnalyzer"
                                                 , "-inputDir" 
                                                 , "./test1/mutation/klfa/klfaoutput"  
                                                 , "applicationLevel"
                                                 , "checking"
                                                 , "./test1/mutation/klfa/transformersConfig.txt"
                                                 , "./test1/mutation/klfa/preprocessingRules.txt"  
                                                 , head cur_traces
                                                 ]
                                               )
                                         anomalies <- readFile "./klfaoutput/anomalies.csv"
                                         when ((length $ lines anomalies) /= 1) $ do
                                           appendFile (logsDir </> vioF) $ basename $ head cur_traces ++ "\n"
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0 


-- | invocation example: 
-- | > measureFPRateLopi "../../Execution/Oracles/flexstore_3_5_statebased_exec" "lopi_new/flexstore_3_5_lopi_new_2.inv" "lopi_new/lopiFP" 
measureFPRateLopi :: FilePath -> FilePath -> FilePath -> IO ()
measureFPRateLopi logsDir invF vioF = do
  curMutDtraces <- glob $ logsDir </> "*.xml"
  print $ show $ length curMutDtraces
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 100 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         report <- run ("lopi", [ "--check-invs"
                                                                , "--in-logs=" ++ unwords cur_traces
                                                                , "--rew-rules=" ++ logsDir </> invF]
                                                       ) :: IO String
                                         when (not $ null report) $ do
                                           writeFile (logsDir </> (vioF ++ show acc) <.> "txt") report
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0  



genLopiFPReport :: FilePath -> IO ()
genLopiFPReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  map (\v -> ( takeWhile (/='}')
                                $ tail
                                $ fromJust
                                $ find (\x -> x!!0 == '{' && isLetter (x!!1)) v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      violGrByTraces = groupBy (\(_, d1) (_, d2) -> d1 == d2)
                       $ sortBy (\(_, d1) (_, d2) -> d1 `compare` d2) rawReport
      violGrByOrcs   = map (groupBy (\(_, d1) (_, d2) -> d1 == d2)
                            . sortBy (\(_, d1) (_, d2) -> d1 `compare` d2))
                       $ groupBy (\(o1, _) (o2, _) -> o1 == o2)
                       $ sortBy (\(o1, _) (o2, _) -> o1 `compare` o2) rawReport
  print $ "Falsified Traces: " ++ (show $ length $ violGrByTraces)
  mapM_ (\x -> do let (or, tr) = head $ head x
                  print $ or ++ ": " ++ (show $ length x)
        ) violGrByOrcs


-- | invocation example
-- | > genLopiFPReportRuleLength "../../Execution/Oracles/flexstore_3_5_statebased_exec/lopi_new/lopiFP_all.txt" 6317 [(1,1),(2,1),(3,1),(4,1)]
-- | > 
genLopiFPReportRuleLength :: FilePath -> Int -> [(Int, Int)] -> IO Float
genLopiFPReportRuleLength vrep traceN sample_map = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ lines $ vreport
  let rawReport =  filter (\(patLen, patCount, _) -> maybe False (\sc -> (read patCount :: Int) >= sc)  $ lookup patLen sample_map) $
                   map (\v -> ( lengthPattern $ getRewPattern $ v!!1 ++ v!!2 ++ v!!3
                              , tail $ fromJust $ find (\x -> x!!0 == '#') v
                              , takeBaseName $ last v)
                       ) tokenizedViol
      violGrByTraces = groupBy (\(_,_, d1) (_,_, d2) -> d1 == d2)
                       $ sortBy (\(_,_, d1) (_,_, d2) -> d1 `compare` d2) rawReport
      violGrByOrcs   = map (groupBy (\(_,_, d1) (_,_, d2) -> d1 == d2)
                            . sortBy (\(_,_, d1) (_,_, d2) -> d1 `compare` d2))
                       $ groupBy (\(o1,_, _) (o2,_, _) -> o1 == o2)
                       $ sortBy (\(o1,_, _) (o2,_, _) -> o1 `compare` o2) rawReport
  print $ "Falsified Traces: " ++ (show $ length  violGrByTraces)
  mapM_ (\x -> do let (or, _, tr) = head $ head x
                  print $ show or ++ ": " ++ (show $ length x)
        ) violGrByOrcs
  return $ ((/) `on` fromIntegral)  (length  violGrByTraces)  traceN


-- | > findOptNumOfSamples "lopiFP_all.txt" "mutation_lopi_all.txt" 4 6317 26715
-- | > findOptNumOfSamples "../../Execution/Oracles/flexstore_3_5_statebased_exec/lopi_new/lopiFP_all.txt" "./test1/mutation/lopi_new/mutation_lopi_all.txt" 4 6317 26715
findOptNumOfSamples :: FilePath -> FilePath -> Int -> Int -> Int -> IO (Float, [(Int,Int)])
findOptNumOfSamples repFP repMUT maxSampLen numFP numMUT = do
  let  integers =  [x ++ [i] | x <- [] : integers, i <- [1..maxSampLen]]
       sampTbls = map (zip [1..]) $ take (maxSampLen^maxSampLen) $ filter (\l -> length l == maxSampLen) integers               
  fscores   <-  mapM (\tbl -> do precision <- liftM (\x -> 1 - x) $ genLopiFPReportRuleLength  repFP numFP  tbl
                                 recall    <- genLopiMutReportRuleLength repMUT numMUT tbl
                                 return (2*(precision * recall)/(precision + recall), tbl)) sampTbls
  let result_ = sortBy (\(f1, _) (f2, _) -> f1 `compare` f2) fscores
  print $ show  result_
  return $ last $ result_
  



measureFPRateSynoptic :: FilePath -> FilePath -> FilePath -> IO ()
measureFPRateSynoptic logsDir invF vioF = do
  curMutDtraces <- glob $ logsDir </> "*.syn"
  print $ show $ length curMutDtraces
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 1 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         report <- run ("haslog", [ "--check-synoptic-invs"
                                                                  , invF
                                                                  ,"-o"
                                                                  , unwords cur_traces]
                                                       ) :: IO String
                                         when (not $ null report) $ do
                                           writeFile (logsDir </> (vioF ++ show acc) <.> "txt") report
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0    


genSynopticFPReport :: FilePath -> IO ()
genSynopticFPReport vrep = do
  vreport <- readFile vrep
  let tokenizedViol =  map words $ filter (\l -> head l /= '*') $ lines $ vreport
  let rawReport =  map (takeBaseName . last) tokenizedViol
      violGrByTraces = groupBy (\d1 d2 -> d1 == d2)
                       $ sortBy (\d1 d2 -> d1 `compare` d2) rawReport
  print $ "Falsified Traces: " ++ (show $ length $ violGrByTraces)



measureFPRateKLFA :: FilePath  -> FilePath -> IO ()
measureFPRateKLFA logsDir vioF = do
  curMutDtraces <- glob $ logsDir </> "*.csv"
  print $ show $ length curMutDtraces
  writeFile (logsDir </> vioF) "Results of KLFA violation analysis"       
  let checkViolations []     acc = return ()
      checkViolations traces acc = let (cur_traces, traces') = splitAt 1 traces
                                   in do print $ "Violation check round: #" ++ (show acc)
                                         checkOutFolder <- glob "klfaoutput"      
                                         when (not $ null checkOutFolder) $ do 
                                           runIO("rm", ("-r":checkOutFolder))
                                         runIO ("java"
                                               , [ "-cp"
                                                 , "/home/alex/tools/testing/KLFA/klfa-201211191358/klfa.jar"
                                                 , "it.unimib.disco.lta.alfa.klfa.LogTraceAnalyzer"
                                                 , "-inputDir" 
                                                 , "../../Execution/Oracles/flexstore_3_5_statebased_exec/klfa/klfaoutput"  
                                                 , "applicationLevel"
                                                 , "checking"
                                                 , "../../Execution/Oracles/flexstore_3_5_statebased_exec/klfa/transformersConfig.txt"
                                                 , "../../Execution/Oracles/flexstore_3_5_statebased_exec/klfa/preprocessingRules.txt"  
                                                 , head cur_traces
                                                 ]
                                               )
                                         anomalies <- readFile "./klfaoutput/anomalies.csv"
                                         when ((length $ lines anomalies) /= 1) $ do
                                           appendFile (logsDir </> vioF) $ basename $ head cur_traces ++ "\n"
                                         checkViolations traces' (acc+1)
  checkViolations curMutDtraces 0 

-- klfaCsvAnalysis.sh splitComponent checking transformersConfig.txt preprocessingRules.txt fail.csv


-- intersect and subtract mutations identified by Lopi and Daikon
genCompareLopiDaikonReport :: FilePath -> FilePath -> IO ()
genCompareLopiDaikonReport lopiViolF daikonViolF = do
  daikonVFCont <- readFile daikonViolF
  lopiVFCont   <- readFile lopiViolF
  let tokenizedVsDaikon =  map words $ lines $ daikonVFCont
      tokenizedVslopi   =  map words $ lines $ lopiVFCont
      rawReportDaikon   =  
          nubBy (\(_,_,tr1) (_,_,tr2) -> tr1 == tr2) $
          map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                     , takeWhile (/=']') $ tail $ fromJust $ find (\x -> x!!0 == '[' && isLetter (x!!1)) v
                     , takeBaseName $ last v)
              ) tokenizedVsDaikon
      grDaikonOracles   = 
          groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2)$
          sortBy (\(t1, _, _) (t2, _, _) -> t1 `compare` t2) rawReportDaikon
      rawReportLopi     = 
          nubBy (\(_,_,tr1) (_,_,tr2) -> tr1 == tr2) $
          map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                     , takeWhile (/='}') $ tail $ fromJust $ find (\x -> x!!0 == '{' && isLetter (x!!1)) v
                     , takeBaseName $ last v)
              ) tokenizedVslopi
      grLopiOracles    = 
          groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2) $
          sortBy (\(t1, _, _) (t2, _, _) -> t1 `compare` t2) rawReportLopi
  -- print $ show $ head grLopiOracles
  -- print $ show $ head grDaikonOracles
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "unique Lopi = " ++ tp ++ " : " ++ (show $ length (foldr (\x xs -> deleteBy (\(_,_,t1) (_,_,t2) -> t1 == t2) x xs) grL grD)))  grLopiOracles grDaikonOracles
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "unique Daikon = " ++ tp ++ " : " ++ (show $ length (foldr (\x xs -> deleteBy (\(_,_,t1) (_,_,t2) -> t1 == t2) x xs) grD grL))) grLopiOracles grDaikonOracles
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "Lopi & Daikon intersection = " ++ tp ++ " : " ++ (show $ length (intersectBy (\(_,_,t1) (_,_,t2) -> t1 == t2) grD grL))) grLopiOracles grDaikonOracles
      -- daikonComplement
      -- commonIntersection


-- | intersect and subtract mutations identified by New Lopi and Daikon
-- | > genCompareNewLopiDaikonReport "./test1/mutation/lopi_new/mutation_lopi_all.txt" [(1,3),(2,2),(3,2),(4,1)] "./test1/mutation/daikon/mutations_daikon_all.txt"
-- | > genCompareNewLopiDaikonReport "./test1/mutation/lopi_new/mutation_lopi_all.txt" [(1,1),(2,1),(3,1),(4,1)] "./test1/mutation/daikon/mutations_daikon_all.txt"
genCompareNewLopiDaikonReport :: FilePath -> [(Int, Int)] -> FilePath -> IO ()
genCompareNewLopiDaikonReport lopiViolF sample_map daikonViolF = do
  daikonVFCont <- readFile daikonViolF
  lopiVFCont   <- readFile lopiViolF
  let tokenizedVsDaikon =  map words $ lines $ daikonVFCont
      tokenizedVslopi   =  map words $ lines $ lopiVFCont
      rawReportDaikon   =  
          nubBy (\(_,_,tr1) (_,_,tr2) -> tr1 == tr2) $
          map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                     , takeWhile (/=']') $ tail $ fromJust $ find (\x -> x!!0 == '[' && isLetter (x!!1)) v
                     , takeBaseName $ last v)
              ) tokenizedVsDaikon
      grDaikonOracles   = 
          groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2)$
          sortBy (\(t1, _, _) (t2, _, _) -> t1 `compare` t2) rawReportDaikon
      rawReportLopi     = 
          nubBy (\(_,_,tr1) (_,_,tr2) -> tr1 == tr2) $ map (\(pt,pl,_,tr) -> (pt,show pl,tr)) $
          filter (\(_,patLen, patCount, _) -> maybe False (\sc -> (read patCount :: Int) >= sc)  $ lookup patLen sample_map) $
          map (\v -> ( takeWhile (/='_') $ tail $ dropWhile (/='_') $ takeBaseName $ last v
                     , lengthPattern $ getRewPattern $ v!!1 ++ v!!2 ++ v!!3
                     , tail $ fromJust $ find (\x -> x!!0 == '#') v
                     , takeBaseName $ last v)
              ) tokenizedVslopi
      grLopiOracles    = 
          groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2) $
          sortBy (\(t1, _, _) (t2, _, _) -> t1 `compare` t2) rawReportLopi
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "unique Lopi = " ++ tp ++ " : " ++ (show $ length (foldr (\x xs -> deleteBy (\(_,_,t1) (_,_,t2) -> t1 == t2) x xs) grL grD)))  grLopiOracles grDaikonOracles
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "unique Daikon = " ++ tp ++ " : " ++ (show $ length (foldr (\x xs -> deleteBy (\(_,_,t1) (_,_,t2) -> t1 == t2) x xs) grD grL))) grLopiOracles grDaikonOracles
  zipWithM_ (\grL grD -> do
               let (tp,_,_) = head grL
               print $ "Lopi & Daikon intersection = " ++ tp ++ " : " ++ (show $ length (intersectBy (\(_,_,t1) (_,_,t2) -> t1 == t2) grD grL))) grLopiOracles grDaikonOracles


-- intersect and subtract FP identified by Lopi and Daikon
genFPCompareLopiDaikonReport :: FilePath -> FilePath -> IO ()
genFPCompareLopiDaikonReport lopiViolF daikonViolF = do
  daikonVFCont <- readFile daikonViolF
  lopiVFCont   <- readFile lopiViolF
  let tokenizedVsDaikon = map words $ lines $ daikonVFCont
      tokenizedVslopi   = map words $ lines $ lopiVFCont
      rawReportDaikon   = nub $ map (takeBaseName . last) tokenizedVsDaikon
      rawReportLopi     = nub $ map (takeBaseName . last) tokenizedVslopi
  -- print $ show $ head grLopiOracles
  -- print $ show $ head grDaikonOracles
  print $ "unique Lopi = " ++ (show $ length $ rawReportLopi \\ rawReportDaikon)
  print $ "unique Daikon = " ++ (show $ length $ rawReportDaikon \\ rawReportLopi)
  print $ "intersection Daikon and Lopi = " ++ (show $ length $ rawReportDaikon `intersect` rawReportLopi)
  print $ "union Daikon and Lopi = " ++ (show $ length $ nub $ rawReportDaikon ++ rawReportLopi)
      -- daikonComplement
      -- commonIntersection


-- intersect and subtract FP identified by New Lopi and Daikon
-- | > genFPCompareLopiNewDaikonReport "../../Execution/Oracles/flexstore_3_5_statebased_exec/lopi_new/lopiFP_all.txt" [(1,3),(2,2),(3,2),(4,1)] "../../Execution/Oracles/flexstore_3_5_statebased_exec/daikon/daikonFP_all.txt"
-- | > genFPCompareLopiNewDaikonReport "../../Execution/Oracles/flexstore_3_5_statebased_exec/lopi_new/lopiFP_all.txt" [(1,1),(2,1),(3,1),(4,1)] "../../Execution/Oracles/flexstore_3_5_statebased_exec/daikon/daikonFP_all.txt"
genFPCompareLopiNewDaikonReport :: FilePath -> [(Int, Int)] -> FilePath -> IO ()
genFPCompareLopiNewDaikonReport lopiViolF sample_map daikonViolF  = do
  daikonVFCont <- readFile daikonViolF
  lopiVFCont   <- readFile lopiViolF
  let tokenizedVsDaikon = map words $ lines $ daikonVFCont
      tokenizedVslopi   = map words $ lines $ lopiVFCont
      rawReportDaikon   = nub $ map (takeBaseName . last) tokenizedVsDaikon
      rawReportLopi     = nub $ map (\(_,_,tr) -> tr) $
                          filter (\(patLen, patCount, _) -> maybe False (\sc -> (read patCount :: Int) >= sc)  $ lookup patLen sample_map) $
                                 map (\v -> ( lengthPattern $ getRewPattern $ v!!1 ++ v!!2 ++ v!!3
                                            , tail $ fromJust $ find (\x -> x!!0 == '#') v
                                            , takeBaseName $ last v)
                                     )  tokenizedVslopi
  print $ "unique Lopi = " ++ (show $ length $ rawReportLopi \\ rawReportDaikon)
  print $ "unique Daikon = " ++ (show $ length $ rawReportDaikon \\ rawReportLopi)
  print $ "intersection Daikon and Lopi = " ++ (show $ length $ rawReportDaikon `intersect` rawReportLopi)
  print $ "union Daikon and Lopi = " ++ (show $ length $ nub $ rawReportDaikon ++ rawReportLopi)


drawGraph :: FilePath -> IO ()
drawGraph inF = do 
  contF <- liftM (\c -> (read c) :: [(Float,[(Int,Int)])]) $ readFile inF
  let outC = map (\(f,[(_,i1),(_,i2),(_,i3),(_,i4)]) -> (f, 2^i1*3^i2*5^i3*7^i4)) contF
  mapM_ (\(x,y) -> putStrLn $ show x ++ "," ++ show y) outC   
  

 
