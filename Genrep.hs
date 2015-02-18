module Main where

-- import System.Directory
-- import System.Exit
-- import System.IO
import HSH
import Text.Regex.Posix
import System.FilePath
import Control.Monad
import Data.List
-- import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import Text.HTML.TagSoup
import qualified System.IO.UTF8 as UTF8

import Eu.Fittest.Logging.XML.XMLparser -- (parseXMLlog)
import Eu.Fittest.Logging.XML.ToFITTESTRawLog -- (events2raw_bytestring)
import Eu.Fittest.Logging.XML.EventLog -- (ppXMLEventLog)

import System.Environment

import Debug.Trace

main :: IO ()
main = runDaikonValidation 5 (flexsLogSource 5 TC_STATE) 1 [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1155]
  -- runLOPIValidation 5 (flexsLogSource 5 TC_STATE) "lopi_flexstore5_valid.pat" "0.99" [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1155] >> return ()
  -- runLOPIValidation 4 (flexsLogSource 4 TC_STATE) "lopi_flexstore4_valid.pat" [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 922]  >> return ()
  -- prepareLogs (flexsLogSource 3 TC_STATE) False
  -- runLOPIValidation 2 (flexsLogSource 2 TC_STATE) "lopi_flexstore2_valid.pat" [50, 75, 100, 125, 150, 175, 200, 225] >> return ()
  -- prepareLogs (flexsLogSource 2 TC_STATE) False
  -- runLOPIValidation 1 (flexsLogSource 1 TC_STATE) "lopi_flexstore1_valid.pat" [50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 536] >> return ()
  -- prepareLogs (flexsLogSource 1 TC_STATE) False
  -- runDaikonValidation 5 (flexsLogSource 5 TC_STATE) [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1155]
  -- prepareLogs (flexsLogSource 5 TC_STATE) False

  -- do prepareLogs (flexsLogSource 5 TC_STATE) False

  -- runDaikonValidation 1 (flexsLogSource 1 TC_STATE) [50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 536]
  -- runDaikonValidation 4 (flexsLogSource 4 TC_STATE) [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 922]
  -- do prepareLogs (flexsLogSource 5 TC_STATE) False
  -- runDaikonValidation 2 (flexsLogSource 2 TC_STATE) [50, 75, 100, 125, 150, 175, 200, 225]
  -- do prepareLogs (flexsLogSource 1 TC_STATE) False
  -- runDaikonValidation 3 (flexsLogSource 3 TC_STATE) [50, 100, 150, 200, 250, 300, 350, 390]
  -- logs2dtraces (flexsLogSource 5 TC_STATE)  False
  
  -- runDaikonOracleInference 5 "flexstore5_daikon"
  
  -- oldNewLogs <- glob $ flexsLogSource 5 TC_STATE </> "*_new.*"
  -- when  (not $ null oldNewLogs) $ runIO ("rm", ["-v"] ++ oldNewLogs)
  -- rawLogs <- glob $ flexsLogSource 5 TC_STATE </> "*.log"
  -- let prepareLog l = do
  --       let lnew = replaceBaseName l $ takeBaseName l ++ "_new"
  --       runIO ("cp", [l, lnew])
  --       runIO ("haslog", ["-c", lnew])
  --       runIO ("haslog", ["-x", "--appEventOnly", replaceExtension lnew "lox"])
  --       runIO ("haslog", ["--toraw", replaceExtension lnew "xml"])
  -- mapM_ prepareLog rawLogs

-- runDaikonValidation $ flexsLogSource 2 TC_STATE
-- runLOPIValidation 4 "nonzerorules_flxs4_state.txt" [30, 50, 70] >> return ()
--  prepareLogs $ flexsLogSource 4 TC_STATE
-- runLOPIValidation 2 "nonzerorules_flxs2_state.txt" [50, 100, 150, 200, 225] >> return ()
-- prepareLogs $ flexsLogSource 1 TC_STATE
-- main = prepareLogs flexstore1_fsm_sequencebased

inference_report, flexstore_folder_base, conf_level :: String
inference_report = "inference_report.txt"
flexstore_folder_base = "flexstorev"
-- flexstore_folder_base = "test"
conf_level="0"

type StepSize = Int

log_step_size :: StepSize
log_step_size = 100

type VersionName = String

type VersionNumber = Int

flexstoreVersion :: VersionNumber -> VersionName
flexstoreVersion = (++) flexstore_folder_base . show

compLogsSize :: [FilePath] -> IO Int
compLogsSize fps = mapM (parseXMLlog >=> return . length) fps >>= return . sum


findLogs :: VersionName -> String -> String ->  IO String
findLogs ver suf ext = run ("find",  [ ".", "-type", "f"
                                     , "-name", "*" ++ suf ++"*." ++ ext
                                     , "-path", "*" ++ ver ++ "*"
                                     ]) :: IO String

raw2xml :: FilePath -> IO ()
raw2xml fp = do runIO ("haslog", ["-c", fp])
                runIO ("haslog", ["-x", "--appEventOnly", replaceExtension fp "lox"])
                runIO ("haslog", ["--toraw", replaceExtension fp "xml"])

genReport :: FilePath -> IO ()
genReport fp = do
  let separator1 = replicate 10 '-'
      header    = "# " ++ separator1 ++ "Pattern Inference Report" ++ separator1 
  writeFile fp header
  logs <- liftM words (run ("find", [ "-type", "f", "-name", "*.log"]) :: IO String)
  mapM_ preprocessL logs
  mapM_ (flip inferRules4Version fp) [1..5]
    where
      
      preprocessL :: FilePath -> IO ()
      preprocessL l = do 
        let lbase = dirname l
        n <- liftM (length . words)
             (run ("find", [ lbase
                            , "-type", "f"
                            , "-name", "*.log"
                            ]) :: IO String)
        when (((n == 2) && ((l =~ "fixed") :: Bool)) || (n == 1)) $ do
          print $ "processing log file: " ++ l
          let lnew = replaceBaseName l ((takeBaseName l) ++ "_new")
          runIO ("cp", [l, lnew])
          runIO ("haslog", ["-c", lnew])
          runIO ("haslog", ["-x", "--appEventOnly", replaceExtension lnew "lox"])
          runIO ("haslog", ["--toraw", replaceExtension lnew "xml"])

      inferRules4Version :: VersionNumber -> FilePath -> IO ()    
      inferRules4Version verN outF = do
          let separator2 = replicate 10 '='
              versionHeader = "\n# "
                              ++ separator2
                              ++ flexstoreVersion verN
                              ++ separator2
                              ++ "\n"
          appendFile outF versionHeader
          inLogs <- findLogs (flexstoreVersion verN) "_new" "log"
          patterns <- run ("lopi", [ "-i"
                                   , "-m"
                                   , "--in-logs=" ++ inLogs
                                   , "--filter-rules=" ++ conf_level]
                          ) :: IO String
          appendFile outF patterns


splitLogsByStep :: VersionNumber -> StepSize -> IO Int
splitLogsByStep vN step = do
  inLogs <- liftM words $ findLogs (flexstoreVersion vN) "_new" "xml" 
  ii <- splitLogsByStep' inLogs step 0
  -- print ("size: " ++ show ii)
  return ii
    where
      splitLogsByStep' :: [FilePath] -> StepSize -> Int -> IO Int
      splitLogsByStep' [] _  acc     = return acc
      splitLogsByStep' (l:ls) st acc = do
        -- print ("step_size: " ++ show st)
        alog <- parseXMLlog l
        let tlog = take st alog
            bite = min st (length alog)
        if not $ null tlog
          then do let trimed_log_name = replaceBaseName l
                                        $ takeBaseName l
                                        ++ "_trimby_"
                                        ++ show step ++ "_"
                                        ++ show bite
                  writeFile trimed_log_name $ ppXMLEventLog tlog
                  let st_new  = st - bite
                      acc_new = acc + bite
                  splitLogsByStep' ls st_new acc_new
          else (return acc)       

runOracleCrossValidation :: VersionNumber -> Int -> IO [Float]
runOracleCrossValidation ver foldN = mapM (runOracleCrossValidation' ver foldN) [100, 500, 1000, 1500]
    where
      runOracleCrossValidation' :: VersionNumber -> Int -> Int -> IO Float
      runOracleCrossValidation' ver_ foldN_ lsize = do
          runIO ("make", ["clean_trim"])
          splitLogsByStep ver_ lsize
          inLogs  <- liftM words $ findLogs (flexstoreVersion ver_) "_trimby" "xml"
          -- print inLogs
          logSize <- compLogsSize inLogs
          print $ show logSize
          oracleCrossValidation ver_ foldN_ logSize inLogs
      

oracleCrossValidation :: VersionNumber -> Int -> Int -> [FilePath] -> IO Float
oracleCrossValidation ver foldN logsL logs = do
  let (quot_, _)    = quotRem logsL foldN
      lefts         = [(i - 1) * quot_ + 1 | i <- [1 .. foldN]]
      rights        = [i * quot_ | i <- [1 .. (foldN-1)]] ++ [logsL]
  print ("quot_ = " ++ show quot_)    
  logsWithBounds <- enumLogs 1 logs
  print ("lefts: " ++ show lefts)
  print ("rights: " ++ show rights)
  print ("intervals " ++ (show $ map snd logsWithBounds))
  let crossFolds    = zipWith3 (crossValidationRound 0 logsWithBounds 1) [1..foldN] lefts rights
  resList <- mapM splitFilesRound crossFolds
  print ("individual Rates: " ++ show resList) 
  print ("FP rate: " ++ show ((sum resList) / 10))
  return $ (sum resList) / 10
    where

      enumLogs :: Int -> [FilePath] -> IO [(FilePath, (Int, Int))]
      enumLogs _ [] = return [] 
      enumLogs c (fp:fps) = do logL <- liftM length $ parseXMLlog fp 
                               fps' <- enumLogs (c + logL) fps
                               return ((fp, (c, c + logL - 1)):fps')

      crossValidationRound :: Int -> [(FilePath, (Int, Int))] -> Int -> Int -> Int -> Int  -> [(FilePath, FilePath, (Int, Int), Int)]
      crossValidationRound _   []                  _      _       _   _     = []
      crossValidationRound acc ((fp, (li, ri)):ls) lbegin round_ valL valR 
        | li < valL  && ri < valL  = (fp, mkNewFileName "_train_" , (li, ri), li)
                                     :crossValidationRound (acc+1) ls (ri+1) round_ valL valR
        | li < valL  && valL <= ri = (fp, mkNewFileName "_train_", (li, valL - 1), li)
                                     :crossValidationRound (acc+1) ((fp, (valL, ri)):ls) li round_ valL valR
        | li >= valL && ri <= valR = (fp, mkNewFileName "_valid_", (li, ri), lbegin)
                                     :crossValidationRound (acc+1) ls (ri+1) round_ valL valR
        | li <= valR && valR < ri  = (fp, mkNewFileName "_valid_", (li, valR), lbegin)
                                     :crossValidationRound (acc+1) ((fp, (valR+1, ri)):ls) lbegin round_ valL valR
        | valR < li                = (fp, mkNewFileName "_train_", (li, ri), lbegin)
                                     :crossValidationRound (acc+1) ls (ri+1) round_ valL valR
        where
          mkNewFileName :: String -> FilePath
          mkNewFileName substr  = replaceBaseName fp
                                  $ takeBaseName fp
                                  ++ substr
                                  ++ show round_
                                  ++ "_"
                                  ++ show acc
  
      mkIntSplit :: Int -> Int -> [a] -> [a]
      mkIntSplit beg end = drop beg . take (end+1)

      splitFilesRound :: [(FilePath, FilePath, (Int, Int), Int)] -> IO Float
      splitFilesRound new_logs_to_split = do
        mapM_ (\(fpIn, fpOut, (i,j), ll) -> do
                 plIn <- parseXMLlog fpIn
                 -- print ("i=" ++ show i ++ " j=" ++ show  j)
                 -- print ("fpIn:" ++ show fpIn)
                 -- print ("ll=" ++ show ll )
                 print ("fpOut: " ++ show fpOut)
                 let fpOutLog = replaceExtension fpOut "log"
                 B.writeFile fpOutLog
                   $ events2raw_bytestring
                   $ mkIntSplit (i - ll) (j - ll) plIn
                 -- print fpOutLog
                 -- readFile fpOutLog >>= putStrLn
             ) new_logs_to_split
        trainLogs <- liftM (unwords . words) $ findLogs (flexstoreVersion ver) "_train_" "log" 
        -- print trainLogs
        validLogs <- liftM (unwords . words) $ findLogs (flexstoreVersion ver) "_valid_" "log"
        runIO ("lopi", [ "-i"
                       , "-m"
                       , "--in-logs=" ++ trainLogs
                       , "--filter-rules=" ++ conf_level
                       , "--rew-rules=train.pat"])
        -- print "train.pat"  
        -- readFile "train.pat" >>= putStrLn
        runIO ("lopi", [ "-i"
                       , "-m"
                       , "--in-logs=" ++ validLogs
                       , "--filter-rules=filterBothZero"
                       , "--rew-rules=valid.pat"])
        -- print "valid.pat"
        -- readFile "valid.pat" >>= putStrLn
        negCount <- run ("lopi", [ "-c"
                                 , "-n"
                                 , "--cross-check-rules=train.pat valid.pat train_valid_report.diff"]) :: IO String
        print "train_valid_report.diff"
        readFile "train_valid_report.diff" >>= putStrLn
        runIO ("rm", ["train.pat", "valid.pat", "train_valid_report.diff"])
        runIO ("make", ["clean_train_valid"])
        return (read negCount :: Float)     
        

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs =  sequence (zipWith3 f xs ys zs)


-- | ======================= Daikon Oracles ============================

-- | Constants 
daikonJar :: FilePath
daikonJar = "./daikon.jar"

optsGHCRTForInferdaikon, eventFiltersOption, nameOfOracleFile, nameOfReportFile, nameOfViolationsRecordFile :: String
optsGHCRTForInferdaikon = ""
eventFiltersOption = ""
nameOfOracleFile = "oracle.inv"
nameOfReportFile = "report.txt"
nameOfViolationsRecordFile = "violations.txt"

fieldsToInclude :: [String]
-- fieldsToInclude = ["selectedProduct", "catalogContents"] -- flexstore1
fieldsToInclude = ["numInShopCart", "cartCurrency", "cartTotal", "selectedProduct", "catalogContents", "shoppingCartContents"] -- flexstore2 -- 
-- fieldsToInclude = ["numOfSelectedItems", "numInShopCart", "cartCurrency", "cartTotal", "numInCompareCart", "selectedProduct", "catalogContents", "shoppingCartContents", "compareCartContents"]  -- flexstore3
-- fieldsToInclude = ["numOfSelectedItems", "numInShopCart", "cartCurrency", "cartTotal", "numInCompareCart", "selectedProduct", "catalogContents", "shoppingCartContents", "compareCartContents"] -- flexstore4
-- fieldsToInclude = ["numOfSelectedItems", "numInShopCart", "cartCurrency", "cartTotal", "numInCompareCart", "selectedProduct", "catalogContents", "shoppingCartContents", "compareCartContents"] -- flexstore5



-- | Preconditions:
-- |   1) logs in raw format must be compressed
-- |   2) temp folder to store logs during oracle inference is created


runDaikonOracleInference :: VersionNumber -> FilePath -> Int -> IO ()
runDaikonOracleInference ver tmpF lsize = do
  oldTmp <- glob tmpF
  when (oldTmp /= [tmpF]) $
    do runIO ("mkdir", [tmpF])
       runIO ("make", ["clean_trim"])
       splitLogsByStep ver lsize
       inLogsXML  <- liftM words $ findLogs (flexstoreVersion ver) "_trimby" "xml"
       let xml2raw l = runIO ("haslog", ["--toraw", l]) >>
                       runIO ("haslog", ["-c", replaceExtension l "log"])
       mapM_ xml2raw inLogsXML
       logLogs <- liftM words $ findLogs (flexstoreVersion ver) "_trimby" "log"
       loxLogs <- liftM words $ findLogs (flexstoreVersion ver) "_trimby" "lox"
       dicLogs <- liftM words $ findLogs (flexstoreVersion ver) "_trimby" "dic"
       runIO ("cp", ["--target-directory=" ++ tmpF] ++ logLogs ++ loxLogs ++ dicLogs)
  logs2dtraces tmpF True


logs2klfa :: FilePath -> IO ()
logs2klfa logsDir = do
  klfaDir    <- return $ logsDir </> "synoptic"
  xmlLogs <- glob $ logsDir </> "*.xml"
  -- trace (show xmlLogs) $ return ()
  let log2klfa l = do
        let klfafile = klfaDir </> takeBaseName l <.> "syn"
        print ("Converting " ++ klfafile ++ " to KLFA format...")
        runIO ("haslog", [ "-s"
                         , l 
                         , "--output=" ++ klfafile])
  mapM_ log2klfa xmlLogs

logs2dtraces_ :: FilePath -> Bool -> IO ()
logs2dtraces_ logsDir isInfer = do
  daikonDir    <- return $ logsDir </> "daikon"
  oldDaikonDir <- glob daikonDir
  invFile      <- return $ daikonDir </> takeBaseName logsDir <.> nameOfOracleFile
  reportFile   <- return $ daikonDir </> takeBaseName logsDir <.> nameOfReportFile
  when (oldDaikonDir /= []) $ runIO ("rm", ["-rf", daikonDir])     
  runIO ("mkdir", [daikonDir])
  -- loxLogs <- glob $ logsDir </> "*_new*.lox"
  loxLogs <- glob $ logsDir </> "*.lox"
  let log2daikon l = do
        let daikonfile = daikonDir </> takeBaseName l <.> "dtrace"
        print ("Converting " ++ daikonfile ++ " to Daikon format...")
        runIO ("haslog", filter (/="") 
                         [ optsGHCRTForInferdaikon
                         , "-d"
                         -- , "--varsSelect=selectedProduct|catalogContents"
                         -- , "--varsSelect=selectedProduct|catalogContents|numInShopCart|cartCurrency|cartTotal|shoppingCartContents"
                         -- , "--varsSelect=numOfSelectedItems|numInShopCart|cartCurrency|cartTotal|numInCompareCart|selectedProduct|catalogContents|shoppingCartContents|compareCartContents"
                         -- , "--varsSelect=numInCompareCart|compareCartContents"
                         , "--output=" ++ daikonfile
                         , eventFiltersOption
                         , l])
  mapM_ log2daikon loxLogs
  when isInfer $ do
    -- dtraceLogs <- glob $ daikonDir </> "*_new*.dtrace"
    dtraceLogs <- glob $ daikonDir </> "*.dtrace"
    -- trace (show dtraceLogs) $ return ()
    report <- run ("java", [ "-cp"
                           , daikonJar
                           , "daikon.Daikon"
                           , "-o"
                           , invFile
                           , "--conf_limit"
                           , "0.99"
                           , "--nohierarchy"
                           , "--output_num_samples"
                           ] ++ dtraceLogs) :: IO String
    writeFile (trace reportFile reportFile) report


logs2dtraces :: FilePath -> Bool -> IO ()
logs2dtraces logsDir isInfer = do
  daikonDir    <- return $ logsDir </> "daikon"
  oldDaikonDir <- glob daikonDir
  when (oldDaikonDir /= []) $ runIO ("rm", ["-rf", daikonDir])     
  runIO ("mkdir", [daikonDir])
  logLogs' <- glob $ logsDir </> "*_new*.log"
  let inferDOraclePerVar :: [FilePath] -> String -> IO ()
      inferDOraclePerVar logs var = do
        let fieldDIR = daikonDir </> ("app." ++ var ++ ".orc")
        runIO ("mkdir", [fieldDIR])
        let log2daikon l = do
              let flog = l 
                  daikonfile = fieldDIR </> takeBaseName l <.> "dtrace"
              print ("Converting " ++ daikonfile ++ " to Daikon format...")
              runIO ("haslog", filter (/="") 
                               [ optsGHCRTForInferdaikon
                               , "-d"
                               , "--output=" ++ daikonfile
                               , eventFiltersOption
                               , "--varsSelect=" ++ var
                               , flog])
        mapM_ log2daikon logs
        daikonfiles     <- glob $ fieldDIR </> "*.dtrace"
        runIO ("dtrsplit", daikonfiles)
        daikonDeclfiles <- glob $ fieldDIR </> "*.decls"
        runIO ("declmerge", daikonDeclfiles)
        let separatedDIR = fieldDIR </> "separated"
        runIO ("mkdir", [separatedDIR])
        recs_dtrace_files <- glob $ fieldDIR </> "*.recs.dtrace"
        runIO ("mv", recs_dtrace_files ++ [separatedDIR])
        runIO ("rm", daikonDeclfiles)
        runIO ("mv", ["merged.decls", separatedDIR])
        when isInfer $ do
          daikonfiles2 <- liftM words $ findLogs separatedDIR ".recs" "dtrace"
          let invFile    = fieldDIR </> nameOfOracleFile
              mergedDecl = separatedDIR </> "merged.decls"
              reportFile = fieldDIR </> "app." ++ var ++ ".orc" <.> "txt"
          report <- run ("java", [ "-cp"
                                 , daikonJar
                                 , "daikon.Daikon"
                                 , "-o"
                                 , invFile
                                 -- , "--conf_limit"
                                 -- , "0.99"
                                 , "--nohierarchy"
                                 , "--output_num_samples"
                                 , mergedDecl
                                 ] ++ daikonfiles2) :: IO String
          writeFile reportFile report
  mapM_ (inferDOraclePerVar logLogs') fieldsToInclude


checkDaikonInferedInvariants :: FilePath -> FilePath -> Int -> Int -> IO ()
checkDaikonInferedInvariants oldTraces newTraces confl lsize = do
  let oraclesGroups   = oldTraces </> "daikon" </> "*.orc"
      daikoniceReport = newTraces </> "daikonice_" ++ show lsize ++ "_" ++ show confl <.> "html"
      checkTracesVsInv invF = do
        let traceDir = newTraces </> "daikon" </> invF
            checkDIR = traceDir </> "check_" ++ show lsize
            oracleFile          = oldTraces </> "daikon" </> invF </> nameOfOracleFile
            violationReportFile = checkDIR </>  invF  <.> nameOfViolationsRecordFile
            reportDumpFile      = checkDIR </> "report.txt"
            haslogOracles       = oldTraces </> "daikon" </> invF </> invF <.> "txt"
        runIO ("rm", ["-rf", checkDIR])
        runIO ("mkdir", [checkDIR])
        runIO ("cp", ["-v", haslogOracles, checkDIR])
        daikontraces <- glob $ traceDir </> "*.dtrace"
        report       <- run ("java", [ "-cp"
                                     , daikonJar
                                     , "daikon.tools.InvariantChecker"
                                     , "--verbose"
                                     , "--conf"
                                     , "--filter"
                                     , "--output"
                                     , violationReportFile
                                     , oracleFile ]
                                     ++ daikontraces) :: IO String
        writeFile reportDumpFile report
        return (haslogOracles, violationReportFile)
  (daikonReports, violationReports) <- liftM unzip . mapM (checkTracesVsInv . basename) <=< glob $ oraclesGroups
  runIO ("daikonice", [ "--oracles=" ++ unwords daikonReports
                      , "--violations=" ++ unwords violationReports
                      , "--output=" ++ daikoniceReport
                      , "--witness-number=" ++ show confl
                      ])
  parseOracleReport  daikoniceReport


checkDaikonInferedInvariants_ :: FilePath -> FilePath -> Int -> IO ()
checkDaikonInferedInvariants_ oldTraces newTraces lsize = do
  let newTraceDir         = newTraces </> "daikon"
      oldTraceDir         = oldTraces </> "daikon"
      checkDIR            = newTraceDir </> "check_" ++ show lsize
      oracleFile          = oldTraceDir </> takeBaseName oldTraces <.> nameOfOracleFile
      violationReportFile = checkDIR </> takeBaseName oldTraces <.> nameOfViolationsRecordFile
      reportDumpFile      = checkDIR </> takeBaseName oldTraces <.> nameOfReportFile
  runIO ("rm", ["-rf", checkDIR])
  runIO ("mkdir", [checkDIR])
  daikontraces_old <- glob $ newTraceDir </> "*.dtrace"
  report       <- run ("java", [ "-cp"
                               , daikonJar
                               , "daikon.tools.InvariantChecker"
                               , "--verbose"
                               , "--conf"
                               , "--filter" 
                               , "--output"
                               , violationReportFile
                               , oracleFile ]
                               ++ daikontraces_old) :: IO String
  writeFile reportDumpFile report


countDaikonReportViolations :: [FilePath] -> [FilePath] -> IO Float
countDaikonReportViolations daikonReportFiles violationsFiles = undefined


prepareLogs :: FilePath -> Bool -> IO ()
prepareLogs logF already = do
  when (already) $ do
    oldNewLogs <- glob $ logF </> "*_new.log"
    when  (not $ null oldNewLogs) $ runIO ("rm", ["-v"] ++ oldNewLogs)
    rawLogs <- glob $ logF </> "*.log"
    let prepareLog l = do
          let lnew = replaceBaseName l $ takeBaseName l ++ "_new"
          runIO ("cp", [l, lnew])
          runIO ("haslog", ["-c", lnew])
          runIO ("haslog", ["-x", "--appEventOnly", replaceExtension lnew "lox"])
          runIO ("haslog", ["--toraw", replaceExtension lnew "xml"])
    mapM_ prepareLog rawLogs
  newLogs <- glob $ logF </> "*_new.log"
  writeFile "inlogsfile.txt" $ unlines $ take 500 $ drop 0 newLogs
  print $ "The numder of supplied logs: " ++ (show $ length newLogs)
  patterns <- run ("lopi", [ "-i"
                           -- , "--inf-step=5000"  
                           , "-m"
                           -- , "--in-logs=" ++ (unwords $ take 1000 newLogs)
                           , "--in-logs-file=inlogsfile.txt"
                           , "--filter-rules=filterBothZero"
                           , "+RTS", "-sstderr", "-hc", "-p", "-K2G"
                           ]
                  ) :: IO String
  writeFile (logF </> "rules.txt") patterns
  return ()

data TCInferenceType = TC_SEQ | TC_STATE 


instance Show TCInferenceType where
  show TC_SEQ   = "fsm_sequencebased_exec"
  show TC_STATE = "fsm_statebased_exec"

flexsLogSource :: VersionNumber -> TCInferenceType -> FilePath
flexsLogSource vN ty = "../../Execution/Oracles/flexstore" ++ show vN ++ "_" ++ show ty ++ "/testgroup_all/logs"  

flexstore1_fsm_sequencebased = "../../Execution/flexstore1_fsm_sequencebased_exec/testgroup_all/logs"

flexstore1_fsm_statebased = "../../Execution/flexstore1_fsm_statebased_exec/testing_sessions/testgroup_all/logs"

parseOracleReport :: FilePath -> IO ()
parseOracleReport fp = do
  ifile <- UTF8.readFile fp
  let tags = parseTags ifile
      tbl_stats = take 80 $ head $ sections (~== (TagOpen "table" [("id","tbl_stats")])) tags
      tbl_rows   = partitions (~== TagOpen "tr" []) tbl_stats
      oracles_row = read (fromTagText $ (tbl_rows !! 2) !! 11) :: Float
      viol_row = read (fromTagText $ (tbl_rows !! 3) !! 11) :: Float
  print $ "*** Total numbder of oracles: " ++ show oracles_row
  print $ "*** Total numbder of violations: " ++ show viol_row
  print $ "*** FP rate: " ++ show (viol_row / oracles_row)

-- | ========================= LOPI Cross-validation ========================



runLOPIValidation :: VersionNumber -> FilePath -> FilePath -> String -> [Int] -> IO [(Int, Float)]
runLOPIValidation ver validSet validRules confLevel = mapM (runLOPIValidation' ver) 
    where
      runLOPIValidation' :: VersionNumber -> Int -> IO (Int, Float) 
      runLOPIValidation' ver_ lsize = do
          print $ "*** Exercising logs of the total size: " ++ show lsize               
          runIO ("make", ["clean_trim"])
          splitLogsByStep ver_ lsize
          inLogsXml  <- liftM words $ findLogs (flexstoreVersion ver_) "_trimby" "xml"
          print inLogsXml
          logSize <- compLogsSize inLogsXml
          let xml2raw l = runIO ("haslog", ["--toraw", replaceExtension l "xml"])
          mapM_ xml2raw inLogsXml
          inLogs    <- liftM words $ findLogs (flexstoreVersion ver_) "_trimby" "log"
          let newRules  = validSet </> "lopi_" ++ confLevel ++ "_" ++ show lsize <.> "pat"
              oldRules  = validSet </> validRules
              diffRules = validSet </> "lopi_valid_" ++ confLevel ++ "_" ++ show lsize <.> "diff"
          runIO ("lopi", [ "-i"
                         , "-m"  
                         , "--in-logs=" ++ unwords inLogs
                         , "--filter-rules=" ++ confLevel
                         , "--rew-rules=" ++ newRules
                         ])
          negCount <- run ("lopi", [ "-c"
                                 , "-n"
                                 , "--cross-check-rules="
                                   ++ newRules
                                   ++ " "
                                   ++ oldRules
                                   ++ " "
                                   ++ diffRules
                                 ]
                          ) :: IO String
          print ("log size: " ++ show lsize ++ " | FP rate: " ++ negCount)
          return (lsize, read negCount :: Float)


-- | ========================= Daikon Cross-validation ========================


runDaikonValidation :: VersionNumber -> FilePath -> Int -> [Int] -> IO ()
runDaikonValidation ver validLogDir lconf lsizes = do
  let dOrcFolder = flexstore_folder_base ++ show ver ++ "_daikon"
      runDaikonValidationRound lsize = do
        runDaikonOracleInference ver dOrcFolder lsize
        checkDaikonInferedInvariants dOrcFolder validLogDir lconf lsize
        runIO ("rm", ["-rf", dOrcFolder])
  mapM_ runDaikonValidationRound lsizes 

mergeRrules = do rules <- run ("lopi", [ "--witness-concat"
                                       , "--rew-rules="
                                         ++ flexsLogSource 5 TC_STATE </> "test_rules" </> "all_rules.txt"
                                       ]) :: IO String
                 writeFile (flexsLogSource 5 TC_STATE </> "test_rules" </> "merged_rules.txt") rules



countEventsInSplits :: VersionNumber -> FilePath -> [Int] -> IO ()
countEventsInSplits ver_ resDir = mapM_ countEventsInSplits'
  where
    countEventsInSplits' :: Int -> IO ()
    countEventsInSplits' lsize = do   
      print $ "*** Exercising logs of the total size: " ++ show lsize               
      runIO ("make", ["clean_trim"])
      splitLogsByStep ver_ lsize
      inLogsXml  <- liftM words $ findLogs (flexstoreVersion ver_) "_trimby" "xml"
      -- print inLogsXml
      -- logSize <- compLogsSize inLogsXml
      let xml2raw l = runIO ("haslog", ["--toraw", replaceExtension l "xml"])
      mapM_ xml2raw inLogsXml
      inLogs    <- liftM words $ findLogs (flexstoreVersion ver_) "_trimby" "log"
      writeFile "inlogsfile.txt" $ unlines inLogs
      events <- run ("lopi", [ "--get-events"
                             , "--in-logs-file=inlogsfile.txt"
                             ]
                    ) :: IO String
      writeFile (resDir </> "events_" ++ show lsize <.> "txt") events



eventsFromValSet logF flxv = do
  newLogs <- glob $ logF </> "*_new.log"
  writeFile "inlogsfile.txt" $ unlines newLogs
  events <- run ("lopi", [ "--get-events"
                         , "--in-logs-file=inlogsfile.txt"
                         ]
                ) :: IO String
  writeFile (logF </> "events_" ++ flxv <.> "txt") events



-- | Utility to count the number of reported oracles in .inv file

countDaikonOracles :: FilePath -> IO ()
countDaikonOracles oFile = do
  ppOrcRep <- run ("java", [ "-cp"
                           , daikonJar
                           , "daikon.diff.Diff"
                           , "-d"
                           , "-u"
                           -- , "-y"
                           -- , "--ignore_unjustified"  
                           -- , "-a"
                           , oFile
                           ]) :: IO String
  -- print $ map head $ lines $ ppOrcRep
  let orcList = (lines ppOrcRep)
      oracleCount = foldr (\(l:_) acc -> acc + (if (l == ' ') then 1 else 0)) 0 orcList
  print $ show oracleCount
  -- print ppOrcRep
  return ()

countDaikonOracles2 :: FilePath -> IO ()
countDaikonOracles2 oFile = do
  ppOrcRep <- run ("java", [ "-cp"
                           , daikonJar
                           , "daikon.PrintInvariants"
                           , oFile
                           ]) :: IO String
  let orcList = (lines ppOrcRep)
      pptN = length $ filter (\l -> head l == '=') orcList
  print $ show ((length orcList) - 2 * pptN)
  -- print ppOrcRep
  return ()

  
