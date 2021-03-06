module Args (
  Args(..)
, runWithArgs
) where


import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Options.Applicative
import           System.Environment


data Args = Args { argVerbose :: !Bool
                 , argNJTLine :: !Text
                 , argNJTTrains :: ![Int]
                 , argLookBackTime :: !NominalDiffTime
                 , argFcmTo :: !Text
                 , argFcmIcon :: !(Maybe Text)
                 , argFcmColor :: !(Maybe Text)
                 , argAuthKey :: !Text
                 , argRefreshIntervalMicroseconds :: !Int
                 } deriving (Show)


parseArgs :: Maybe String -- ^ default auth key, comes from env var
          -> Parser Args
parseArgs defAuthKey = Args
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "Be verbose.")
     <*> (T.pack <$> strOption
         ( long "nj-line"
        <> short 'l'
        <> help "NJT line abbreviation (https://www.njtransit.com/mt/mt_servlet.srv?hdnPageAction=MTNotificationsTo)." ))
     <*> some (read <$> strOption
         ( long "nj-train"
        <> short 'a'
        <> help "NJT train number"))
     <*> ((\m -> - fromIntegral (m * 60)) <$>
         (option auto
         ( long "look-back-time-minutes"
        <> short 't'
        <> value 60
        <> showDefault
        <> help "Discard notifications older than this number of minutes." ) :: Parser Word))
     <*> (T.pack <$> strOption
         ( long "fcm-to"
        <> short 'T'
        <> help "FCM 'to' (token or topic)." ))
     <*> optional (T.pack <$> strOption
         ( long "fcm-icon"
        <> short 'I'
        <> help "FCM notification icon." ))
     <*> optional (T.pack <$> strOption
         ( long "fcm-color"
        <> short 'C'
        <> help "FCM notification color rrggbb." ))
     <*> (T.pack <$> strOption
         ( long "fcm-auth-key"
        <> short 'k'
        <> case defAuthKey
             of Just k -> value k
                Nothing -> mempty
        <> help "FCM auth key. Defaults to FCM_AUTH_KEY env var." ))
     <*> ((\m -> fromIntegral m * 60 * 1000 * 1000) <$>
         (option auto
         ( long "refresh-interval-minutes"
        <> short 'r'
        <> value 10
        <> showDefault
        <> help "RSS feed refresh interval, minutes." ) :: Parser Word))


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = do
  k <- lookupEnv "FCM_AUTH_KEY"
  execParser (opts k) >>= rwa
  where
    opts k = info (helper <*> parseArgs k)
      ( fullDesc
     <> progDesc "Polls NJT alerts, sends Android FCM push notifications."
     <> header "Polls NJT alerts, sends Android FCM push notifications." )
