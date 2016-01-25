import System.Environment
import DcServerIO
import DcPeerIO

main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-p"] = runPeer
parseArgs _ = runServer
