import           Conduit
import           Control.Arrow
import           Control.Monad.State.Strict
import           System.Random

type BotId      = Int
type TeamId     = Int
type Index      = Int
type Position   = (Float, Float)
type Mass       = Int
type Blob       = (BotId, TeamId, Index, Position, Mass)
type Food       = (Position, Mass)
type Toxin      = (Position, Mass)
type OwnBlobs   = [Blob]
type OtherBlobs = [Blob]
type Input      = (OwnBlobs, OtherBlobs, [Food], [Toxin])

data BlobAction = None | Throw | Split
                deriving (Eq, Show, Enum, Bounded)

type Output     = (BlobAction, Position)

-- | A 'Bot' is a mealy automaton carrying its internal state.
data Bot = Bot { runBot :: Input -> (Output, Bot) }

-- Dimensions:
-- (0,0) .. .. .. .. .. ..
-- .. .. .. .. .. .. .. ..
-- .. .. .. .. .. .. .. ..
-- .. .. .. .. (1000, 1000)

-- | Given a random generator, generates a BlobAction
randomBlobAction :: RandomGen g => g -> (BlobAction, g)
randomBlobAction g =
  first toEnum
  $ randomR (fromEnum minAction, fromEnum maxAction)
  $ g
  where
    minAction :: BlobAction
    minAction = minBound

    maxAction :: BlobAction
    maxAction = maxBound

-- | Creates a new random bot doing random actions and
-- going to random places.
newRandomBot :: BotId -> IO Bot
newRandomBot _bid = do
  rnd0 <- newStdGen
  return $ Bot (go rnd0)
  where
    go :: StdGen -> Input -> (Output, Bot)
    go rnd (_ownBlobs, _otherBlobs, _foods, _toxins) =
      let
        (out, rnd') = runState (do action <- state randomBlobAction
                                   x_pos  <- state (randomR (0.0, 1000.0))
                                   y_pos  <- state (randomR (0.0, 1000.0))
                                   return (action, (x_pos, y_pos))
                               ) rnd
      in (out, Bot (go rnd'))

main :: IO ()
main = do
  rbot <- newRandomBot 1

  stdinC
    =$= linesUnboundedC
    =$= inputC
    =$= bot rbot
    =$= outputC
    =$= unlinesC
    $$ stdoutC
  where
    bot :: Bot -> Conduit Input IO Output
    bot = go
      where go b = do
              minp <- await
              case minp of
                Just inp -> do let (output, b') = runBot b inp
                               yield output
                               go b'
                Nothing -> return ()

    inputC :: Conduit String IO Input
    inputC = awaitForever (yield . read)

    outputC :: Conduit Output IO String
    outputC = awaitForever (yield . show)
