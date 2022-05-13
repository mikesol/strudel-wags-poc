module StrudelPoc (poc, mini, Cycle) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons, span)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (oneOfMap, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (pow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Effect (Effect, foreachE)
import Effect.Now (now)
import Effect.Ref (new, read, write)
import Effect.Ref as Ref
import Effect.Timer (clearInterval, setInterval)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (Behavior, sampleBy, step)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, bang, create, keepLatest, makeEvent, mapAccum, subscribe)
import FRP.Event.AnimationFrame (animationFrame)
import Foreign (Foreign)
import Simple.JSON as JSON
import WAGS.Clock (withACTime)
import WAGS.Control (gain, gain_, sawtoothOsc)
import WAGS.Core (Audible, AudioEnvelope(..), dyn, sound, silence)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Properties as P
import WAGS.Run (run2)
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.Window (cancelIdleCallback, requestIdleCallback)

type Span = { s :: Int, n :: Int, d :: Int }
type Hap = { part :: { begin :: Span, end :: Span }, value :: Foreign }

data Cycle

foreign import mini :: Void
foreign import mkCycle :: String -> Cycle
foreign import queryArc :: Cycle -> Number -> Number -> Array Hap

lowPrioritySchedule :: (Number -> Effect Unit -> Effect Unit) -> Number -> Event ~> Event
lowPrioritySchedule f n e = makeEvent \k -> do
  void $ subscribe e \i ->
    f n (k i)
  pure (pure unit)

animate :: AudioContext -> Behavior Number -> Behavior Cycle -> Event { removeAt :: Number, time :: Number, pitch :: Int }
animate ctx clengthB cycleB = keepLatest $ map (oneOfMap bang) $ mapAccum
  ( \{ behaviors: { clength, cycle, tnow }, acTime } { writeAdj, prevACTime, prevAdjTime } -> do
      let prevAC = fromMaybe 0.0 prevACTime
      let prevAJ = fromMaybe 0.0 prevAdjTime
      let gap = acTime - prevAC
      let adjGap = gap / clength
      let adjTime = adjGap + prevAJ
      let lookAhead = 0.3
      let
        f wa =
          if wa < adjTime + lookAhead then
            ( let
                wa1 = wa + 1.0
                q /\ r = f wa1
              in
                q
                  /\
                    ( cons
                        ( let
                            haps = queryArc cycle wa wa1
                          in
                            mapWithIndex (\i { part: { begin }, value } -> { removeAt: (unwrap $ unInstant tnow) + 5000.0 + toNumber i, pitch: either (const 60) identity $ JSON.read value, time: calcSlope prevAJ prevAC adjTime acTime ((toNumber begin.n) / (toNumber begin.d)) }) haps
                        )
                        r
                    )
            )
          else wa /\ []
      let w /\ a = f writeAdj
      { writeAdj: w, prevACTime: Just acTime, prevAdjTime: Just adjTime } /\ join a
  )
  ( sampleBy { behaviors: _, acTime: _ }
      ({ clength: _, cycle: _, tnow: _ } <$> clengthB <*> cycleB <*> instant)
      (withACTime ctx animationFrame <#> _.acTime)
  )
  { writeAdj: 0.0, prevACTime: Nothing, prevAdjTime: Nothing }

midi2cps :: Int -> Number
midi2cps i = 440.0 * (2.0 `pow` (((toNumber i) - 69.0) / 12.0))

graph
  :: forall lock payload
   . (Number -> Effect Unit -> Effect Unit)
  -> Event { removeAt :: Number, time :: Number, pitch :: Int }
  -> Array (Audible D2 lock payload)
graph lps e =
  [ gain_ 1.0
      [ dyn $
          map
            ( \x ->
                ( ( bang $ sound
                      ( gain 0.0 (bang $ P.gain (AudioEnvelope { d: 0.5, o: x.time + 0.02, p: [ 0.0, 0.1, 0.5, 0.2, 0.05, 0.01, 0.0 ] }))
                          [ sawtoothOsc (midi2cps x.pitch) (bang (P.onOff x.time)) ]
                      )
                  )
                    <|> (lowPrioritySchedule lps x.removeAt (bang silence))
                )
            )
            e
      ]
  ]

poc :: Effect { start :: Effect Unit, stop :: Effect Unit, clen :: Number -> Unit, pat :: Cycle -> Unit }
poc = do
  w <- window
  eClen <- create
  ePat <- create
  icid <- new Nothing
  running <- new false
  cancel <- new (pure unit)
  unschedule <- new Map.empty
  pure
    { clen: map unsafePerformEffect eClen.push
    , pat: map unsafePerformEffect ePat.push
    , start: do
        r <- read running
        unless r do
          ctx <- context
          hk <- constant0Hack ctx
          ci <- setInterval 5000 do
            Ref.read icid >>= traverse_ (flip cancelIdleCallback w)
            requestIdleCallback { timeout: 0 }
              ( do
                  n <- unwrap <<< unInstant <$> now
                  mp <- Map.toUnfoldable <$> Ref.read unschedule
                  let lr = span (fst >>> (_ < n)) mp
                  foreachE lr.init snd
                  Ref.write (Map.fromFoldable lr.rest) unschedule
                  pure unit
              )
              w <#> Just >>= flip Ref.write icid
          st <- run2 ctx (graph (\k v -> Ref.modify_ (Map.insert k v) unschedule) (animate ctx (step 1.0 eClen.event) (step (mkCycle "~") ePat.event)))
          write (st *> hk *> clearInterval ci *> close ctx) cancel
        write true running
    , stop: do
        r <- read running
        when r do
          join (read cancel)
          write (pure unit) cancel
        write false running
    }