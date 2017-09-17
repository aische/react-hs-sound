module Expr where


import GHCJS.DOM.JSFFI.AudioContext
import GHCJS.DOM.JSFFI.Generated.AudioNode
import Control.Concurrent
import System.Random as Rand


import Parser
import Synth
import Timed


type Name = String


type OName = String


data Expr
  = BoolExpr Bool
  | IntExpr Int
  | FloatExpr Double
  | VarExpr Name
  | AbsExpr Name Expr
  | AppExpr Expr Expr
  | IfExpr Expr Expr Expr
  | AssignExpr Name Expr Expr
  | Op1 OName Expr
  | Op2 OName Expr Expr
  | PlayExpr Expr Expr
  | WaitExpr Expr Expr
  | ForkExpr Expr Expr
  | LoopExpr Expr Expr Expr
  | LoopInfExpr Expr
  | DoneExpr
  | DcExpr Expr Expr -- amp duration
  | OscExpr WaveForm Expr Expr -- freq-param, duration
  | EnvExpr Expr [(Expr, Expr)]
  | RandomExpr Expr Expr
  | GainExpr [Expr] Expr -- gain-param
  | FilterExpr FilterType [Expr] Expr Expr -- cutoff-param, q-param
  | LetSynthExpr Name Expr Expr
  | LookupSynthExpr Name
  deriving (Eq, Show, Read)


data Value
  = BoolValue Bool
  | IntValue Int
  | FloatValue Double
  | BlockValue [Value]
  | SynthValue Synth
  | EnvValue Double [(Double, Double)]
  | ClosureValue Environment Name Expr
  | StatementValue Environment Statement
  | ActionValue


data Statement
  = AssignStatement Name Expr Expr
  | ForkStatement Expr Expr
  | LoopStatement Expr Expr Expr
  | LoopInfStatement Expr
  | WaitStatement Expr Expr
  | PlayStatement Expr Expr
  | DoneStatement


type Environment = [(Name, MVar Value)]


lookupEnvironment :: Environment -> Name -> IO (Maybe Value)
lookupEnvironment env name =
  case lookup name env of
    Just mvar -> do
      v <- readMVar mvar
      return $ Just v

    Nothing ->
      return Nothing


updateEnvironment :: Environment -> Name -> Value -> IO ()
updateEnvironment env name value =
  case env of
    (n, mvar) : env' ->
      if name == n then do
        _ <- swapMVar mvar value
        return ()
      else
        updateEnvironment env' name value
    [] ->
      error (name ++ " not found in environment")


apply :: Value -> Value -> Timed Value
apply fun arg =
  case fun of
    ClosureValue env name body -> do
      mvar <- liftTimed $ newMVar arg
      eval ((name, mvar) : env) body
    _ ->
      error "apply: not a function"


evalStatement :: Value -> Timed Value
evalStatement value =
  case value of
    StatementValue environment statement ->
      applyStatement environment statement

    _ ->
      error "evalStatement: not a statement"


applyStatement :: Environment -> Statement -> Timed Value
applyStatement environment value =
  case value of
    AssignStatement name e enext -> do
      v <- eval environment e
      liftTimed $ updateEnvironment environment name v
      eval environment enext >>= evalStatement

    PlayStatement e enext -> do
      synth <- eval environment e
      case synth of
        SynthValue s -> do
          playSynth s
          return ActionValue
        _ ->
          error "play: not a synth"
      eval environment enext >>= evalStatement

    WaitStatement e enext -> do
      t <- eval environment e
      case t of
        FloatValue d -> do
          wait d
          eval environment enext >>= evalStatement

        IntValue d -> do
          wait (fromIntegral d)
          eval environment enext >>= evalStatement

        _ ->
          error "wait: not a number"

    ForkStatement e enext -> do
      Timed $ \ac t ots env -> do
        tid <- forkIO $ do
          _ <- unTimed (eval environment e >>= evalStatement) ac t ots env
          myThreadId >>= removeOpenThread ots
          return ()
        addOpenThread ots tid
        return (ActionValue, t)
      eval environment enext >>= evalStatement

    LoopStatement ie e enext -> do
      ie' <- eval environment ie
      case ie' of
        IntValue i -> do
          mapM_ (\ix -> do
            e' <- eval environment e
            case e' of
              ClosureValue _ _ _ -> do
                apply e' (IntValue ix) >>= evalStatement
              _ ->
                error "loop: not a function"
            ) [0..(i-1)]
          eval environment enext >>= evalStatement
        _ ->
          error "loop: not an int value"

    LoopInfStatement e -> do
      mapM_ (\ix -> do
        e' <- eval environment e
        case e' of
          ClosureValue _ _ _ -> do
            apply e' (IntValue ix) >>= evalStatement
          _ ->
            error "loop: not a function"
        ) [0..]
      return ActionValue

    DoneStatement ->
      return ActionValue


eval :: Environment -> Expr -> Timed Value
eval environment expr =
  case expr of
    BoolExpr b ->
      return (BoolValue b)

    IntExpr i ->
      return (IntValue i)

    FloatExpr d ->
      return (FloatValue d)

    VarExpr name -> do
      value <- liftTimed $ lookupEnvironment environment name
      case value of
        Nothing ->
          error (name ++ " undefined")
        Just v ->
          return v

    AbsExpr name body ->
      return (ClosureValue environment name body)

    AppExpr fun arg -> do
      fun' <- eval environment fun
      arg' <- eval environment arg
      apply fun' arg'

    IfExpr cond e1 e2 -> do
      b <- eval environment cond
      case b of
        BoolValue True ->
          eval environment e1
        BoolValue False ->
          eval environment e2
        _ ->
          error "if: not a boolean"

    RandomExpr e1 e2 -> do
      v1 <- eval environment e1
      v2 <- eval environment e2
      case castNumbersToFloat v1 v2 of
        Just (x, y) -> do
          d <- liftTimed $ randomRIO (x, y)
          return $ FloatValue d
        Nothing ->
          error "random: not a number"

    EnvExpr e0 es -> do
      d0 <- eval environment e0
      case d0 of
        FloatValue d0' -> do
          ds <- mapM (\(v,t) -> do
            v' <- eval environment v
            t' <- eval environment t
            case castNumbersToFloat v' t' of
              Just (val, tim) -> do
                return (val, tim)
              Nothing ->
                error "env: not a float value"
            ) es
          return $ EnvValue d0' ds
        IntValue d0' -> do
          ds <- mapM (\(v,t) -> do
            v' <- eval environment v
            t' <- eval environment t
            case castNumbersToFloat v' t' of
              Just (val, tim) -> do
                return (val, tim)
              Nothing ->
                error "env: not a float value"
            ) es
          return $ EnvValue (fromIntegral d0') ds
        _ ->
          error "env: d0 not a float value"

    OscExpr osctype freq dur -> do
      freq' <- eval environment freq
      dur' <- eval environment dur
      case dur' of
        FloatValue duration ->
          case valueToParam freq' of
            Just p ->
              return (SynthValue $ Osc osctype 0.3 p (Just duration))
            _ ->
              error "unknown param type"
        IntValue duration ->
          case valueToParam freq' of
            Just p ->
              return (SynthValue $ Osc osctype 0.3 p (Just $ fromIntegral duration))
            Nothing ->
              error "unknown param type"
        _ ->
          error "Osc: dur not a float"

    GainExpr inputs gain -> do
      inputs' <- mapM (eval environment) inputs
      gain' <- eval environment gain
      case inputsToSynths inputs' of
        Just inputs'' ->
          case valueToParam gain' of
            Just p ->
              return (SynthValue $ Gain inputs'' p)
            Nothing ->
              error "unknown param type"
        Nothing ->
          error "gain: inputs must be synths"


    FilterExpr filtertype inputs cutoff q -> do
      inputs' <- mapM (eval environment) inputs
      cutoff' <- eval environment cutoff
      q' <- eval environment q
      case inputsToSynths inputs' of
        Just inputs'' ->
          case valueToParam cutoff' of
            Just cutoff'' ->
              case valueToParam q' of
                Just q'' ->
                  return (SynthValue $ Filter filtertype inputs'' cutoff'' q'')
                Nothing ->
                  error "filter"
            Nothing ->
              error "filter"
        Nothing ->
          error "filter"

    LetSynthExpr n e1 e2 -> do
      v1 <- eval environment e1
      v2 <- eval environment e2
      case (v1, v2) of
        (SynthValue s1, SynthValue s2) ->
          return $ SynthValue $ Let n s1 s2
        _ ->
          error "let synth: not a synth"

    LookupSynthExpr n ->
      return $ SynthValue $ Var n

    AssignExpr name e enext -> do
      return $ StatementValue environment $ AssignStatement name e enext
      --v <- eval environment e
      --liftTimed $ updateEnvironment environment name v
      --eval environment enext

    PlayExpr e enext ->
      return $ StatementValue environment $ PlayStatement e enext

    WaitExpr e enext ->
      return $ StatementValue environment $ WaitStatement e enext

    ForkExpr e enext ->
      return $ StatementValue environment $ ForkStatement e enext

    LoopExpr ie e enext ->
      return $ StatementValue environment $ LoopStatement ie e enext

    LoopInfExpr e ->
      return $ StatementValue environment $ LoopInfStatement e

    DoneExpr ->
      return $ StatementValue environment $ DoneStatement

    Op2 oname e1 e2 -> do
      v1 <- eval environment e1
      v2 <- eval environment e2
      case numOp2 oname v1 v2 of
        Just v ->
          return v
        Nothing ->
          error (oname ++ " not a number")

    Op1 oname e -> do
      v <- eval environment e
      case evalOp1 oname v of
        Just v' ->
          return v'
        _ ->
          error (oname ++ " some error")



evalOp1 :: OName -> Value -> Maybe Value
evalOp1 oname value =
  case oname of
    "Not" ->
      case value of
        BoolValue b ->
          Just $ BoolValue (not b)
        _ ->
          Nothing

    "Round" ->
      case value of
        FloatValue d ->
          Just $ IntValue (round d)
        IntValue d ->
          Just $ IntValue d
        _ ->
          Nothing

    "Floor" ->
      case value of
        FloatValue d ->
          Just $ IntValue (floor d)
        IntValue d ->
          Just $ IntValue d
        _ ->
          Nothing

    _ ->
      Nothing


castNumbers :: Value -> Value -> Maybe (Either (Int, Int) (Double, Double))
castNumbers v1 v2 =
  case (v1, v2) of
    (IntValue x, IntValue y) ->
      Just $ Left (x, y)

    (IntValue x, FloatValue y) ->
      Just $ Right (fromIntegral x, y)

    (FloatValue x, IntValue y) ->
      Just $ Right (x, fromIntegral y)

    (FloatValue x, FloatValue y) ->
      Just $ Right (x, y)

    _ ->
      Nothing


castNumbersToFloat :: Value -> Value -> Maybe (Double, Double)
castNumbersToFloat v1 v2 =
  case (v1, v2) of
    (IntValue x, IntValue y) ->
      Just (fromIntegral x, fromIntegral y)

    (IntValue x, FloatValue y) ->
      Just (fromIntegral x, y)

    (FloatValue x, IntValue y) ->
      Just (x, fromIntegral y)

    (FloatValue x, FloatValue y) ->
      Just (x, y)

    _ ->
      Nothing


numOp2 :: String -> Value -> Value -> Maybe Value
numOp2 oname v1 v2 = do
  e_ii_ff <- castNumbers v1 v2
  case oname of
    "+" -> do
      either (\(x,y) -> Just $ IntValue (x+y)) (\(x,y) -> Just $ FloatValue (x+y)) e_ii_ff

    "*" ->
      either (\(x,y) -> Just $ IntValue (x*y)) (\(x,y) -> Just $ FloatValue (x*y)) e_ii_ff

    "-" ->
      either (\(x,y) -> Just $ IntValue (x-y)) (\(x,y) -> Just $ FloatValue (x-y)) e_ii_ff

    "/" ->
      either (\(x,y) -> Just $ IntValue (div x y)) (\(x,y) -> Just $ FloatValue (x / y)) e_ii_ff

    "^" ->
      either (\(x,y) -> Just $ IntValue (x ^ y)) (\(x,y) -> Just $ FloatValue (x ** y)) e_ii_ff

    "%" ->
      either (\(x,y) -> Just $ IntValue (mod x y)) (error "%: not an int") e_ii_ff


    "<" ->
      either (\(x,y) -> Just $ BoolValue (x<y)) (\(x,y) -> Just $ BoolValue (x<y)) e_ii_ff

    ">" ->
      either (\(x,y) -> Just $ BoolValue (x>y)) (\(x,y) -> Just $ BoolValue (x>y)) e_ii_ff

    "<=" ->
      either (\(x,y) -> Just $ BoolValue (x<=y)) (\(x,y) -> Just $ BoolValue (x<=y)) e_ii_ff

    ">=" ->
      either (\(x,y) -> Just $ BoolValue (x>=y)) (\(x,y) -> Just $ BoolValue (x>=y)) e_ii_ff

    "==" ->
      either (\(x,y) -> Just $ BoolValue (x==y)) (\(x,y) -> Just $ BoolValue (x==y)) e_ii_ff

    "/=" ->
      either (\(x,y) -> Just $ BoolValue (x/=y)) (\(x,y) -> Just $ BoolValue (x/=y)) e_ii_ff

    _ ->
      Nothing


inputsToSynths :: [Value] -> Maybe [Synth]
inputsToSynths [] = return []
inputsToSynths (SynthValue s : xs) = do
  xs' <- inputsToSynths xs
  return (s : xs')
inputsToSynths _ = Nothing


valueToParam :: Value -> Maybe Param
valueToParam value =
  case value of
    FloatValue v ->
      return (ConstParam v)
    IntValue v ->
      return (ConstParam $ fromIntegral v)
    EnvValue d0 ds ->
      return (EnvParam d0 ds)
    SynthValue s ->
      return (NodeParam s)
    _ ->
      Nothing


runExpr :: Expr -> IO (AudioContext, OpenThreads)
runExpr expr = do
  ac <- newAudioContext
  t <- getCurrentTime ac
  ots <- newMVar []
  tid <- forkIO $ do
    (_, _) <- unTimed (eval [] expr >>= evalStatement) ac t ots []
    myThreadId >>= removeOpenThread ots
    return ()
  addOpenThread ots tid
  return (ac, ots)

