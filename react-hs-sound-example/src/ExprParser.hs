module ExprParser where


import Control.Applicative
import Data.Char as Char


import Parser
import Synth
import Timed
import Expr


pWhitespace :: Parser String
pWhitespace = many (sat Char.isSpace)


pName :: Parser Name
pName = do
  c <- sat Char.isLower
  cs <- many (sat Char.isAlphaNum)
  pWhitespace
  return (c:cs)


pNameUppercase :: Parser Name
pNameUppercase = do
  c <- sat Char.isUpper
  cs <- many (sat Char.isAlphaNum)
  pWhitespace
  return (c:cs)


pSynthName :: Parser String
pSynthName = do
  c <- sat (=='$')
  cs <- many (sat Char.isAlphaNum)
  pWhitespace
  return (c:cs)


pLookupSynth :: Parser Expr
pLookupSynth = do
  n <- pSynthName
  return $ LookupSynthExpr n


pLetSynth :: Parser Expr
pLetSynth = do
  x <- pNameUppercase
  if x == "Def" then do
    n <- pSynthName
    e1 <- pExpr
    e2 <- pExpr
    return $ LetSynthExpr n e1 e2
  else
    empty


pDigits :: Parser String
pDigits = some (sat Char.isDigit)


pVar :: Parser Expr
pVar = do
  name <- pName
  return (VarExpr name)


pAbs :: Parser Expr
pAbs = do
  sat (=='\\')
  pWhitespace
  name <- pName
  sat (=='.')
  pWhitespace
  body <- pExpr
  return (AbsExpr name body)


pApp :: Parser Expr
pApp = do
  sat (=='@')
  pWhitespace
  a <- pExpr
  b <- pExpr
  return (AppExpr a b)


pLet :: Parser Expr
pLet = do
  s1 <- pName
  if s1 == "let" then do
    x <- pName
    sat (=='=')
    pWhitespace
    e1 <- pExpr
    e2 <- pExpr
    return (AppExpr (AbsExpr x e2) e1)
  else
    empty


pBool :: Parser Expr
pBool = do
  u <- pNameUppercase
  if (u == "True") then
    return (BoolExpr True)
  else if (u == "False") then
    return (BoolExpr False)
  else
    empty


pFloat :: Parser Expr
pFloat = do
  d1 <- pDigits
  sat (=='.')
  d2 <- pDigits
  pWhitespace
  return (FloatExpr (read (d1++"."++d2)))


pInt :: Parser Expr
pInt = do
  d1 <- pDigits
  pWhitespace
  return (IntExpr (read d1))


pIf :: Parser Expr
pIf = do
  s1 <- pName
  if s1 == "if" then do
    e1 <- pExpr
    e2 <- pExpr
    e3 <- pExpr
    return (IfExpr e1 e2 e3)
  else
    empty


pAssign :: Parser Expr
pAssign = do
  s1 <- pNameUppercase
  if s1 == "Set" then do
    n <- pName
    e1 <- pExpr
    e2 <- pExpr
    return (AssignExpr n e1 e2)
  else
    empty


pWait :: Parser Expr
pWait = do
  s1 <- pNameUppercase
  if s1 == "Wait" then do
    e1 <- pExpr
    e2 <- pExpr
    return (WaitExpr e1 e2)
  else
    empty


pFork :: Parser Expr
pFork = do
  s1 <- pNameUppercase
  if s1 == "Fork" then do
    e1 <- pExpr
    e2 <- pExpr
    return (ForkExpr e1 e2)
  else
    empty


pPlay :: Parser Expr
pPlay = do
  s1 <- pNameUppercase
  if s1 == "Play" then do
    e1 <- pExpr
    e2 <- pExpr
    return (PlayExpr e1 e2)
  else
    empty


pLoop :: Parser Expr
pLoop = do
  s1 <- pNameUppercase
  if s1 == "Loop" then do
    e1 <- pExpr
    e2 <- pExpr
    e3 <- pExpr
    return (LoopExpr e1 e2 e3)
  else
    empty


pLoopInf :: Parser Expr
pLoopInf = do
  s1 <- pNameUppercase
  if s1 == "LoopInf" then do
    e1 <- pExpr
    return (LoopInfExpr e1)
  else
    empty


pDone :: Parser Expr
pDone = do
  s1 <- pNameUppercase
  if s1 == "End" then
    return DoneExpr
  else
    empty


pOsc :: Parser Expr
pOsc = do
  s1 <- pNameUppercase
  if s1 == "Osc" then do
    wf <- pWaveForm
    e1 <- pExpr
    e2 <- pExpr
    return (OscExpr wf e1 e2)
  else
    empty


pGain :: Parser Expr
pGain = do
  s1 <- pNameUppercase
  if s1 == "Gain" then do
    sat (=='[')
    pWhitespace
    ins <- many pExpr
    sat (==']')
    pWhitespace
    e <- pExpr
    return (GainExpr ins e)
  else
    empty


pFilterType :: Parser FilterType
pFilterType = do
  s1 <- pNameUppercase
  case s1 of
    "Lowpass" ->
      return Lowpass

    "Highpass" ->
      return Highpass
    _ ->
      empty


pWaveForm :: Parser WaveForm
pWaveForm = do
  s1 <- pNameUppercase
  case s1 of
    "Sine" ->
      return Sine

    "Saw" ->
      return Saw
    _ ->
      empty


pFilter :: Parser Expr
pFilter = do
  s1 <- pNameUppercase
  if s1 == "Filter" then do
    ft <- pFilterType
    sat (=='[')
    pWhitespace
    ins <- many pExpr
    sat (==']')
    pWhitespace
    e1 <- pExpr
    e2 <- pExpr
    return (FilterExpr ft ins e1 e2)
  else
    empty


pEnv :: Parser Expr
pEnv = do
  s1 <- pNameUppercase
  if s1 == "Env" then do
    d0 <- pExpr
    sat (=='(')
    pWhitespace
    ds <- some $ do
      v <- pExpr
      t <- pExpr
      return (v, t)
    sat (==')')
    pWhitespace
    return (EnvExpr d0 ds)
  else
    empty


pRandom :: Parser Expr
pRandom = do
  s1 <- pNameUppercase
  if s1 == "Random" then do
    e1 <- pExpr
    e2 <- pExpr
    return (RandomExpr e1 e2)
  else
    empty


pSymbols :: Parser String
pSymbols = some (sat (\c -> elem c ("+-*/%<>=^" :: String)))


pOp2 :: Parser Expr
pOp2 = do
  s <- pSymbols
  pWhitespace
  if elem s ["+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "/=", "^"] then do
    e1 <- pExpr
    e2 <- pExpr
    return $ Op2 s e1 e2
  else
    empty


pOp1 :: Parser Expr
pOp1 = do
  n <- pNameUppercase
  pWhitespace
  if elem n ["Not", "Round", "Floor" :: String] then do
    e <- pExpr
    return $ Op1 n e
  else
    empty


pExpr :: Parser Expr
pExpr = pIf <|> pLet <|> pVar <|> pAbs <|> pApp <|> pBool <|> pFloat <|> pInt <|> pAssign <|> pWait <|> pFork <|> pPlay <|> pLoop <|> pLoopInf <|> pOsc <|> pGain <|> pEnv <|> pFilter <|> pOp2 <|> pOp1 <|> pRandom <|> pDone <|> pLetSynth <|> pLookupSynth


parseExpr :: String -> Maybe Expr
parseExpr s =
  case unParser (pWhitespace >> pExpr) (removeComments s) of
    Just (expr, "") -> Just expr
    _ -> Nothing


removeComments :: String -> String
removeComments s =
  unlines (removeComment <$> lines s)


removeComment :: String -> String
removeComment ('#' : _) = ""
removeComment (c : r) = c : removeComment r
removeComment [] = ""

