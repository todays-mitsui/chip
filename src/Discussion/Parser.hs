type StParser = Parsec [Token] ()

stpAssign :: StParser [Token]
stpAssign = do
  lhs <- many1 stpWord
  stpAssignOp
  rhs <- many1 stpAnyToken
  eof
  return $ lhs ++ [Symbol "="] ++ rhs

--------------------------------------------------------------------------------

stpAnyToken :: StParser Token
stpAnyToken = satisfy' $ const True

stpWord :: StParser Token
stpWord = satisfy' isWord

stpAssignOp :: StParser Token
stpAssignOp = satisfy' $
  \tok -> case tok of
               Symbol "=" -> True
               _          -> False

--------------------------------------------------------------------------------

satisfy' :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy' f = tokenPrim (\t -> show t)
                       (\pos t _ts -> updatePosChar pos '_')
                       (\t -> if f t then Just t else Nothing)
