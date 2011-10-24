{-
  This file is part of BajaMud.
    
  BajaMud -- MUD client library
  Copyright (C) 2011  Endre Tamas SAJO

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Game.BajaMud.Telnet
  --( Connection
  --, Option
  --, connect
  --, send
  --, receive
  --, close
  --)
  where

import Control.Applicative ((<|>), (<$>), (<*>), (<*), (*>))
import Control.Monad.State
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Network.Socket hiding (connect, send, sendTo, recv, recvFrom)
import qualified Network.Socket as S
import Network.Socket.ByteString (sendAll, recv)

type Telnet = StateT Connection IO

data Connection = MkConn
  { connSocket :: Socket
  , connParseBuf :: Maybe (Result Message)
  }
  deriving (Show)

defaultConnection :: Connection
defaultConnection = MkConn 
  { connSocket = error "no value for connSocket"
  , connParseBuf = Nothing
  }

poll :: Telnet (Maybe Message)
poll = do
  connection <- get
  connected <- lift . sIsConnected $ connSocket connection

  if connected
    then do
      received <- lift $ receive connection

      let result = case connParseBuf connection of
            Nothing -> parse messageLine received
            Just (Done str _) -> parse messageLine (str `BS.append` received)
            Just part@(Partial _) -> feed part received

      case result of
          Fail _ _ _ -> return Nothing

          part@(Partial _) -> do
            put connection { connParseBuf = Just part }
            return Nothing

          done@(Done _ msg) -> do
            put connection { connParseBuf = Just done }
            return $ Just msg

    else return Nothing

newtype Option = MkOption Char
  deriving (Eq)

instance Show Option where
  show (MkOption ch) = show $ ord ch

data SubCommand = Will | Wont | Do | Dont
  deriving (Eq, Show)

data Message =
    OnBandLine String
  | Command Char
  | Negotiation SubCommand Option
  | SubNegotiation Option String
  deriving (Show)

connect :: HostName -> PortNumber -> IO Connection
connect host port = do
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
  addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  let addr = head addrs
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  S.connect sock (addrAddress addr)
  return $ defaultConnection { connSocket = sock }

send :: Connection -> String -> IO ()
send conn msg = do
  let sock = connSocket conn

  sendAll sock $ BS.pack msg

receive :: Connection -> IO BS.ByteString
receive conn = do
  let sock = connSocket conn
  msg <- recv sock 1024

  return msg

close :: Connection -> IO ()
close conn = sClose $ connSocket conn


messageLine :: Parser Message
messageLine = onBand <|> offBand
  where
    onBand = OnBandLine <$> onBandText <* onBandTextEnd <?> "on-band message"
    onBandText = many onBandTextChar <?> "on-band text"
    onBandTextChar = satisfy (notInClass [iac, cr]) <|> char iac *> char iac
                     <?> "on-band character"
    onBandTextEnd = char cr *> char lf <|> char cr *> char nul

    offBand = char iac *> command <?> "off-band message"
    command = simpleCommand <|> negotiation <|> subNegotiation <?> "command"

    simpleCommand = Command <$> simpleCommandChar <?> "simple command"
    simpleCommandChar = satisfy $ inClass [nop,dm,brk,ip,ao,ayt,ec,el,ga]

    negotiation = Negotiation <$> subCommand <*> (MkOption <$> anyChar)
                  <?> "negotiation"
    subCommand = const Will <$> char optWill
            <|> const Wont <$> char optWont
            <|> const Do <$> char optDo
            <|> const Dont <$> char optDont
            <?> "negotiation subcommand"

    subNegotiation = SubNegotiation <$> (MkOption <$> (char sb *> anyChar))
                                    <*> subNegotiationText
                                    <* char iac <* char se
                                    <?> "subnegotiation"
    subNegotiationText = many1 subNegotiationChar
                          <?> "subnegotiation parameters"
    subNegotiationChar = notChar iac <|> char iac *> char iac
                          <?> "subnegotiation character"

nul, cr, lf, se, nop, dm, brk,
  ip, ao, ayt, ec, el, ga, sb,
  optWill, optWont, optDo, optDont,
  iac
  :: Char

nul     = chr 0
cr      = chr 13
lf      = chr 10
se      = chr 240
nop     = chr 241
dm      = chr 242
brk     = chr 243
ip      = chr 244
ao      = chr 245
ayt     = chr 246
ec      = chr 247
el      = chr 248
ga      = chr 249
sb      = chr 250
optWill = chr 251
optWont = chr 252
optDo   = chr 253
optDont = chr 254
iac     = chr 255

