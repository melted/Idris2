||| Low-Level C Sockets bindings for Idris. Used by higher-level, cleverer things.
|||
||| Original (C) SimonJF, MIT Licensed, 2014
||| Modified (C) The Idris Community, 2015, 2016, 2019
module Network.Socket

import public Network.Socket.Data
import Network.Socket.FFI
import Network.Socket.Raw

import Data.Buffer
import Data.List


export
interface ToBuffer a where
  toBuffer : HasIO io => a -> io Buffer

export
interface FromBuffer a where
  fromBuffer : HasIO io => Buffer -> io a

export
ToBuffer Buffer where
  toBuffer = pure

export
FromBuffer Buffer where
  fromBuffer = pure

export
ToBuffer String where
  toBuffer s = do
    Just buf <- newBuffer $ stringByteLength s
      | Nothing => toBuffer s -- Looks dangerous but it will never get here
    setString buf 0 s
    pure buf
  
export
FromBuffer String where
  fromBuffer buf = do
    size <- rawSize buf
    getString buf 0 size

-- ----------------------------------------------------- [ Network Socket API. ]

||| Creates a UNIX socket with the given family, socket type and protocol
||| number. Returns either a socket or an error.
export
socket : HasIO io
      => (fam  : SocketFamily)
      -> (ty   : SocketType)
      -> (pnum : ProtocolNumber)
      -> io (Either SocketError Socket)
socket sf st pn = do
  socket_res <- primIO $ idrnet_socket (toCode sf) (toCode st) pn

  if socket_res == -1
    then map Left getErrno
    else pure $ Right (MkSocket socket_res sf st pn)

||| Close a socket
export
close : HasIO io => Socket -> io ()
close sock = do _ <- primIO $ socket_close $ descriptor sock
                pure ()

||| Binds a socket to the given socket address and port.
||| Returns 0 on success, an error code otherwise.
export
bind : HasIO io
    => (sock : Socket)
    -> (addr : Maybe SocketAddress)
    -> (port : Port)
    -> io Int
bind sock addr port = do
    bind_res <- primIO $ idrnet_bind
                  (descriptor sock)
                  (toCode $ family sock)
                  (toCode $ socketType sock)
                  (saString addr)
                  port
    if bind_res == (-1)
      then getErrno
      else pure 0
  where
    saString : Maybe SocketAddress -> String
    saString (Just sa) = show sa
    saString Nothing   = ""

||| Connects to a given address and port.
||| Returns 0 on success, and an error number on error.
export
connect : HasIO io
       => (sock : Socket)
       -> (addr : SocketAddress)
       -> (port : Port)
       -> io ResultCode
connect sock addr port = do
  conn_res <- primIO $ idrnet_connect
              (descriptor sock) (toCode $ family sock) (toCode $ socketType sock) (show addr) port

  if conn_res == (-1)
    then getErrno
    else pure 0

||| Listens on a bound socket.
|||
||| @sock The socket to listen on.
export
listen : HasIO io => (sock : Socket) -> io Int
listen sock = do
  listen_res <- primIO $ socket_listen (descriptor sock) BACKLOG
  if listen_res == (-1)
    then getErrno
    else pure 0

||| Accept a connection on the provided socket.
|||
||| Returns on failure a `SocketError`
||| Returns on success a pairing of:
||| + `Socket`        :: The socket representing the connection.
||| + `SocketAddress` :: The
|||
||| @sock The socket used to establish connection.
export
accept : HasIO io
      => (sock : Socket)
      -> io (Either SocketError (Socket, SocketAddress))
accept sock = do

  -- We need a pointer to a sockaddr structure. This is then passed into
  -- idrnet_accept and populated. We can then query it for the SocketAddr and free it.

  sockaddr_ptr <- primIO idrnet_create_sockaddr

  accept_res <- primIO $ idrnet_accept (descriptor sock) sockaddr_ptr
  if accept_res == (-1)
    then map Left getErrno
    else do
      let (MkSocket _ fam ty p_num) = sock
      sockaddr <- getSockAddr (SAPtr sockaddr_ptr)
      sockaddr_free (SAPtr sockaddr_ptr)
      pure $ Right ((MkSocket accept_res fam ty p_num), sockaddr)

||| Send data on the specified socket.
|||
||| Returns on failure a `SocketError`.
||| Returns on success the `ResultCode`.
|||
||| @sock The socket on which to send the message.
||| @msg  The data to send.
export
send : HasIO io => ToBuffer a
    => (sock : Socket)
    -> (msg  : a)
    -> io (Either SocketError ResultCode)
send sock dat = do
  buf <- toBuffer dat
  send_res <- primIO $ idrnet_send_buffer (descriptor sock) buf !(rawSize buf)
  if send_res == (-1)
    then map Left getErrno
    else pure $ Right send_res

||| Receive data on the specified socket.
|||
||| Returns on failure a `SocketError`
||| Returns on success a pairing of:
||| + `String`     :: The payload.
||| + `ResultCode` :: The result of the underlying function.
|||
||| @sock The socket on which to receive the message.
||| @len  How much of the data to receive.
export
recv : HasIO io => FromBuffer a
    => (sock : Socket)
    -> (len : ByteLength)
    -> io (Either SocketError (a, ResultCode))
recv sock len = do
  -- Firstly make the request, get some kind of recv structure which
  -- contains the result of the recv and possibly the retrieved payload
  Just buf <- newBuffer len
        | Nothing => pure $ Left EBUFFER
  recv_res <- primIO $ idrnet_recv_buffer (descriptor sock) buf len

  if recv_res == (-1)
    then do
      errno <- getErrno
      freeBuffer buf
      pure $ Left errno
    else
      if recv_res == 0
        then do
           freeBuffer buf
           pure $ Left 0
        else do
           dat <- fromBuffer buf
           pure $ Right (dat, recv_res)

||| Receive all the remaining data on the specified socket.
|||
||| Returns on failure a `SocketError`
||| Returns on success the payload `String`
|||
||| @sock The socket on which to receive the message.
export
recvAll : HasIO io => FromBuffer a => (sock : Socket) -> io (Either SocketError a)
recvAll sock = recvRec [] 64
  where
    recvRec : List Buffer -> Int -> io (Either SocketError a)
    recvRec acc n = case !(recv sock n) of
                      Left 0 => case !(concatBuffers (reverse acc)) of
                                  Just buf => pure $ Right !(fromBuffer buf)
                                  Nothing => pure $ Left EBUFFER
                      Left c => pure $ Left c
                      Right (buf, _) => let n' = min (n*2) 65536 in recvRec (buf :: acc) n'


||| Send a message.
|||
||| Returns on failure a `SocketError`
||| Returns on success the `ResultCode`
|||
||| @sock The socket on which to send the message.
||| @addr Address of the recipient.
||| @port The port on which to send the message.
||| @msg  The message to send.
export
sendTo : HasIO io => ToBuffer a
      => (sock : Socket)
      -> (addr : SocketAddress)
      -> (port : Port)
      -> (msg  : a)
      -> io (Either SocketError ByteLength)
sendTo sock addr p dat = do
  buf <- toBuffer dat
  sendto_res <- primIO $ idrnet_sendto_buffer
                (descriptor sock) buf !(rawSize buf) (show addr) p (toCode $ family sock)

  if sendto_res == (-1)
    then map Left getErrno
    else pure $ Right sendto_res

||| Receive a message.
|||
||| Returns on failure a `SocketError`.
||| Returns on success a triple of
||| + `UDPAddrInfo` :: The address of the sender.
||| + `String`      :: The payload.
||| + `Int`         :: Result value from underlying function.
|||
||| @sock The channel on which to receive.
||| @len  Size of the expected message.
|||
export
recvFrom : HasIO io => FromBuffer a
        => (sock : Socket)
        -> (len  : ByteLength)
        -> io (Either SocketError (UDPAddrInfo, a, ResultCode))
recvFrom sock bl = do
  Just buf <- newBuffer bl
      | Nothing => pure $ Left EBUFFER
  recv_ptr <- primIO $ idrnet_recvfrom_buffer
                       (descriptor sock) buf bl

  let recv_ptr' = RFPtr recv_ptr
  isNull <- (nullPtr recv_ptr)
  if isNull
    then do
      freeBuffer buf
      map Left getErrno
    else do
      result <- primIO $ idrnet_get_recvfrom_res recv_ptr
      if result == -1
        then do
          freeBuffer buf
          freeRecvfromStruct recv_ptr'
          map Left getErrno
        else do
          port <- foreignGetRecvfromPort recv_ptr'
          addr <- foreignGetRecvfromAddr recv_ptr'
          freeRecvfromStruct recv_ptr'
          dat <- fromBuffer buf
          pure $ Right (MkUDPAddrInfo addr port, dat, result)
