||| FFI binding to the low-Level C Sockets bindings for Idris.
|||
||| Modified (C) The Idris Community, 2020
module Network.Socket.FFI

import Network.Socket.Data
import Data.Buffer
-- From sys/socket.h

%foreign "C:close,libidris2_support"
export
socket_close : (sockdes : SocketDescriptor) -> PrimIO Int

%foreign "C:listen,libc 6"
export
socket_listen : (sockfd : SocketDescriptor) -> (backlog : Int) -> PrimIO Int


-- From idris_net.h

%foreign "C:idrnet_socket,libidris2_support"
export
idrnet_socket : (domain, type, protocol : Int) -> PrimIO Int

%foreign "C:idrnet_bind,libidris2_support"
export
idrnet_bind : (sockfd : SocketDescriptor) ->
              (family, socket_type : Int) ->
              (host : String) -> (port : Port) -> PrimIO Int

%foreign "C:idrnet_connect,libidris2_support"
export
idrnet_connect : (sockfd : SocketDescriptor) -> (family, socket_type : Int) -> (host : String)
               -> (port : Port) -> PrimIO Int

%foreign "C:idrnet_sockaddr_family,libidris2_support"
export
idrnet_sockaddr_family : (sockaddr : AnyPtr) -> PrimIO Int

%foreign "C:idrnet_sockaddr_ipv4,libidris2_support"
export
idrnet_sockaddr_ipv4 : (sockaddr : AnyPtr) -> PrimIO String

%foreign "C:idrnet_sockaddr_ipv4_port,libidris2_support"
export
idrnet_sockaddr_ipv4_port : (sockaddr : AnyPtr) -> PrimIO Int

%foreign "C:idrnet_sockaddr_port,libidris2_support"
export
idrnet_sockaddr_port : (sockfd : SocketDescriptor) -> PrimIO Int


%foreign "C:idrnet_create_sockaddr,libidris2_support"
export
idrnet_create_sockaddr : PrimIO AnyPtr

%foreign "C:idrnet_accept,libidris2_support"
export
idrnet_accept : (sockfd : SocketDescriptor) -> (sockaddr : AnyPtr) -> PrimIO Int

%foreign "C:idrnet_send,libidris2_support"
export
idrnet_send : (sockfd : SocketDescriptor) -> (dataString : String) -> PrimIO Int

%foreign "C:idrnet_send_buf,libidris2_support"
export
idrnet_send_buf : (sockfd : SocketDescriptor) -> (dataBuffer : AnyPtr) -> (len : Int) -> PrimIO Int
%foreign "C:idrnet_send_buf,libidris2_support"
export
idrnet_send_buffer : (sockfd : SocketDescriptor) -> (dataBuffer : Buffer) -> (len : Int) -> PrimIO Int

%foreign "C:idrnet_recv,libidris2_support"
export
idrnet_recv : (sockfd : SocketDescriptor) -> (len : Int) -> PrimIO AnyPtr

%foreign "C:idrnet_recv_buf,libidris2_support"
export
idrnet_recv_buf : (sockfd : SocketDescriptor) -> (buf : AnyPtr) -> (len : Int)
               -> PrimIO Int


%foreign "C:idrnet_recv_buf,libidris2_support"
export
idrnet_recv_buffer : (sockfd : SocketDescriptor) -> (buf : Buffer) -> (len : Int)
               -> PrimIO Int

%foreign "C:idrnet_sendto,libidris2_support"
export
idrnet_sendto : (sockfd : SocketDescriptor) -> (dataString,host : String)
             -> (port : Port) -> (family : Int) -> PrimIO Int

%foreign "C:idrnet_sendto_buf,libidris2_support"
export
idrnet_sendto_buf : (sockfd : SocketDescriptor) -> (dataBuf : AnyPtr) -> (buf_len : Int)
                 -> (host : String) -> (port : Port) -> (family : Int) -> PrimIO Int

%foreign "C:idrnet_sendto_buf,libidris2_support"
export
idrnet_sendto_buffer : (sockfd : SocketDescriptor) -> (dataBuf : Buffer) -> (buf_len : Int)
                 -> (host : String) -> (port : Port) -> (family : Int) -> PrimIO Int

%foreign "C:idrnet_recvfrom,libidris2_support"
export
idrnet_recvfrom : (sockfd : SocketDescriptor) -> (len : Int) -> PrimIO AnyPtr

%foreign "C:idrnet_recvfrom_buf,libidris2_support"
export
idrnet_recvfrom_buf : (sockfd : SocketDescriptor) -> (buf : AnyPtr) -> (len : Int)
                   -> PrimIO AnyPtr

%foreign "C:idrnet_recvfrom_buf,libidris2_support"
export
idrnet_recvfrom_buffer : (sockfd : SocketDescriptor) -> (buf : Buffer) -> (len : Int)
                   -> PrimIO AnyPtr
%foreign "C:idrnet_get_recv_res,libidris2_support"
export
idrnet_get_recv_res : (res_struct : AnyPtr) -> PrimIO Int

%foreign "C:idrnet_get_recv_payload,libidris2_support"
export
idrnet_get_recv_payload : (res_struct : AnyPtr) -> PrimIO String

%foreign "C:idrnet_free_recv_struct,libidris2_support"
export
idrnet_free_recv_struct : (res_struct : AnyPtr) -> PrimIO ()

%foreign "C:idrnet_get_recvfrom_res,libidris2_support"
export
idrnet_get_recvfrom_res : (res_struct : AnyPtr) -> PrimIO Int

%foreign "C:idrnet_get_recvfrom_payload,libidris2_support"
export
idrnet_get_recvfrom_payload : (res_struct : AnyPtr) -> PrimIO String

%foreign "C:idrnet_get_recvfrom_sockaddr,libidris2_support"
export
idrnet_get_recvfrom_sockaddr : (res_struct : AnyPtr) -> PrimIO AnyPtr

%foreign "C:idrnet_free_recvfrom_struct,libidris2_support"
export
idrnet_free_recvfrom_struct : (res_struct : AnyPtr) -> PrimIO ()


%foreign "C:idrnet_geteagain,libidris2_support"
export
idrnet_geteagain : PrimIO Int

%foreign "C:idrnet_errno,libidris2_support"
export
idrnet_errno : PrimIO Int

%foreign "C:idrnet_malloc,libidris2_support"
export
idrnet_malloc : (size : Int) -> PrimIO AnyPtr

%foreign "C:idrnet_free,libidris2_support"
export
idrnet_free : (ptr : AnyPtr) -> PrimIO ()

%foreign "C:idrnet_peek,libidris2_support"
export
idrnet_peek : (ptr : AnyPtr) -> (offset : {-Unsigned-} Int) -> PrimIO {-Unsigned-} Int

%foreign "C:idrnet_poke,libidris2_support"
export
idrnet_poke : (ptr : AnyPtr) -> (offset : {-Unsigned-} Int) -> (val : Int {- should be Char? -})
           -> PrimIO ()
