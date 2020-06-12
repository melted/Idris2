||| HTTP 1.1 handling

module Network.HTTP.Common

import Data.Buffer
import Data.List

public export
data Method = GET | PUT | HEAD | POST | DELETE | OPTIONS | TRACE |
              CONNECT | CUSTOM String

Show Method where
    show GET = "GET"
    show PUT = "PUT"
    show HEAD = "HEAD"
    show POST = "POST"
    show DELETE = "DELETE"
    show OPTIONS = "OPTIONS"
    show TRACE = "TRACE"
    show CONNECT = "CONNECT"
    show (CUSTOM str) = str

Eq Method where
    x == y = (show x) == (show y)

record Body where
    constructor MkBody
    contentType : Maybe String
    contentSize : Int
    content : String

export
record Request where
    constructor MkRequest
    -- TODO: make an URI type
    requestUri : String
    requestMethod : Method
    requestVersion : String
    requestHeaders : List (String, String)
    requestBody : Maybe Body

export
record Response where
    constructor MkResponse
    responseVersion : String
    responseCode : Int
    responseText : String
    responseHeaders : List (String, String)
    responseBody : Maybe Body

crlf : String
crlf = "\r\n"

renderBody : Body -> String
renderBody b = maybe "" (\t => "content-type: " ++ t ++ crlf) (contentType b) ++
               "content-size: " ++ show (contentSize b) ++ crlf ++ crlf ++
               content b

renderHeader : (String, String) -> String
renderHeader (name, content) = show name ++ ": " ++ show content ++ crlf

export
buildRequestString : Request -> String
buildRequestString req = show (requestMethod req) ++ " " ++ requestUri req ++ " " ++
    requestVersion req ++ crlf ++ concatMap renderHeader (requestHeaders req) ++
    maybe "" renderBody (requestBody req)

export
request : Method -> String -> List (String, String) -> Maybe String -> Maybe String -> Request
request meth uri headers ct body = MkRequest uri meth "HTTP/1.1" headers
                                    (map (\c => MkBody ct (cast $ length c) c) body)

export
getRequest : String -> Request
getRequest uri = request GET uri [] Nothing Nothing

