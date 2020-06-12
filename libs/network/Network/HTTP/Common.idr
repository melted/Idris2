||| HTTP 1.1 handling

module Network.HTTP.Common

import Data.Buffer
import Data.List

public export
data HTTPError = ParseError String | OtherError String

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
    -- TODO: should be able to use a Buffer here
    content : String


-- TODO: need to keep apart URI that's used by network and
-- what goes into the Request
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

version : String
version = "HTTP/1.1"

renderBody : Body -> String
renderBody b = maybe "" (\t => "content-type:" ++ t ++ crlf) (contentType b) ++
               "content-size:" ++ show (contentSize b) ++ crlf ++ crlf ++
               content b

renderHeader : (String, String) -> String
renderHeader (name, content) = name ++ ":" ++ content ++ crlf

export
buildRequestString : Request -> String
buildRequestString req = show (requestMethod req) ++ " " ++ requestUri req ++ " " ++
    requestVersion req ++ crlf ++ concatMap renderHeader (requestHeaders req) ++
    maybe "" renderBody (requestBody req)

export
buildResponseString : Response -> String
buildResponseString resp = responseVersion resp ++ " " ++
    show (responseCode resp) ++ " " ++  responseText resp ++ crlf ++
    concatMap renderHeader (responseHeaders resp) ++
    maybe "" renderBody (responseBody resp)

export
request : Method -> String -> List (String, String) -> Maybe String -> Maybe String -> Request
request meth uri headers ct body = MkRequest uri meth version headers
                                    (map (\c => MkBody ct (cast $ length c) c) body)

export
getRequest : String -> Request
getRequest uri = request GET uri [] Nothing Nothing

getStatusText : Int -> String
getStatusText 100 = "Continue"
getStatusText 101 = "Switching Protocols"
getStatusText 200 = "OK"
getStatusText 201 = "Created"
getStatusText 202 = "Accepted"
getStatusText 203 = "Non-Authoritative Information"
getStatusText 204 = "No Content"
getStatusText 205 = "Reset Content"
getStatusText 300 = "Multiple Choices"
getStatusText 301 = "Moved Permanently"
getStatusText 302 = "Found"
getStatusText 303 = "See Other"
getStatusText 305 = "Use Proxy"
getStatusText 307 = "Temporary Redirect"
getStatusText 400 = "Bad Request"
getStatusText 402 = "Payment Required"
getStatusText 403 = "Forbidden"
getStatusText 404 = "Not Found"
getStatusText 405 = "Method Not Allowed"
getStatusText 406 = "Not Acceptable"
getStatusText 408 = "Request Timeout"
getStatusText 409 = "Conflict"
getStatusText 410 = "Gone"
getStatusText 411 = "Length Required"
getStatusText 413 = "Payload Too Large"
getStatusText 414 = "URI Too Long"
getStatusText 415 = "Unsupported Media Type"
getStatusText 417 = "Expectation Failed"
getStatusText 426 = "Upgrade Required"
getStatusText 500 = "Internal Server Error"
getStatusText 501 = "Not Implemented"
getStatusText 502 = "Bad Gateway"
getStatusText 503 = "Service Unavailable"
getStatusText 504 = "Gateway Timeout"
getStatusText 505 = "HTTP Version Not Supported"
getStatusText _ = ""

export
response : Int -> List (String, String) -> Maybe String -> Maybe String -> Response
response code headers ctype body = MkResponse version code (getStatusText code)
            headers (map (\c => MkBody ctype (cast $ length c) c) body)

export
ok : String -> Response
ok text = response 200 [] (Just "text/html") (Just text)

export
notFound : Maybe String -> Response
notFound text = response 404 [] (Just "text/html") text
