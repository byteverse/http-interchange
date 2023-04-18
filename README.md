# http-interchange

Types, encode functions, and decode functions for HTTP requests and responses.
This is similar in spirit to `http-types`, but it differs in these ways:

* This library includes encoders and decoders for the parts of a
  request/response leading up to the body (but not including the body).
* This library does not have use types from `case-insensitive`.
* This library defines data types instead of aliasing tuples. For example,
  `Header` in `http-types` is defined as `type Header = (HeaderName, ByteString)`.
  Here, it is defined as `data Header = Header {...}`.

This library aims to provide just enough common functionality that someone
could build an HTTP client on top of it. Things that are not goals:

* This library does not use network sockets.

Other notable design decisions:

* This library tries to follow RFC 7230. Response header values are restricted
  to the subset of visible ASCII characters (plus space and tab). This is
  enforced by the decoders but not by the encoders.
