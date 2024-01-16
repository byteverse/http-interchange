# Revision history for http-interchange

## 0.3.2.0 -- 2024-01-16

* Add `Eq` and `Show` to all data types.

## 0.3.1.0 -- 2023-08-16

* Add these to `Http.Headers`: cons, snoc, lookupHost, lookupAccept,
  lookupDate, lacksContentLengthAndTransferEncoding,
  snocContentLength.

## 0.3.0.0 -- 2023-08-15

* Add `Http.Headers.fromList`. In examples, this makes it easier
  to create HTTP requests.
* Add `Eq` and `Show` instances for `Bodied`.
* Get rid of `versionMajor` and `versionMinor` from both requests
  and responses. Only support HTTP/1.1.

## 0.2.0.0 -- 2023-08-14

* Redo module structure
* Introduce Headers type. Currently a low performance implementation.
* Introduce Http.Types module, which reexports all types

## 0.1.0.0 -- 2023-07-25

* Initial release
