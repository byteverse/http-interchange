# Revision history for http-interchange

## 0.3.0.0 -- 2023-??-??

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
