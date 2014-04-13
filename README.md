Distributed WebSockets
======================

Erlang/OTP demo project to show:
- Cowboy
- WebSockets
- Erlang cluster
- Erlang maps datatype
- Mnesia

Usage
-----
```
$ git clone https://github.com/ztmr/dws.git
$ cd dws
$ make release rundevrel
```
Now let's point your browser to:
- http://localhost:10011/
- http://localhost:10021/
- http://localhost:10031/

(Note that `rundevrel` expects you to have `screen` installed.)

Notes
-----
If you end up with the following message:
```
ERROR: OTP release R15B does not match required regex 17
```
...you need to upgrade your Erlang/OTP to 17.0+.

