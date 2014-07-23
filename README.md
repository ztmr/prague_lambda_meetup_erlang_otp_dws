Distributed WebSockets
======================

Erlang/OTP demo project to show:
- Cowboy
- WebSockets
- Erlang cluster
- Erlang maps datatype
- Mnesia
- Crash-aware ETS tables

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

Also, we don't recommend to use service `on_load` together
with `dws_broker_handlers` configuration option since it
may interfere each other.
The `on_load` is actually the better way of doing the job.

TODO
----
- automatized JavaScript proxy class generator
- implement a nice D3.js powered OTP processes visualisation
  (using the `DWS.Example.get_system_processes`)
  and Mnesia statistics (using the `DWS.Example.get_mnesia_info`)
- implement a nice WebChat demo:
  - `dws_service_chat` backend
  - a React.js based frontend
- add a way to "upgrade" the communication protocol to BERT
  or MsgPack, since the JSON is evil. What about WAMPv2?

