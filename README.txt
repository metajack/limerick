limerick

limerick is a mocking XMPP BOSH server for testing BOSH client
implementations.

It is mainly useful for testing various connection errors.

A client connects to limerick, authenticates using SASL ANONYMOUS, and
then idles. limerick will not respond to anything other than during
authentication.

Once connected, limerickctl can be used to inject errors into the BOSH
session. Hard connection failures and various HTTP errors can be
simulated in order to test their affect on client implementations.

Building:

limerick is an Erlang/OTP application and is built using rebar
(https://github.com/basho/rebar). It depends on mochiweb, which rebar
will fetch during get-deps.

$ rebar get-deps
$ rebar compile
$ start.sh

Using:

limerick currently runs on port 5280 and responds to either
/xmpp-httpbind or /http-bind URLs.

License:

limerick is licensed under the MIT license included as LICENSE.txt.
