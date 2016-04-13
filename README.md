### Status
[![Build Status](https://travis-ci.org/sergadin/SecurityServer.svg?branch=master)](https://travis-ci.org/sergadin/SecurityServer)
[![Coverage Status](https://coveralls.io/repos/github/sergadin/SecurityServer/badge.svg?branch=master)](https://coveralls.io/github/sergadin/SecurityServer?branch=master)

### CBAC SecurityServer

How to start
============

Load the system by evaluating
  (asdf:operate 'asdf:load-op :secsrv)

Then run main function
  (secsrv:main)


How to run tests
================

(asdf:operate 'asdf:load-op :secsrv-test)

(lift:run-tests :suite 'root :break-on-errors? nil)

### External dependencies

https://github.com/sergadin/dbd-oracle
