#!/usr/bin/env bash

# This is just a script to let me quickly iterate and test this on a device.

cp bluecoins_empty.fydb bluecoins_workingcopy.fydb && \
stack build --fast && \
stack exec financius2bluecoin -- --financiusFile Financius\ 2017-08-23\ 184323.json --bluecoinFile bluecoins_workingcopy.fydb && \
adb push bluecoins_workingcopy.fydb /sdcard/Bluecoins/Database_Export/bluecoins_bak.fydb

# vim:tw=0
