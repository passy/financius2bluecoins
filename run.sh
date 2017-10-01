#!/usr/bin/env bash

# This is just a script to let me quickly iterate and test this on a device.

cp bluecoins_empty.fydb bluecoins_workingcopy.fydb && \
stack build --fast && \
stack exec financius2bluecoin -- \
      --baseCurrency GBP \
      --financiusFile "./Financius 2017-09-30 121507.json" \
      --bluecoinFile bluecoins_workingcopy.fydb \
      --eurofxrefFile eurofxref-hist.zip && \
adb push bluecoins_workingcopy.fydb /sdcard/Bluecoins/Database_Export/bluecoins_bak.fydb

# vim:tw=0
