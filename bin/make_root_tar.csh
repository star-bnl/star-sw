#! /usr/local/bin/tcsh -f
tar cvfz root_${SystemName}.gz `find . -type f ! -name "*.o" ! -name "*.d" ! -name "G__*.*"`
# eod
