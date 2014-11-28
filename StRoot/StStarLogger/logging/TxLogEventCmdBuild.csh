#!/bin/tcsh
setenv LOGGER_DIR  $STAR/StRoot/StStarLogger/logging
g++ -O2 -o .${STAR_HOST_SYS}/bin/TxLogEventCmd -I$OPTSTAR/include -I$OPTSTAR/include/mysql $LOGGER_DIR/TxLogEventCmd.C $LOGGER_DIR/*.cxx $LOGGER_DIR/*.cpp -L$OPTSTAR/lib -L$OPTSTAR/lib/mysql -lmysqlclient -llog4cxx
ls -l .${STAR_HOST_SYS}/bin/TxLogEventCmd
