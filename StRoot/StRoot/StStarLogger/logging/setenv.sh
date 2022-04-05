#!/bin/sh

###############################################################
#   It is used to set  the shared lib for all servlets        #
#   It is required for JNI-based servlets                     #
#   $Id: setenv.sh,v 1.3 2010/04/13 21:31:26 fine Exp $
###############################################################

if [ -d $CATALINA_HOME/shared/lib ]; then 
   LD_LIBRARY_PATH=$CATALINA_HOME/shared/lib
   # we need "tx_ucm.jar" on the global classpath to be used from JSP scripts
   export CLASSPATH=$CLASSPATH:$CATALINA_HOME/shared/lib/tx-ucm.jar:$CATALINA_HOME/shared/lib

   # we need "tx_ucm.jar" and "JNI" shared lib to be used from the different servlets concurrently
   export LD_LIBRARY_PATH=$CATALINA_HOME/shared/lib
   export JAVA_OPTS="$JAVA_OPTS -Djava.library.path=$CATALINA_HOME/shared/lib"

else
   echo "ATTENTION !!! no $CATALINA_HOME/shared/lib is defined yet"
   echo "Make sure the shared.loader property from $CATALINA_HOME/conf/catalina.properties is set as well" 
   echo "grep  shared.loader  $CATALINA_HOME/conf/catalina.properties"
   grep  shared.loader  $CATALINA_HOME/conf/catalina.properties 
fi
