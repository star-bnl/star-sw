#!/bin/sh
echo "$Id: deploy.sh,v 1.1 2010/04/13 16:51:39 fine Exp $"
# This script has be invoked from $CATALINA/share/lib in the server side
rm -rf ucmdist.tar.gz
wget http://www.rhic.bnl.gov/~fine/tmp/ucmdist.tar.gz
echo "----------DO -------------"
echo "tar -xzvf ucmdist.tar.gz"
echo "----------now------------"
