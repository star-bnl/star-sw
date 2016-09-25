#!/bin/sh
echo "Convert to Athena environment..."
DEPENDENCIES=platform/to-Athena.dependencies

if [ -e $DEPENDENCIES ]
then
	echo "WARNING: $DEPENDENCIES exist!"
	echo "         Which means this script has already been used."
	echo "         Remove this file to force re-run."
	exit 0
fi

# Routine for changing single routine
swap()
{
	N1=$1
	N2=$1mc
	N3=`echo $1 | tr '[:lower:]' '[:upper:]'`
	N4="$N3"MC
	sed -i -e "s/$N1/$N2/g" `find tauola-fortran/tauola-F/*.F src/*/*.cxx src/*/*.h src/*/*.f src/*/*.c -type f`
	sed -i -e "s/$N3/$N4/g" `find tauola-fortran/tauola-F/*.F src/*/*.cxx src/*/*.h src/*/*.f src/*/*.c -type f`
}

# If someone already compiled the interface
# We can use up-to-date fortran routine list
# Used by our interface.
if [ -e lib/libTauolaCxxInterface.so ]
then
	echo "INFO: Updating dependencies list..."
	ldd -r lib/*.so 2>&1 | grep -e "^undefined symbol: [^_].*_" > $DEPENDENCIES
	if [ ! -e $DEPENDENCIES ]
	then
		echo "ERROR: Something went wrong with ldd."
		echo "       $DEPENDENCIES file does not exist."
	fi
else
	echo "INFO: No libraries created. Using old dependencies list."
	if [ ! -e $DEPENDENCIES.list ]
	then
		echo "ERROR: Dependencies list missing. "
		echo "       To restore it, compile libraries normally,"
		echo "       then run this script again."
		exit -1
	fi
	cp -f $DEPENDENCIES.list $DEPENDENCIES
fi


# Use the list of dependencies to substitute references
# Temporaly skipping ranmar - it seems to restore itself.
# Don't know why yet.
while read line
do
	NLINE=`expr index "$line" _`
	SLINE=${line:18:$NLINE-19}
	if [ "${line:$NLINE-21:$NLINE-19}" == "mc" ]
	then
		echo "ERROR: Reference to: $SLINE has 'mc' at the end!"
		echo "       Which means some conversion has already been used."
		echo "       It would be dangerous to apply any changes."
		exit -1
	fi
	if [ "$SLINE" == "ranmar" ]
	then
		echo "(Skipping ranmar for now)"
		continue
	fi
	echo "      Substituting references to: $SLINE"
	swap $SLINE
done < $DEPENDENCIES

# Copy as our backup list only if everything was ok.
if [ -e $DEPENDENCIES ]
then
	cp -f $DEPENDENCIES $DEPENDENCIES.list
fi
echo "Done."