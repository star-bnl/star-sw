#!/bin/csh
#
if ( $#argv < 2 ) then
	echo " "
	echo Usage: $0 pkg file "[ ... ]"
	echo Example:
	echo '	'$0' xxx ../idl/*.idl'
	echo " "
	exit
endif
#
set pkg = $1
set argv[1] = " "
set pams = ` egrep -c amiModule $* | egrep -v :0 | sed -e 's/:.*//' `
set pams = ` egrep -h 'interface.*:.*amiModule' $* | sed -e 's/:.*//' -e 's/.*interface[ 	]//' `
#
echo '/* automatically generated file -- DO NOT EDIT */'
foreach p ($pams)
	echo '#include "'$p'.h"'
end
echo 'extern "C" int'" ${pkg}_init(void), ${pkg}_start(void), ${pkg}_stop(void);"
echo "int ${pkg}_init() { return 1; }"
echo "int ${pkg}_start() {"
foreach p ($pams)
	echo "	${p}_load_ami(ami);"
end
echo 'return 1; }'
echo "int ${pkg}_stop() { return 1; }"
#
