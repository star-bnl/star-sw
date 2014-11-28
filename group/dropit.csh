#!/bin/csh -f


set count=0
set FS=":"
set DS=":"
set STRINGS=""
set SUBSTR=""

source $GROUP_DIR/unix_programs.csh

while ( $count <= $#argv )
    set arg=$argv[$count]
    @ count++

    switch ($arg)
	case "-i":
	    set FS=$argv[$count]
	    @ count++
	    breaksw

	case "-d":
	    set DS=$argv[$count]
	    @ count++
	    breaksw

	case "-p":
	    set addst=$argv[$count]
	    if ("$addst" != "") set STRINGS="$STRINGS $addst"
	    @ count++
	    breaksw

	case "--help":
	    /bin/cat <<EOF

% dropit [-i input_delimiter] [-d output_delimiter] [-p string] [substring] 

      DESCRIPTION
          dropit's intended purpose is to remove program library names
          from a PATH shell variable value, which has colon separated
          fields.  dropit is usable in sh, ksh, and csh shell script
          and source files.

          If the -i option is not specified, the input separator is a
          colon.  If the -d option is not specified, the output
          separator is a colon.  If the -p option is not specified,
          the value of the PATH shell environment variable is used.

          Option value meanings are:

             input_delimiter
                       input separator

             output_delimiter
                       output separator

             string    the string containing colon separated fields to
                       be operated on

             substring a string which if it is a substring of a field
                       causes the field to be omitted


EOF
	breaksw

	default:
	    if ("$arg" == "$0.csh") then
		if ("$arg" != "") set SUBSTR="$SUBSTR $arg"
	    endif
	    breaksw
    endsw
end

if ("$STRINGS" == "") set STRINGS=`echo $PATH`

# Replace space by separator
set STRINGS=`echo $STRINGS | sed "s/ /$FS/g"`
set CLEANED=`echo $STRINGS | sed "s/$FS/ /g"`
set newword=""


# Remove $SUBSTR from any element
if ( "$SUBSTR" != "") then
    foreach word ($CLEANED)
	#echo "Trying [$word] [$SUBSTR]"
	if (`echo $word | $GREP $SUBSTR` == "") then
	    set newword="$newword $word"
	endif
    end
    set FINALST="$newword"
else
    set FINALST="$CLEANED"
endif


# Remove duplicate 
set newword=""
foreach word ($FINALST)
    if (`echo $word | /usr/bin/cut -b 1` == ".") then
	set newword="$newword $word"
	goto NEXTW
    endif
    # beware that the "." paths are short-ends for longer
    # path. This test needs to be done after the "." test
    if (`echo " $newword " | $GREP " $word "` != "") then
	#echo "DEBUG:: excluding [$word] from [$newword]"
	goto NEXTW
    endif

  
    # The next section will have for effect to skip directory check
    if (`echo $word | $GREP ROOT` != "") then
	set newword="$newword $word"
	goto NEXTW
    endif
    if (`echo $word | $GREP scratch` != "") then
	set newword="$newword $word"
	goto NEXTW
    endif
  
    # Check if directory exist 
    if ( -d $word || -l $word ) then
	set newword="$newword $word"
    endif

    NEXTW:
end

# We are done
set FINALST=`echo $newword | sed "s/ /$DS/g"`

if ("$FINALST" != "") then
    echo $FINALST
else
    echo $STRINGS
endif

