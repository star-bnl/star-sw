#!/bin/csh

# Find and set a few unix program automatically
# May/will be sourced by other scripts

#
# awk is not in a very standard location
# 
if ( ! $?AWK ) then
 set AWK="echo"
 foreach prgm (/sw/bin/awk /bin/awk /usr/bin/awk)
    if ( -x $prgm ) then
	set AWK = $prgm
	break
    endif
 end
endif


# 
# ... and while grep is typically instaled in /bin/,
# several newer variance of Linux has it elsewhere.
# On Unbuntu, Mac OSX for example, it is in /usr/bin
#
if ( ! $?GREP ) then
 set GREP = "grep"
 foreach prgm (/bin/grep /usr/bin/grep)
    if ( -x $prgm ) then
	set GREP = $prgm
	break
    endif
 end
endif


