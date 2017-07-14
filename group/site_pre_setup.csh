#
# This file is used for local setups / env defines
# which needs to be done prior to executing the
# group_env script
#
# Developpers can still re-define XROOTDSYS in their cshrc
# to switch to a beta version for testing purposes.
#

# This should not happen
if( ! $?AFS_RHIC)   setenv AFS_RHIC  /afs/rhic.bnl.gov

# Define DOMAINNAME if does not exists
if( ! $?DOMAINNAME) then
    if ( -x "domainname" ) then
       setenv DOMAINNAME `domainname`
    else
       # Be aware that NIS/YP could be disabled 
       setenv DOMAINNAME "(none)"
    endif
    if ( "$DOMAINNAME" == "(none)") then 
       setenv DOMAINNAME `hostname | sed 's/^[^\.]*\.//'`
    endif
endif



switch ($DOMAINNAME)
   # BNL
   case "rhic.bnl.gov":
   case "rcf.bnl.gov":
   case "usatlas.bnl.gov":
    # This detects everything
    if ( ! $?XROOTDSYS ) then
	# in AFS land 
	set xrootd=${AFS_RHIC}/star/ROOT/Xrootd/prod

	if ( -d $xrootd ) then
	    setenv XROOTDSYS $xrootd
	endif
    endif

    # We have it valid for Linux only
    set PP=${AFS_RHIC}/star/Grid/OSG/WNC
    if ( -d $PP ) then
	if ( `uname` == "Linux") then
	    setenv WNOSG $PP
	endif
    endif

    # users coming with this defined would mess perl up (RT  #2307)
    if ( $?LC_CTYPE ) then
    	unsetenv LC_CTYPE
    endif
    breaksw



  default:
    # DO NOTHING
endsw     
