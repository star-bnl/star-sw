#
# This file is used to setup site specific
# environment variables (like printers, 
# default web pages, proxies etc ...)
#
#  J.Lauret Aug 31  2006
#  Revised J.L Sept 9th 2008
#  Revised J.L Feb  9th 2012
#
#

# Define DOMAINNAME if does not exists
if( ! $?DOMAINNAME) then
    if ( -x "/bin/domainname" ) then
       setenv DOMAINNAME `/bin/domainname`
    else
       # Be aware that NIS/YP could be disabled 
       setenv DOMAINNAME "(none)"
    endif
    if ( "$DOMAINNAME" == "(none)") then 
       setenv DOMAINNAME `/bin/hostname | /bin/sed 's/^[^\.]*\.//'`
    endif
endif


switch ($DOMAINNAME)
   # BNL 
   case "rhic.bnl.gov":
   case "rcf.bnl.gov":
   case "usatlas.bnl.gov":
    setenv WWW_HOME     "http://www.rhic.bnl.gov"
    setenv NNTPSERVER   "news.bnl.gov"
    setenv http_proxy   "http://proxy.sec.bnl.local:3128/"
    setenv https_proxy  "http://proxy.sec.bnl.local:3128/"
    setenv ftp_proxy    "http://proxy.sec.bnl.local:3128/"
    breaksw

  default:
    # DO NOTHING
endsw     


# generic environment variables
# Those variables are part of the StDbLib/StDbServiceBroker.cxx
setenv STAR_DEBUG_DB_RETRIES_ADMINS  "jlauret@bnl.gov,arkhipkin@bnl.gov"
setenv STAR_DEBUG_DB_RETRIES_SECONDS 1800                                  # 30 mnts and default
