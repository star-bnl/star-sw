# cons EXTRA_CPPFLAGS="-IStRoot/data/base -IStRoot/data/modules -I/opt/star/include/mysql"
cons EXTRA_CPPFLAGS="-D_UCMLOGGER_ -IStRoot/StUCMApi/data/base -IStRoot/StUCMApi -IStRoot/StUCMApi/data -IStRoot/StUCMApi/data/modules -I/opt/star/include/mysql" +StUC +StStarLogger
 setenv UCM_STORE_INFO "mysql:StarLogger(logger)@heston.star.bnl.gov:3306/logger"
 echo To test the build set the env variables:
 echo setenv JOBINDEX 1
 echo setenv REQUESTID 1
