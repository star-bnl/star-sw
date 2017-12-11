setup gcc
setenv MANPATH `dropit subsys -p $MANPATH`
setenv LD_LIBRARY_PATH `dropit subsys -p $LD_LIBRARY_PATH`
setenv GROUPPATH `dropit subsys -p $GROUPPATH`
setenv PYTHONPATH `dropit subsys -p $PYTHONPATH`
setenv PATH `dropit subsys`
unsetenv AFS_RHIC
unsetenv STAR_ROOT
unsetenv ROOT
unsetenv PROD_LOG
unsetenv GROUP_DIR
unsetenv optstar
unsetenv xoptstar
unsetenv STAF
unsetenv STAF_LIB
unsetenv STAF_BIN
unsetenv STAR
unsetenv STAR_LIB
unsetenv STAR_OBJ
unsetenv STAR_BIN
unsetenv STARL
unsetenv GARFIELD_HOME
unsetenv HEED_DATABASE
unsetenv DB_SERVER_LOCAL_CONFIG
unsetenv STAR_SCRIPTS
unsetenv STAR_CGI
unsetenv STAR_MGR
unsetenv STAR_PAMS
unsetenv TMVASYS
unsetenv CINTSYSDIR
unsetenv IVROOT
unsetenv CLHEP_BASE_DIR
unsetenv XOPTSTAR
unsetenv OPTSTAR
unsetenv QTDIR
unsetenv STAR_PATH
unsetenv GROUP_PATH
unsetenv ROOTROOT
unsetenv ROOTSYS
source /afs/rhic.bnl.gov/star/group/templates/cshrc
source /afs/rhic.bnl.gov/star/group/templates/login
starver dev
