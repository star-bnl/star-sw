if (-x $GROUP_DIR/dropit && ! $?QTDIR) then
   setenv PATH `dropit qt Qt`
   setenv LD_LIBRARY_PATH `dropit -p $LD_LIBRARY_PATH qt Qt`
#if ( $STAR_HOST_SYS == 'sl44_icc101') then
#setenv QTDIR $ROOTROOT/Qt4/sl44_icc101/Qt-4.4.3
#else
#setenv QTDIR /opt/star/sl44_gcc346/qt4
#setenv QTDIR $ROOTROOT/Qt4/.sl44_gcc346/4.4.0
endif
setenv PATH ${PATH}:${QTDIR}/bin
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${QTDIR}/lib
#setenv IVROOT  $ROOT/5.99.99/Coin2/.$STAR_HOST_SYS/Coin2Qt4
setenv IVROOT  $ROOT/5.99.99/Coin2Qt4/$STAR_HOST_SYS/coin3d
if (-x $GROUP_DIR/dropit) setenv PATH `$GROUP_DIR/dropit Coin`
setenv LD_LIBRARY_PATH `dropit Coin -p $LD_LIBRARY_PATH`
setenv PATH $IVROOT/bin:$PATH
setenv LD_LIBRARY_PATH $IVROOT/lib:$LD_LIBRARY_PATH
setenv QTROOTSYSDIR $STAR/.$STAR_HOST_SYS
setenv COIN_NO_CHECK_GL_ERROR 1 
setenv COIN_FULL_INDIRECT_RENDERING 1 
