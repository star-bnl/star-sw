# -Dafs=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=/usr/bin/clang -DCMAKE_CXX_COMPILER=/usr/bin/clang++ -DCMAKE_LINKER=clang++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_LINKER=g++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
setenv GSL_DIR ${XOPTSTAR}
#setenv PYTHIA6_LIBRARY ${STAR_LIB}
setenv PYTHIA6_LIBRARY ${XOPTSTAR}/lib
#setenv PYTHIA6_INCLUDE_DIR ${XOPTSTAR}/include
#setenv PYTHIA6_INCLUDE_DIR ${STAR}/.${STAR_HOST_SYS}/include
#setenv PYTHIA8_LIBRARY ${XOPTSTAR}/lib
#setenv PYTHIA8_INCLUDE_DIR ${XOPTSTAR}}/include
setenv PYTHIA6_DIR ${XOPTSTAR}
setenv PYTHIA8_DIR ${XOPTSTAR}
setenv CC `which gcc`
setenv CXX `which g++`
setenv FC `which gfortran`
setenv QT_LIBRARY_DIR ${QTDIR}/lib
setenv QT_INCLUDE_DIR ${QTDIR}/include
setenv QT_MOC_EXECUTABLE ${QTDIR}/bin/moc
setenv Qt4 ${QTDIR}
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug \
-Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=OFF \
-Dpython=OFF \
-Dpythia6=ON  -Dpythia8=ON \
-DCMAKE_C_COMPILER="${CC}" -DCMAKE_CXX_COMPILER="${CXX}" -DCMAKE_Fortran_COMPILER="${FC}" \
-DXROOTD_ROOT_DIR="${XOPTSTAR}" \
-Dbuiltin_cfitsio=On -DCMAKE_CXX_FLAGS="-fdiagnostics-color=always -msse -msse2 -msse3 -msse4.1 -mssse3" \
-DCMAKE_C_FLAGS="-fdiagnostics-color=always -msse -msse2 -msse3 -msse4.1 -mssse3" 
#  -Dbuiltin_xrootd=ON 
#-DPYTHIA6_DIR=${XOPTSTAR} \
#-DPYTHIA8_DIR=${XOPTSTAR} \
#-DPYTHIA8_INCLUDE_DIR=${XOPTSTAR}}/include \
