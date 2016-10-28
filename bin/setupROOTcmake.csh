# -Dafs=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=/usr/bin/clang -DCMAKE_CXX_COMPILER=/usr/bin/clang++ -DCMAKE_LINKER=clang++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_LINKER=g++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
setenv GSL_DIR ${XOPTSTAR}
setenv PYTHIA6_LIBRARY ${STAR_LIB}
setenv PYTHIA6_INCLUDE_DIR ${STAR}/.${STAR_HOST_SYS}/include
setenv PYTHIA8_LIBRARY ${STAR_LIB}
setenv PYTHIA8_INCLUDE_DIR ${STAR}/.${STAR_HOST_SYS}/include
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
cmake $ROOT/5.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=OFF \
-Dbuiltin_cfitsio=On -Dbuiltin_xrootd=ON  -DCMAKE_CXX_FLAGS="-fdiagnostics-color=always -msse -msse2 -msse3 -msse4.1 -mssse3" -DCMAKE_C_FLAGS="-fdiagnostics-color=always -msse -msse2 -msse3 -msse4.1 -mssse3" 

