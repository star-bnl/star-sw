# -Dafs=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=/usr/bin/clang -DCMAKE_CXX_COMPILER=/usr/bin/clang++ -DCMAKE_LINKER=clang++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
#cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_LINKER=g++ -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
setenv GSL_DIR ${XOPTSTAR}
cmake cmake $ROOT/6.99.99/root -DCMAKE_INSTALL_PREFIX=$ROOTSYS -DCMAKE_BUILD_TYPE=Debug -Dall=ON -Dcxx11=ON -Dlibcxx=ON -Dcacoa=ON -Dgdml=ON -Dgsl_shared=ON -Dminuit2=ON -Dqt=ON -Drootfit=ON -Dtable=ON -Dvc=ON 
