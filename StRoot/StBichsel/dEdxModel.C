/* 
   root.exe SL15AuAu200Z6cmBLStiCAKFV.root lBichsel.C  dEdxModel.C+
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "StBichsel/StdEdxModel.h"
#endif
//________________________________________________________________________________
void dEdxModel() {
  StdEdxModel::_debug = 1;
  StdEdxModel::MakedEdxModel();
}
