
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
Load(){
  //  myLoad("/opt/SUNWspro/lib/libF77.so");
  //  myLoad("/opt/SUNWspro/lib/libM77.so");
  myLoad("/afs/rhic/star/packages/dev/lib/St_base.so");
  myLoad("/afs/rhic/star/packages/dev/lib/St_Tables.so");
  myLoad("$STAF_LIB/libmsg.so");
  myLoad("$STAF_LIB/libtls.so");
  myLoad("/afs/rhic/star/packages/dev/lib/tpc.sl");
  myLoad("/afs/rhic/star/packages/dev/lib/St_tpc.so");
  //  myLoad("/afs/rhic/star/packages/dev/lib/svt.sl");   
  //  myLoad("/afs/rhic/star/packages/dev/lib/St_svt.so");
  //  myLoad("/afs/rhic/star/packages/dev/lib/emc.sl");
  //  myLoad("/afs/rhic/star/packages/dev/lib/St_emc.so");
  //  myLoad("/afs/rhic/star/packages/dev/lib/global.sl");
  //  myLoad("/afs/rhic/star/packages/dev/lib/St_global.so");
  myLoad("/afs/rhic/star/packages/dev/lib/StChain.so");
}
