
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem.Load(file)) cout << "Cannot load "<< file << endl;
}
Load(){
  myLoad("/afs/rhic/star/packages/dev/lib/St_base.so");
  myLoad("/afs/rhic/star/packages/dev/lib/St_Tables.so");
  myLoad("/afs/rhic/star/packages/dev/lib/tpc.sl");
  myLoad("/afs/rhic/star/packages/dev/lib/St_tpc.so");
  myLoad("/afs/rhic/star/packages/dev/lib/svt.sl");   
  myLoad("/afs/rhic/star/packages/dev/lib/St_svt.so");
  myLoad("/afs/rhic/star/packages/dev/lib/emc.sl");
  myLoad("/afs/rhic/star/packages/dev/lib/St_emc.so");
  myLoad("/afs/rhic/star/packages/dev/lib/global.sl");
  myLoad("/afs/rhic/star/packages/dev/lib/St_global.so");
}
