
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
Load(){
  //  myLoad("/opt/SUNWspro/lib/libF77.so");
  //  myLoad("/opt/SUNWspro/lib/libM77.so");
  myLoad("$STAR_LIB/St_base.so");
  myLoad("$STAR_LIB/St_Tables.so");
  myLoad("$STAF_LIB/libmsg.so");
  myLoad("$STAF_LIB/libtls.so");
  myLoad("$STAR_LIB/tpc.sl");
  myLoad("$STAR_LIB/St_tpc.so");
  myLoad("$STAR_LIB/svt.sl");   
  myLoad("$STAR_LIB/St_svt.so");
  //  myLoad("$STAR_LIB/emc.sl");
  //  myLoad("$STAR_LIB/St_emc.so");
  myLoad("$STAR_LIB/global.sl");
  myLoad("$STAR_LIB/St_global.so");
}
