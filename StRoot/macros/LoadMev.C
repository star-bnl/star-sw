
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
LoadMev(){
  //  myLoad("/opt/SUNWspro/lib/libF77.so");
  //  myLoad("/opt/SUNWspro/lib/libM77.so");
  myLoad("$STAR_LIB/St_base.so");
  myLoad("$STAR_LIB/St_Tables.so");
  myLoad("$STAR_LIB/mev.sl");   
  myLoad("$STAR_LIB/St_mev.so");
  myLoad("$STAR_LIB/StChain.so");
}
