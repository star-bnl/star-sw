
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
LoadXDF(){
  myLoad("$STAR_LIB/St_base.so");
  myLoad("$STAR_LIB/St_Tables.so");
  myLoad("$STAR_LIB/StChain.so");
}
