
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
LoadXDF(){
  myLoad("St_base.so");
  myLoad("libasu.so");
  myLoad("libdsl.so");
  myLoad("xdf2root.so");
  myLoad("St_Tables.so");
  myLoad("StChain.so");
}
