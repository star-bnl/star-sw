
// gROOT->Reset();
#include "iostream.h"
void myLoad(Char_t  *file){
  if (gSystem->Load(file)) cout << "Cannot load "<< file << endl;
}
LoadMev(){
  myLoad("St_base.so");
  myLoad("St_Tables.so");
  myLoad("mev.sl");   
  myLoad("St_mev.so");
  myLoad("StChain.so");
}
