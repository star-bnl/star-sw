#include <TROOT.h>
#include <TSystem.h>
#include <TString.h>

//_________________
void RunAnalyzer(const Char_t *inFileName = "../st_physics_12126101_raw_3040006.picoDst.root") {
  gROOT->ProcessLine("#define _VANILLA_ROOT_");
  gSystem->Load("../libStPicoDst.so");
  TString str;
  str = ".x PicoDstAnalyzer.C+(\"";
  str += inFileName;
  str += "\")";
  gROOT->ProcessLine( str.Data() );
  gROOT->ProcessLine(".!rm -f PicoDstAnalyzer_C* ");
}
