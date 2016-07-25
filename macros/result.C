#include "TROOT.h"
#include "TGlobal.h"
#include "Riostream.h"
using namespace std;
void result() {
  gROOT->ProcessLine("float var = 1.2345e-6;");
  gROOT->ProcessLine("float result = 3.0*var;");
  float result = *((float*)((TGlobal*)gROOT->GetListOfGlobals()->FindObject("result"))->GetAddress());
  cout << "result = " << result << endl;
}
