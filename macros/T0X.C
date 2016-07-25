#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TProfile2D.h"
#include "TFile.h"
#endif
using namespace std;
//________________________________________________________________________________
Double_t T0X(Int_t sector, Int_t row) {
  static TProfile2D* T0SR = 0;
  if (! T0SR) {
    static TFile *fin = 0;
    if (! fin ) {
      fin = new TFile("T0.20130107.133034.root");
      if (!fin ) {
	cout << "Can't open file with T0" << endl;
	return 0;
      }
    }
    T0SR = (TProfile2D*) fin->Get("T0SR");
    if (! T0SR) {
      cout << "Can't find T0SR histogram" << endl;
      return 0;
    }
  }
  return T0SR->GetBinContent(row,sector%100);
}
