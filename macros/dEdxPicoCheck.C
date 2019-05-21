#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TCanvas.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TLegend.h"
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
#endif
using namespace std;
void dEdxPicoCheck(const Char_t *topDir = "./")
{
  //open all input files and insert them in the list of files
  TFileSet dirs(topDir);
  TDataSetIter next(&dirs,0);
  TDataSet *set = 0; 
  Int_t nfiles = 0;
  while ( (set = next()) ) {           
    if (strcmp(set->GetTitle(),"file") || 
        !strstr(set->GetName(),".root")) continue;
    TString Fname(gSystem->BaseName(set->Path()));
    cout << "Open " <<   Fname.Data();
    TFile *afile = new TFile(Fname.Data());
    if (!afile) continue;
    TH2 *dEdxP = (TH2 *) afile->Get("dEdxP");
    if (! dEdxP) {
      cout << " missing dEdxP" << endl;
    } else {
      TH1 *dEdx = dEdxP->ProjectionY();
      cout << gSystem->BaseName(gSystem->DirName(gDirectory->GetName())) << "\t" << dEdx->GetEntries() << "\t" << dEdx->GetMean() << " +/- " << dEdx->GetRMS() << endl;
      delete dEdx;
    }
    delete afile;
  }
}
