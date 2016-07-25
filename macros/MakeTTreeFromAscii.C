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
#include <stdio.h>
#include "TROOT.h"
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
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#endif
class BPoint_t : public TObject {
public:
  BPoint_t() {}
  virtual ~BPoint_t() {}
  Int_t Run, Event;
  Float_t entriesT;
  Float_t entries[24][45];
  ClassDef(BPoint_t,1)
};
BPoint_t BP;

void MakeTTreeFromAscii(Char_t *FileName="./HitsD.data") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".data",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TTree *tree = new TTree("tree","Hits in sector/row");
  Int_t bufsize = 64000;
  Int_t splitlevel = 99;
  TBranch *branch = tree->Branch("event", &BP, bufsize, splitlevel);
  char line[121];
  memset (&BP, 0, sizeof(BPoint_t));
  while (fgets(&line[0],120,fp)) {
    if (line[0] == 'R') {
      if (BP.Event) {
	tree->Fill();
	memset (&BP, 0, sizeof(BPoint_t));
      }
      Int_t n = sscanf(&line[0],"Run %d  Event %d",&BP.Run,&BP.Event);
    } else {
      Int_t sec, row, entries;
      Int_t n = sscanf(&line[0],"%d %d = %d",&sec,&row,&entries);
      BP.entries[sec-1][row-1] += entries;
      BP.entriesT  += entries;
    }
  }
  fclose(fp);
  if (BP.Event) {
    tree->Fill();
  }
  f->Write();
}
