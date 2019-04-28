/*
  root.exe lDb.C $STAR/StarDb/Calibrations/tpc/TpcSecRowB.20190201.000601.root MakeNTuleFromTpcSecRowB.C+
 */

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include "TH2.h"
#include "TStyle.h"
#include "TNtuple.h"
#include "TH2.h"
#include "tables/St_TpcSecRowCor_Table.h"
class FitP {
   public :
   Float_t         sector;
   Float_t         row;
   Float_t         mu;
   Float_t         sigma;
};
const Char_t *FitPVar = "sector:row:mu:sigma";
//________________________________________________________________________________
void MakeNTupleFromTpcSecRowB() {
  St_TpcSecRowCor *TpcSecRowB = (St_TpcSecRowCor *) gDirectory->Get("TpcSecRowB");
  if (! TpcSecRowB) return;
  TFile *fout = new TFile("TpcSecRowB.root","recreate");
  TNtuple *P = new TNtuple("FitP","Tpc Sec RowB Correction",FitPVar);
  FitP T;
  TH2F *mu = new TH2F("mu","mu",24,0.5,24.5,72,0.5,72.5);
  for (Int_t sector = 1; sector <= 24; sector++) {
    TpcSecRowCor_st *r = TpcSecRowB->GetTable() + sector -1;
    for (Int_t row = 1; row <= 72; row++) {
      T.sector = sector;
      T.row    = row;
      T.mu     = r->GainScale[row-1];
      T.sigma  = r->GainRms[row-1];
      P->Fill(&T.sector);
      mu->Fill(sector,row,T.mu);
    }
  }
}
