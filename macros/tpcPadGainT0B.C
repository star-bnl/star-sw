/* 
   ln -s $STAR/online/RTS/src/TPX_SUPPORT/tpx_gains.txt .
   root.exe lDb.C 'tpcPadGainT0B.C(20000615,0)'
*/
#ifndef __CINT__
#include "TFile.h"
#include <stdio.h>
#include "Riostream.h"
#include "TSystem.h"
#include "TString.h"
#include "tables/St_tpcPadGainT0B_Table.h"
#endif /* __CINT__ */
//________________________________________________________________________________
void tpcPadGainT0B(Int_t date = 20140131, Int_t time = 0, 
		   const Char_t *FileName = "tpx_gains.txt") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString Name(Form("tpcPadGainT0B.%06i.%06i.root",date,time));
  TFile *fOut = new TFile(Name,"recreate");
  cout << "Open file " << Name.Data() << endl;
  St_tpcPadGainT0B *tableSet = new St_tpcPadGainT0B("tpcPadGainT0B",24);
  tpcPadGainT0B_st rowB;
  memset(&rowB,0,tableSet->GetRowSize());
  for (Int_t s = 1; s <= 24; s++) {
    tableSet->AddAt(&rowB);
  }
  char line[121];
  Int_t sOld = 0;
  Int_t s, r, p;
  Float_t gain, t0;
  tpcPadGainT0B_st *row = tableSet->GetTable();
  tpcPadGainT0B_st *rowc;
  while (fgets(&line[0],120,fp)) {
    if (line[0] == '#') continue;
    Int_t n = sscanf(&line[0],"%i %i %i %f %f",&s,&r,&p,&gain,&t0);
    if (n != 5) continue;
    if (s < 1 || s >  24) continue;
    if (r < 1 || r >  45) continue;
    if (p < 1 || p > 182) continue;
    if (s != sOld) {
      sOld = s; 
      cout << Form("%2i %2i %3i %10.3f %10.3f",s,r,p,gain,t0) << endl;
    }
    rowc = (row+s-1);
    rowc->Gain[r-1][p-1] = gain;
    rowc->T0[r-1][p-1]   = t0;
  }
  tableSet->Write();
  delete fOut;
}
