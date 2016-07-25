#if !defined(__CINT__)
#include "TString.h"
#include <stdio.h>
#include "Riostream.h"
#include "TClassTable.h"
#include "TSystem.h"
#include "TFile.h"
#include "tables/St_tpcPadGainT0_Table.h"
#endif
void MaketpcPadGainT0(const Char_t *FileName, Int_t d = 20110203, Int_t t = 112539) {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("St_tpcPadGainT0") < 0) gSystem->Load("libStDb_Tables.so");
  St_tpcPadGainT0 *padGainT0 = new St_tpcPadGainT0("tpcPadGainT0",1);
  Char_t line[121];
  tpcPadGainT0_st GainT0; 
  memset (&GainT0, 0, sizeof(GainT0));
  Int_t run = 0, sec, row, pad;
  Float_t gain, t0;
  Int_t n = 0;
  while (fgets(&line[0],120,fp)) {
    if (line[0] == '#') {
      TString Line(line);
      Int_t index = Line.Index("Run ");
      if (index < 0) continue;
      n = sscanf(&line[index+4],"%d",&run);
      GainT0.run = run;
      continue;
    }
    n = sscanf(&line[0],"%d%d%d%f%f",&sec,&row,&pad,&gain,&t0);
    if (sec < 1 || sec > 24) continue;
    if (row < 1 || row > 45) continue;
    if (pad < 1 || pad > 182) continue;
    GainT0.Gain[sec-1][row-1][pad-1] = gain;
    GainT0.T0[sec-1][row-1][pad-1] = t0;
  }
  padGainT0->AddAt(&GainT0);
  //  padGainT0->Print(0,1);
  TDatime  time(d,t);
  TString filename(Form("tpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
  printf("Create %s\n",filename.Data());
  filename += ".root";
  TFile *f = new TFile(filename.Data(),"recreate");
  padGainT0->Write();
  delete f;
}
