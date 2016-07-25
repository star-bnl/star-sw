#include "TFile.h"
#include "TNtuple.h"
#include <stdio.h>
#include "Riostream.h"
#include "TSystem.h"
struct BPoint_t {
  Float_t sec, row, pad, ped, tb, rms;
};
BPoint_t BPoint;
void MakePedFromAscii(const Char_t *FileName="./Ped.txt") {
  /*  rts_example -Dtpx st_pedestal_11003025_raw_1080001.daq > Ped.txt */
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".txt",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("Peds","Peds","sec:row:pad:ped:tb:rms");
  char line[121];
  Int_t i = 0;
  Int_t sec, row, pad;
  Int_t tb, ped;
  Float_t rms;
  while (fgets(&line[0],120,fp)) {
    if (line[0] == '*') continue;
    if (line[0] == 'T') {
      sscanf(&line[0],"TPX: sec %i, row %i, pad %i",&sec,&row,&pad);
      BPoint.sec = sec;
      BPoint.row = row;
      BPoint.pad = pad;
      continue;
    }
    Int_t n = sscanf(&line[0],"  tb %i: ped %i, rms %f",&tb,&ped,&rms);
    if (n != 3) continue;
    if (ped > 1000) continue;
    BPoint.tb = tb;
    BPoint.ped = ped;
    BPoint.rms = rms;
    FitP->Fill(&BPoint.sec);
    if (i%10000 == 0) {
      printf("%s",line);
      printf("sec %f row %f ped %f tb %f ped %f rms %f\n",BPoint.sec,BPoint.row,BPoint.pad,BPoint.tb,BPoint.ped,BPoint.rms);
    }
    i++;
  }
  fclose(fp);
  f->Write();
}
