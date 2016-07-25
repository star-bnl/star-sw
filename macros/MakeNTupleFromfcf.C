#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TFile.h"
#include "TNtuple.h"
#endif
struct BPoint_t {
  Float_t Event,sector,row,rcid_simtrk,rcid_quality;
  Float_t rcx,rcy,rcz,rcq,rcl_f,rcl_c,rcm,rcn,mcx,mcy,mcz,mcq,mcdED;
};
BPoint_t BPoint;
void MakeNTupleFromfcf(Char_t *FileName="fcf.dta") {
  TString fName(FileName);
  fName.ReplaceAll(".dta",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("FitP","fcf","Event:sector:row:rcid_simtrk:rcid_quality:rcx:rcy:rcz:rcq:rcl_f:rcl_c:rcm:rcn:mcx:mcy:mcz:mcq:mcdED");
  FILE *fp = fopen(FileName,"r");
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
	   &BPoint.Event,&BPoint.sector,&BPoint.row,&BPoint.rcid_simtrk,&BPoint.rcid_quality,
	   &BPoint.rcx,&BPoint.rcy,&BPoint.rcz,&BPoint.rcq,&BPoint.rcl_f,
	   &BPoint.rcl_c,&BPoint.rcm,&BPoint.rcn,&BPoint.mcx,&BPoint.mcy,
	   &BPoint.mcz,&BPoint.mcq,&BPoint.mcdED);
    FitP->Fill(&BPoint.Event);
    i++;
    if (i%1000 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
