#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TFile.h"
#include "TNtuple.h"
#endif
struct BPoint_t {Float_t R, Z, Phi, Br, Bz, Bphi;};
const Char_t *v = "R:Z:Phi:Br:Bz:Bphi";
TFile *f = 0;
TNtuple *FitP = 0;
//________________________________________________________________________________
void readFieldMap(const Char_t *FileName = "/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/StMagF/bfield_full_positive_3D.dat") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  fName.ReplaceAll(".dat",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP",FileName,v);
  char line[121];
  BPoint_t B;
  Int_t i = 0;
  while (fgets(&line[0],120,fp)) {
    Int_t n = sscanf(&line[0]," %f %f %f %f %f %f",&B.R,&B.Z,&B.Phi,&B.Br,&B.Bz,&B.Bphi);
    if (n != 6) {printf("skip %s",line); continue;}
    if (i%(37*28) == 0) {printf("get %s",line);}
    FitP->Fill(&B.R);
    i++;
  }
  fclose(fp);
  f->Write();
}
