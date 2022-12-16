#include <assert.h>
#include "MARCOMagField.h"
#include "TSystem.h"
#include "TFile.h"
#include "TMath.h"
MARCOMagField* MARCOMagField::fgInstance = 0;
//________________________________________________________________________________
MARCOMagField::MARCOMagField() : TVirtualMagField("MARCOMagField"), fBr(0), fBz(0) { 
  const Char_t *path  = ".:./ePIC:$STAR/StarDb/ePIC";
  const Char_t *rootf = "ARCO_v.6.4.1.1.3_1.7T_Magnetic_Field_Map_2022_11_14_rad_coords_cm_T.root";
  Char_t *file = gSystem->Which(path,rootf,kReadPermission);
  if (! file) Fatal("MARCOMagField::MARCOMagField","File %s has not been found in path %s",rootf,path);
  TFile *f = new TFile(file);
  delete file;
  fBr = (TH2F *) f->Get("br"); assert(fBr); fBr->SetDirectory(0);
  fBz = (TH2F *) f->Get("bz"); assert(fBz); fBz->SetDirectory(0);
}
//________________________________________________________________________________
void MARCOMagField::Field    ( const Double_t *x, Double_t *B ) {
  Double_t z = x[2];
  Double_t r = TMath::Sqrt(x[0]*x[0] + x[1]*x[1]);
  Double_t Br = 10*fBr->Interpolate(r, z);
  Double_t Bz = 10*fBz->Interpolate(r, z);
  B[0] = B[1] = 0;
  B[2] = Bz;
  if (r > 0) {
    B[0] = x[0]/r * Br;
    B[1] = x[1]/r * Br;
  }
}
