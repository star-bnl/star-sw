#include <assert.h>
#include "AthenaMagField.h"
#include "TSystem.h"
#include "TFile.h"
#include "TMath.h"
AthenaMagField* AthenaMagField::fgInstance = 0;
//________________________________________________________________________________
AthenaMagField::AthenaMagField() : TVirtualMagField("AthenaMagField"), fBr(0), fBz(0) { 
  const Char_t *path  = ".:./ATHENA/BeastMagneticField/data:$STAR/ATHENA/BeastMagneticField/data";
  const Char_t *rootf = "EIC_v.2.0.3_Magnetic_Field_Map_2021_09_28_radial_coords_cm_T.Bmap.root";
  Char_t *file = gSystem->Which(path,rootf,kReadPermission);
  if (! file) Fatal("AthenaMagField::AthenaMagField","File %s has not been found in path %s",rootf,path);
  TFile *f = new TFile(file);
  delete file;
  fBr = (TH2F *) f->Get("br"); assert(fBr); fBr->SetDirectory(0);
  fBz = (TH2F *) f->Get("bz"); assert(fBz); fBz->SetDirectory(0);
}
//________________________________________________________________________________
void AthenaMagField::Field    ( const Double_t *x, Double_t *B ) {
  Double_t z = x[2];
  Double_t r = TMath::Sqrt(x[0]*x[0] + x[1]*x[1]);
  Double_t Br = fBr->Interpolate(r, z);
  Double_t Bz = fBz->Interpolate(r, z);
  B[0] = B[1] = 0;
  B[2] = Bz;
  if (r > 0) {
    B[0] = x[0]/r * Br;
    B[1] = x[1]/r * Br;
  }
}
