// @(#)root/main:$Name:  $:$Id: h1.C,v 1.1 2004/03/30 23:26:58 fisyak Exp $
// Author: Rene Brun   20/09/96
/////////////////////////////////////////////////////////////////////////
//      Program to convert an HBOOK file into a ROOT file
//                      Author: Rene Brun
//
//  This program is invoked via:
//     h2root hbook_file_name root_file_name  compress tolower
//  if the second parameter root_file_name is missing the name will be
//  automatically generated from the hbook file name. Example:
//       h2root test.hbook
//     is identical to
//       h2root test.hbook test.root
//  if compress is missing (or = 1)the ROOT file will be compressed
//  if compress = 0 the ROOT file will not be compressed.
//  if tolower is missing (or = 1) ntuple column names are converted to lower case
/////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iostream.h>
#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#endif
int Error;   //to be removed soon
const Int_t PAWC_SIZE = 2000000;

//  Define the names of the Fortran common blocks for the different OSs
int pawc_[PAWC_SIZE];
int quest_[100];
int hcbits_[37];
int hcbook_[51];
int rzcl_[11];

int *iq, *lq;
float *q;
char idname[128];
int nentries;
char chtitl[128];
int ncx,ncy,nwt,idb;
int lcont, lcid, lcdir;
float xmin,xmax,ymin,ymax;
const Int_t kMIN1 = 7;
const Int_t kMAX1 = 8;

//  Define the names of the Fortran subroutine and functions for the different OSs
extern "C" {
  void     hlimit_(const int*);
  void     hparmn_(Float_t *xy, Float_t *vex, Float_t *ex, 
		   Int_t *np, Int_t *ndim, Int_t *ic, Float_t *r2mini,
		   Int_t *mx, Double_t *Coef, Int_t *Iterm, Int_t *nCo);
  Double_t hrval_ (Float_t *x);
  void     hsetpr_ (Char_t *, Float_t *, Int_t);
}
Int_t golower  = 1;
Int_t bufsize  = 8000;

//____________________________________________________________________________
Double_t hr(Double_t *x, Double_t *par) {
  Double_t COEFF[7] = {-0.29414629E+00,-0.65181924E-15,-0.19702754E-09,
		       0.20519117E-01,-0.28383705E-03, 0.59486243E-12,
		       0.25575770E-07};
  Int_t IBASFT[7]   = {0, 7, 5, 1, 2, 6, 4};
  Double_t X = x[0];
  Double_t xx[8];
  Double_t FPARAM=0.;
  xx[0] = 1.; 
  xx[1] = X;
  for (int J=2; J<8;J++) {
    double j = J;
    xx[J] = (2*j-1)/j*xx[J-1]*X-(j-1)/j*xx[J-2];
  }
  for (int i=0; i<7; i++) {
    int k = IBASFT[i];
    FPARAM += COEFF[i]*xx[k];
  }
  return FPARAM;  
}
//____________________________________________________________________________
Double_t hrd(Double_t *x, Double_t *par) {
  Float_t xx = x[0];
  return hrval_(&xx);  
}
  //________________________________________________________________________________
void h1(TH1 *binY=0, Int_t max=5){
  if (!binY) return;
  lq = &pawc_[9];
  iq = &pawc_[17];
  void *qq = iq;
  q = (float*)qq;
  int pawc_size = PAWC_SIZE;
  printf ("call hlimit\n");
  hlimit_(&pawc_size);
  float plun = 45.;
  hsetpr_("PLUN",&plun,4);
  Int_t mx[4], iterm[50], ndd[10];
  Double_t coef[50];
  Float_t xy[12000],  vex[5000], ex[5000], xp[6000][4], vex2[5000];
  Float_t xminn[10], xmaxx[10], ex2[5000], xx[4];
  Int_t np = 0;
  Int_t nbins = binY->GetNbinsX();
  int i;
  for (i = 1; i<= nbins; i++) {
    xy[np] = binY->GetBinCenter(i);
    vex[np] = binY->GetBinContent(i);
    ex[np] =  binY->GetBinError(i);
    if (ex[np] <=0) continue;
    //   ex[np] /= vex[np];
    //    vex[np] = TMath::Log(vex[np]);
    vex2[np] = vex[np];
    //    printf("%i x =%f y=%f dy=%f\n",np,xy[np],vex2[np],ex[np]);
    np++;
  }
  Int_t ndim  = 1;
  mx[0] = max;
  Int_t nco;
  //         NRCBTWPS
  Int_t ic = 00004021;
  Float_t r2min = 1.;
  hparmn_(xy, vex, ex, &np, &ndim, &ic, &r2min, mx, coef, iterm, &nco);
  for (i = 0; i< np; i++) {
    Double_t val = hrval_(&xy[i]);
    Double_t res = (val - vex2[i])/ex[i];
    if (abs(res)> 3.) printf("%i x =%f y=%f dy=%f res=%f dev=%f\n",i,xy[i],vex2[i],ex[i],val,res);
  }
  
}
//____________________________________________________________________________
