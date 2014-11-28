//#define DEBUG
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
#include "StLaserAnalysisMaker/laserino.h"
void CheckMirrors(Int_t sector, Int_t var = 1) {
  static const Char_t *Names[3] = {"X","Y","Z"};
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser ) return;
  TCanvas *c = new TCanvas(Form("Sector%02i%s",sector,Names[var-1]),Form("Sector%02i%s",sector,Names[var-1]),600,800);
  c->Divide(2,3);
  for (Int_t Bundle = 1; Bundle <= 6; Bundle++) {
    c->cd(Bundle);
    laser->Draw(Form("fTracks.Laser.XyzB.mX%i:fTracks.Laser.Mirror",var),
		Form("fTracks.Laser.Sector==%i&&fTracks.Laser.Bundle==%i",sector,Bundle),"colz",10000);
  }
}
//________________________________________________________________________________
void CheckMirrors(const Char_t *files = "./laser_8102110.root") {
  TDirIter Dir(files);
  TTreeIter iter("laser");
  iter.AddFile(files);
  const Int_t&       fNtrack                                  = iter("fNtrack");
  const UShort_t*&   fTracks_mNumberOfFitPointsTpc            = iter("fTracks.mNumberOfFitPointsTpc");
  const Double32_t*& fTracks_fpTInv                           = iter("fTracks.fpTInv");
  const Int_t*&      fTracks_Flag                             = iter("fTracks.Flag");
  const Double_t       *&fTracks_dirPU_mX1			   = iter("fTracks.dirPU.mX1");					      
  const Double_t*&   fTracks_dirPM_mX1                        = iter("fTracks.dirPM.mX1");
  const Double_t*&   fTracks_dirPM_mX2                        = iter("fTracks.dirPM.mX2");
  const Int_t          *&fTracks_Laser_Sector			   = iter("fTracks.Laser.Sector");				      
  const Int_t          *&fTracks_Laser_Bundle			   = iter("fTracks.Laser.Bundle");				      
  const Int_t          *&fTracks_Laser_Mirror			   = iter("fTracks.Laser.Mirror");				      
  const Double_t*&   fTracks_XyzPB_mX1                        = iter("fTracks.XyzPB.mX1");
  const Double_t*&   fTracks_XyzPB_mX2                        = iter("fTracks.XyzPB.mX2");
  const Double_t*&   fTracks_XyzPB_mX3                        = iter("fTracks.XyzPB.mX3");
  const Double_t*&   fTracks_XyzPL_mX3                        = iter("fTracks.XyzPL.mX3");
  const Double_t*&   fTracks_Laser_XyzB_mX1                   = iter("fTracks.Laser.XyzB.mX1");
  const Double_t*&   fTracks_Laser_XyzB_mX2                   = iter("fTracks.Laser.XyzB.mX2");
  const Double_t*&   fTracks_Laser_XyzB_mX3                   = iter("fTracks.Laser.XyzB.mX3");
  const Int_t NS = 12;
  const Int_t NB =  6;
  const Int_t NM =  7;
  TH2D *X = new TH2D("X","dX versus mirror, bundle, sector",NS*NB*NM,0,NS*NB*NM,500,-.5,0.5); 
  TH2D *Y = new TH2D("Y","dY versus mirror, bundle, sector",NS*NB*NM,0,NS*NB*NM,500,-.5,0.5); 
  TH2D *Z = new TH2D("Z","dZ versus mirror, bundle, sector",NS*NB*NM,0,NS*NB*NM,500,-.5,0.5); 
  while (iter.Next()) {
    for (Int_t k = 0; k < fNtrack; k++) {
      if (fTracks_Flag[k] < 2) continue;
      if (fTracks_dirPU_mX1[k] > -0.5) continue;
      if (fTracks_mNumberOfFitPointsTpc[k] < 35) continue;
      static const Double_t pTInv0 = 4.78815e-03;
      static const Double_t DpTInv0 = 9.75313e-03;
      if (TMath::Abs(fTracks_fpTInv[k] - pTInv0) > 3*DpTInv0) continue;
      Int_t s = fTracks_Laser_Sector[k]/2 - 1;
      if (s < 0 || s >= NS) continue;
      Int_t b = fTracks_Laser_Bundle[k] - 1;
      if (b < 0 || b >= NB) continue;
      Int_t m = fTracks_Laser_Mirror[k] - 1;
      if (m < 0 || m >= NM) continue;
      Double_t x = fTracks_XyzPB_mX1[k] -  fTracks_Laser_XyzB_mX1[k];
      Double_t y = fTracks_XyzPB_mX2[k] -  fTracks_Laser_XyzB_mX2[k];
      Double_t z = fTracks_XyzPB_mX3[k] -  fTracks_Laser_XyzB_mX3[k];
      //laser->Draw("fTracks.XyzPB.mX3-fTracks.Laser.XyzB.mX3:fTracks.XyzPL.mX3>>WE6","fTracks.Flag>1&&abs(fTracks.XyzPM.mX3-6.5)<1&&fTracks.Laser.Sector!=6","colz")
#if 0
      if (s < 6) z -= 6.39057;
      else       z -= 6.59919;
      z -= 3.29319000000000009e-04*fTracks_XyzPL_mX3[k];
#else
      if (s < 6) z -= 6.45043e+00+1.26835e-04*fTracks_XyzPL_mX3[k];
      else       z -= 6.60302e+00+3.93078e-04*fTracks_XyzPL_mX3[k];
#endif
      Double_t indx = m + NM*(b + NB*s) + 0.5; 
      X->Fill(indx,x);
      Y->Fill(indx,y);
      Z->Fill(indx,z);
    }
  } 
  X->FitSlicesY();
  Y->FitSlicesY();
  Z->FitSlicesY();
  TH1D *Fit[3][3];
  const Char_t *xyz[3] = {"X","Y","Z"};
  const Char_t *res[3] = {"1","2","chi2"};
  for (Int_t i = 0; i < 3; i++) 
    for (Int_t j = 0; j < 3; j++) Fit[i][j] = (TH1D*) gDirectory->Get(Form("%s_%s",xyz[i],res[j]));
  ofstream out;
  TString fOut(files);
  fOut,ReplaceAll("*","");
  fOut.ReplaceAll(".root","");
  fOut,ReplaceAll(".","");
  fOut.ReplaceAll("/","");
  fOut += ".data";
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  for (Int_t s = 0; s < 12; s++) {
    for (Int_t b = 0; b < 6; b++) {
      for (Int_t m = 0; m < 7; m++) {
	Int_t bin = m + NM*(b + NB*s) + 1;
	Double_t dxyz[3] = {0,0,0};
	Double_t ddxyz[3] = {0,0,0};
	Int_t    iflag[3] = {0,0,0};
	for (Int_t i = 0; i < 3; i++) {
	  if (Fit[i][0]->GetBinError(bin) > 0 &&  Fit[i][0]->GetBinError(bin) < 0.1) {
	    if (TMath::Abs(Fit[i][0]->GetBinContent(bin)) >  Fit[i][0]->GetBinError(bin)) iflag[i] = 1;
	    dxyz[i] = Fit[i][0]->GetBinContent(bin);
	    ddxyz[i] = Fit[i][0]->GetBinError(bin);
	  }
	}
	if (iflag[0] + iflag[1] + iflag[2] || ddxyz[0] > 0 || dxyz[1] > 0 || dxyz[2] > 0)
	  cout << Form("Sector: %2i",2*s + 2) << " Bundle: " << b+1 << " Mirror: " << m+1 
	       << " <x> = " << Form("%7.4f",dxyz[0]) << " +/- " << Form("%7.4f",ddxyz[0])
	       << " <y> = " << Form("%7.4f",dxyz[1]) << " +/- " << Form("%7.4f",ddxyz[1])
	       << " <z> = " << Form("%7.4f",dxyz[2]) << " +/- " << Form("%7.4f",ddxyz[2]) 
	       << endl;
	out << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dX+dxyz[0],ddxyz[0])
	    << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dY+dxyz[1],ddxyz[1])
	    << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dZ+dxyz[2],ddxyz[2])
	    << "}";
	if (m == 6) out << "}";
	if (b == 5 && m == 6) out << "}";
	out << ",";
	out << "// S/B/M " << 2*(s+1) << "/" << b+1 << "/" << m+1 << endl;
      }
    }
  }
  out.close();   
}
#if 0
//________________________________________________________________________________
void CheckMirrors(const Char_t *files = "./laser_8102110.root") {
  TDirIter Dir(files);
  TTreeIter iter("laser");
  iter.AddFile(files);
  const Int_t&       fNtrack                                  = iter("fNtrack");
  const UShort_t*&   fTracks_mNumberOfFitPointsTpc            = iter("fTracks.mNumberOfFitPointsTpc");
  const Double32_t*& fTracks_fpTInv                           = iter("fTracks.fpTInv");
  const Int_t*&      fTracks_Flag                             = iter("fTracks.Flag");
  const Double_t       *&fTracks_dirPU_mX1			   = iter("fTracks.dirPU.mX1");					      
  const Double_t*&   fTracks_dirPM_mX1                        = iter("fTracks.dirPM.mX1");
  const Double_t*&   fTracks_dirPM_mX2                        = iter("fTracks.dirPM.mX2");
  const Int_t          *&fTracks_Laser_Sector			   = iter("fTracks.Laser.Sector");				      
  const Int_t          *&fTracks_Laser_Bundle			   = iter("fTracks.Laser.Bundle");				      
  const Int_t          *&fTracks_Laser_Mirror			   = iter("fTracks.Laser.Mirror");				      
  const Double_t*&   fTracks_XyzPB_mX1                        = iter("fTracks.XyzPB.mX1");
  const Double_t*&   fTracks_XyzPB_mX2                        = iter("fTracks.XyzPB.mX2");
  const Double_t*&   fTracks_XyzPB_mX3                        = iter("fTracks.XyzPB.mX3");
  const Double_t*&   fTracks_Laser_XyzB_mX1                   = iter("fTracks.Laser.XyzB.mX1");
  const Double_t*&   fTracks_Laser_XyzB_mX2                   = iter("fTracks.Laser.XyzB.mX2");
  const Double_t*&   fTracks_Laser_XyzB_mX3                   = iter("fTracks.Laser.XyzB.mX3");
  Int_t N = 0;
  struct Kxy_t {
    Int_t N;
    Double_t K, Kx, K2, K2x, Ky, x, y, x2, xy, y2, del, Kdel, z, z2;
  };
  Kxy_t Offset[12][6][8];
  const Int_t NW = (sizeof(Kxy_t) - sizeof(Int_t))/sizeof(Double_t);
  memset(Offset, 0, 12*6*8*sizeof(Kxy_t));
  while (iter.Next()) {
    //    cout << " fEvtHdr_fRun " <<  fEvtHdr_fRun << " fEvtHdr_fEvtNum " << fEvtHdr_fEvtNum << " fNtrack " << fNtrack << endl;
    N++;
    //    if (N > 10000) break;
    for (Int_t k = 0; k < fNtrack; k++) {
      if (fTracks_Flag[k] < 2) continue;
      if (fTracks_dirPU_mX1[k] > -0.5) continue;
      if (fTracks_mNumberOfFitPointsTpc[k] < 35) continue;
      static const Double_t pTInv0 = 4.78815e-03;
      static const Double_t DpTInv0 = 9.75313e-03;
      if (TMath::Abs(fTracks_fpTInv[k] - pTInv0) > 3*DpTInv0) continue;
      Int_t s = fTracks_Laser_Sector[k]/2 - 1;
      if (s < 0 || s > 11) continue;
      Int_t b = fTracks_Laser_Bundle[k] - 1;
      if (b < 0 || b >  5) continue;
      Int_t m = fTracks_Laser_Mirror[k] - 1;
      if (m < 0 || m > 6) continue;
    LOOP:
      Kxy_t *mirror = &Offset[s][b][m];
      Double_t K = fTracks_dirPM_mX2[k]/fTracks_dirPM_mX1[k];
      Double_t x = fTracks_XyzPB_mX1[k] -  fTracks_Laser_XyzB_mX1[k];
      Double_t y = fTracks_XyzPB_mX2[k] -  fTracks_Laser_XyzB_mX2[k];
      Double_t z = fTracks_XyzPB_mX3[k] -  fTracks_Laser_XyzB_mX3[k];
      if (s < 6) z -= 6.46746;
      else       z -= 6.56439;
      if (TMath::Abs(z) > 0.5) continue;
      if (TMath::Abs(x) > 1 || TMath::Abs(y) > 1) continue;
      Double_t del = y - K*x;
      mirror->N++;
      mirror->K   += K;
      mirror->Kx  += x*K;
      mirror->K2  += K*K;
      mirror->K2x += K*K*x;
      mirror->Ky  += K*y;
      mirror->x   +=   x;
      mirror->y   +=   y;
      mirror->x2  +=   x*x;
      mirror->xy  +=   x*y;
      mirror->y2  +=   y*y;
      mirror->del += del;
      mirror->Kdel+= K*del;
      mirror->z   +=   z;
      mirror->z2  +=   z*z;
      if (m != 7) {
	m = 7; goto LOOP;
      }
    }
  } 
  ofstream out;
  TString fOut = "LaserCorrection.data";
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  for (Int_t s = 0; s < 12; s++) {
    for (Int_t b = 0; b < 6; b++) {
      for (Int_t m = 0; m < 8; m++) {
      	Kxy_t *mirror = &Offset[s][b][m];
	Double_t *xx = &mirror->K;
	if (mirror->N > 2) for (Int_t i = 0; i < NW; i++) xx[i] /= mirror->N;
	else               for (Int_t i = 0; i < NW; i++) xx[i]  = 0;
	Double_t dx, dy, dz;
	dx = dy = dz = 0;
	if ( mirror->N > 2 ) {
	  dx = TMath::Sqrt(mirror->x2 - mirror->x* mirror->x);
	  dy = TMath::Sqrt(mirror->y2 - mirror->y* mirror->y);
	  dz = TMath::Sqrt(mirror->z2 - mirror->z* mirror->z);
	  cout << "Sector: " << 2*s + 2 << " Bundle: " << b+1 << " Mirror: " << m+1 << " N: " << Form("%6i",mirror->N) 
	       << " <x> = " << Form("%7.4f",mirror->x) << " +/- " << Form("%7.4f",dx)
	       << " <y> = " << Form("%7.4f",mirror->y) << " +/- " << Form("%7.4f",dy)
	       << " <z> = " << Form("%7.4f",mirror->z) << " +/- " << Form("%7.4f",dz) 
	       << endl;
	}
	if (m < 7) {
	  out << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dX+mirror->x,dx)
	      << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dY+mirror->y,dy)
	      << Form(",%7.4f,%7.4f",Mirrors[s][b][m].dZ+mirror->z,dz)
	      << "}";
	  if (m == 6) out << "}";
	  if (b == 5 && m == 6) out << "}";
	  out << ",";
	  out << "// S/B/M " << 2*(s+1) << "/" << b+1 << "/" << m+1 << " => " << mirror->N << endl;
	}
#if 0
	else {
	  Double_t K    = mirror->K;
	  Double_t K2   = mirror->K2;
	  Double_t del  = mirror->del;
	  Double_t Kdel = mirror->Kdel;
	  Double_t det = K2 - K*K;
	  cout << "======================================================" << endl;
	  cout << "Sector: " << 2*s + 2 << " Bundle: " << b+1 << " Mirror: " << m+1 << " N: " << mirror->N << " det: " << det << endl;
	  if (TMath::Abs(det) < 1e-7) continue;
	  Double_t x0 = - (Kdel - K*del)/det;
	  Double_t y0 = K*x0 + del;
	  cout << "Sector: " << 2*s + 2 << " Bundle: " << b+1 << " Mirror: " << m+1
	       << " x0/y0 = " << x0 << " / " << y0 << endl;
	  cout << "======================================================" << endl;
	}
#endif
      }
    }
  }
  out.close();   
}
//________________________________________________________________________________
void dZ(const Char_t *files = "./laser_8102110.root") {
  TDirIter Dir(files);
  TTreeIter iter("laser");
  iter.AddFile(files);
  const Int_t           &fNtrack				   = iter("fNtrack");						      
  const Int_t          *&fTracks_Flag				   = iter("fTracks.Flag");					      
  const Double_t       *&fTracks_dU_mX1			   = iter("fTracks.dU.mX1");					      
  const Double_t       *&fTracks_dU_mX2			   = iter("fTracks.dU.mX2");					      
  const Double_t       *&fTracks_dU_mX3			   = iter("fTracks.dU.mX3");					      
  const Int_t          *&fTracks_Laser_Sector			   = iter("fTracks.Laser.Sector");				      
  const Int_t          *&fTracks_Laser_Bundle			   = iter("fTracks.Laser.Bundle");				      
  const Int_t          *&fTracks_Laser_Mirror			   = iter("fTracks.Laser.Mirror");				      
  const Double_t       *&fTracks_Laser_XyzU_mX3		   = iter("fTracks.Laser.XyzU.mX3");				      
  //laser->Draw("fTracks.XyzPU.mX3:fTracks.dU.mX3","fTracks.Flag>1&&abs(fTracks.dU.mX1)<.1&&abs(fTracks.dU.mX2)<.1&&fTracks.Laser.Sector==2&&fTracks.Laser.Bundle==4&&fTracks.Laser.Mirror==3","colz")
  while (iter.Next()) {
    for (Int_t k = 0; k < fNtrack; k++) {
      if (fTracks_Flag[k] < 2) continue;
      if (TMath::Abs(fTracks_dU_mX1[k]) > 0.1 || TMath::Abs(fTracks_dU_mX2[k]) > 0.1) continue;
    }
}
#endif
