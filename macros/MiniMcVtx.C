#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
TCanvas *c1 = 0;
#define PULL
//________________________________________________________________________________
void Fit(const Char_t *dir = "") {
  if (dir) c1 = new TCanvas(dir,dir);
  else     c1 = new TCanvas();
  Int_t nx = 3;
  const Char_t *hName[3] = {"dXvsZ","dYvsZ","dZvsZ"};
#ifdef PULL
  Int_t ny = 2;
  const Char_t *hOpt[2]  = {"","P"};
#else
  Int_t ny = 1;
  const Char_t *hOpt[1]  = {""};
#endif
  c1->Divide(nx,ny);
  TString Name;
  TString Title(dir);
  for (Int_t i = 0; i < nx; i++) 
    for (Int_t j = 0; j < ny; j++) {
      Int_t ij = i + nx*j + 1;
      c1->cd(ij);
      Name = hName[i]; Name += hOpt[j];
      TH2D *h2 = (TH2D *) gDirectory->Get(Name);
      if (! h2) continue;
      h2->ProjectionY()->Fit("gaus");
    }
}
//________________________________________________________________________________
void MiniMcVtx(const Char_t *dir =  "NewVtx100", Double_t zmax=10) {
  TString Out(dir);
  Out += ".root";
  TFile *fOut = new TFile(Out,"recreate");
  TH2D *dXvsZ = new TH2D("dXvsZ","mVertexX-mMcVertexX versus Z",50,-zmax,zmax,100,-.10,.10);
  TH2D *dYvsZ = new TH2D("dYvsZ","mVertexY-mMcVertexY versus Z",50,-zmax,zmax,100,-.10,.10);
  TH2D *dZvsZ = new TH2D("dZvsZ","mVertexZ-mMcVertexZ versus Z",50,-zmax,zmax,100,-.10,.10);
#ifdef PULL
  TH2D *dXvsZP = new TH2D("dXvsZP","mVertexX-mMcVertexX Pulls versus Z",50,-zmax,zmax,100,-5.,5.);
  TH2D *dYvsZP = new TH2D("dYvsZP","mVertexY-mMcVertexY Pulls versus Z",50,-zmax,zmax,100,-5.,5.);
  TH2D *dZvsZP = new TH2D("dZvsZP","mVertexZ-mMcVertexZ Pulls versus Z",50,-zmax,zmax,100,-5.,5.);
#endif
  TString Files(dir);
  Files += "/*.minimc.root";
  TDirIter Dir(Files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("StMiniMcTree");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
#if 0
  const Int_t        &mOriginMult =			iter("mOriginMult");			         
  const Int_t        &mCentralMult =			iter("mCentralMult");			         
  const Int_t        &mCentrality =			iter("mCentrality");			         
  const Int_t        &mNUncorrectedNegativePrimaries =  iter("mNUncorrectedNegativePrimaries");      
  const Int_t        &mNUncorrectedPrimaries =	      	iter("mNUncorrectedPrimaries");	   
  const Int_t        &mNFtpcWUncorrectedPrimaries =	iter("mNFtpcWUncorrectedPrimaries");	         
  const Int_t        &mNFtpcEUncorrectedPrimaries =	iter("mNFtpcEUncorrectedPrimaries");	         
  const Int_t        &mMcMult =			      	iter("mMcMult");			   
  const Int_t        &mNMcNch =			      	iter("mNMcNch");			   
  const Int_t        &mNMcGlobal =			iter("mNMcGlobal");			         
  const Int_t        &mNRcGlobal =			iter("mNRcGlobal");			         
#endif
  const Float_t      &mVertexX =			iter("mVertexX");			   
  const Float_t      &mVertexY =			iter("mVertexY");			   
  const Float_t      &mVertexZ =			iter("mVertexZ");			   
#ifdef PULL
  const Float_t     *&mVertexCovMatrix =                iter("mVertexCovMatrix[6]");		         
#endif
  const Float_t      &mMcVertexX =			iter("mMcVertexX");			         
  const Float_t      &mMcVertexY =			iter("mMcVertexY");			         
  const Float_t      &mMcVertexZ =                      iter("mMcVertexZ");                          
  //         Now iterations
  while (iter.Next()) {
    //    if (TMath::Abs(mVtx[2]) > 20) continue;
    Double_t dx = mVertexX-mMcVertexX;
    Double_t dy = mVertexY-mMcVertexY;
    Double_t dz = mVertexZ-mMcVertexZ;
    dXvsZ->Fill(mMcVertexZ,dx);
    dYvsZ->Fill(mMcVertexZ,dy);
    dZvsZ->Fill(mMcVertexZ,dz);
#ifdef PULL
    Double_t dxP = dx/TMath::Sqrt(mVertexCovMatrix[0]);
    Double_t dyP = dy/TMath::Sqrt(mVertexCovMatrix[2]);
    Double_t dzP = dz/TMath::Sqrt(mVertexCovMatrix[5]);
    dXvsZP->Fill(mMcVertexZ,dxP);
    dYvsZP->Fill(mMcVertexZ,dyP);
    dZvsZP->Fill(mMcVertexZ,dzP);
#endif
  }
  Fit(dir);
  fOut->Write();
}
