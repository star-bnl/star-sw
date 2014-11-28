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
#include "TDirIter.h"
#include "TTreeIter.h"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "TObjArray.h"
#include "TObjString.h"
#include "TCanvas.h"
#include "TLegend.h"
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
class TDirIter;
class TTreeIter;
#endif
using namespace std;
//________________________________________________________________________________
TFile *fOut = 0;
//________________________________________________________________________________
class Tracklet : public TObject {
public:
  Tracklet() {Clear();}
  ~Tracklet() {}
  Char_t         beg[1]; // !
  Int_t          run;
  Int_t          sector;
  Int_t          row;
  Int_t          nhits;
  Double_t       AdcSum;
  Double_t       x0, tX, y0, tY;
  Double_t       x0T, tXT, y0T, tYT; // in Tpc coordinate system
  Char_t         end[1]; // !
  StThreeVectorD BG;
  StThreeVectorD BL;
  StThreeVectorD BT;
  StThreeVectorD posG;
  StThreeVectorD posL;
  StThreeVectorD dirL;
  StThreeVectorD posT;
  StThreeVectorD dirT;
  StThreeVectorD dirST;
  StThreeVectorD posRMSG;
  StThreeVectorD posRMSL;
  StThreeVectorD posRMST;
  static Bool_t         FieldTypeFF;
  TH1F* dEdx; //log(AdcSum/nhits)
  TH1F* dEdxClean; //log(AdcSum/nhits)
  TH2D* dEdxVsSectorDirty;
  TH2D* dEdxVsRowDirty;
  TH2D* dEdxVsSectorClean;
  TH2D* dEdxVsRowClean;
  TH2D* y0vsx0Dirty;
  TH2D* y0vsx0Clean;
  
  //Local c.s.
  TH2D* tXvsSectorOuter;
  TH2D* tXvsSectorInner;
  TH2D* x0vsSectorOuter;
  TH2D* x0vsSectorInner;
  
  TH2D* tYvsSectorOuter;
  TH2D* tYvsSectorInner;
  TH2D* y0vsSectorOuter;
  TH2D* y0vsSectorInner;

  TH2D* tBXvsSectorOuter;
  TH2D* tBXvsSectorInner;
  TH2D* tBYvsSectorOuter;
  TH2D* tBYvsSectorInner;

  TH2D* BXvsSectorOuter;
  TH2D* BXvsSectorInner;
  TH2D* BYvsSectorOuter;
  TH2D* BYvsSectorInner;
  //________________________________________________________________________________
  //Tpc c.s.
  TH2D* tXTpcvsSectorOuter;
  TH2D* tXTpcvsSectorInner;
  TH2D* x0TpcvsSectorOuter;
  TH2D* x0TpcvsSectorInner;
  
  TH2D* tYTpcvsSectorOuter;
  TH2D* tYTpcvsSectorInner;
  TH2D* y0TpcvsSectorOuter;
  TH2D* y0TpcvsSectorInner;
  TH2D* BXTpcvsSectorOuter;
  TH2D* BXTpcvsSectorInner;
  TH2D* BYTpcvsSectorOuter;
  TH2D* BYTpcvsSectorInner;
  void           Clear(Option_t *opt = 0) {
    if (opt) {}; memset(beg, 0, end-beg); 
    BG = BL = posG = posL = posT = posRMSG = posRMSL = posRMST = dirL = dirT = StThreeVectorD();
  }
  //________________________________________________________________________________
  void CreateHistograms(const Char_t *opt = "") {
    cout<<"Creating histograms...";
    Int_t x0_nbins = 100;
    Double_t x0_min = -50;
    Double_t x0_max = +50;
    Int_t  y0_nbins = 150;
    Double_t  y0_min = 40;
    Double_t y0_max = 190;
    Int_t  sec_nbins = 24;
    Double_t sec_min = 0.5;
    Double_t sec_max = 24.5;
    
    Int_t row_nbins = 45;
    Double_t row_min = 0.5;
    Double_t row_max = 45.5;
    
    Int_t tX_nbins = 1000;
    Double_t tX_min = -50.0;
    Double_t tX_max = +50.0;
    
    Int_t tY_nbins = 400;
    Double_t tY_min = -20.0;
    Double_t tY_max = +20.0;
    Int_t dEdx_nbins = 60;
    Double_t dEdx_min = 3.;
    Double_t dEdx_max = 9.;
  
  
    dEdx = new TH1F("dEdx",Form("log(AdcSum/nhits) for %s",opt),dEdx_nbins,dEdx_min,dEdx_max);
    dEdxClean = new TH1F("dEdxClean",Form("log(AdcSum/nhits) for %s",opt),dEdx_nbins,dEdx_min,dEdx_max);
    dEdxVsSectorDirty = new TH2D("dEdxVsSectorDirty",Form("log(AdcSum/nhits) vs sector for %s;Sector# ;log(AdcSum/nhits)",opt),
				  sec_nbins,sec_min,sec_max,dEdx_nbins,dEdx_min,dEdx_max);
    dEdxVsRowDirty = new TH2D("dEdxVsRowDirty",Form("log(AdcSum/nhits) vs Row for %s;Row# ;log(AdcSum/nhits)",opt),
			       row_nbins,row_min,row_max,dEdx_nbins,dEdx_min,dEdx_max);
    dEdxVsSectorClean = new TH2D("dEdxVsSectorClean",Form("log(AdcSum/nhits) vs sector for %s;Sector# ;log(AdcSum/nhits)",opt),
				  sec_nbins,sec_min,sec_max,dEdx_nbins,dEdx_min,dEdx_max);
    dEdxVsRowClean = new TH2D("dEdxVsRowClean",Form("log(AdcSum/nhits) vs Row for %s;Row# ;log(AdcSum/nhits)",opt),
			       row_nbins,row_min,row_max,dEdx_nbins,dEdx_min,dEdx_max);
    y0vsx0Dirty = new TH2D("y0vsx0Dirty",Form("Local y0 vs x0 before cuts for %s;x0;y0",opt),
			    x0_nbins,x0_min,x0_max,y0_nbins,y0_min,y0_max);
    y0vsx0Clean = new TH2D("y0vsx0Clean",Form("Local y0 vs x0 after cuts for %s;x0;y0",opt),
			    x0_nbins,x0_min,x0_max,y0_nbins,y0_min,y0_max);
    
    //Local coordinate system
    tXvsSectorOuter = new TH2D("tXvsSectorOuter",Form("tX vs sector (outer) for %s;Sector# ;tX [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    tXvsSectorInner = new TH2D("tXvsSectorInner",Form("tX vs sector (inner) for %s;Sector# ;tX [mrad]",opt),
				sec_nbins,sec_min,sec_max,
				tX_nbins,tX_min,tX_max);
    x0vsSectorOuter = new TH2D("x0vsSectorOuter",Form("x0 vs sector (outer) for %s;Sector# ;x0 [cm]",opt),
				sec_nbins,sec_min,sec_max,x0_nbins,x0_min,x0_max);
    x0vsSectorInner = new TH2D("x0vsSectorInner",Form("x0 vs sector (inner) for %s;Sector# ;x0 [cm]",opt),
				sec_nbins,sec_min,sec_max,x0_nbins,x0_min,x0_max);
    
    tYvsSectorOuter = new TH2D("tYvsSectorOuter",Form("tY vs sector (outer) for %s;Sector# ;tY [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    tYvsSectorInner = new TH2D("tYvsSectorInner",Form("tY vs sector (inner) for %s;Sector# ;tY [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    y0vsSectorOuter = new TH2D("y0vsSectorOuter",Form("y0 vs sector (outer) for %s;Sector# ;y0 [cm]",opt),
				sec_nbins,sec_min,sec_max,y0_nbins,y0_min,y0_max);
    y0vsSectorInner = new TH2D("y0vsSectorInner",Form("y0 vs sector (inner) for %s;Sector# ;y0 [cm]",opt),
				sec_nbins,sec_min,sec_max,y0_nbins,y0_min,y0_max);
    //Local coordinate system
    tBXvsSectorOuter = new TH2D("tBXvsSectorOuter",Form("tX-Bx vs sector (outer) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    tBXvsSectorInner = new TH2D("tBXvsSectorInner",Form("tX-Bx vs sector (inner) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    tBYvsSectorOuter = new TH2D("tBYvsSectorOuter",Form("tY-By vs sector (outer) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    tBYvsSectorInner = new TH2D("tBYvsSectorInner",Form("tY-By vs sector (inner) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    BXvsSectorOuter = new TH2D("BXvsSectorOuter",Form("Bx vs sector (outer) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    BXvsSectorInner = new TH2D("BXvsSectorInner",Form("Bx vs sector (inner) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    BYvsSectorOuter = new TH2D("BYvsSectorOuter",Form("By vs sector (outer) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    BYvsSectorInner = new TH2D("BYvsSectorInner",Form("By vs sector (inner) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    //Tpc coordinate system
    tXTpcvsSectorOuter = new TH2D("tXTpcvsSectorOuter",Form("tX Tpc vs sector (outer) for %s;Sector# ;tX [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    tXTpcvsSectorInner = new TH2D("tXTpcvsSectorInner",Form("tX Tpc vs sector (inner) for %s;Sector# ;tX [mrad]",opt),
				sec_nbins,sec_min,sec_max,
				tX_nbins,tX_min,tX_max);
    x0TpcvsSectorOuter = new TH2D("x0TpcvsSectorOuter",Form("x0 Tpc vs sector (outer) for %s;Sector# ;x0 [cm]",opt),
				sec_nbins,sec_min,sec_max,x0_nbins,x0_min,x0_max);
    x0TpcvsSectorInner = new TH2D("x0TpcvsSectorInner",Form("x0 Tpc vs sector (inner) for %s;Sector# ;x0 [cm]",opt),
				sec_nbins,sec_min,sec_max,x0_nbins,x0_min,x0_max);
    
    tYTpcvsSectorOuter = new TH2D("tYTpcvsSectorOuter",Form("tY Tpc vs sector (outer) for %s;Sector# ;tY [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    tYTpcvsSectorInner = new TH2D("tYTpcvsSectorInner",Form("tY Tpc vs sector (inner) for %s;Sector# ;tY [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    y0TpcvsSectorOuter = new TH2D("y0TpcvsSectorOuter",Form("y0 Tpc vs sector (outer) for %s;Sector# ;y0 [cm]",opt),
				sec_nbins,sec_min,sec_max,y0_nbins,y0_min,y0_max);
    y0TpcvsSectorInner = new TH2D("y0TpcvsSectorInner",Form("y0 Tpc vs sector (inner) for %s;Sector# ;y0 [cm]",opt),
				sec_nbins,sec_min,sec_max,y0_nbins,y0_min,y0_max);
    
    BXTpcvsSectorOuter = new TH2D("BXTpcvsSectorOuter",Form("Bx Tpc vs sector (outer) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    BXTpcvsSectorInner = new TH2D("BXTpcvsSectorInner",Form("Bx Tpc vs sector (inner) for %s;Sector# ;tX-Bx [mrad]",opt),
				sec_nbins,sec_min,sec_max,tX_nbins,tX_min,tX_max);
    BYTpcvsSectorOuter = new TH2D("BYTpcvsSectorOuter",Form("By Tpc vs sector (outer) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    BYTpcvsSectorInner = new TH2D("BYTpcvsSectorInner",Form("By Tpc vs sector (inner) for %s;Sector# ;tY-By [mrad]",opt),
				sec_nbins,sec_min,sec_max,tY_nbins,tY_min,tY_max);
    cout<<"\t [DONE]"<<endl;
  }

  //________________________________________________________________________________
  void FillHistograms() {
    //    static Double_t sqrt3 = TMath::Sqrt(3.);
    if (nhits < 25) return;
    Float_t AdcL = TMath::Log(AdcSum/nhits);
    dEdx->Fill(AdcL);
    dEdxVsSectorDirty->Fill(sector, AdcL);
    dEdxVsRowDirty->Fill(row,AdcL);
    if (posRMSL.z() < 10.0 || posRMSL.z() > 70) return;
    //    if (posL.z()+sqrt3*posRMSL.z() > 210) return; // vers.5
    //    if (posL.z() < 50 || posRMSL.z() > 15) return;
    if (AdcL < 5) return;
#if 0
    //    if (AdcSum/nhits < 150) return;
    if (! FieldTypeFF &&         //false - RFF; true - FF
	(( sector== 5 && row==13) || 
	 ( sector== 6 && row==34) || 
	 ( sector==13 && row==36) || 
	 ( sector== 7 && row== 1) || 
	 ( sector==18 && row==35))) return;
#endif
    dEdxVsSectorClean->Fill(sector, AdcL);
    dEdxClean->Fill(AdcL);
    dEdxVsRowClean->Fill(row,AdcL);
    if (row <= 13) {
      tXvsSectorInner->Fill(sector,1e3*tX);
      tBYvsSectorInner->Fill(sector,1e3*(tY-BL.y()/BL.z()));
      tBXvsSectorInner->Fill(sector,1e3*(tX-BL.x()/BL.z()));
      BYvsSectorInner->Fill(sector,1e3*(BL.y()/BL.z()));
      BXvsSectorInner->Fill(sector,1e3*(BL.x()/BL.z()));

      BYTpcvsSectorInner->Fill(sector,1e3*(BT.y()/BT.z()));
      BXTpcvsSectorInner->Fill(sector,1e3*(BT.x()/BT.z()));
      tYvsSectorInner->Fill(sector,1e3*tY);
      x0vsSectorInner->Fill(sector,x0);
      y0vsSectorInner->Fill(sector,y0);

      tXTpcvsSectorInner->Fill(sector,1e3*tXT);
      tYTpcvsSectorInner->Fill(sector,1e3*tYT);
      x0TpcvsSectorInner->Fill(sector,x0T);
      y0TpcvsSectorInner->Fill(sector,y0T);
    } else {
      if (row > 14 && row < 45) {
	tXvsSectorOuter->Fill(sector,1e3*tX);
	tYvsSectorOuter->Fill(sector,1e3*tY);
	tBXvsSectorOuter->Fill(sector,1e3*(tX-BL.x()/BL.z()));
	tBYvsSectorOuter->Fill(sector,1e3*(tY-BL.y()/BL.z()));
	BXvsSectorOuter->Fill(sector,1e3*(BL.x()/BL.z()));
	BYvsSectorOuter->Fill(sector,1e3*(BL.y()/BL.z()));
	BXTpcvsSectorOuter->Fill(sector,1e3*(BT.x()/BT.z()));
	BYTpcvsSectorOuter->Fill(sector,1e3*(BT.y()/BT.z()));
	x0vsSectorOuter->Fill(sector,x0);
	y0vsSectorOuter->Fill(sector,y0);

	tXTpcvsSectorOuter->Fill(sector,1e3*tXT);
	tYTpcvsSectorOuter->Fill(sector,1e3*tYT);
	x0TpcvsSectorOuter->Fill(sector,x0T);
	y0TpcvsSectorOuter->Fill(sector,y0T);
      }
    }
  }
  //________________________________________________________________________________
  void FitHistograms() {
    cout<<"Creating 2D histograms slicesY...";
    //Local c.s.
    tXvsSectorOuter->FitSlicesY();
    tXvsSectorInner->FitSlicesY();
    tYvsSectorOuter->FitSlicesY();
    tYvsSectorInner->FitSlicesY();
    tBXvsSectorOuter->FitSlicesY();
    tBXvsSectorInner->FitSlicesY();
    tBYvsSectorOuter->FitSlicesY();
    tBYvsSectorInner->FitSlicesY();
    BXvsSectorOuter->FitSlicesY();
    BXvsSectorInner->FitSlicesY();
    BYvsSectorOuter->FitSlicesY();
    BYvsSectorInner->FitSlicesY();

    tXTpcvsSectorOuter->FitSlicesY();
    tXTpcvsSectorInner->FitSlicesY();
    tYTpcvsSectorOuter->FitSlicesY();
    tYTpcvsSectorInner->FitSlicesY();
    cout<<"\t [DONE]"<<endl;
  }
  ClassDef(Tracklet,2)
};
Bool_t Tracklet::FieldTypeFF = kTRUE;

//________________________________________________________________________________
void EandB(Int_t nevents = -1, const Char_t *select = "", const Char_t *out = "CompAllMLX.root") {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  fOut               = new TFile(out,"update");
  if (! fOut)   fOut = new TFile(out,"recreate");
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  TString Select(select);
  Int_t all = 0;
  if (Select.Contains("all",TString::kIgnoreCase)) all = 1;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    F.ReplaceAll(".root","");
    F.ReplaceAll("EandB_","");
    F.ReplaceAll("corr6_","");
    if (F.Contains("Comp",TString::kIgnoreCase)) continue;
    if (! (Select != "") && ! F.Contains(Select,TString::kIgnoreCase)) continue;
    TTree *tree = (TTree *) f->Get("TrackletTree");
    if (! tree) continue;
    cout << "Do " << F.Data() << "  set ====================" << endl;
    TBranch *branch = tree->GetBranch("Tracklet");
    Tracklet *T = new Tracklet;
    branch->SetAddress(&T);
    Int_t nentries = (Int_t)tree->GetEntries();
    if (nevents > 0)  nevents = TMath::Min(nevents, nentries);
    else              nevents = nentries;
    Int_t cachesize = -1; 
    tree->SetCacheSize(cachesize);
    tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
    tree->SetCacheEntryRange(0,nentries);
    FitFiles[NF] = f; NF++;
    f->cd();
    if (! fOut->cd(F)) {fOut->mkdir(F); fOut->cd(F); cout << "Create directory " << F.Data() << endl;}
    T->FieldTypeFF = kTRUE;
    if (F.Contains("RF",TString::kIgnoreCase)) T->FieldTypeFF = kFALSE;
    T->CreateHistograms(F);
    for (Int_t ev = 0; ev < nevents; ev++) {
      tree->LoadTree(ev);  //this call is required when using the cache
      tree->GetEntry(ev); 
      T->FillHistograms();
    }
    T->FitHistograms();
    delete T;
  }
  delete [] FitFiles;
  fOut->Write();
}
//________________________________________________________________________________
Double_t phiN(Double_t phi) {
  while (TMath::Abs(phi) > 180.) {
    if (phi > 180) phi -= 360;
    if (phi <-180) phi += 360;
  }
  return phi;
}
//________________________________________________________________________________
Double_t Phi(Double_t sector) {
  Double_t phi;
  if (sector <= 12.5) {
    phi =  (360 + 90 - 30* sector      ); 
  } else { 
    phi =  (      90 + 30*(sector - 12));
  }
  return TMath::DegToRad()*phi;
}
//________________________________________________________________________________
Double_t func(Double_t *x, Double_t *p) {
  //  Int_t sec = TMath::Nint(x[0]);
  return p[0] + p[1]*TMath::Sin(TMath::DegToRad()*(p[2] - Phi(x[0])));
}
//________________________________________________________________________________
Double_t funcSumtX(Double_t *x, Double_t *p) {
  Double_t dPhi = p[2] + Phi(x[0]); // 
  Double_t val = p[1]*TMath::Cos(TMath::DegToRad()*(dPhi));
  //  if (x[0] < 12.5) val = -val;
  return p[0] + val;
}
//________________________________________________________________________________
Double_t funcSumtY(Double_t *x, Double_t *p) {
  Double_t dPhi = p[2] + Phi(x[0]); // phi_0 - phi;
  Double_t val =  p[0] + p[1]*TMath::Sin(TMath::DegToRad()*(dPhi));
  if (x[0] < 12.5) val = - val;
  return val;
}
/* MagG.dem
tXWS:$$\left[  \sin \phi (\lambda - \alpha) +\beta  \cos \phi - B_\rho \cos \Phi\right] $$
tYWS:$$\left[ -\cos \phi (\lambda - \alpha) +\beta  \sin \phi + B_\rho \sin \Phi\right] $$
tXES:$$\left[ -\sin \phi (\lambda - \alpha) -\beta  \cos \phi + B_\rho \cos \Phi\right] $$
tYES:$$\left[ -\cos \phi (\lambda - \alpha) +\beta  \sin \phi - B_\rho \sin \Phi\right] $$
*/
Double_t functS(Double_t *x, Double_t *p) { 
  Double_t offset = p[0];  
  Double_t alpha  = p[1]; // -(alpha - lambda)
  Double_t beta   = p[2]; 
  Double_t b_rho  = p[3]; 
  Double_t Phi0   = p[4]; 
  Int_t            we = 0; // West; 
  if (x[0] > 12.5) we = 2; // East 
  Int_t    k = TMath::Nint(p[5]) + we; /* 0 => tX, 1 => tY */ 
  Double_t phi = Phi(x[0]);  
  Double_t val = 0; 
  switch (k) {  
    /* tXWS */ case 0: val =    alpha*TMath::Sin(phi) +beta* TMath::Cos(phi) - b_rho*TMath::Cos(Phi0); break; 
    /* tYWS */ case 1: val =   -alpha*TMath::Cos(phi) +beta* TMath::Sin(phi) + b_rho*TMath::Sin(Phi0); break;
    /* tXES */ case 2: val =-( -alpha*TMath::Sin(phi) -beta* TMath::Cos(phi) + b_rho*TMath::Cos(Phi0)); break;				       
    /* tYES */ case 3: val =-( -alpha*TMath::Cos(phi) +beta* TMath::Sin(phi) - b_rho*TMath::Sin(Phi0)); break; 
  default: break;
  };
  return offset + val;
}
//________________________________________________________________________________
Double_t funcSumtXN(Double_t *x, Double_t *p) {
  Double_t phi = Phi(x[0]); //  p[1] = delta = alpha - lambda, [2] = beta
  Double_t val = p[0] + p[1]*TMath::Sin(phi);
#if 1
  if (x[0] < 12.5) val += p[2]*TMath::Cos(phi);
  else             val -= p[2]*TMath::Cos(phi);
#else
  val += p[2]*TMath::Cos(phi);
#endif
  return val;
}
//________________________________________________________________________________
Double_t funcSumtYN(Double_t *x, Double_t *p) {
  Double_t phi = Phi(x[0]); // p[1] = delta = alpha - lambda, [2] = beta
  Double_t val = p[0] + p[1]*TMath::Cos(phi);
  if (x[0] < 12.5) val -= p[2]*TMath::Sin(phi);
  else             val += p[2]*TMath::Sin(phi);
  return val;
}
#if 0
//________________________________________________________________________________
Double_t funcDiftX(Double_t *x, Double_t *p) {
  Double_t dPhi = p[2] - Phi(x[0]); // phi_0 - phi;
  Double_t val = p[1]*TMath::Cos(TMath::DegToRad()*(dPhi));
  if (x[0] > 12.5) val = - val;
  return p[0] + val;
}
//________________________________________________________________________________
Double_t funcDiftY(Double_t *x, Double_t *p) {
  Double_t dPhi = p[2] - Phi(x[0]); // phi_0 - phi;
  return p[0] + p[1]*TMath::Sin(TMath::DegToRad()*(dPhi));
}
#endif
//________________________________________________________________________________
TF1 *Func(const Char_t *name, Int_t xy = 0) {
  TF1 *f = new TF1(name, functS,0.5,24.5,6);
  //  f->SetParNames("o","#alpha-#lambda","#beta","b_{#rho}","#Phi","XY");
  f->SetParNames("o","#alpha","#beta","b_{#rho}","#Phi","XY");
  f->SetParameters(0,0,0,0,0,0);
  //  f->FixParameter(0,0);
  f->SetParLimits(3,-1,1); 
  f->FixParameter(3,0); // b_rho => 0
  f->FixParameter(4,0); // #Phi => 0
  f->FixParameter(5,xy);
  return f;
}
//________________________________________________________________________________
void Draw(TString Opt, TString tag ) {
  if (! gFile) return;
  TObjString *objs;
#if 0  
  TObjArray *arrayn = Names.Tokenize("&|");
  Int_t Nplots = arrayn->GetEntries();
  TString names[Nplots];
  TString title[Nplots];
  TIter nextN(arrayn);
  Int_t i = 0;
  while ((objs = (TObjString *) nextN())) {
    names[i] = objs->GetString();
    title[i] = names[i];
    title[i].ReplaceAll("vs"," versus ");
    title[i].ReplaceAll("Sector"," sector ");
    title[i].ReplaceAll("_1","");
    i++;
    if (i >= Nplots) break;
  }
  delete array;
#else
  Int_t Nplots = 4;
  TString names[Nplots];
  TString title[Nplots];
  const Char_t *plotN[4] = {"XvsSectorInner_1","XvsSectorOuter_1","YvsSectorInner_1","YvsSectorOuter_1"};
  for (Int_t i = 0; i < Nplots; i++) {
    names[i] = tag; names[i] += plotN[i];
    title[i] = names[i];
    title[i].ReplaceAll("vs"," versus ");
    title[i].ReplaceAll("Sector"," sector ");
    title[i].ReplaceAll("_1","");
  }
#endif
  TObjArray *array = Opt.Tokenize("&|");
  TIter next(array);
  TString opt = Opt; opt += tag;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(opt);
  //  if (! c1) c1 = new TCanvas(opt,opt,1000,1200);
  if (! c1) c1 = new TCanvas(opt,opt,800,1000);
  c1->Clear();
  Int_t nx = 2;
  Int_t ny = (Nplots-1)/2 + 1;
  c1->Divide(nx,ny);
  //  TF1 *fW = new TF1("fW","[0]+[1]*TMath::Sin(-TMath::DegToRad()*((360 + 90 - 30*x       )+[2]))",0.5,12.5);
  //  TF1 *fW = new TF1("fW",funcSumtXN,0.5,12.5,3);
  //  TF1 *fs[6] = { Func("fWx",0), Func("fEx",0), Func("fWESumtX",0),
  //		 Func("fWy",1), Func("fEy",1), Func("fWESumtY",1)};
  const Char_t *NamesF[6] = {"fWx", "fEx", "fWESumtX",
			     "fWy", "fEy", "fWESumtY"};
//   TF1 *fWx      = fs[0];
//   TF1 *fEx      = fs[1];
//   TF1 *fWESumtX = fs[2];
//   TF1 *fWy      = fs[3];
//   TF1 *fEy      = fs[4];
//   TF1 *fWESumtY = fs[5];
  TF1 *f = 0;
  for (Int_t i = 1; i <= Nplots; i++) {
    next.Reset();
    TVirtualPad *pad = c1->cd(i);
    TH1F *frame = pad->DrawFrame(0.5,-1.0,24.5,1.5,title[i-1]);
    //    frame->SetTitle(title[i-1]);
    frame->SetXTitle("sector");
    Int_t itXY = 0;
    if (names[i-1].BeginsWith("tX") || names[i-1].BeginsWith("tBX") || names[i-1].BeginsWith("BX"))  frame->SetYTitle("t_{X} [mrad]");
    if (names[i-1].BeginsWith("tY") || names[i-1].BeginsWith("tBY") || names[i-1].BeginsWith("BY")) {frame->SetYTitle("t_{Y} [mrad]"); itXY = 1;}
    Int_t color = 0;
    TLegend *l = new TLegend(0.1,0.6,0.9,0.9);
    l->Draw();
    TH1D *hfr[10]; memset(hfr, 0, sizeof(hfr));
    TString Tags[10];
    Int_t NF = 0;
    while ((objs = (TObjString *) next())) {
      cout << objs->GetString() << endl;
      Tags[NF] = objs->GetString();
      TString hName(Form("%s/%s",Tags[NF].Data(),names[i-1].Data()));
      hfr[NF] = (TH1D *) gFile->Get(hName);
      cout << " Histogram " << hName;
      if (hfr[NF]) cout << " has been found" << endl;
      else {cout << " has not been found" << endl; continue;}
      NF++;
      if (NF > 10) break;
    }
    if (NF == 2  && hfr[0] && hfr[1]) {
      hfr[2] = hfr[3] = 0;
      hfr[2] = new TH1D(*hfr[0]);
      hfr[2]->Scale(0.5);
      hfr[2]->Add(hfr[1],0.5), hfr[2]->SetName(Form("sum%sand%s",hfr[0]->GetName(),hfr[1]->GetName()));
      Tags[2] = Tags[0]; Tags[2] += "+";  Tags[2] += Tags[1];     
      //      } else {
      // 	hfr[3] = new TH1D(*hfr[0]);
      // 	hfr[3]->Add(hfr[1],-1.), hfr[3]->SetName(Form("dif%sand%s",hfr[0]->GetName(),hfr[1]->GetName()));
      // 	Tags[3] = Tags[0]; Tags[3] += "-";  Tags[3] += Tags[1];     
      //      }
      NF++;
    }
    const Char_t *sides[3] = {"West:","East:","All:"};
    Double_t lim[3][2] = {{0.5,12.5},{12.5,24.5},{0.5,24.5}};
    TString line;
    ofstream out;
    TString Out("EandB.out");
    if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
    else                              out.open(Out, ios::app);
    out << Opt.Data() << "|" << tag.Data() << endl;
    for (Int_t j = 0; j < NF; j++) {
      color++;
      if (! hfr[j]) continue;
      hfr[j]->SetStats(0);
      hfr[j]->SetMarkerColor(color);
      hfr[j]->Draw("same");
      l->AddEntry(hfr[j],Tags[j]);
      if (j >= 2) {
	for (Int_t wel = 0; wel < 3; wel++) {
	  //	f = fs[3*itXY + wel];
	  f = Func(NamesF[3*itXY + wel],itXY);
	  f->SetLineColor(color);
	  f->SetMarkerColor(color);
	  line = sides[wel]; cout << f->GetName() << line.Data() << " limits :" << lim[wel][0] << "," <<  lim[wel][1] << endl;
	  hfr[j]->Fit(f,"er+","same",lim[wel][0],lim[wel][1]);
	  //	f = hfr[j]->GetFunction(f->GetName());
	  TString hN(hfr[j]->GetName());
	  hN.ReplaceAll("sum","");
	  hN.ReplaceAll("vsSectorInner_1","I");
	  hN.ReplaceAll("vsSectorOuter_1","O");
	  TString And("and");
	  Int_t m = hN.Index("and");
	  if (m > 0) {
	    And += TString(hN.Data(),m);
	    hN.ReplaceAll(And.Data(),"");
	  }
	  hN.ReplaceAll("andtBYSectorInner_1","");
	  hN.ReplaceAll("andtBYSectorOuter_1","");
	  hN.ReplaceAll("andtBXSectorInner_1","");
	  hN.ReplaceAll("andtBXSectorOuter_1","");
	  out << hN.Data();
	  if (f) {
	    for (Int_t p = 0; p < 5; p++)	{
	      if (f->GetParError(p) > 0) {
		if (p != 4) 
		  line += Form(" %s (%5.2f+/-%4.2f)",f->GetParName(p), f->GetParameter(p),f->GetParError(p));
		else 
		  line += Form("%s(%5.2f+/-%4.2f)",f->GetParName(p), 
			       phiN(TMath::RadToDeg()*f->GetParameter(p)),TMath::RadToDeg()*f->GetParError(p));
	      }
	    }
	    cout << line.Data() << endl;
	    l->AddEntry(f,line);
	    out << line.Data() << endl;
	  }
	}
      }
    }
    out.close();
  }
  delete array;
}
//________________________________________________________________________________
void Draw(TString opt="FF2.NewOAll|RFF2.NewOAll", Int_t k = 0) {
  if      (k == 0) Draw(opt,"tB");
  else if (k == 1) Draw(opt,"B");
  else             Draw(opt,"t");
}
/*
  1.  Compare 1D and 2D for both FF and RFF, no correction (1Dvs2D_NoCorrections)
  Over all shift in tX (inner -0.6 mrad, outer -0.8 mrad for 1D => 2D). <tX East> ~ 0
  No effect on tY. 
  <tY_inner_West> ~ 0.3 mrad, <tY_inner_East> ~ 0
  <tY_outer_West> ~ 1.2 mrad, <tY_inner_East> ~ 0.4 mrad
2. 2D OshortR.
     There is offset between FF and RFF. West (Outer  to East
3. Align 
new jobs

FF2.OShortR.OBmap.Align2 -

RFF2B -
RFF2.AlignB -
RFF2.Align2 -
RFF2.OShortRB -
RFF2.OShortR.AlignB -
RFF2.OShortR.Align2 -
RFF2.OShortR.OBmapB -
RFF2.OShortR.OBmap.AlignB -
RFF2.OShortR.OBmap.Align2 -



FF1.root			RFF1.root			
FF1.Align.root                  RFF1.Align.root			
FF2.root			RFF2.root                       
FF2.Align.root			RFF2.Align.root			
                                RFF2.Align2.root		
FF2.OShortR.root		RFF2.OShortR.root
FF2.OShortR.Align.root		RFF2.OShortR.Align.root
                                RFF2.OShortR.Align2.root		
FF2.OShortR.OBmap.root		RFF2.OShortR.OBmap.root
FF2.OShortR.OBmap.Align.root	RFF2.OShortR.OBmap.Align.root	
FF2.OShortR.OBmap.Align2.root	RFF2.OShortR.OBmap.Align2.root	

1. NoCorrections

================================================================================
09/13/13
Align2 => Align (no mechanical distortion for OFC)

================================================================================
09/18/13 
    (  1 -g  b  x)
R = (  g  1 -a  y)
    ( -b  a  1  z)
    (  0  0  0  1)
Rx: matrix([1,0,0,0],[0,1,-a,0],[0,a,1,0],[0,0,0,1])$
Ry: matrix([1,0,b,0],[0,1,0,0],[-b,0,1,0],[0,0,0,1])$
Rz: matrix([1,-g,0,0],[g,1,0,0],[0,0,1,0],[0,0,0,1])$

root.exe tpcDbTest.C
StTpcDb::instance()->FieldCage()
 westClockError  -0.00043 : ==> 0.43 mrad
                  (1     0      0)    (0)    (     0)
RotateX(alpha) =  (0     1 -alpha) x  (0) =  (-alpha)
                  (0 alpha      1)    (1)    (     1)
                  (1     0 beta)
RotateY(beta)  =  (0     1    0)
                  (-beta 0    1)

                  (1     -gamma      0)
RotateZ(gamma) =  (gamma      1      0)
                  (0          0      1)

Default 
root.exe [2] StTpcDb::instance()->Tpc2GlobalMatrix()->Print()
matrix Tpc2Glob - tr=1  rot=1  refl=0  scl=0
  1.000000    0.000000    0.000190    Tx =  -0.178000           beta = 0.19 mrad, alpha = 0.044 mrad
  0.000000    1.000000   -0.000044    Ty =  -0.675200
 -0.000190    0.000044    1.000000    Tz =  -0.080860

root.exe [5] StTpcDb::instance()->TpcHalf(0)->Print()  east
matrix  - tr=1  rot=1  refl=0  scl=0
  1.000000    0.000000    0.000000    Tx =   0.000000
  0.000000    1.000000    0.000000    Ty =   0.000000
  0.000000    0.000000    1.000000    Tz =   0.000000

root.exe [6] StTpcDb::instance()->TpcHalf(1)->Print() west
matrix  - tr=1  rot=1  refl=0  scl=0
  1.000000    0.000000    0.000000    Tx =   0.000000
  0.000000    1.000000    0.000000    Ty =   0.000000
  0.000000    0.000000    1.000000    Tz =   0.000000

-------------------
Survey
root.exe [1] StTpcDb::instance()->Tpc2GlobalMatrix()->Print()
matrix Tpc2Glob - tr=1  rot=1  refl=0  scl=0
  1.000000   -0.000000   -0.000420    Tx =  -0.245000        beta = -0.42 mrad
  0.000000    1.000000    0.000000    Ty =  -0.143100
  0.000420   -0.000000    1.000000    Tz =  -0.196100

root.exe [2] StTpcDb::instance()->TpcHalf(0)->Print()   east
matrix  - tr=1  rot=1  refl=0  scl=0
  1.000000   -0.000390   -0.000030    Tx =   0.006700        gamma = 0.39 mrad, alpha = -0.38 mrad, beta  = -0.03 mrad
  0.000390    1.000000    0.000380    Ty =  -0.002300
  0.000030   -0.000380    1.000000    Tz =   0.000000

root.exe [3] StTpcDb::instance()->TpcHalf(1)->Print()  west
matrix  - tr=1  rot=1  refl=0  scl=0
  1.000000    0.000030    0.000310    Tx =   0.026000        gamma = -0.03 mrad, alpha = 0.36 mrad, beta = 0.31 mrad
 -0.000030    1.000000   -0.000360    Ty =  -0.071200
 -0.000310    0.000360    1.000000    Tz =   0.000000


List of productions
FF and RFF field directions
"1"       1D-cluster finder no. correction
"1.Align" -"-        with my alignment   
"1.MC     -"- ideal MC

"2"       2D-cluster finder no. correction
"2.Align" -"-        with my alignment (E west turned)
"2.MC"    -"-  ideal MC
"2.OShortR"
"2.OShortR.Align"         
"2.OShortR.OBmap.eval"

"2.OShortR.OBmap"
"2.OShortR.OBmap.Align"
"2.Align2"               my alignment with (E and W the same E direction)
"2.OShortR.Align2
"2.OShortR.OBmap.Align2"   
"2.OAll"                 ExB,OShortR,OBmap,OPr13,OIFC,OSectorAlign
"2OAllW                  const_1 == const_2 in MagUtil
"2OAllWA                + my alignment
"AllWAR"                  my alignment, const_1 != const_2 in MagUtil, 1-st iteration with mag rotation
"OAllA.R2"                -"-, 2nd interation with mag rotation (lambda => - lambda)
"OAllA.R3"                -"-, 3rd interation with mag rotation (lambda => - lambda, phi0 => -phi0)
"OAllA.R4"                -"-, 4th interation with mag rotation (lambda =>   lambda, phi0 => -phi0)
"OAll.eval"
================================================================================
11/01/13 
2NewOAll               + new alignment + all corrections

12/09/13
rotate mag.field with missing resistor 2.2 Mohm
FF2.Short2.2Mom|FF2.Short2.2Mom |tB tBXIWest: o ( 0.09+/-0.01) a ( 0.20+/-0.02) b ( 0.16+/-0.02) tBXIEast: o (-0.03+/-0.02) a ( 0.11+/-0.02) b ( 0.06+/-0.02) tBXIAll: o ( 0.04+/-0.01) a ( 0.16+/-0.02) b ( 0.11+/-0.02)
FF2.Short2.2Mom|FF2.Short2.2Mom |tB tBXOWest: o (-0.04+/-0.01) a ( 0.11+/-0.01) b ( 0.28+/-0.01) tBXOEast: o ( 0.08+/-0.01) a (-0.01+/-0.01) b ( 0.04+/-0.01) tBXOAll: o ( 0.02+/-0.01) a ( 0.05+/-0.01) b ( 0.17+/-0.01)
FF2.Short2.2Mom|FF2.Short2.2Mom |tB tBYIWest: o (-0.45+/-0.00) a ( 0.07+/-0.00) b ( 0.33+/-0.00) tBYIEast: o (-0.02+/-0.00) a (-0.08+/-0.00) b ( 0.16+/-0.00) tBYIAll: o (-0.09+/-0.00) a (-0.07+/-0.00) b ( 0.18+/-0.00)
FF2.Short2.2Mom|FF2.Short2.2Mom |tB tBYOWest: o (-0.12+/-0.01) a ( 0.18+/-0.01) b ( 0.23+/-0.01) tBYOEast: o (-0.09+/-0.01) a ( 0.08+/-0.01) b ( 0.09+/-0.01) tBYOAll: o (-0.11+/-0.01) a ( 0.14+/-0.01) b ( 0.17+/-0.01)

FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBXIWest: o ( 0.05+/-0.02) a ( 0.07+/-0.02) b ( 0.23+/-0.02) tBXIEast: o (-0.05+/-0.02) a (-0.05+/-0.02) b ( 0.07+/-0.02) tBXIAll: o ( 0.00+/-0.01) a ( 0.01+/-0.02) b ( 0.15+/-0.02)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBXOWest: o (-0.01+/-0.01) a ( 0.01+/-0.01) b ( 0.25+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.06+/-0.01) b ( 0.06+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.03+/-0.01) b ( 0.16+/-0.01)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBYIWest: o (-0.44+/-0.00) a ( 0.02+/-0.00) b ( 0.17+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b ( 0.02+/-0.00) tBYIAll: o (-0.08+/-0.00) a (-0.10+/-0.00) b ( 0.04+/-0.00)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.21+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b ( 0.03+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.13+/-0.01)

FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.28+/-0.02) tBXIEast: o (-0.04+/-0.01) a (-0.04+/-0.02) b ( 0.12+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.02+/-0.01) b ( 0.20+/-0.01)
FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.29+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.06+/-0.01) b ( 0.11+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.20+/-0.01)
FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.20+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b ( 0.05+/-0.00) tBYIAll: o (-0.09+/-0.00) a (-0.11+/-0.00) b ( 0.08+/-0.00)
FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.24+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b ( 0.07+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.16+/-0.01)

FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.18+/-0.02) tBXIEast: o (-0.05+/-0.01) a (-0.04+/-0.02) b ( 0.05+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.02+/-0.01) b ( 0.12+/-0.01)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.20+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b ( 0.03+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.12+/-0.01)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBYIWest: o (-0.44+/-0.00) a ( 0.02+/-0.00) b ( 0.15+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b (-0.02+/-0.00) tBYIAll: o (-0.10+/-0.00) a (-0.15+/-0.00) b ( 0.00+/-0.00)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.17+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b ( 0.00+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.09+/-0.01)

FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.14+/-0.02) tBXIEast: o (-0.04+/-0.01) a (-0.04+/-0.02) b ( 0.00+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.03+/-0.01) b ( 0.07+/-0.01)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.16+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.05+/-0.01) b (-0.01+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.08+/-0.01)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.13+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b (-0.05+/-0.00) tBYIAll: o (-0.09+/-0.00) a (-0.11+/-0.00) b (-0.03+/-0.00)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.14+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.00+/-0.01) b (-0.03+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.06+/-0.01)

FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBXIWest: o ( 0.02+/-0.01) a ( 0.09+/-0.02) b ( 0.10+/-0.02) tBXIEast: o (-0.03+/-0.02) a (-0.03+/-0.02) b (-0.04+/-0.02) tBXIAll: o (-0.01+/-0.01) a ( 0.03+/-0.02) b ( 0.03+/-0.01)
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.12+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b (-0.06+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.01+/-0.01) b ( 0.03+/-0.01)
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.10+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b (-0.08+/-0.00) tBYIAll: o (-0.10+/-0.00) a (-0.11+/-0.00) b (-0.06+/-0.00)
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.11+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b (-0.06+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.03+/-0.01)

FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBXIWest: o ( 0.03+/-0.01) a ( 0.08+/-0.02) b ( 0.06+/-0.02) tBXIEast: o (-0.04+/-0.02) a (-0.03+/-0.02) b (-0.09+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.03+/-0.02) b (-0.01+/-0.01)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.08+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b (-0.10+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.01+/-0.01) b (-0.01+/-0.01)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBYIWest: o (-0.46+/-0.00) a ( 0.02+/-0.00) b ( 0.08+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.10+/-0.00) b (-0.11+/-0.00) tBYIAll: o (-0.11+/-0.00) a (-0.09+/-0.00) b (-0.11+/-0.00)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.08+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b (-0.10+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b (-0.00+/-0.01)

FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBXIWest: o ( 0.05+/-0.01) a ( 0.09+/-0.02) b ( 0.02+/-0.02) tBXIEast: o (-0.05+/-0.01) a (-0.03+/-0.02) b (-0.13+/-0.02) tBXIAll: o ( 0.00+/-0.01) a ( 0.03+/-0.01) b (-0.05+/-0.01)
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.03+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.05+/-0.01) b (-0.14+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b (-0.05+/-0.01)
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBYIWest: o (-0.47+/-0.00) a ( 0.02+/-0.00) b ( 0.05+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.10+/-0.00) b (-0.14+/-0.00) tBYIAll: o (-0.11+/-0.00) a (-0.09+/-0.00) b (-0.13+/-0.00)
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.04+/-0.01) tBYOEast: o (-0.08+/-0.01) a (-0.01+/-0.01) b (-0.13+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b (-0.04+/-0.01)
===========

FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.28+/-0.02) tBXIEast: o (-0.04+/-0.01) a (-0.04+/-0.02) b ( 0.12+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.02+/-0.01) b ( 0.20+/-0.01)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBXIWest: o ( 0.05+/-0.02) a ( 0.07+/-0.02) b ( 0.23+/-0.02) tBXIEast: o (-0.05+/-0.02) a (-0.05+/-0.02) b ( 0.07+/-0.02) tBXIAll: o ( 0.00+/-0.01) a ( 0.01+/-0.02) b ( 0.15+/-0.02)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.18+/-0.02) tBXIEast: o (-0.05+/-0.01) a (-0.04+/-0.02) b ( 0.05+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.02+/-0.01) b ( 0.12+/-0.01)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBXIWest: o ( 0.04+/-0.01) a ( 0.08+/-0.02) b ( 0.14+/-0.02) tBXIEast: o (-0.04+/-0.01) a (-0.04+/-0.02) b^( 0.00+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.03+/-0.01) b ( 0.07+/-0.01) <
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBXIWest: o ( 0.02+/-0.01) a ( 0.09+/-0.02) b ( 0.10+/-0.02) tBXIEast: o (-0.03+/-0.02) a (-0.03+/-0.02) b (-0.04+/-0.02) tBXIAll: o (-0.01+/-0.01) a ( 0.03+/-0.02) b ( 0.03+/-0.01) <
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBXIWest: o ( 0.03+/-0.01) a ( 0.08+/-0.02) b ( 0.06+/-0.02) tBXIEast: o (-0.04+/-0.02) a (-0.03+/-0.02) b (-0.09+/-0.02) tBXIAll: o (-0.00+/-0.01) a ( 0.03+/-0.02) b^(-0.01+/-0.01)
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBXIWest: o ( 0.05+/-0.01) a ( 0.09+/-0.02) b^( 0.02+/-0.02) tBXIEast: o (-0.05+/-0.01) a (-0.03+/-0.02) b (-0.13+/-0.02) tBXIAll: o ( 0.00+/-0.01) a ( 0.03+/-0.01) b (-0.05+/-0.01)

FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.29+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.06+/-0.01) b ( 0.11+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.20+/-0.01)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBXOWest: o (-0.01+/-0.01) a ( 0.01+/-0.01) b ( 0.25+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.06+/-0.01) b ( 0.06+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.03+/-0.01) b ( 0.16+/-0.01)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.20+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b ( 0.03+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.12+/-0.01)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.16+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.05+/-0.01) b^(-0.01+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b ( 0.08+/-0.01)
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.12+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b (-0.06+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.01+/-0.01) b ( 0.03+/-0.01)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b ( 0.08+/-0.01) tBXOEast: o (-0.01+/-0.01) a (-0.05+/-0.01) b (-0.10+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.01+/-0.01) b^(-0.01+/-0.01)<
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBXOWest: o (-0.02+/-0.01) a ( 0.02+/-0.01) b^( 0.03+/-0.01) tBXOEast: o (-0.00+/-0.01) a (-0.05+/-0.01) b (-0.14+/-0.01) tBXOAll: o (-0.01+/-0.01) a (-0.02+/-0.01) b (-0.05+/-0.01)<

FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.20+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b ( 0.05+/-0.00) tBYIAll: o (-0.09+/-0.00) a (-0.11+/-0.00) b ( 0.08+/-0.00)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBYIWest: o (-0.44+/-0.00) a ( 0.02+/-0.00) b ( 0.17+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b^( 0.02+/-0.00) tBYIAll: o (-0.08+/-0.00) a (-0.10+/-0.00) b ( 0.04+/-0.00)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBYIWest: o (-0.44+/-0.00) a ( 0.02+/-0.00) b ( 0.15+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b^(-0.02+/-0.00) tBYIAll: o (-0.10+/-0.00) a (-0.15+/-0.00) b^( 0.00+/-0.00) >2 
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.13+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b (-0.05+/-0.00) tBYIAll: o (-0.09+/-0.00) a (-0.11+/-0.00) b (-0.03+/-0.00)
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBYIWest: o (-0.45+/-0.00) a ( 0.02+/-0.00) b ( 0.10+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.11+/-0.00) b (-0.08+/-0.00) tBYIAll: o (-0.10+/-0.00) a (-0.11+/-0.00) b (-0.06+/-0.00)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBYIWest: o (-0.46+/-0.00) a ( 0.02+/-0.00) b ( 0.08+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.10+/-0.00) b (-0.11+/-0.00) tBYIAll: o (-0.11+/-0.00) a (-0.09+/-0.00) b (-0.11+/-0.00)
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBYIWest: o (-0.47+/-0.00) a ( 0.02+/-0.00) b^( 0.05+/-0.00) tBYIEast: o (-0.03+/-0.00) a (-0.10+/-0.00) b (-0.14+/-0.00) tBYIAll: o (-0.11+/-0.00) a (-0.09+/-0.00) b (-0.13+/-0.00)

FF2.Alpha0.15|RFF2.Alpha0.15    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.24+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b ( 0.07+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.16+/-0.01)
FF2.Short2.2Mom|RFF2.Short2.2Mom|tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.21+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b ( 0.03+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.13+/-0.01)
FF2.Alpha-.15|RFF2.Alpha-.15    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.17+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b^( 0.00+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.09+/-0.01)
FF2.Alpha-.30|RFF2.Alpha-.30    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.14+/-0.01) tBYOEast: o (-0.09+/-0.01) a^(-0.00+/-0.01) b (-0.03+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.06+/-0.01)>
FF2.Alpha-.45|RFF2.Alpha-.45    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.11+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b (-0.06+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b ( 0.03+/-0.01)
FF2.Alpha-.60|RFF2.Alpha-.60    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b ( 0.08+/-0.01) tBYOEast: o (-0.09+/-0.01) a (-0.01+/-0.01) b (-0.10+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b^(-0.00+/-0.01)>
FF2.Alpha-.75|RFF2.Alpha-.75    |tB tBYOWest: o (-0.10+/-0.01) a ( 0.06+/-0.01) b^( 0.04+/-0.01) tBYOEast: o (-0.08+/-0.01) a (-0.01+/-0.01) b (-0.13+/-0.01) tBYOAll: o (-0.09+/-0.01) a ( 0.03+/-0.01) b (-0.04+/-0.01)>

--------------------------------------------------------------------------------
01/07/14 
Increase window from 5 to 10 cm.

 */

