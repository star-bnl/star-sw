#define TpcT_cxx
#include "TpcT.h"
#include <TH1.h>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "TProfile2D.h"
#include "TF1.h"
#include "TLegend.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
//#include "StTpcMcAnalysisMaker/TpcCluster.h"
TF1  *TpcT::mShaperResponse = 0;             //!
TF1  *TpcT::mChargeFractionInner = 0;        //!
TF1  *TpcT::mPadResponseFunctionInner = 0;   //!
TF1  *TpcT::mChargeFractionOuter = 0;        //!
TF1  *TpcT::mPadResponseFunctionOuter = 0;   //!
TF1  *TpcT::mConvolution = 0;                //!
//static Double_t K3I = 1.5236; // fit data - res. 1.709;// data Full Field; 0.68;  // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 0.5
//static Double_t K3O = 1.0270; // -"-      1.044;//      -"-       ; 0.89;    // K3 from E.Mathieson, Fig. 5.3a (row) for a/s = 2.5e-3 and h/s = 0.5
static const Double_t K3IP = 0.68;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 0.5
static const Double_t K3OP = 0.55;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 1
static const Double_t tau     = 71e-9; // from pulser fit
static const Double_t pShaper = 5.15;  //     -"- 
//static const Double_t CrossTalkInner = 0.15;
//static const Double_t CrossTalkOuter = 0.025;
static const Double_t CrossTalkInner = 0.004;
static const Double_t CrossTalkOuter = 0.004;
//--------------------------------------------------------------------------------
void TpcT::rLoop() {
}
//--------------------------------------------------------------------------------
void TpcT::Loop(Int_t ev, Double_t tanCut, Int_t NpadCut, Double_t pMomin, Double_t pMomax) {
//   In a ROOT session, you can do:
//      Root > .L TpcT.C
//      Root > TpcT t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  //  fChain->SetMarkerStyle(20);
  //  fChain->SetMarkerColor(opt%10);
  static Int_t nx = 200;
  static Double_t xmin =  -10.;
  static Double_t xmax =   10.;
  static Double_t zmin = -210;
  static Double_t zmax = -zmin;
  static Int_t ny = 140;
  static Double_t ymin = -4;
  static Double_t ymax = 10;
  static Int_t nz = 42;
  TProfile2D *OuterPadRc = (TProfile2D *) gDirectory->Get("OuterPadRc");
  Int_t color = 1;
  if (! OuterPadRc) {
    OuterPadRc = new TProfile2D("OuterPadRc","OuterPadRc",nx,xmin,xmax,nz,zmin,zmax,""); 
    OuterPadRc->SetMarkerStyle(20); 
    OuterPadRc->SetMarkerColor(color++);
  }
  TProfile2D *OuterPadMc = (TProfile2D *) gDirectory->Get("OuterPadMc");
  if (! OuterPadMc) {
    OuterPadMc = new TProfile2D("OuterPadMc","OuterPadMc",nx,xmin,xmax,nz,zmin,zmax,""); 
    OuterPadMc->SetMarkerStyle(20); 
    OuterPadMc->SetMarkerColor(color++);
  }
  TProfile2D *InnerPadRc = (TProfile2D *) gDirectory->Get("InnerPadRc");
  if (! InnerPadRc) {
    InnerPadRc = new TProfile2D("InnerPadRc","InnerPadRc",nx,xmin,xmax,nz,zmin,zmax,""); 
    InnerPadRc->SetMarkerStyle(20); 
    InnerPadRc->SetMarkerColor(color++);
  }
  TProfile2D *InnerPadMc = (TProfile2D *) gDirectory->Get("InnerPadMc");
  if (! InnerPadMc) {
    InnerPadMc = new TProfile2D("InnerPadMc","InnerPadMc",nx,xmin,xmax,nz,zmin,zmax,""); 
    InnerPadMc->SetMarkerStyle(20); 
    InnerPadMc->SetMarkerColor(color++);
  }
  TProfile2D *OuterTimeRc = (TProfile2D *) gDirectory->Get("OuterTimeRc");
  if (! OuterTimeRc) {
    OuterTimeRc = new TProfile2D("OuterTimeRc","OuterTimeRc",ny,ymin,ymax,nz,zmin,zmax,""); 
    OuterTimeRc->SetMarkerStyle(20); 
    OuterTimeRc->SetMarkerColor(color++);
  }
  TProfile2D *OuterTimeMc = (TProfile2D *) gDirectory->Get("OuterTimeMc");
  if (! OuterTimeMc) {
    OuterTimeMc = new TProfile2D("OuterTimeMc","OuterTimeMc",ny,ymin,ymax,nz,zmin,zmax,""); 
    OuterTimeMc->SetMarkerStyle(20); 
    OuterTimeMc->SetMarkerColor(color++);
  }
  TProfile2D *InnerTimeRc = (TProfile2D *) gDirectory->Get("InnerTimeRc");
  if (! InnerTimeRc) {
    InnerTimeRc = new TProfile2D("InnerTimeRc","InnerTimeRc",ny,ymin,ymax,nz,zmin,zmax,""); 
    InnerTimeRc->SetMarkerStyle(20); 
    InnerTimeRc->SetMarkerColor(color++);
  }
  TProfile2D *InnerTimeMc = (TProfile2D *) gDirectory->Get("InnerTimeMc");
  if (! InnerTimeMc) {
    InnerTimeMc = new TProfile2D("InnerTimeMc","InnerTimeMc",ny,ymin,ymax,nz,zmin,zmax,""); 
    InnerTimeMc->SetMarkerStyle(20); 
    InnerTimeMc->SetMarkerColor(color++);
  }
  TProfile2D *InnerMc = (TProfile2D *) gDirectory->Get("InnerMc");
  if (! InnerMc) InnerMc = new TProfile2D("InnerMc","InnerMc",nx,xmin,xmax,ny,ymin,ymax,""); 
  TProfile2D *InnerRc = (TProfile2D *) gDirectory->Get("InnerRc");
  if (! InnerRc) InnerRc = new TProfile2D("InnerRc","InnerRc",nx,xmin,xmax,ny,ymin,ymax,""); 
  TProfile2D *OuterMc = (TProfile2D *) gDirectory->Get("OuterMc");
  if (! OuterMc) OuterMc = new TProfile2D("OuterMc","OuterMc",nx,xmin,xmax,ny,ymin,ymax,""); 
  TProfile2D *OuterRc = (TProfile2D *) gDirectory->Get("OuterRc");
  if (! OuterRc) OuterRc = new TProfile2D("OuterRc","OuterRc",nx,xmin,xmax,ny,ymin,ymax,""); 
  Int_t nentries = Int_t(fChain->GetEntriesFast());
  Int_t jentry=0;
  if (ev >= 0) { jentry = ev; nentries = jentry + 1;}
  
  Int_t nbytes = 0, nb = 0;
  for (; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if (ientry%1000 == 1 || ev >= 0) Print(ientry);
    if (! fNoPixels || fNoPixels >kMaxfPixels) continue;
    if (fRcTrack_fNfitpoints[0] < 25) continue;
    if (fNoPixels > 80) continue;
    if (fRcPad_fNPads[0] < NpadCut) continue;
    if (fRcHit_mCharge[0] < 1.e-6 || fRcHit_mCharge[0] > 100.e-6) continue;
    Double_t pmom = TMath::Sqrt(fRcTrack_fpx[0]*fRcTrack_fpx[0]+fRcTrack_fpy[0]*fRcTrack_fpy[0]);//+fRcTrack_fpz[0]*fRcTrack_fpz[0]);
    if (pMomin > 0 && pmom < pMomin || pMomax >0 && pmom > pMomax) continue;
    if (tanCut > 0 && TMath::Abs(fRcTrack_fpy[0]) > 1e-7 && TMath::Abs(fRcTrack_fpx[0]/fRcTrack_fpy[0]) > tanCut) continue;
    // if (Cut(ientry) < 0) continue;
    if (fNoRcPad != 1) continue;
    if (fAdcSum < 100 || fAdcSum > 2.e3) continue;
    // Sector 1; RDO 4;
    if (fRcTrack_fSector[0] == 1 && fRcTrack_fRow[0] >= 22 && fRcTrack_fRow[0] <= 29) continue;
    for (int i = 0; i < fNoPixels; i++) {
      if (fPixels_mTimeBin[i] > 512) {
	if (fPixels_mRow[i] > 13) {
	  OuterPadRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]), fRcHit_mPosition_mX3[0],
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	    OuterPadMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]), fRcHit_mPosition_mX3[0],
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	} else {
	  InnerPadRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]), fRcHit_mPosition_mX3[0],
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  InnerPadMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]), fRcHit_mPosition_mX3[0],
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	}
      }
      if (fPixels_mPad[i] > 200) {
	if (fPixels_mRow[i] > 13) {
	  OuterTimeRc->Fill(fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]), fRcHit_mPosition_mX3[0],
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  OuterTimeMc->Fill(fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]), fRcHit_mPosition_mX3[0],
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
 	} else {
	  InnerTimeRc->Fill(fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]), fRcHit_mPosition_mX3[0],
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  InnerTimeMc->Fill(fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]), fRcHit_mPosition_mX3[0],
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum)); 
	}
      }
      if (fPixels_mTimeBin[i] <= 512 && fPixels_mPad[i] <= 200) {
	if (fPixels_mRow[i] > 13) {
	  OuterRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]),
			fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]),
			((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	    OuterMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]),
			  fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]),
			  ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	}
	else {
	  InnerRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]),
			fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]),
			((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	    InnerMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]),
			  fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]),
			  ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	}
      }
    }
  }
}
//________________________________________________________________________________
void DrawTpcTPlots(const Char_t *histname="OuterPad",const Char_t *Option="") {
  if (Option);
  const Int_t NT = 6; 
  const Char_t *types[NT] = {"Rc","Mc","sY","Y","sD","D"};
  TFile *FitFiles[10];
  Int_t NF = 0;
  TList *files = (TList *) gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    for (int type = 0; type < NT; type++) {
      TProfile *h = (TProfile *) f->Get(Form("%s%s",histname,types[type]));
      if ( h ) {
	FitFiles[NF] = f; NF++;
	cout << "Found " << h->GetName() << " in " << f->GetName() << endl;
	break;
      }
    }
  }
  TCanvas *c1 = new TCanvas(histname,Form("Compare %s",histname));
  c1->SetGrid(); 
  TH1F *frame = c1->DrawFrame(-5,0.0,10,0.42);
  TString Histname(histname);
  TLegend *leg = new TLegend(0.65,0.6,0.9,0.9,"");
  frame->Draw();
  TString same("same");
  for (int i = 0; i<NF; i++) {
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      for (int type = 0; type < NT; type++) {
	TProfile *h = (TProfile *) FitFiles[i]->Get(Form("%s%s",histname,types[type]));
	if (! h) {
	  cout << Form("%s%s",histname,types[type]) << " has not been found." << endl;
	  continue;
	}
	if (h->GetEntries() < 1) {
	  cout << h->GetName() << " is empty." << endl;
	  continue;
	}
	cout << "Draw " << h->GetName() << endl;
	h->SetMarkerColor(2*i + 1 + type);
	h->Draw(same.Data());
	same = "same";
	TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
	Title.ReplaceAll(".root","");
	Title += " ";
	Title += types[type];
	leg->AddEntry(h,Form("%s",Title.Data()));
      }
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
void TpcT::FitSlices(const Char_t *name, const Char_t *opt, Int_t iy) {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  TString Opt(opt);
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  TString Name(h->GetName());
  Name += opt;
  const double pPI = 0.335;
  const double pPO = 0.670;
  const double rI  =  89.9;
  const double rO  = 156.2;
  const double lI  = 1.15;
  const double lO  = 1.95;
  if (! h) return;
  TF1 *gd = 0;
  if (Name.Contains("Pad",TString::kIgnoreCase)) {
    if (Name.Contains("Inner",TString::kIgnoreCase)) 
      gd = new TF1("gdI",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)))/(%f*%f)",pPI,pPI),-210,210); // to Ground wires
    if (Name.Contains("Outer",TString::kIgnoreCase)) 
      gd = new TF1("gdO",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)))/(%f*%f)",pPO,pPO),-210,210);
    if (gd) {
      gd->SetParName(0,"#sigma_{C}");
      gd->SetParName(1,"#sigma_{D}");
      gd->SetParLimits(0,0,10.);
      gd->SetParLimits(1,0,10.);
    }
  }
  if (Name.Contains("Time",TString::kIgnoreCase)) {
    Double_t timeBin = 5.78602945878541108e-01;// (cm)
    if (Name.Contains("Outer",TString::kIgnoreCase)) 
      gd = new TF1("gdOT",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)) + %f*x*x)/(%f*%f)",lO*lO/(rO*rO)/12.,timeBin,timeBin),-210,210);
    else 
      gd = new TF1("gdIT",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)) + %f*x*x)/(%f*%f)",lI*lI/(rI*rI)/12.,timeBin,timeBin),-210,210);
    if (gd) {
      gd->SetParName(0,"#sigma_{C}");
      gd->SetParName(1,"#sigma_{D}");
      gd->SetParLimits(0,0,10.);
      gd->SetParLimits(1,0,10.);
      gd->SetParName(2,"#sigma_{B}");
    }
  }
  if (! mConvolution) MakeFunctions();
  TF1 *ga = mConvolution;
  // default parameters
  if (Name.Contains("Pad",TString::kIgnoreCase)) 
    if (Name.Contains("Inner",TString::kIgnoreCase))         ga->FixParameter(0,0);
    else  {if (Name.Contains("Outer", TString::kIgnoreCase)) ga->FixParameter(0,1);}
  else                                                       ga->FixParameter(0,2); // Time
  ga->ReleaseParameter(2); ga->SetParLimits(2,0,10); ga->SetParameter(2,1e-5);
  ga->FixParameter(3,K3IP); 
  ga->FixParameter(4,K3OP);
  ga->FixParameter(5,0); // shift
  ga->FixParameter(6,0); // noise
  ga->FixParameter(7,pShaper);
  ga->FixParameter(8,tau);
  if (Name.Contains("Inner",TString::kIgnoreCase)) ga->FixParameter(9,CrossTalkInner); 
  else                                             ga->FixParameter(9,CrossTalkOuter);
  // Pads
  if (Name.Contains("Pad",TString::kIgnoreCase)) {
    // Cross Talk fit
    if (Opt.Contains("Cross",TString::kIgnoreCase)) {
      ga->FixParameter(2,0);
      ga->ReleaseParameter(9); 
      ga->SetParLimits(9,0,0.5);
    }
    if (Opt.Contains("K3",TString::kIgnoreCase)) {
      Int_t k = 3;
      if (Name.Contains("Outer",TString::kIgnoreCase)) k = 4; 
      Double_t K3 = ga->GetParameter(k);
      ga->SetParLimits(k,0.5*K3,10*K3);
      if (! Opt.Contains("K3S")) ga->FixParameter(2,0);
    }
  }
  if (Name.Contains("Time",TString::kIgnoreCase)) {
    ga->ReleaseParameter(5);    ga->SetParLimits(5,-5,5); // shift
    ga->ReleaseParameter(6);    ga->SetParLimits(6, 0,1); // noise
    if (Opt.Contains("pShape",TString::kIgnoreCase)) {
      ga->ReleaseParameter(7); // 
      ga->SetParLimits(7,2,10);
    }
    if (Opt.Contains("Tau",TString::kIgnoreCase)) {
      ga->SetParLimits(8,20e-9,100e-9);
    }
  }
  Int_t Npar = ga->GetNpar();
  TH1D **out = new TH1D*[Npar+1];
  memset(out, 0, (Npar+1)*sizeof(TH1D*));
  Double_t pmin, pmax;
  TString pName("");
  TString pTitle("");
  for (Int_t i = 0; i <= Npar; i++) {
    pmin = -1.;
    pmax =  1.;
    if (i < Npar) {
      ga->GetParLimits(i,pmin,pmax);
      pName = Form("%s_p%i",Name.Data(),i);
      pTitle = Form("Fit result for %s",ga->GetParName(i));
    }
    else {
      pName = Form("%s_Chisq",Name.Data());
      pTitle = Form("Chisq for %s",ga->GetParName(i));
    }
    if (pmin < pmax) { 
      out[i] = (TH1D *) gDirectory->Get(pName.Data());
      if (! out[i]) 
	out[i] = new TH1D(pName.Data(),pTitle.Data(),
			  ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
    }
  }
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  if (iy) {iy1 = iy2 = iy;}
  for (int i = iy1; i <= iy2; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i%s",Name.Data(),i,opt),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) continue;
    proj->Reset();
    Double_t xmin =  9999.;
    Double_t xmax = -9999.;
    for (int j = 1; j <= nx; j++) {
      Int_t bin    = h->GetBin(j,i);
      Double_t ent = h->GetBinEntries(bin); 
      Double_t val = h->GetBinContent(bin);
      Double_t err = h->GetBinError(bin);
      Double_t xce = h->GetXaxis()->GetBinCenter(j);
      //      cout << "j" << "\t" << xce << "\t" << ent << "\t" << val << "\t+/-\t" << err << endl;
      if (ent > 2) {
	//      if (ent > 10 && val > 3*err && err > 0) {
	proj->SetBinContent(j,val);
	proj->SetBinError(j,err);
	if (val > 0.01) {
	  if (xmin > xce) xmin = xce;
	  if (xmax < xce) xmax = xce;
	}
      }
    }
    proj->Fit(ga);
    //    Double_t prob = ga->GetProb();
    Double_t chisq = ga->GetChisquare(); 
    out[Npar]->SetBinContent(i,chisq);
    if (chisq < 1.e5) {
      for (Int_t j = 0; j < Npar; j++) {
	if (out[j]) {
	  out[j]->SetBinContent(i,ga->GetParameter(j));
	  out[j]->SetBinError(i,ga->GetParError(j));
	}
      } 
    }
  }
  if (! iy && out[2]) {
    out[2]->SetMarkerStyle(20);
    if (Name.Contains("Outer",TString::kIgnoreCase)) out[2]->SetMarkerColor(2);
    out[2]->Fit(gd);
  }
}
//________________________________________________________________________________
TH1D *TpcT::RMSSlices(const Char_t *name, const Char_t *opt) {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  TString Name(h->GetName());
  Name += opt;
  TH1D *out = new TH1D(Name.Data(),"RMS**2 result",ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  for (int i = iy1; i <= iy2; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i",Name.Data(),i),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) continue;
    proj->Reset();
    Double_t xmin =  9999.;
    Double_t xmax = -9999.;
    for (int j = 1; j <= nx; j++) {
      Int_t bin    = h->GetBin(j,i);
      Double_t ent = h->GetBinEntries(bin); 
      Double_t val = h->GetBinContent(bin);
      Double_t err = h->GetBinError(bin);
      Double_t xce = h->GetXaxis()->GetBinCenter(j);
      //      cout << "j" << "\t" << xce << "\t" << ent << "\t" << val << "\t+/-\t" << err << endl;
      if (ent > 2) {
	//      if (ent > 10 && val > 3*err && err > 0) {
	proj->SetBinContent(j,val);
	proj->SetBinError(j,err);
	if (val > 0.01) {
	  if (xmin > xce) xmin = xce;
	  if (xmax < xce) xmax = xce;
	}
      }
    }
    Double_t rms = proj->GetRMS()*proj->GetRMS();
    out->SetBinContent(i,rms);
  } 
  
  out->SetMarkerStyle(20);
   return out;
}
//________________________________________________________________________________
Double_t TpcT::ShaperFunc(Double_t *x, Double_t *par) {
  Double_t tau = par[0];
  Double_t width = par[1];
  Double_t p = par[2];
  Double_t t = x[0]*width/tau;
  Double_t Delta = width/tau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = TMath::Gamma(p,t2) - TMath::Gamma(p,t1);
  //  return (val > 0) ? TMath::Log(val) : -999.;
  return val;
}
//________________________________________________________________________________
Double_t TpcT::PadResponseFunc(Double_t *x, Double_t *par) {
  Double_t CrossTalk = par[4];
  Double_t Value = 0;
  if (CrossTalk > 0) {
    for (int i = -1; i <= 1; i++) {
      Double_t xx = x[0] + i;
      if (i == 0) Value += (1. - 2.*CrossTalk)*Gatti(&xx,par);
      else        Value +=          CrossTalk *Gatti(&xx,par);
    }
  } else   Value = Gatti(x,par);
  return Value;
}
//________________________________________________________________________________
Double_t TpcT::Gatti(Double_t *x, Double_t *par) {
  /************************************************************************
   *  Function    : generates the cathode signal using                    *
   *                the single-parameter Gatti formula:                   *
   *                              1 - tanh(K2 * lambda)**2                *
   *     GFunc(lambda) = K1 * -------------------------------             *
   *                           1 + K3 * tanh (K2 *lambda)**2              *
   *     lambda = x/h, h is anode cathode spacing                         *
   *                                                                      *
   *     K2 = pi/2*(1 - 0.5*sqrt(K3))                                     *
   *                                                                      *
   *              K2*sqrt(K3)                                             *
   *     K1 = -------------------                                         *
   *            4 * atan(sqrt(K3))                                        *
   *                                                                      *
   *  References  : E.Gatti, A.Longoni, NIM 163 (1979) 82-93.             *
   *  Authors : V.Balagura,V.Cherniatin,A.Chikanian                       *
   ************************************************************************/
  Double_t w = par[0]; // w = width of pad       
  Double_t y = w*TMath::Abs(x[0]);   // distance to center of strip
  Double_t h = par[1]; // h = Anode-Cathode gap  
  Double_t lambda = TMath::Abs(y/h);
  Double_t K3 = par[3]; 
  Double_t K2 = TMath::PiOver2()*(1. - 0.5*TMath::Sqrt(K3));  
  //  Double_t K1 = K2*TMath::Sqrt(K3)/(2*TMath::ATan(TMath::Sqrt(K3)));
  Double_t sqK3 = TMath::Sqrt(K3);
  Double_t ATsqK3 = 0.5/TMath::ATan(sqK3);
  Double_t Y1 = lambda - w/h/2;
  Double_t Y2 = Y1 + w/h;
  Double_t X1 = K2*Y1;
  Double_t X2 = K2*Y2;
  Double_t Z1 = sqK3*TMath::TanH(X1);
  Double_t Z2 = sqK3*TMath::TanH(X2);
  Double_t val = ATsqK3*(TMath::ATan(Z2) - TMath::ATan(Z1));
  //  return val > 0 ? TMath::Log(val) : -999.;
  return val;
}
//________________________________________________________________________________
void TpcT::MakeFunctions() {
  Double_t timeBinMin = -0.5;
  Double_t timeBinMax = 10.5;
  if (! mShaperResponse) 
    mShaperResponse = new TF1("ShaperFunc",TpcT::ShaperFunc,timeBinMin,timeBinMax,3);  
  Double_t mTimeBinWidth              = 1.06580379191673078e-07;//1.e-6/gStTpcDb->Electronics()->samplingFrequency();
  Double_t mTau                       = tau; //1.e-9*gStTpcDb->Electronics()->tau();// s 
  mShaperResponse->SetParameters(mTau,mTimeBinWidth,pShaper);
  Double_t params[10];
  params[0] = 2.85e-01;//gStTpcDb->PadPlaneGeometry()->innerSectorPadWidth();                     // w = width of pad       
  params[1] = 0.2;//gStTpcDb->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
  params[2] = 0.4;//gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
    //    params[3] = anodeWireRadius;                                                         // a = Anode wire radius  
  params[4] = 0;
  params[5] = 0;
  Double_t xmin = 0; 
  Double_t xmax = 5.0;//4.5*gStTpcDb->PadPlaneGeometry()->innerSectorPadWidth();// 4.5 
  if (! mPadResponseFunctionInner)
    mPadResponseFunctionInner = new TF1("PadResponseFunctionInner",
				      TpcT::PadResponseFunc,xmin,xmax,5); 
  params[3] =  K3IP;  
  params[4] =  0; // CrossTalk
  mPadResponseFunctionInner->SetParameters(params);
  //      mPadResponseFunctionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5.0;//5*gStTpcDb->PadPlaneGeometry()->innerSectorPadLength(); // 1.42
  if (! mChargeFractionInner) 
    mChargeFractionInner = new TF1("ChargeFractionInner",
				 TpcT::PadResponseFunc,xmin,xmax,5);
  params[0] = 1.15;//gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
  params[3] = K3OP;
  params[4] =  0; // CrossTalk
  mChargeFractionInner->SetParameters(params);
  //  mChargeFractionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5.*gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth(); // 3.
  if (! mPadResponseFunctionOuter) 
    mPadResponseFunctionOuter = new TF1("PadResponseFunctionOuter",
				      TpcT::PadResponseFunc,xmin,xmax,5); 
  params[0] = 6.2e-01;//gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth();                    // w = width of pad       
  params[1] = 0.4;//gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
  params[2] = 0.4;//gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
  //      params[3] = gStTpcDb->WirePlaneGeometry()->anodeWireRadius();                        // a = Anode wire radius  
  params[3] = K3OP;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 1
  params[4] =  0; // CrossTalk
  params[5] = 0;
  mPadResponseFunctionOuter->SetParameters(params);
  //    mPadResponseFunctionOuter->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5*gStTpcDb->PadPlaneGeometry()->outerSectorPadLength(); // 1.26
  if (! mChargeFractionOuter) 
    mChargeFractionOuter = new TF1("ChargeFractionOuter",
				 TpcT::PadResponseFunc,xmin,xmax,5);
  params[0] = 1.95; //gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
  params[3] =  0.61;    // K3 from E.Mathieson, Fig. 5.3a (row) for a/s = 2.5e-3 and h/s = 1.0
  params[4] =  0; // CrossTalk
  mChargeFractionOuter->SetParameters(params);
  //  mChargeFractionOuter->Save(xmin,xmax,0,0,0,0);
  if (! mConvolution) 
    mConvolution = new TF1("Convolution",TpcT::ConvolutionF,-8,8,10);
  mConvolution->SetParName(0,"icase"); mConvolution->FixParameter(0,0);
  mConvolution->SetParName(1,"Area");  mConvolution->FixParameter(1,1); mConvolution->SetParLimits(1,0,1e5);
  mConvolution->SetParName(2,"#sigma^{2}");  mConvolution->SetParLimits(2,0,10);
  mConvolution->SetParName(3,"K3I");   mConvolution->FixParameter(3,K3IP);
  mConvolution->SetParName(4,"K3O");   mConvolution->FixParameter(4,K3OP);
  mConvolution->SetParName(5,"Shift"); mConvolution->FixParameter(5,0);
  mConvolution->SetParName(6,"Noise"); mConvolution->FixParameter(6,0);
  mConvolution->SetParName(7,"p"); mConvolution->FixParameter(7,pShaper);
  mConvolution->SetParName(8,"#tau"); mConvolution->FixParameter(8,tau);
  mConvolution->SetParName(9,"CrossTalk"); mConvolution->FixParameter(9,0);
}
//________________________________________________________________________________
Double_t TpcT::ConvolutionF(Double_t *x, Double_t *par) {
  Int_t icase = (Int_t) par[0]; // 0 - Inner, 1 - Outer, 2 - Time Shape
  TF1 *pfunc = mPadResponseFunctionInner;
  pfunc->SetParameter(3,par[3]);
  pfunc->SetParameter(4,par[9]);
  Double_t xshft = par[5];
  Double_t noise = par[6];
  if (icase == 1) {
    pfunc = mPadResponseFunctionOuter;
    pfunc->SetParameter(3,par[4]);
    pfunc->SetParameter(4,par[9]);
  }
  else if (icase == 2) {
    pfunc = mShaperResponse;
    pfunc->SetParameter(3,par[7]); // p
    pfunc->SetParameter(0,par[8]); // tau
  }
  if (! pfunc) return 0;
  Double_t Area   = par[1];
  Double_t sigma2 = TMath::Abs(par[2]);
  Double_t sigma  = TMath::Sqrt(sigma2);
  Double_t sg = 5;
  static const Int_t N = 100;
  Double_t dx = 2*sg/N;
  Double_t xc = x[0] - xshft;
  Double_t value = 0;
  if (sigma > 0.0) {
    for (Int_t i = 0; i <= N; i++) {
      Double_t xx = xc + sigma*(-sg + dx*i);
      Double_t xa = xx;
      if (icase < 2) xa = TMath::Abs(xx);
      value += pfunc->Eval(xa)*TMath::Gaus(xx, xc, sigma, 1);
    }
    value *= dx*Area*sigma;
  }
  else value = Area*pfunc->Eval(xc);
  return value + noise;
}
//________________________________________________________________________________
void TpcT::Print(Int_t i) {
  cout << i << "\t =======================================================" << endl;
  cout << "fNoPixels \t" << fNoPixels << "\tNPads " << fRcPad_fNPads[0] << endl;
  cout << "Charge(keV) \t" << 1e6*fRcHit_mCharge[0];
  if (TMath::Abs(fRcTrack_fpy[0]) > 1e-7) 
    cout << "\tpx/py\t" << fRcTrack_fpx[0]/fRcTrack_fpy[0] 
	 << "\tpT\t" << TMath::Sqrt(fRcTrack_fpx[0]*fRcTrack_fpx[0]+fRcTrack_fpy[0]*fRcTrack_fpy[0]);
  cout << endl;
  cout << "fNoRcPad " << fNoRcPad << "\tfAdcSum " << fAdcSum << endl;
  cout << "Rc:Pad\t" << fRcPad_fPad[0]        << "\tRc:dX\t" << fRcPad_fdX[0]
       << "\tRc:Time\t" << fRcPad_fTimeBin[0] << "\tRc:dZ\t" << fRcPad_fdZ[0] << endl;
  cout << "Mc:Pad\t" << fMcPad_fPad[0]        << "\tMc:dX\t" << fMcPad_fdX[0]
       << "\tMc:Time\t" << fMcPad_fTimeBin[0] << "\tMc:dZ\t" << fMcPad_fdZ[0] << endl;
  Int_t kPadMin = 999;
  Int_t kPadMax =   0;
  Int_t kTbMin  = 999;
  Int_t kTbMax  =   0;
  Int_t pad  = 0;
  Int_t tb   = 0;
  Int_t indx = 0;
  for (int i = 0; i < fNoPixels; i++) {
    pad = fPixels_mPad[i];
    tb  = fPixels_mTimeBin[i];
    if (tb  >= 512) continue;
    if (pad > 182) continue;
    if (pad < kPadMin) kPadMin = pad;
    if (pad > kPadMax) kPadMax = pad;
    if (tb  < kTbMin) kTbMin = tb;
    if (tb  > kTbMax) kTbMax = tb;
  }
  Int_t *adcs = new Int_t[(kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)];
  cout << "kPadMin/kPadMax\t" << kPadMin << "/" << kPadMax
       << "\tkTbMin/kTbMax\t" << kTbMin << "/" << kTbMax << endl;
  memset (adcs, 0, (kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)*sizeof(Int_t));
  for  (int i = 0; i < fNoPixels; i++) {
    pad = fPixels_mPad[i];
    tb  = fPixels_mTimeBin[i];
    if (tb >= 512) tb = kTbMax + 1;
    if (pad > 182) pad = kPadMax + 1;
    indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
    adcs[indx] = fPixels_mAdc[i];
  }
  cout << " ";
  for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|" << tb;
  cout << endl;
  cout << "_";
  for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|____";
  cout << endl;
  for (pad = kPadMin; pad <= kPadMax + 1; pad++) {
    if (pad == kPadMax + 1) cout << "---------------------------------------------" << endl;
    cout << pad << "|";
    for (tb = kTbMin; tb <= kTbMax + 1; tb++) {
      indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
      if (tb <= kTbMax) cout << "\t" << adcs[indx];
      else              cout << "\t|" << adcs[indx];
    }
    cout << endl;
  }
  delete [] adcs;
#if 0
  for (int i = 0; i < fNoPixels; i++) 
    cout << i 
	 << "\tRow\t" << (int) fPixels_mRow[i]
	 << "\tPad\t" << (int) fPixels_mPad[i]
	 << "\tTimeBin\t" << (int) fPixels_mTimeBin[i]
	 << "\tadc\t" << (int) fPixels_mAdc[i]
	 << "\tId\t" << (int) fPixels_mIdTruth[i]
	 << "\tR\t" << ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum)
	 << endl;
  for (int i = 0; i < fNoRcPad; i++) 
    cout << "Rc\t" << i << "\tRow\t" << fRcPad_fRow[i]
	 << "\tPad\t" << fRcPad_fPad[i]
	 << "\tTimeBin\t" << fRcPad_fTimeBin[i]
	 << "\tdX\t" << fRcPad_fdX[i]
	 << "\tdZ\t" << fRcPad_fdZ[i] 
	 << endl;
  for (int i = 0; i < fNoMcPad; i++) 
    cout << "Mc\t" << i << "\tRow\t" << fMcPad_fRow[i]
	 << "\tPad\t" << fMcPad_fPad[i]
	 << "\tTimeBin\t" << fMcPad_fTimeBin[i]
	 << "\tdX\t" << fMcPad_fdX[i]
	 << "\tdZ\t" << fMcPad_fdZ[i] 
	 << endl;
#endif
}
//#include "StTpcMcAnalysisMaker/TpcCluster.cxx"
//________________________________________________________________________________
TGraphErrors *OmegaTau() {
  double xx[]={0,0.5,1,0.,0.5,1};
  double yy[]={647,331,111,517,311,143};
  double dxx[6];
  double dyy[6];
  for (int i = 0; i < 6; i++) {dxx[i] = 0.1; dyy[i] = 0.1*yy[i];}
  TGraphErrors *gr2 = new TGraphErrors(6,xx,yy,dxx,dyy);
  gr2->SetMarkerStyle(20);
  return gr2;
}
//________________________________________________________________________________
void TpcTAdc() {
  TTree *TpcT = (TTree *) gDirectory->Get("TpcT");
  if (! TpcT) return;
  //  TF1* off = new TF1("off","exp(log(1.+[0]/exp(x)))",3,10);
  TpcT->SetMarkerStyle(20);
  TpcT->SetMarkerColor(1);
  TpcT->Draw("fMcHit.mlgam/fAdcSum:log(fAdcSum)>>I(70,3.,10.)",
	     "fNoMcHit==1&&fNoRcHit==1&&fAdcSum/fMcHit.mlgam>0.5&& fAdcSum/fMcHit.mlgam<1.4&&fMcPad.fRow<14",
	     "prof");
  TProfile *I = (TProfile *) gDirectory->Get("I");
  if (! I) return;
  I->Fit("off","r","",3,10);
  TpcT->SetMarkerColor(2);
  TpcT->Draw("fMcHit.mlgam/fAdcSum:log(fAdcSum)>>O(70,3.,10.)",
	     "fNoMcHit==1&&fNoRcHit==1&&fAdcSum/fMcHit.mlgam>0.5&& fAdcSum/fMcHit.mlgam<1.4&&fMcPad.fRow>=14",
	     "prof");
  TProfile *O = (TProfile *) gDirectory->Get("O");
  if (! O) return;
  O->Fit("off","r","",3,10);
}
