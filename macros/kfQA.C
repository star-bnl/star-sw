/* 
   FPE_OFF
   root.exe -q -b lBichsel.C dEdxFit.C+  'kfQA.C+("J/dev","kfQAdev.P.root")'
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
//#define __DRAW__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <map>
#include <vector>
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TPRegexp.h"
//#include "TDirIter.h"
#include "TSystemDirectory.h"
#include "TSystemFile.h"
#include "TObjArray.h"
#include "TObjectTable.h"
#include "brtw.C"
#endif
TF1 *FitGF(TH1 *proj, Option_t *opt="");
TF1* FitGP(TH1* proj, Option_t* opt = "RQ", Double_t nSigma = 3, Int_t pow = 3, Double_t zmin = -0.2, Double_t zmax = 0.2);
enum {NTVar = 12};
const Char_t *Var[NTVar] = {"mean", "RMS", "mu", "sigma","sg10","sg100", "muGP", "sigmaGP", "M", "Gamma", "Significance", "PerEvent"};
class Hist_t {
public:
  Hist_t (const Char_t *name = "", const Char_t *path = "", const Int_t Opt = 0) : Name(name), Path(path), opt(Opt) {}
  //  virtual ~Hist_t() {}
  const Char_t *Name; 
  const Char_t *Path;
  const Int_t opt; // 1 - mean, 2 - RMS, 4 - mu, 8 - sigma, 16 - sg10, 32 - sg100, 64 - muGP, 128 - sigmaGP, 256 - M, 512 - Gamma, 1024 - Significance
};

static const Hist_t Histos[] = {
  Hist_t("Pr","/Particles/KFParticlesFinder/PrimaryVertexQA/x", 1+2),
  Hist_t("Pr","/Particles/KFParticlesFinder/PrimaryVertexQA/y", 1+2),
  Hist_t("Pr","/Particles/KFParticlesFinder/PrimaryVertexQA/z", 1+2),
  Hist_t("Pr","/Particles/KFParticlesFinder/PrimaryVertexQA/Ntracks", 1+2),
  Hist_t("Tk","/Tracks/hPrimaryRatio", 1+2),
  Hist_t("Tk","/Tracks/hPVError",1+2),
  Hist_t("Tk","/Tracks/hPVErrorVsNPVTracks",16+32),
  Hist_t("dEdx_Pull","/Tracks/pi-/hdEdXPull",64+128),
  Hist_t("dEdx_Pull","/Tracks/pi+/hdEdXPull",64+128),
  Hist_t("dEdx_Pull","/Tracks/K-/hdEdXPull",64+128),
  Hist_t("dEdx_Pull","/Tracks/K+/hdEdXPull",64+128),
  Hist_t("dEdx_Pull","/Tracks/p/hdEdXPull",64+128),
  Hist_t("dEdx_Pull","/Tracks/p-/hdEdXPull",64+128),

  Hist_t("dNdx_Pull","/Tracks/pi-/hdNdXPull",64+128),
  Hist_t("dNdx_Pull","/Tracks/pi+/hdNdXPull",64+128),
  Hist_t("dNdx_Pull","/Tracks/K-/hdNdXPull",64+128),
  Hist_t("dNdx_Pull","/Tracks/K+/hdNdXPull",64+128),
  Hist_t("dNdx_Pull","/Tracks/p/hdNdXPull",64+128),
  Hist_t("dNdx_Pull","/Tracks/p-/hdNdXPull",64+128),

  Hist_t("M2","/Tracks/pi-/hTofPID",64+128),
  Hist_t("M2","/Tracks/pi+/hTofPID",64+128),
  Hist_t("M2","/Tracks/K-/hTofPID",64+128),
  Hist_t("M2","/Tracks/K+/hTofPID",64+128),
  Hist_t("M2","/Tracks/p/hTofPID",64+128),
  Hist_t("M2","/Tracks/p-/hTofPID",64+128),
  Hist_t("Mass","/Particles/KFParticlesFinder/Particles/Ks/Parameters/M",256+512+1024+2048)

}; 
//________________________________________________________________________________
void kfQA(const Char_t *pattern = "21185_", const Char_t *Out = 0){
  const Int_t N = sizeof(Histos)/sizeof(Hist_t); cout << " N " << N << endl;
  TPRegexp Pattern(pattern);
#if 0
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    //    m_Bichsel = Bichsel::Instance();
  }
  gROOT->LoadMacro("dEdxFit.C+");
#endif
  TString OutFile;
  if (!Out) OutFile = "kfQAN.root";
  else      OutFile = Out;
  TFile *fout = new TFile(OutFile,"recreate");
  TString Tuple("Run:run:no:events");
  for (Int_t i = 0; i < N; i++) {
    TString var(Histos[i].Name);
    TString histName(gSystem->BaseName(Histos[i].Path));
    TString part(gSystem->BaseName(gSystem->DirName(Histos[i].Path)));
    TString part2(gSystem->BaseName(gSystem->DirName(gSystem->DirName(Histos[i].Path))));
    part.ReplaceAll("+","P");
    part.ReplaceAll("-","N");
    for (Int_t j = 0; j < NTVar; j++) {
      if ((1 << j) &  Histos[i].opt) {
	Tuple += ":"; 
	TString variable(var); variable += "_";
	if      (histName == "hdEdXPull") variable += part;
	else if (histName == "hdNdXPull") variable += part;
	else if (histName == "hTofPID") variable += part;
	else if (histName == "M") variable += part2;
	else                      variable += histName;
	Tuple += variable; 
	Tuple += "_"; Tuple += Var[j];
      }
    }
  } 
  cout << "Tuple: \t" << Tuple << endl;
  TNtuple *QA = new TNtuple("QA","Summary of PicoDst per Run",Tuple.Data()); 
  Int_t cachesize = 10000000; //this is the default value: 10 MBytes
  QA->SetCacheSize(cachesize);
  Int_t run = 0;
  Int_t Run = 0;
  Int_t nFile = 0;
  TString title;
  TF1 *gaus = 0;
  TF1 *pol4 = 0;
  TF1 *gp = 0;
  TF1 *BW  = 0;
  TH1 *proj = 0;
  TH2 *h2 = 0;
  Int_t no = 0;
  Int_t noRuns = 0;
  //  TDirIter Dir(files);
  //  const Char_t *file = Dir.NextFile();
  TSystemDirectory Dir("",".");
  TList *files = Dir.GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TSystemFile *file = 0;
  multimap<Int_t,TString> fileMap;
  while ((file = (TSystemFile *) next())) {
    TString File(file->GetName());
    if (! File.EndsWith(".root")) continue;
    if (  File.BeginsWith("All")) continue;
    if (! File.BeginsWith(pattern)) continue;
    //    cout << file->GetName() << "\t" << file->GetTitle() << endl;
    sscanf(File.Data(),"%i_",&run); 
    TString path( file->GetTitle());
    path += "/";
    path += file->GetName();
    fileMap.insert(pair<Int_t,TString>(run,path));
    nFile++;
  }
#if 0
  for (std::multimap<Int_t,TString>::iterator it=fileMap.begin(); it!=fileMap.end(); ++it) {
    std::cout << (*it).first << " => " << (*it).second.Data() << '\n';
  }
#endif
  // Unique keys
  TH1F *hist = 0;
  for( auto it = fileMap.begin(), end = fileMap.end();
       it != end;
       it = fileMap.upper_bound(it->first)) {
    run = it->first;
    cout << run << ' ' << it->second.Data() << endl;
    std::pair <std::multimap<Int_t,TString>::iterator, std::multimap<Int_t,TString>::iterator> ret;
    ret = fileMap.equal_range(run);
    std::cout << run << " =>";
    vector<TFile *> TFiles;
    for (std::multimap<Int_t,TString>::iterator itt=ret.first; itt!=ret.second; ++itt) {
      std::cout << ' ' << itt->second.Data();
      cout << "\tOpen " <<  itt->second.Data() << endl;
      TFile *f = new TFile(itt->second.Data());
      if (f) TFiles.push_back(f);
      else {cout << "\t====================== failed " << endl;}
    }
    //    std::cout << '\n';
    if (! TFiles.size()) continue;
    no++;
    Run = run%1000000;
    Float_t params[100] = {0};
    params[0] = run;
    params[1] = Run;
    params[2] = no;
    Int_t p = 4;
    for (Int_t i = 0; i < N; i++) { // loop over histograms
      if (hist) delete hist;
      hist = 0;
      for (auto f : TFiles) {
	if (! hist) {
	  hist = (TH1F *) f->Get(Histos[i].Path);
	  hist->SetDirectory(0);
	} else {
	  TH1F *h = (TH1F *) f->Get(Histos[i].Path);
	  if (h) hist->Add(h);
	  delete h;
	}
      } 
      TObjArray* arr = 0;
      Double_t binWidth;
      Double_t S;
      if (! hist) continue;
      if (i == 2) {// "/Particles/KFParticlesFinder/PrimaryVertexQA/z"
	params[3] = hist->GetEntries();
	if (params[3] < 100) continue;
      }
      for (Int_t j = 0; j < NTVar; j++) { // loop over variables
	if ((1 << j) &  Histos[i].opt) {
	  params[p] = 0;
	  params[p+1] = 0;
	  if (hist && hist->GetEntries() > 0) {
	    switch (j+1) {
	    case 1: // mean
	      params[p] = hist->GetMean(); 
	      break;
	    case 2: // RMS
	      params[p] = hist->GetRMS();
	      break;
	    case 3: // mu
	    case 4: // sigma
	      hist->Fit("gaus");
	      gaus = (TF1 *) hist->GetListOfFunctions()->FindObject("gaus");
	      if (gaus) {
		params[p] = gaus->GetParameter(1);
		params[++p] = gaus->GetParameter(2);
#ifdef __DRAW__
		TCanvas *c = new TCanvas(Histos[i].Path,Histos[i].Path);
		hist->Draw();
		c->Update();
#endif
	      } else {
		++p;
	      }
	      j++; 
	      break;
	    case 5:  // sg10
	    case 6:  // sg100
	      if (hist->GetDimension() == 2) {
		h2 = (TH2 *) hist;
		arr =  new TObjArray(4);
		arr->SetOwner(kTRUE);
		h2->FitSlicesY(0, 0, -1, 0, "QNR", arr);
		TH1* mu = (TH1 *) (*arr)[1];
		mu->Fit("pol4");
		pol4 = (TF1 *) mu->GetListOfFunctions()->FindObject("pol4");
		if (pol4) {
		  params[p] = pol4->Eval(1);
		  params[++p] = pol4->Eval(2);
		} else {
		  ++p;
		}
		delete arr;
		j++;
	      }
	      break;
	    case 7: // muGF
	    case 8: // sigmaGP
	      h2 = (TH2 *) hist;
	      proj = h2->ProjectionY();
	      gp = FitGP(proj);
	      if (gp) {
#ifdef __DRAW__
		TCanvas *c = new TCanvas(Histos[i].Path,Histos[i].Path);
		proj->Draw();
		c->Update();
#endif
		params[p] = gp->GetParameter(1);
		params[++p] = gp->GetParameter(2);
	      }
	      delete proj;
	      j++;
	      break;
	    case 9: // M
	    case 10: // Gamma
	      BW = K0BW(hist);
	      if (BW) {
		params[p] = BW->GetParameter(1);
		params[p+1] = BW->GetParameter(2);
		params[p+2] = 1./BW->GetParError(0);
		binWidth = hist->GetBinWidth(1);
		S = BW->Integral(params[p]-3*params[p+1],params[p]+3*params[p+1])/binWidth;
		params[p+3] = S/params[3];
		p += 3;
		j += 3;
#ifdef __DRAW__
		TCanvas *c = new TCanvas(Histos[i].Path,Histos[i].Path);
		hist->Draw();
		c->Update();
#endif
	      }
	      break;
	    default: 
	      break;
	    }
	    p++;
	  }
	}
      }
    }
    QA->Fill(params);
  CLOSEFILES:
    for (auto f : TFiles) {
      delete f;
    }
    //    gObjectTable->Print();
  }
  fout->cd();
  QA->Write();
  //  fout->Close();
  //  delete fout;
}
