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
#include "TLegend.h"
//#include "TDirIter.h"
#include "TSystemDirectory.h"
#include "TSystemFile.h"
#include "TObjArray.h"
#include "TObjectTable.h"
#include "TFileMerger.h"
#include "TDirIter.h"
#include "TInterpreter.h"
//#include "dEdxFit.C"
#include "brtw.C"
#endif
TF1 *FitGF(TH1 *proj, Option_t *opt="");
TF1* FitGP(TH1* proj, Option_t* opt = "RQ", Double_t nSigma = 3, Int_t pow = 3, Double_t zmin = -0.2, Double_t zmax = 0.2);
enum {NTVar = 12};     // bits   1      2     3        4      5       6       7          8       9       10              11          12
const Char_t *Var[NTVar] = {"mean", "RMS", "mu", "sigma","sg10","sg100", "muGP", "sigmaGP", "Mass", "Gamma", "Significance", "PerEvent"};
class Plot_t {
public:
  Plot_t(const Char_t *var="", const Char_t *path="", Int_t P=-1) : Var(var), Path(path), p(P) {}
  const Char_t *Var;
  const Char_t *Path;
  Int_t         p;
};
vector<Plot_t> Plots;
class Hist_t {										
public:											
  Hist_t (const Char_t *name = "", const Char_t *path = "", const Int_t Opt = 0, Int_t P = -1) : Name((Char_t *) name), Path(path), opt(Opt), p(P) {}
  //  virtual ~Hist_t() {}								
        Char_t *Name; 
  const Char_t *Path;
  const Int_t   opt; // 1 - mean, 2 - RMS, 4 - mu, 8 - sigma, 16 - sg10, 32 - sg100, 64 - muGP, 128 - sigmaGP, 256 - M, 512 - Gamma, 1024 - Significance, 2048 - PerEvent
  Int_t         p;   //  
  void SetName(const Char_t *name) {Name = (Char_t *) name;}
};

static Hist_t Histos[] = {
  Hist_t("PrZ","/Particles/KFParticlesFinder/PrimaryVertexQA/z", 1+2, -1),
  Hist_t("PrX","/Particles/KFParticlesFinder/PrimaryVertexQA/x", 1+2, -1),
  Hist_t("PrY","/Particles/KFParticlesFinder/PrimaryVertexQA/y", 1+2, -1),
  Hist_t("Pr ","/Particles/KFParticlesFinder/PrimaryVertexQA/Ntracks", 1+2, -1),
  Hist_t("PrRatio","/Tracks/hPrimaryRatio", 1+2, -1),
  Hist_t("VxError","/Tracks/hPVError",1+2, -1),
  Hist_t("PvsG","/Tracks/hPVErrorVsNPVTracks",16+32, -1),
  Hist_t("dEdxPull","/Tracks/pi-/hdEdXPull",64+128, -1),
  Hist_t("dEdx_Pull","/Tracks/pi+/hdEdXPull",64+128, -1),
  Hist_t("dEdx_Pull","/Tracks/K-/hdEdXPull",64+128, -1),
  Hist_t("dEdx_Pull","/Tracks/K+/hdEdXPull",64+128, -1),
  Hist_t("dEdx_Pull","/Tracks/p/hdEdXPull",64+128, -1),
  Hist_t("dEdx_Pull","/Tracks/p-/hdEdXPull",64+128, -1),
#if 0  
  Hist_t("dNdx_Pull","/Tracks/pi-/hdNdXPull",64+128, -1),
  Hist_t("dNdx_Pull","/Tracks/pi+/hdNdXPull",64+128, -1),
  Hist_t("dNdx_Pull","/Tracks/K-/hdNdXPull",64+128, -1),
  Hist_t("dNdx_Pull","/Tracks/K+/hdNdXPull",64+128, -1),
  Hist_t("dNdx_Pull","/Tracks/p/hdNdXPull",64+128, -1),
  Hist_t("dNdx_Pull","/Tracks/p-/hdNdXPull",64+128, -1),
#endif  
  Hist_t("M2","/Tracks/pi-/hTofPID",64+128, -1),
  Hist_t("M2","/Tracks/pi+/hTofPID",64+128, -1),
  Hist_t("M2","/Tracks/K-/hTofPID",64+128, -1),
  Hist_t("M2","/Tracks/K+/hTofPID",64+128, -1),
  Hist_t("M2","/Tracks/p/hTofPID",64+128, -1),
  Hist_t("M2","/Tracks/p-/hTofPID",64+128, -1),
  Hist_t("Mass","/Particles/KFParticlesFinder/Particles/Ks/Parameters/M",256+512+1024+2048, -1)
  
}; 
const Int_t NoHists = sizeof(Histos)/sizeof(Hist_t); 
//________________________________________________________________________________
TString InitVars() {
  cout << " NoHists " << NoHists << endl;
  TString Tuple("Run:run:no:events");
  Int_t p = 4;
  for (Int_t i = 0; i < NoHists; i++) {
    TString NameB(Histos[i].Path);
    NameB.ReplaceAll("/Particles/KFParticlesFinder/PrimaryVertexQA/","");
    NameB.ReplaceAll("/Tracks/","");
    NameB.ReplaceAll("/Particles/KFParticlesFinder/Particles/","");
    NameB.ReplaceAll("Parameters/","");
    NameB.ReplaceAll("+","P");
    NameB.ReplaceAll("-","N");
    NameB.ReplaceAll("/","_");
    NameB.ReplaceAll("_M","");
    Histos[i].SetName(NameB);
    Histos[i].p = p;
    cout << i << "\t" << Histos[i].Name << "\t" << Histos[i].Path << "\topt = " << Histos[i].opt << "\tp = " << Histos[i].p << endl;
    for (Int_t j = 0; j < NTVar; j++) {
      if ((1 << j) &  Histos[i].opt) {
	Tuple += ":"; 
	TString variable = NameB + "_" + Var[j];
	p++;
	Tuple += variable; 
	Plot_t plot = Plot_t(variable, Histos[i].Path, p);
	cout << plot.Var << "\t" << plot.Path << "\t" << plot.p << endl;
	Plots.push_back(plot); 
      }
    }
  } 
  cout << "Tuple: \t" << Tuple << endl;

  return Tuple;
}
//________________________________________________________________________________
TDirectory *Merge(Int_t run) {
  TString targetFile(Form("R%i.root",run));
  const Char_t *targetname = targetFile.Data();
  Char_t *tfile = gSystem->Which(".",targetFile,kReadPermission);
  if (! tfile) {
   gSystem->Load("libTreePlayer");
   TClass::GetClass("ROOT::Cintex::Cintex"); // autoload Cintex if it exist.
   if (gInterpreter->IsLoaded("libCintex")) {
      gROOT->ProcessLine("ROOT::Cintex::Cintex::Enable();");
   }
   TDirIter Dir(Form("%i_*.root",run));
   Bool_t force = kFALSE;
   Bool_t skip_errors = kFALSE;
   Bool_t reoptimize = kFALSE;
   Bool_t noTrees = kFALSE;
   Int_t maxopenedfiles = 0;
   Int_t verbosity = 99;
   Int_t newcomp = 1;
   if (verbosity > 1) {
     std::cout << "hadd Target file: " << targetname << std::endl;
   }
   
   TFileMerger merger(kFALSE,kFALSE);
   //    merger.SetMsgPrefix("hadd");
   //    merger.SetPrintLevel(verbosity - 1);
   if (maxopenedfiles > 0) {
     merger.SetMaxOpenedFiles(maxopenedfiles);
    }
   if (!merger.OutputFile(targetname,force,newcomp) ) {
     std::cerr << "hadd error opening target file (does " << targetname << " exist?)." << std::endl;
     std::cerr << "Pass \"-f\" argument to force re-creation of output file." << std::endl;
     exit(1);
    }
   
   Char_t *file = 0;
   while ((file = (Char_t *) Dir.NextFile())) {
     if( ! merger.AddFile(file) ) {
       if ( skip_errors ) {
	 std::cerr << "hadd skipping file with error: " << file << std::endl;
       } else {
	 std::cerr << "hadd exiting due to error in " << file << std::endl;
	  return 0;
       }
     }
   }
   if (reoptimize) {
     merger.SetFastMethod(kFALSE);
   } else {
     if (merger.HasCompressionChange()) {
       // Don't warn if the user any request re-optimization.
       std::cout <<"hadd Sources and Target have different compression levels"<<std::endl;
      std::cout <<"hadd merging will be slower"<<std::endl;
     }
   }
   merger.SetNotrees(noTrees);
   Bool_t status = merger.Merge();
   
   if (status) {
     if (verbosity == 1) {
       std::cout << "hadd merged " << merger.GetMergeList()->GetEntries() << " input files in " << targetname << ".\n";
     }
   } else {
     if (verbosity == 1) {
       std::cout << "hadd failure during the merge of " << merger.GetMergeList()->GetEntries() << " input files in " << targetname << ".\n";
     }
     return 0;
   }
  }
  return new TFile(tfile);
}
//________________________________________________________________________________
void kfQA(Int_t run = 22141041, const Char_t *Out = 0){
  TDirectory *myDir = Merge(run);
  if (! myDir) return;
  TString OutFile;
  if (!Out) OutFile = "kfQAN.root";
  else      OutFile = Out;
  TFile *fout = new TFile(OutFile,"recreate");
  TString Tuple = InitVars();
  TNtuple *QA = new TNtuple("QA","Summary of PicoDst per Run",Tuple.Data()); 
  Int_t cachesize = 10000000; //this is the default value: 10 MBytes
  QA->SetCacheSize(cachesize);
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
  myDir->cd();
  no++;
  Run = run%1000000;
  Float_t params[100] = {0};
  params[0] = run;
  params[1] = Run;
  params[2] = no;
  Int_t p = 4;
  for (Int_t i = 0; i < NoHists; i++) { // loop over histograms
    TH1F *hist = (TH1F *) myDir->Get(Histos[i].Path);
    if (! hist) continue;
    TObjArray* arr = 0;
    Double_t binWidth;
    Double_t S;
    if (i == 0) {// "/Particles/KFParticlesFinder/PrimaryVertexQA/z"
      params[3] = hist->GetEntries();
      if (params[3] < 100) break;;
    }
    p = Histos[i].p;
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
	    gp = FitGP(proj,"Q",1,1,-2,2);
	    if (gp) {
#ifdef __DRAW__
	      TCanvas *c = new TCanvas(Histos[i].Path,Histos[i].Path);
	      proj->Draw();
	      c->Update();
#endif
	      params[p] = gp->GetParameter(1);
	      p++;
	      params[p] = gp->GetParameter(2);
	    }
	    delete proj;
	    j++;
	    break;
	  case 9: // M
	  case 10: // Gamma
	  case 11 : // Significance
	  case 12 : // PerEvent
	    BW = K0BW(hist);
	    if (BW) {
	      params[p]   = BW->GetParameter(1);
	      p++;
	      params[p] = BW->GetParameter(2);
	      p++;
	      params[p] = BRTW::Significance;
#if 0
	      binWidth = BRTW::binWidth; // hist->GetBinWidth(1);
	      S = BW->Integral(params[p]-3*params[p+1],params[p]+3*params[p+1])/binWidth;
#endif
	      p++;
	      params[p] = BRTW::SperE;
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
  fout->cd();
  QA->Write();
}
#if 0
//________________________________________________________________________________
void PrintVars(const Char_t *file = "K/dEdx/21174052_1.root") {
  for (Int_t i = 0; i < NoHists; i++) {
    TString var(Histos[i].Name);
    TString histName(gSystem->BaseName(Histos[i].Path));
    
    TString part(gSystem->BaseName(gSystem->DirName(Histos[i].Path)));
    TString part2(gSystem->BaseName(gSystem->DirName(gSystem->DirName(Histos[i].Path))));
    part.ReplaceAll("+","P");
    part.ReplaceAll("-","N");
    //    TFile *f = new TFile(file);
    auto *f = new TFile(file);
    if (! f) return;
    TH1 *hist = (TH1 *) f->Get(Histos[i].Path);
    if (! hist) continue;
    //    cout << "histogram name " << hist->GetName() << "\t" << hist->GetTitle() << endl;
    for (Int_t j = 0; j < NTVar; j++) {
      if ((1 << j) &  Histos[i].opt) {
	TString variable(var); variable += "_";
	if      (histName == "hdEdXPull") variable += part;
	else if (histName == "hdNdXPull") variable += part;
	else if (histName == "hTofPID") variable += part;
	else if (histName == "M") variable += part2;
	else                      variable += histName;
	variable += "_"; variable += Var[j];
	//	cout << "histogram name " << hist->GetName() << "\t" << hist->GetTitle() << "\t" << Var[j] << "\t" << variable.Data() <<  endl;
	cout << "{\"" <<  variable.Data() << "\",\t\"" << part.Data() << "\",\t\"" << Var[j] << "\",\t\"" <<  hist->GetTitle() << "\"}," << endl;
      }
    }
  } 
}
//________________________________________________________________________________
Float_t day(Float_t Run) {
  Int_t r = ((Int_t)Run)%1000000;
  Int_t D = r/1000;
  Int_t s = r%1000;
  return D + ((Float_t) (s))/60.;
}
//________________________________________________________________________________
void Plot(Int_t iplot1=0, Int_t iplot2=0, const Char_t *tfg = "kfQA.K.dEdx.W.root", const Char_t *dev = "kfQA.K.dev.W.root") {
  TCanvas *c1 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1) c1 = new TCanvas("c1","c1",10,10,600,600);
  else      c1->Clear();
  TCanvas *c2 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c2");
  if (! c2) c2 = new TCanvas("c2","c2",10,640,1800,600);
  else      c2->Clear();
  const Char_t *v[2] = {"tfg","dev"};
  Int_t d1 = 169;
  Int_t d2 = 192;
  static TFile *files[2] = {0};
  if (! files[0]) files[0] = new TFile(tfg);
  if (! files[1]) files[1] = new TFile(dev);
  TH2F *h[12] ={0};
  TH1  *h1[12] = {0};
  TString same;
  TNtuple *QA[2] = {0};
  const Char_t *dt[2] = {"tfg","dev"};
  TLegend *l = new TLegend(0.2,0.2,0.6,0.4);
  for (Int_t iplot = iplot1, p = 0; iplot <= iplot2; iplot++, p++) {
    for (Int_t iv = 0; iv < 2; iv++) {
      Int_t ivp = iv + 2*p;
      Int_t color = ivp + 1;
      if (files[iv]) {
	files[iv]->cd(); 
	QA[iv] = (TNtuple *) files[iv]->Get("QA");
	if (QA[iv]) {
	  QA[iv]->SetMarkerColor(color);
	  QA[iv]->SetLineColor(color);
	  TString name(Form("%s_%s",Plots[iplot].Var,Plots[iplot].hTitle));
	  name.ReplaceAll(" ","");
	  c1->cd();
	  TString h1n(name); h1n += "1D"; h1n += Plots[iplot].particle;  h1n += dt[iv];
	  QA[iv]->Draw(Form("%s>>%s",Plots[iplot].VarInTuple,h1n.Data()),"",same);
	  h1[ivp] = (TH1 *) files[iv]->Get(h1n);
	  h1[ivp]->SetLineColor(color);
	  h1[ivp]->SetMarkerColor(color);
	  if (h1[ivp]) {
	    TAxis *xax = h1[ivp]->GetXaxis();
	    Double_t xmin =  xax->GetXmin();
	    Double_t xmax =  xax->GetXmax();
	    TF1 *gaus = (TF1 *) gROOT->GetListOfFunctions()->FindObject("gaus");
	    if (gaus) gaus->SetLineColor(color);
	    h1[ivp]->Fit("gaus","","goff");
	    gaus = (TF1 *) h1[ivp]->GetListOfFunctions()->FindObject("gaus");
	    if (gaus) {
	      //	    gaus->SetLineColor(color);
	      //	    gaus->Draw("same");
	      Double_t mu = gaus->GetParameter(1);
	      Double_t sigma = gaus->GetParameter(2);
	      cout << files[iv]->GetName() << "\t" << name.Data() << " = " << mu << " +/- " << sigma << "\tBad Runs (> 5 sigma)" << endl;
	      QA[iv]->Scan(Form("Run:%s",Plots[iplot].VarInTuple),Form("abs(%s-%f)>5*%f",Plots[iplot].VarInTuple,mu,sigma));
	    }
	    TString h2n(name); h2n += "2D"; h2n += Plots[iplot].particle;  h2n += dt[iv];
	    h[ivp] = new TH2F(h2n,Form("%s from day",Plots[iplot].hTitle),60*(d2 - d1 +1),d1, d2+1,200,xmin,xmax);
	    h[ivp]->SetMarkerColor(color);
	    c2->cd();
	    QA[iv]->Draw(Form("%s:day(Run)>>%s",Plots[iplot].VarInTuple,h[ivp]->GetName()),"",same);
	    same = "sames";
	    l->AddEntry(h[ivp],Form("%s (%s)",Plots[iplot].hTitle,dt[iv]));
	  }
	}
      }
    }
    l->Draw();
  }
}  
#endif
//________________________________________________________________________________
/*
  QA->Draw("Mass_Ks_Gamma:run","Mass_Ks_Gamma<1&&Mass_Ks_Gamma>0.004")
  QA->Draw("1000*(Mass_Ks_M-0.497611):run","Mass_Ks_Gamma<0.009&&Mass_Ks_Gamma>0.004")
  QA->Draw("Mass_Ks_PerEvent:run","Mass_Ks_Gamma<0.009&&Mass_Ks_Gamma>0.004")
*/  
