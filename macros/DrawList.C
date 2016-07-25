/* 
   Draw Pads:
   root.exe AuAu200_9_20evts_Bichsel_1_20.Pads.BT.root
   .L DrawList.C+
   DrawPadsAll();
   DrawList("AvLaser_23_??_.*_px","",0,0)
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
//#include <ostream>
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THStack.h"
#include "TPaveStats.h"
#include "TStyle.h"
#include "TF1.h"
#include "TKey.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TPRegexp.h"
#include "TList.h"
#include "TLegend.h"
#include "TList.h"
#include "TIterator.h"
#include "TQtZoomPadWidget.h"
#include "TQtCanvas2Html.h"
#include "TPolynomial.h"
#endif
void DrawList(const Char_t *pattern = "^pad_X_RC*", const Char_t *ctitle = "", 
	      Int_t nx = 7, Int_t ny = 2, Int_t color = 0, Int_t iSlices=2, Double_t ymin=-0.4, Double_t ymax = 0.6, Int_t NparMax = 9, Bool_t zoom=kTRUE) {
#ifdef __CINT__
  cout << "Please run this script in compiled mode by running \".x DrawList.C+\"" << endl;
   return;
#endif
  TString patt(pattern);
  TPRegexp reg(pattern);
  TString cTitle = patt;
  cTitle += ctitle;
  cTitle.ReplaceAll("^","");
  cTitle.ReplaceAll(".*","");
  cTitle.ReplaceAll("*","");
  TList *listOfKeys = gDirectory->GetListOfKeys();
  TList *listOfObjects = gDirectory->GetList();
  if (! listOfObjects && ! listOfKeys) return;
  TObjArray array;
  TIter nextobj(listOfObjects); 
  TObject *obj = 0;
#if 1
  while ((obj = nextobj())) {
    TString Name(obj->GetName());
    if (patt == "*" || Name.Contains(reg)) {
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
#endif
  TKey *key = 0;
  TIter nextkey(listOfKeys); 
  while ((key = (TKey*) nextkey())) {
    TString Name(key->GetName());
    if (patt == "*" || Name.Contains(reg)) {
      if (array.FindObject(key->GetName())) continue;
      obj = key->ReadObj();
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
  Int_t NF = array.GetEntriesFast();
  if (NF < 1) return;
  if (! nx || ! ny) {
    ny = (Int_t) TMath::Sqrt(NF);
    nx = NF/ny;
    if (nx*ny != NF) nx++;
  }
  cout << "no. of histograms " << NF << " nx x ny " << nx << " x " << ny << endl;
  TCanvas *c = new TCanvas(cTitle,cTitle);
  c->Divide(nx,ny);
  Double_t params[20]; memset (params, 0, sizeof(params));
  TF1 *fmu = 0;
  TF1 *fsigma = 0;
  for (Int_t i = 0; i < NF; i++) {
    TH1 *hist = (TH1*) array.At(i);
    if (! hist) continue;
    c->cd(i+1)->SetLogz(1);
    if (color) hist->SetMarkerColor(color);
    if (hist->IsA()->InheritsFrom( "TH2" ) ) {
      if (ymax > ymin) {
	hist->GetYaxis()->SetRangeUser(ymin,ymax);
      }
      if (iSlices) {
	TString Name(hist->GetName());
	const Char_t *IO[2] = {"Innerp","Outerp"};
	Int_t iy = 0;
	Int_t ix = 0;
	for (Int_t io = 0; io < 2; io++) {
	  if (Name.Contains(IO[io])) {
	    Int_t index = Name.Index(IO[io]);
	    if (index >= 0) {
	      TString D(Name.Data()+index+6);
	      ix = D.Atoi();
	      iy = io + 1;
	      break;
	    }
	  }
	}
	hist->SetStats(110011);
	((TH2 *)hist)->FitSlicesY();
	TH1 *mu = (TH1 *) gDirectory->Get(Form("%s_1",hist->GetName()));
	TH1 *sigma = (TH1 *) gDirectory->Get(Form("%s_2",hist->GetName()));
	Int_t R = 8; // asymmetric
	Int_t n = -1;
	if (mu) {
	  ofstream out;
	  TString Out("Tcheb.out");
	  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
	  else                              out.open(Out, ios::app);
	  memset (params, 0, sizeof(params));
	  
	  for (n = 1; n <= NparMax; n++) {
	    fmu = TPolynomial::MakePoly(Form("Tcheb_%i_%i",ix,iy),n,R);
	    cout << "Fit " << mu->GetName() << " with " << fmu->GetName() << endl;
	    params[0] = fmu->GetParameter(0); 
	    fmu->SetParameters(params);
	    mu->Fit(fmu);
	    fmu->GetParameters(params);
	    cout << "prob\t" << fmu->GetProb() << endl;
	    if (fmu->GetProb() > 0.01) break;
	  }
	  if (fmu) {
	    memset (params, 0, sizeof(params));
	    fmu->GetParameters(params);
	    out << "{\"" << mu->GetName() << "\",\"Tcheb\"";
	    out << Form(",%1i,%1i,%2i,%1i",ix,iy,n,R);
	    for (Int_t k = 0; k <= NparMax; k++) {
	      if (! k) out << Form(",%5i",(Int_t) params[k]);
	      else     out << Form(",%12.5e",params[k]);
	    }
	    Double_t prob = fmu->GetProb();
	    if (prob < 1.e-7) prob  = 0;
	    out << "," << Form("%12.5e",prob);
	    out << "}," << endl;
	    out.close();
	  }
	}
	if (iSlices > 1 && sigma) {
	  ofstream out;
	  TString Out("Tcheb.out");
	  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
	  else                              out.open(Out, ios::app);
	  memset (params, 0, sizeof(params));
	  R = 7; // symmetric
	  for (n = 1; n < NparMax; n++) {
	    fsigma = TPolynomial::MakePoly(Form("Tcheb_%i_%i",ix,iy),n,R);
	    cout << "Fit " << sigma->GetName() << " with " << fsigma->GetName() << endl;
	    params[0] = fsigma->GetParameter(0);
	    fsigma->SetParameters(params);
	    sigma->Fit(fsigma);
	    fsigma->GetParameters(params);
	    cout << "prob\t" << fsigma->GetProb() << endl;
	    if (fsigma->GetProb() > 0.01) break;
	  }
	  if (fsigma) {
	    memset (params, 0, sizeof(params));
	    fsigma->GetParameters(params);
	    out << "{\"" << sigma->GetName() << "\",\"Tcheb\"";
	    out << Form(",%1i,%1i,%2i,%1i",ix,iy,n,R);
	    for (Int_t k = 0; k <= NparMax; k++) {
	      if (! k) out << Form(",%5i",(Int_t) params[k]);
	      else     out << Form(",%12.5e",params[k]);
	    }
	    Double_t prob = fsigma->GetProb();
	    if (prob < 1.e-7) prob  = 0;
	    out << "," << Form("%12.5e",prob);
	    out << "}," << endl;
	    out.close();
	  }
	}
	hist->Draw("colz");
	if (mu) {mu->SetMarkerStyle(20);  mu->SetMarkerSize(0.4); mu->Draw("same");}
	if (sigma) {sigma->SetMarkerStyle(21); sigma->SetMarkerSize(0.4); sigma->Draw("same");}
	c->Update();
	continue;
      } else hist->Draw("colz");
    }
    else                                     hist->Draw("");
    TH1 *fit = (TH1 *) gDirectory->Get(Form("%s_1",hist->GetName()));
    if (fit) {
      fit->Draw("same");
      TList *list = fit->GetListOfFunctions();
      if (list) {
	TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
	TIter iter(list);
	TF1 *func = 0;
	while ((func = (TF1*) iter())) {
	  if (patt == "^SL") {
	    leg->AddEntry(func, Form("SL = (%7.2f +/- %7.2f) x 10^-3",func->GetParameter(0), func->GetParError(0)));
	  } else {
	    leg->AddEntry(func, Form("DV = (%10.5f +/- %10.5f) cm/mksec",func->GetParameter(0), func->GetParError(0)));
	  }
	}
	leg->Draw();
      }
    }
  }
  c->Update();
  if (zoom) {
    TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();  // Create the Pad zoomer widget
    //  Double_t zoom = 1.;
    //  zoomer->SetZoomFactor(zoom);
    TQtCanvas2Html  TQtCanvas2Html(c,  900, 600, "./", zoomer);
    //  TQtCanvas2Html  TQtCanvas2Html(c, zoom, "./", zoomer);
  }
}
//________________________________________________________________________________
void DrawFits(const Char_t *opt="Rc") {
  const Char_t *InOut[2] = {"Inner","Outer"};
  const Char_t *PadTime[2] =           {"Pad"       ,"Time"};
  const Char_t *PadTimeFitOptions[2] = {"NoiseConv","FWHMNoise"};
  //  Char_t *X[2]                 = {"","X"};
  for (Int_t ix = 0; ix < 1; ix++) {
    for (Int_t ip = 0; ip < 2; ip++) {
      for (Int_t io = 0; io < 2; io++) {
	//	TString Name(Form("%s%s%s%s%s",InOut[io],X[ix],PadTime[ip],opt,PadTimeFitOptions[ip]));
	TString Name(Form("%s%s%s%s",InOut[io],PadTime[ip],opt,PadTimeFitOptions[ip]));
	DrawList(Name);
      }
    }
  }
}
//________________________________________________________________________________
void DrawHftG(const Char_t *pattern = "d.*13", const Char_t *ctitle = "", Int_t nx = 0, Int_t ny = 0) {
  DrawList(pattern,"",9,3,0,2,0,0,1,kFALSE);
}
//________________________________________________________________________________
void DrawFList(const Char_t *pattern = "OuterPadRcNoiseConv*", const Char_t *ctitle = "", Int_t nx = 0, Int_t ny = 0) {
  TString patt(pattern);
  TPRegexp reg(pattern);
  TString cTitle = patt;
  cTitle += ctitle;
  cTitle.ReplaceAll(".*","");
  cTitle.ReplaceAll("^","");
  cTitle.ReplaceAll("$","");
  cTitle.ReplaceAll("*","");
  Int_t NFiles = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();  cout << "No. input files " << nn << endl;
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter nextF(files);
  TFile *f = 0;
  TObjArray array;
  TObject *obj = 0;
  TKey *key = 0;
  while ( (f = (TFile *) nextF()) ) { 
    f->cd();
    TString F(f->GetName()); cout << "File " << F << endl;
    TList *listOfKeys = f->GetListOfKeys();
    TList *listOfObjects = f->GetList();
    Int_t nh = 0;
    if (! listOfObjects && ! listOfKeys) continue;
    TIter nextobj(listOfObjects); 
    while ((obj = nextobj())) {
      TString Name(obj->GetName());
      if (Name.Contains(reg)) {
	if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	  TH1 *hist = (TH1 *) obj;
	  if (hist->GetEntries() <= 0) continue;
	  nh++;
	  if (! array.FindObject(obj->GetName())) array.Add(obj);
	}
      }
    }
    TIter nextkey(listOfKeys); 
    while ((key = (TKey*) nextkey())) {
      TString Name(key->GetName());
      if (Name.Contains(reg)) {
	obj = key->ReadObj();
	if (! obj) continue;
	if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	  TH1 *hist = (TH1 *) obj;
	  if (hist->GetEntries() > 0) {
	    nh++;
	    if (array.FindObject(obj->GetName())) continue;
	    array.Add(obj);
	  }
	}
      }
    }
    if (nh) {
      FitFiles[NFiles] = f; 
      cout << "Add " << NFiles << "\t" << FitFiles[NFiles]->GetName() << "\t" << nh << endl;
      NFiles++; 
    }
  }
  //________________________________________________________________________________
  Int_t NF = array.GetEntriesFast();
  if (NF < 1) return;
  if (! nx || ! ny) {
    ny = (Int_t) TMath::Sqrt(NF);
    nx = NF/ny;
    if (nx*ny != NF) nx++;
  }
  cout << "no. of histograms " << NF << " nx x ny " << nx << " x " << ny << endl;
  TCanvas *c = new TCanvas(cTitle,cTitle);
  c->Divide(nx,ny);
  THStack *hstack = 0; // new THStack("hs",histName);
  Double_t ymax = 0.98;
  Double_t ymin = 0.10 + 0.05*NFiles;
  Double_t dy   = (ymax - ymin)/(NFiles + 1);
  if (dy > 0.25) dy = 0.25;
  for (Int_t i = 0; i < NF; i++) {
    TH1 *hist = (TH1*) array.At(i);
    c->cd(i+1)->SetLogz(1);
    TLegend *leg = new TLegend(0.66,0.10,0.98,ymin,"");
    Double_t yref = -1;
    for (Int_t l = 0; l < NFiles; l++) {
      f = FitFiles[l];
      TH1 *h = (TH1 *) f->Get(hist->GetName());
      if (! h) continue;
      h->SetMarkerStyle(20);
      h->SetMarkerColor(l+1);
      h->SetLineColor(l+1);
      TList *fl = h->GetListOfFunctions();
      if (fl) {
	TF1 *fun = 0;
	TIter next(fl);
	while ((fun = (TF1 *) next())) {
	  TString Name(fun->GetName());
	  if (Name != "stats") {
	    fun->SetLineColor(l+1);
	  }
	}
      }
      //      h->SetNormFactor(1.);
      TString nameH(hist->GetName());
      if (! nameH.Contains("_p")) {
	Double_t yMax = h->GetMaximum();
	if (yref < 0) yref = yMax;
	if (yMax > 0) h->Scale(yref/yMax);
      }
      if (nameH.BeginsWith("InnerTime") || 
	  nameH.BeginsWith("OuterTime")) {
	if (h->GetMaximum() > 2) h->SetMaximum(2);
      }
      h->Draw();
      c->Update();
      leg->AddEntry(h, gSystem->DirName(f->GetName()));
      TPaveStats *st = (TPaveStats*) h->FindObject("stats");
      if (st) {
	//	st->Print();
	st->SetX1NDC(0.72);
	st->SetX2NDC(0.98);
	st->SetY1NDC(ymax - dy*(l+1));
	st->SetY2NDC(ymax - dy*(l  ));
      }
      if (l == 0) {
	hstack = new THStack(h->GetName(),h->GetTitle());
      }
      hstack->Add(h);
    }
    //    hstack->Draw("nostack,e1p");
    //    TAxis *xax = hstak->GetXaxis();
    hstack->Draw("nostack");
    leg->Draw();
  }
  c->Update();
  TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();  // Create the Pad zoomer widget
  //  Double_t zoom = 1.;
  //  zoomer->SetZoomFactor(zoom);
  TQtCanvas2Html  TQtCanvas2Html(c,  900, 600, "./", zoomer);
  //  TQtCanvas2Html  TQtCanvas2Html(c, zoom, "./", zoomer);
}
//________________________________________________________________________________
void DrawF2List(const Char_t *pattern = "OuterPadRcNoiseConv*", const Char_t *ctitle = "", Int_t nx = 0, Int_t ny = 0) {
  TString patt(pattern);
  TPRegexp reg(pattern);
  TString cTitle = patt;
  cTitle += ctitle;
  cTitle.ReplaceAll(".*","");
  cTitle.ReplaceAll("^","");
  cTitle.ReplaceAll("$","");
  cTitle.ReplaceAll("*","");
  Int_t NFiles = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();  cout << "No. input files " << nn << endl;
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  TObjArray array;
  TObject *obj = 0;
  TKey *key = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName()); cout << "File " << F << endl;
    TList *listOfKeys = f->GetListOfKeys();
    TList *listOfObjects = f->GetList();
    Int_t nh = 0;
    if (! listOfObjects && ! listOfKeys) continue;
    TIter nextobj(listOfObjects); 
    while ((obj = nextobj())) {
      TString Name(obj->GetName());
      if (Name.Contains(reg)) {
	if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	  TH1 *hist = (TH1 *) obj;
	  if (hist->GetEntries() <= 0) continue;
	  nh++;
	  if (! array.FindObject(obj->GetName())) array.Add(obj);
	}
      }
    }
    TIter nextkey(listOfKeys); 
    while ((key = (TKey*) nextkey())) {
      TString Name(key->GetName());
      if (Name.Contains(reg)) {
	obj = key->ReadObj();
	if (! obj) continue;
	if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	  TH1 *hist = (TH1 *) obj;
	  if (hist->GetEntries() > 0) {
	    nh++;
	    if (array.FindObject(obj->GetName())) continue;
	    array.Add(obj);
	  }
	}
      }
    }
    if (nh) {
      FitFiles[NFiles] = f; 
      cout << "Add " << NFiles << "\t" << FitFiles[NFiles]->GetName() << "\t" << nh << endl;
      NFiles++; 
    }
  }
  //________________________________________________________________________________
  Int_t NF = array.GetEntriesFast();
  if (NF < 1) return;
  if (! nx || ! ny) {
    ny = (Int_t) TMath::Sqrt(NF);
    nx = NF/ny;
    if (nx*ny != NF) nx++;
  }
  cout << "no. of histograms " << NF << " nx x ny " << nx << " x " << ny << endl;
  TCanvas *c = new TCanvas(cTitle,cTitle);
  c->Divide(nx,ny);
  for (Int_t i = 0; i < NF; i++) {
    TH2 *hist = (TH2*) array.At(i);
    c->cd(i+1);
    hist->Draw("colz");
  }
  c->Update();
#if 0
  TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();  // Create the Pad zoomer widget
  //  Double_t zoom = 1.;
  //  zoomer->SetZoomFactor(zoom);
  TQtCanvas2Html  TQtCanvas2Html(c,  900, 600, "./", zoomer);
  //  TQtCanvas2Html  TQtCanvas2Html(c, zoom, "./", zoomer);
#endif
}
//________________________________________________________________________________
void DrawFAll(const Char_t *opt="Rc", const Char_t *select="") {
  const Char_t *InOut[2] =             {"Inner","Outer"};
  const Char_t *PadTime[2] =           {"Time"         ,"Pad"};
  //  const Char_t *PadTimeFitOptions[2] = {"ConvFWHM","ConvNoise"};
  //  const Char_t *PadTimeFitOptions[2] = {"FWHMNoise","NoiseConv"};
  //  const Char_t *PadTimeFitOptions[2] = {"FWHMNoise","SigmaSQ"};
  const Char_t *PadTimeFitOptions[2] = {"SigmaSqSpread","SigmaSQ"};
  const Char_t *X[2]                 = {"","X"};
  for (Int_t ix = 0; ix < 2; ix++) {
    for (Int_t ip = 0; ip < 2; ip++) {
      for (Int_t io = 0; io < 2; io++) {
	TString Name(Form("^%s%s%s%s%s%s",InOut[io],X[ix],PadTime[ip],opt,PadTimeFitOptions[ip],select));
	Name += ".*$";
	cout << "Name = " << Name.Data() << endl;
	DrawFList(Name);
      }
    }
  }
}
//________________________________________________________________________________
void DrawPads(const Char_t *pattern="^RC") {
  DrawList(pattern,"",7,2,0,1,-0.3,0.4);
}
//________________________________________________________________________________
void DrawPadsAll() {
  DrawPads("^pad_X_MC");
  DrawPads("^pad_X_RC");
  DrawPads("^pad_phiL_RC");
  DrawPads("^pad_eta_RC");
  DrawPads("^pad_zL_RC");
  DrawPads("^pad_AdcL_RC");
  DrawPads("^tmbk_X_MC");
  DrawPads("^tmbk_X_RC");
  DrawPads("^tmbk_phiL_RC");
  DrawPads("^tmbk_eta_RC");
  DrawPads("^tmbk_zL_RC");
  DrawPads("^tmbk_AdcL_RC");
}
//________________________________________________________________________________
void DrawLaser(const Char_t *pattern = "^SL.*", const Char_t *ctitle = "", 
	      Int_t nx = 0, Int_t ny = 0, Int_t color = 0, Int_t iSlices=0, Double_t ymin=0, Double_t ymax = 0, Int_t NparMax = 7) {
#ifdef __CINT__
  cout << "Please run this script in compiled mode by running \".x DrawList.C+\"" << endl;
   return;
#endif
  TString patt(pattern);
  TPRegexp reg(pattern);
  TString cTitle = patt;
  cTitle += ctitle;
  cTitle.ReplaceAll("^","");
  cTitle.ReplaceAll(".*","");
  cTitle.ReplaceAll("*","");
  TList *listOfKeys = gDirectory->GetListOfKeys();
  TList *listOfObjects = gDirectory->GetList();
  if (! listOfObjects && ! listOfKeys) return;
  TObjArray array;
  TIter nextobj(listOfObjects); 
  TObject *obj = 0;
#if 1
  while ((obj = nextobj())) {
    TString Name(obj->GetName());
    if (Name.Contains("_")) continue;
    if (patt == "*" || Name.Contains(reg)) {
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
#endif
  TKey *key = 0;
  TIter nextkey(listOfKeys); 
  while ((key = (TKey*) nextkey())) {
    TString Name(key->GetName());
    if (Name.Contains("_")) continue;
    if (patt == "*" || Name.Contains(reg)) {
      if (array.FindObject(key->GetName())) continue;
      obj = key->ReadObj();
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > 0) 
	  array.Add(obj);
      }
    }
  }
  Int_t NF = array.GetEntriesFast();
  if (NF < 1) return;
  if (! nx || ! ny) {
    ny = (Int_t) TMath::Sqrt(NF);
    nx = NF/ny;
    if (nx*ny != NF) nx++;
  }
  cout << "no. of histograms " << NF << " nx x ny " << nx << " x " << ny << endl;
  TCanvas *c = new TCanvas(cTitle,cTitle);
  c->Divide(nx,ny);
  Double_t params[20]; memset (params, 0, sizeof(params));
  TF1 *fmu = 0;
  TF1 *fsigma = 0;
  for (Int_t i = 0; i < NF; i++) {
    TH1 *hist = (TH1*) array.At(i);
    c->cd(i+1)->SetLogz(1);
    if (color) hist->SetMarkerColor(color);
    if (hist->IsA()->InheritsFrom( "TH2" ) ) {
      if (ymax > ymin) {
	hist->GetYaxis()->SetRangeUser(ymin,ymax);
      }
      if (iSlices) {
	hist->SetStats(110011);
	((TH2 *)hist)->FitSlicesY();
	TH1 *mu = (TH1 *) gDirectory->Get(Form("%s_1",hist->GetName()));
	TH1 *sigma = (TH1 *) gDirectory->Get(Form("%s_2",hist->GetName()));
	if (mu) {
	  ofstream out;
	  TString Out("Tcheb.out");
	  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
	  else                              out.open(Out, ios::app);
	  memset (params, 0, sizeof(params));
	  for (Int_t n = 1; n <= NparMax; n++) {
#if 0
	    fmu = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Form("Tcheb_8_%i",n));
	    if (! fmu) fmu = TPolynomial::MakePoly("Tcheb",n,8);
#else
	    fmu = TPolynomial::MakePoly("Tcheb",n,8);
#endif
	    cout << "Fit " << mu->GetName() << " with " << fmu->GetName() << endl;
	    params[0] = fmu->GetParameter(0); 
	    fmu->SetParameters(params);
	    mu->Fit(fmu);
	    fmu->GetParameters(params);
	    cout << "prob\t" << fmu->GetProb() << endl;
	    if (fmu->GetProb() > 0.01) break;
	  }
	  if (fmu) {
	    memset (params, 0, sizeof(params));
	    fmu->GetParameters(params);
	    out << "{\"" << mu->GetName() << "\",\"" << fmu->GetName() << "\"";
	    for (Int_t k = 0; k <= NparMax; k++) {
	      if (k == 0) out << "," << Form("%10i",TMath::Nint(params[k]));
	      else        out << "," << Form("%10.5f",params[k]);
	    }
	    out << "," << Form("%10.5f",fmu->GetProb());
	    out << "}," << endl;
	    out.close();
	  }
	}
	if (iSlices > 1 && sigma) {
	  ofstream out;
	  TString Out("Tcheb.out");
	  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
	  else                              out.open(Out, ios::app);
	  memset (params, 0, sizeof(params));
	  for (Int_t n = 1; n < NparMax; n++) {
#if 0
	    fsigma = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Form("Tcheb_7_%i",n));
	    if (! fsigma) fsigma = TPolynomial::MakePoly("Tcheb",n,7);
#else
	    fsigma = TPolynomial::MakePoly("Tcheb",n,7);
#endif
	    cout << "Fit " << sigma->GetName() << " with " << fsigma->GetName() << endl;
	    params[0] = fsigma->GetParameter(0);
	    fsigma->SetParameters(params);
	    sigma->Fit(fsigma);
	    fsigma->GetParameters(params);
	    cout << "prob\t" << fsigma->GetProb() << endl;
	    if (fsigma->GetProb() > 0.01) break;
	  }
	  if (fsigma) {
	    memset (params, 0, sizeof(params));
	    fsigma->GetParameters(params);
	    out << "{\"" << sigma->GetName() << "\",\"" << fsigma->GetName() << "\"";
	    for (Int_t k = 0; k <= NparMax; k++) {
	      if (k == 0) out << "," << Form("%10i",TMath::Nint(params[k]));
	      else        out << "," << Form("%10.5f",params[k]);
	    }
	    out << "," << Form("%10.5f",fsigma->GetProb());
	    out << "}," << endl;
	    out.close();
	  }
	}
	hist->Draw("colz");
	if (mu) {mu->SetMarkerStyle(20);  mu->SetMarkerSize(0.4); mu->Draw("same");}
	if (sigma) {sigma->SetMarkerStyle(21); sigma->SetMarkerSize(0.4); sigma->Draw("same");}
	c->Update();
	continue;
      } else hist->Draw("colz");
    }
    else                                     hist->Draw("");
    TH1 *fit = (TH1 *) gDirectory->Get(Form("%s_1",hist->GetName()));
    if (fit) {
      fit->Draw("same");
      TList *list = fit->GetListOfFunctions();
      if (list) {
	TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
	TIter iter(list);
	TF1 *func = 0;
	while ((func = (TF1*) iter())) {
	  if (patt == "^SL") {
	    leg->AddEntry(func, Form("SL = (%7.2f +/- %7.2f) x 10^-3",func->GetParameter(0), func->GetParError(0)));
	  } else {
	    leg->AddEntry(func, Form("DV = (%10.5f +/- %10.5f) cm/#mus",func->GetParameter(0), func->GetParError(0)));
	  }
	}
	leg->Draw();
      }
    }
  }
  c->Update();
  TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();  // Create the Pad zoomer widget
  //  Double_t zoom = 1.;
  //  zoomer->SetZoomFactor(zoom);
  TQtCanvas2Html  TQtCanvas2Html(c,  900, 600, "./", zoomer);
  //  TQtCanvas2Html  TQtCanvas2Html(c, zoom, "./", zoomer);
}


