// $ID:$
// Compare with Kolmogorov test set of QA Histogram into *.hist.root files
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "StTree.h"
#endif
TCanvas *c1 = 0;
TFile *F12[2] = {0,0};
//________________________________________________________________________________
void Kolmogorov(const Char_t *file1 = "", const Char_t *file2 = "", 
		const Char_t *histName = "", 
		Bool_t allProc = kTRUE,
		Bool_t makePNG = kTRUE,
		Double_t probCut = 1.e-2) {
  TString File1(file1);
  TString File2(file2);
  if (File1 == "" && File2 == "") {
    cout << "Usage: " << endl;
    cout << "root.exe \'Kolmogorov.C+("
	 << "\"/star/rcf/test/dev/trs_sl302/Tue/year_2005/cucu200_minbias/rcf1216_05_200evts.hist.root\","
	 << "\"/star/rcf/test/dev/trs_sl302/Wed/year_2005/cucu200_minbias/rcf1216_05_200evts.hist.root\")\'" << endl;
    return;
  }
  if ( gClassTable->GetID("StIOEvent") < 0) { 
    gSystem->Load("St_base");
    gSystem->Load("StUtilities");
  }
  TString HistName(histName);
  cout << "Run Kolmogorov Test for files:" << File1 << " and " << File2;
  if (HistName != "") cout << " for Histogram: " << HistName;
  else                cout << " for all histograms in file";
  if (allProc)        cout << " Process them non stop";
  else                cout << " Start Dialog after test fails";
  cout << endl;
  if (makePNG)        cout << " Create png files with result of failed comparision" << endl;
  cout << "Probability cut is " << probCut << endl;
  Int_t NF = 0;
  if (File1 != "")  F12[0] = TFile::Open(file1);
  if (File2 != "")  F12[1] = TFile::Open(file2);
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter nextF(files);
  TFile *f = 0;
  TString FileN[2];
  while ((f = (TFile *) nextF())) {
    if (NF == 2) {cout << "more than " << NF << " files. Skip the rest." << endl; break;}
    F12[NF] = f;
    FileN[NF] = f->GetName();
    FileN[NF].ReplaceAll(".hist.root","");
    if (NF == 1) {FileN[NF].ReplaceAll(FileN[0].Data(),""); FileN[NF].ReplaceAll("/star/rcf/test/","");}
    TListIter nextkey( f->GetListOfKeys() );
    TKey *key = 0;
    while ((key = (TKey*) nextkey())) {
      TString Name(key->GetName());
      Int_t nh  = 0;
      if (Name.BeginsWith("histBranch")) {
	StIOEvent *io = (StIOEvent *) f->Get(key->GetName());
	TList *makerList = (TList *) io->fObj;
	if (makerList) {
	  TListIter nextMaker(makerList);
	  TObjectSet *o = 0;
	  while ((o = (TObjectSet *) nextMaker())) {
	    TList *histList = (TList *) o->GetObject();
	    if (histList) {
	      TH1 *h = 0;
	      TListIter nextH(histList);
	      while ((h = (TH1 *) nextH())) {
		if (h->InheritsFrom("TH1")) {
		  h->SetDirectory(f);
		  nh++;
		}
	      }
	    }
	  }
	}
	cout << "Source file " << NF++ << ": " << f->GetName() << " with " << nh << " Histograms" <<  endl;
      }
    }
  }
  if (NF != 2) {cout << "Problem to get 2 files" << endl; return;}
  TListIter next(F12[0]->GetList());
  TObject *obj = 0;
  TH1 *h, *h1, *h2;
  if (! gROOT->IsBatch()) c1 = new TCanvas("c1","c1",800,500);
  Int_t nhists = 0;
  while ((obj = next()) && nhists <10000 ) {
    nhists++; cout << obj->GetName() << " hist " << nhists << endl;
    Double_t prob = 0;
    Double_t probN= 0;
    Int_t ifMult = 0;
    Bool_t makepng = makePNG;
    cout << "obj " << obj->GetName() << "/" << obj->GetTitle() << endl;
    if ( obj->IsA()->InheritsFrom( "TH1" )) {
      if (obj->IsA()->InheritsFrom( "StMultiH1F" )) ifMult = 1;
      if (ifMult) {
	h1 = new TH2F(* ((TH2F *) obj));
      } else 
	h1 = (TH1 *) obj;
      Stat_t e1 = h1->GetEntries();
      if (HistName != "" && HistName != TString(h1->GetName())) continue;
      cout << "Found histogram " << h1->GetName() << " in " << F12[0]->GetName() << " with " << e1 << " Entries" << endl;
      h = (TH1 *) F12[1]->Get(h1->GetName());
      if (! h) {cout << "Can't find histogram " << h1->GetName() << " in " << F12[1]->GetName() << endl; continue;}
      if (ifMult) {
	h2 = new TH2F(* ((TH2F *)h));
      } else 
	h2 = (TH1 *) h;
      Stat_t e2 = h2->GetEntries();
      cout << "Found histogram " << h2->GetName() << " in " << F12[1]->GetName() << " with " << e2 << " Entries" << endl;
      if (e1 <= 0.0 || e2 <= 0.0) {
	if (e1 + e2 > 0.0) 
	  cout << "==== Incomparibale Histograms" << h1->GetName() << " e1: " << e1 << "\te2: " << e2 << endl;
	continue;
      }
      if ( h1->Integral() <= 0.0) { cout << "\tis empty" << endl; continue;}
      cout << "Found histogram " << h2->GetName() << " in " << F12[1]->GetName() << endl;
      
      h1->SetXTitle(FileN[0]);
      h2->SetXTitle(FileN[1]);
      prob = h1->KolmogorovTest(h2,"UOD");
      probN = h1->KolmogorovTest(h2,"UOND");
      TString pngName(h1->GetName());
      Int_t Fail = prob < probCut || probN < probCut;
      if (! Fail) makepng = kFALSE;
      if (prob < probCut) pngName += "FailS";
      if (probN < probCut) pngName += "FailN";
      h2->SetLineColor(2);
      h2->SetMarkerColor(2);
      //      TLegend *leg = new TLegend(0.7,0.6,1.1,0.8,"");
      TLegend leg(0.7,0.6,1.1,0.8,"");
      TString Title("_file0 (new)");//F12[0]->GetName());
      leg.AddEntry(h1,Title.Data());
      Title = Form("prob = %f",prob);
      leg.AddEntry(h1,Title.Data());
      Title = "_file1 (ref)"; //F12[1]->GetName();
      leg.AddEntry(h2,Title.Data());
      Title = Form("probN = %f",probN);
      leg.AddEntry(h2,Title.Data());
      if (! gROOT->IsBatch()) {
	c1->Clear();
#if 0
	TString cName("");
	cName += h1->GetName();
	c1->SetTitle(cName.Data());
	c1->SetName(cName.Data());
#endif
	c1->Divide(2,1);
	
	if (  h1->InheritsFrom( "TH2" ) ) {// && !obj->IsA()->InheritsFrom( "StMultiH1F" )) {
	  c1->cd(1);
	  h1->Draw();
	  leg.Draw();
	  c1->cd(2);
	  h2->Draw();
	} else if (h1->InheritsFrom( "TH1" )) {
	  c1->cd(2);
	  h2->Draw();
	  c1->cd(1);
	  h1->Draw();
	  //	  h2->SetNormFactor(e1);
	  h2->Draw("same");
	  leg.Draw();
	} 
	c1->Update();
	pngName += ".png";
	pngName.ReplaceAll(" ","");
	if (makepng)
	  c1->SaveAs(pngName);
	//	  TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,pngName.Data());
      }
      if (prob < probCut || probN < probCut) {
	cout << "========  KolmogorovTest fails for " <<  h1->GetName() << " with  prob " 
	     << prob << " ===================" << endl;	
	if (! allProc) {
	  Int_t i;
	  cout << "Type in a number (<0 break, >=0 go to the next histogram <";
	  cin >> i; 
	  if (i < 0) break;
	}
      }
    }
    if (ifMult) {delete h1; delete h2;}
  }
}
