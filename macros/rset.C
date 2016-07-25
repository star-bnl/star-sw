#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include <string.h>
#include "TPaveText.h"
#endif
//________________________________________________________________________________
void rcd(const Char_t *dir) {
  TString Dir(dir);
  Dir += ".root";
  if (Dir == TString(gSystem->BaseName(gDirectory->GetName()))) {cout << gDirectory->GetName() << endl; return;}
  TList *files = gROOT->GetListOfFiles();
  if (! files) {cout << "No root files " << endl; return;}
  TIter  next(files);
  TFile *f = 0;  
  while ((f = (TFile *) next())) {
    if (Dir == TString(gSystem->BaseName(f->GetName()))) 
      {f->cd(); cout << "cd " << gDirectory->GetName() << endl; return;}
    //    cout << gSystem->BaseName(f->GetName()) << " does not match with requested " << Dir << endl;
  }
}
//________________________________________________________________________________
void chdir(const Char_t *dir) {rcd(dir);}
//________________________________________________________________________________
void rdir() {
  TString Current(gDirectory->GetName());
  TList *files = gROOT->GetListOfFiles();
  if (! files) {cout << "No root files " << endl; return;}
  TIter  next(files);
  TFile *f = 0;  
  while ((f = (TFile *) next())) {
    TString name(gSystem->BaseName(f->GetName()));
    name.ReplaceAll(".root","");
    cout << "dir \t" << name;
    if (Current == TString(f->GetName())) cout << "\t <======= ";
    cout << endl;
  }
}
//________________________________________________________________________________
void rls() {rdir();}
//________________________________________________________________________________
void rpwd() {
  cout << "dir \t" << gDirectory->GetName()  << endl;
  //  cout << "dir \t" << gSystem->BaseName(gDirectory->GetName())  << endl;
}
//________________________________________________________________________________
void set(Int_t color) {
  TIter nextkey( gDirectory->GetListOfKeys() );
  TKey *key = 0;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
      if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
	TTree *t = (TTree*)obj;
	cout << "set attributes for " << t->GetName() << endl;
	t->SetMarkerStyle(20);
	t->SetMarkerColor(color);
	t->SetLineColor(color);
	t->SetLineWidth(2);
      }
  }  
}
//________________________________________________________________________________
void rset(Int_t color) {if (color);}
