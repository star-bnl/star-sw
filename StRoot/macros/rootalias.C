#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include <string.h>
#include "TPaveText.h"
#endif
//______________________________________________________________________________
void edit(char *file)
{
   char s[64], *e;
   if (!strcmp(gSystem->GetName(), "WinNT")) {
     if ((e = getenv("EDITOR")))
         sprintf(s, "start %s %s", e, file);
      else
         sprintf(s, "start notepad %s", file);
   } else {
     if ((e = getenv("EDITOR")))
         sprintf(s, "%s %s", e, file);
      else
         sprintf(s, "xterm -e vi %s &", file);
   }
   gSystem->Exec(s);
}
//______________________________________________________________________________
char *pwd()
{
  return (char *) gSystem->WorkingDirectory();
}

#if 1
//______________________________________________________________________________
void ls(char *path=0)
{
  TString s = (!strcmp(gSystem->GetName(), "WinNT")) ? "dir /w " : "ls ";
  s += path;
  gSystem->Exec(s.Data());
}
//______________________________________________________________________________
void dir(char *path=0)
{
  ls(path);
}

//______________________________________________________________________________
char *cd(char *path=0)
{
 if (path)
   gSystem->ChangeDirectory(path);
 return pwd();
}
#if defined(__CINT__) && defined(__MAKECINT__)
//______________________________________________________________________________
void bexec(char *macro)
{
   if (gROOT->IsBatch()) printf("Processing benchmark: %s\n",macro);
   TPaveText *summary = (TPaveText*)bench->GetPrimitive("TPave");
   TText *tmacro = summary->GetLineWith(macro);
   if (tmacro) tmacro->SetTextColor(4);
   bench->Modified(); bench->Update();

   gROOT->Macro(macro);

   TPaveText *summary2 = (TPaveText*)bench->GetPrimitive("TPave");
   TText *tmacro2 = summary2->GetLineWith(macro);
   if (tmacro2) tmacro2->SetTextColor(2);
   bench->Modified(); bench->Update();
}
#else
void bexec(char *macro=0){if (macro);}
#endif
#endif
void rcd(const Char_t *dir = "") {
  TString Dir(dir);
  Dir += ".root";
  if (Dir == TString(gSystem->BaseName(gDirectory->GetName()))) {cout << gDirectory->GetName() << endl; return;}
  TSeqCollection *files = gROOT->GetListOfFiles();
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
//void chdir(const Char_t *dir = "") {rcd(dir);}
//________________________________________________________________________________
void rdir(const Char_t *dir="") {
  TString Current(gDirectory->GetName());
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) {cout << "No root files " << endl; return;}
  TIter  next(files);
  TFile *f = 0;  
  TString Dir(dir);
  TRegexp reg(dir);
  
  while ((f = (TFile *) next())) {
    TString name(gSystem->BaseName(f->GetName()));
    if (Dir != "" && Dir != "*" && ! name.Contains(reg)) continue; 
    name.ReplaceAll(".root","");
    cout << "dir \t" << name;
    if (Current == TString(f->GetName())) cout << "\t <======= ";
    cout << endl;
  }
}
//________________________________________________________________________________
void rls(const Char_t *dir="") {rdir(dir);}
//________________________________________________________________________________
void rpwd() {
  cout << "dir \t" << gDirectory->GetName()  << endl;
  //  cout << "dir \t" << gSystem->BaseName(gDirectory->GetName())  << endl;
}
#if 0
#if 0
//________________________________________________________________________________
void set(Int_t color=1) {
  Char_t *histos[] = {"MuDst","FitP","mu","sigma","dEdxP","dEdxS","SumT","TpcResNtuple",0};
  for (int i = 0; histos[i]; i++) {
    TObject *obj = gDirectory->Get(histos[i]);
    if (! obj) continue;
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      TH1 *h1 = (TH1*)obj;
      h1->SetMarkerStyle(20);
      h1->SetMarkerColor(color);
      h1->SetLineColor(color);
      h1->SetLineWidth(2);
      h1->SetMarkerSize(0.3);
    }
    else { 
      if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
	TTree *t = (TTree*)obj;
	t->SetMarkerStyle(20);
	t->SetMarkerColor(color);
	t->SetLineColor(color);
	t->SetLineWidth(2);
	//	t->SetMarkerSize(0.3);
      }
    }
  }
}
#else
//________________________________________________________________________________
void set(Int_t color=1) {
  TIter nextkey( gDirectory->GetListOfKeys() );
  TKey *key = 0;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
#if 0
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      TH1 *h1 = (TH1*)obj;
      //      cout << "set attributes for " << h1->GetName() << endl;
      h1->SetMarkerStyle(20);
      h1->SetMarkerColor(color);
      h1->SetLineColor(color);
      h1->SetLineWidth(2);
      h1->SetMarkerSize(0.3);
    }
    else { 
#endif
      if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
	TTree *t = (TTree*)obj;
	cout << "set attributes for " << t->GetName() << endl;
	t->SetMarkerStyle(20);
	t->SetMarkerColor(color);
	t->SetLineColor(color);
	t->SetLineWidth(2);
      }
#if 0
    }
#endif
  }  
}
//________________________________________________________________________________
void rset(Int_t color=1) {set(color);}
#endif
#endif
