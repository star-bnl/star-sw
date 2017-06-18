#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include <string.h>
#include "TPaveText.h"
#endif
//#if ! ROOT_VERSION_CODE >= 393216 /* ROOT_VERSION(6,0,0) */
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
//#endif
