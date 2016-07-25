/* 
   rts_example -DSC /star/data03/daq/2013/113/14113089/st_physics_14113089_raw_1020001.daq
*/

#ifndef __CINT__
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "TPRegexp.h"
#endif
struct Vars_t {
  Int_t utime, date, time;
  Int_t 
  BBCE, BBCW, BBCEandW, YellowBackground, BlueBackground, 
    ZDCE, ZDCW, ZDCEandW, 
    VPDE, VPDW, VPDEandW, 
    ZDCWnokiller, ZDCEnokiller, BBCEandWnokiller, ZDCEandWnokiller, MTD;
};
const Char_t *vars = "utime/I:date/I:time/I:BBCE/I:BBCW/I:BBCEandW/I:YellowBackground/I:BlueBackground/I:ZDCE/I:ZDCW/I:ZDCEandW/I:VPDE/I:VPDW/I:VPDEandW/I:ZDCWnokiller/I:ZDCEnokiller/I:BBCEandWnokiller/I:ZDCEandWnokiller/I:MTD/I";
//________________________________________________________________________________
void MakeScNTuple(const Char_t* dir = ".", const Char_t *pattern="", const Char_t *out="") { 
  TFileSet *fs = new TFileSet(dir);
  TString Path();
  TString Pattern(".*"); Pattern += pattern; Pattern += ".*.log"; cout << Pattern.Data() << endl;
  TDataSetIter next(fs,1);
  TPRegexp reg(Pattern.Data());
  TString Out(out);
  if (Out == "") {
    Out = pattern; Out += "Out.root";
  }
  TFile *f = new TFile(Out.Data(),"RECREATE");
  TTree *FitP = new TTree("FitP","Fit parameters");
  Vars_t B;
  FitP->Branch("Fit", &B.utime, vars);
  //  Float_t *rs = &B.BBCE;
  Int_t *rs = &B.BBCE;
  TDataSet *set = 0;
  Int_t i = 0;
  while ((set = next())) { //loop over DIR 
    TString Path(set->Path());// cout << Path.Data() << endl;
    if (! Path.Contains(reg)) continue;
    FILE *fp = fopen(Path.Data(),"r");
    if (! fp) {
      cout << "Can't open" << Path.Data() << endl;
      return;
    }
    cout << "Open " << Path.Data() << endl;
    Char_t line[320];
    Int_t  utime = 0;
    Int_t  valid = 0;
    while (fgets(&line[0],320,fp)) {
      TString Line(line);// cout << Line.Data();
      if (Line.Contains("INFO")) continue;
      if (Line.Contains("SC: valid 1, time")) {
        Int_t u;
	Int_t n = sscanf(Line.Data(),"SC: valid 1, time %d,",&u);
	if (n != 1) continue;
	if (utime > 0 && u == utime) {valid = 0; continue;}
	if (utime) FitP->Fill();
	utime = u;
	valid = utime;
	TDatime t; 
	t.Set(u);
	u = t.Convert(kTRUE);
	t.Set(u);
	B.utime = u;
	B.date = t.GetDate();
	B.time = t.GetTime();
        cout << Line.Data() << endl;
	t.Print();
	continue;
      }
      if (! valid) continue;
      //cout << Line.Data() << endl;
      Int_t j, sc;
      Int_t n = sscanf(Line.Data(),"        RICH scaler  %d: %d",&j,&sc);
      if (n != 2) continue;
      if (j < 0 || j > 15) continue;
      rs[j] = sc;
      cout << "rs[" << j << "] = " << rs[j] << endl;
      i++;
      //      if (i > 1000) break;
    }
    fclose(fp);
    if (valid) FitP->Fill();
    //    if (i > 1000) break;
  }
  f->Write();
}
