/* 
cd ~/work/Nightlies/
#foreach log (`ls -1d .DEV2/Sti/year_201*/*/*.log`)
foreach log (`ls -1d gcc*/*/*/*/*.log`)
  set d = `dirname ${log}`;
  cd ${d}
  if ( -r cpu_per_event) rm cpu_per_event
    grep 'Done with Event' *.log | awk  'BEGIN {n=0;j = 0;}{j++; n += $17;} END {if (j > 0) print "\tCPU/event = "n/j "(sec) \tfor "j" events"; else print "Failed";}' | tee cpu_per_event
  cd -;
end
egrep '(CPU|Failed)' gcc*/*/*/*/cpu_per_event | tee cpu_per_event.data
   root.exe cpu_per_event.root cpu_per_event.C+
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
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TGraph.h"
#endif
struct BP_t {
  Int_t   run, reco, version, comp, opt, noEvents;
  Float_t CPU;
};
BP_t BP;
//________________________________________________________________________________
Int_t iucomp(const TString &Line, const Char_t *comps[], Int_t no) {
  Int_t i = -1;
  for (Int_t comp = no-1; comp >= 0; comp--) {
    if (Line.Contains(comps[comp])) {
      i = comp;
      break;
    }
  }
  return i;
}
//________________________________________________________________________________
void cpu_per_event(const Char_t *FileName="/star/subsys/tpc/fisyak/Nightlies/cpu_per_event.data") {
  enum {kRecos = 3, kRuns = 77, kVersions = 5, kComps = 2, kOpts = 2};
  const Char_t *recos[kRecos] = {"Sti","StiCA","StiCAKFV"};
  const Char_t *runs[kRuns]  = {
    "2000/MC.stan",   "2000/RC.centr",  "2000/RC.minb",
    "2001d/RC.cent",  "2001d/RC.minb",  "2001d/RC.pp",    "2001/MC.pp",          "2001/MC.stan",     "2001/RC.cent",    "2001/RC.minb",   "2001/RC.pp",    "2001_vfppvd/RC.pp",
    "2003/MC.dau",    "2003/RC.dau",    "2003/RC.pp",
    "2004/MC.auau",   "2004/MC.auauCtr","2004/RC.auau",   "2004/RC.auau.lo",     "2004/RC.auau.ph",   "2004/RC.pp",
    "2005/MC.cucu200","2005/MC.cucu62", "2005/RC.cucu200","2005/RC.cucu200.ht",  "2005/RC.cucu22",    "2005/RC.cucu62", "2005/RC.pp200",
    "2006/MC.pp200",  "2006/RC.pp200.Long","2006/RC.pp200.Trans",
    "2007/MC.auau200","2007/RC.auau200","2007/RC.auau200.MB","2007/RC.dau200",   "2008/MC.dau200",
    "2008/MC.pp200",  "2008/RC.dau200", "2008/RC.pp200",
    "2009/MC.pp200",  "2009/MC.pp500",  "2009/RC.pp200",  "2009/RC.pp500",
    "2010/MC.auau11", "2010/MC.auau200","2010/MC.auau39", "2010/MC.auau62",      "2010/MC.auau7",      "2010/RC.auau11","2010/RC.auau200","2010/RC.auau39","2010/RC.auau62","2010/RC.auau7",
    "2011/MC.auau200","2011/MC.pp500",  "2011/MC.pp500.pileup", "2011/RC.auau20","2011/RC.auau200",    "2011/RC.auau27","2011/RC.pp500",
    "2012/MC.CuAu200","2012/MC.pp200",  "2012/MC.pp500",  "2012/MC.UU200",       "2012/RC.cuAu200",    "2012/RC.pp200", "2012/RC.pp500",  "2012/RC.UU193",
    "2013/RC.pp500",
    "2014/MC.AuAu200","2014/RC.AuAu15", "2014/RC.AuAu200","2014/RC.AuAu200.low", "2014/RC.AuAu200.mid","2014/RC.He3Au200",
    "2015/RC.pp200long","2015/RC.pp200long.NoHFT",
  };
  const Char_t *versions[kVersions]  = {".DEV2","eval","devC","TFG16a","TFG16b"};
  const Char_t *comps[kComps] = {"gcc492","gcc482"};
  const Char_t *opts[kOpts]  = {"","NODEBUG"};
  TNtuple*  FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".data",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","CPU per event","run/I:reco/I:version/I:comp/I:opt/I:noEvents/I:CPU");
  Char_t line[121];
  Int_t i = 0;
  while (fgets(&line[0],120,fp)) {
    BP.run = BP.reco = BP.version = BP.comp = BP.opt = BP.noEvents = -1;
    BP.CPU = 1e4;
    TString Line(line);
#ifdef __DEBUG__
    cout << Line.Data();
#endif
    BP.comp    = iucomp(Line, comps, kComps);
    BP.version = iucomp(Line, versions, kVersions);
    BP.reco    = iucomp(Line, recos, kRecos);
    BP.run     = iucomp(Line, runs, kRuns);
    BP.opt     = iucomp(Line, opts, kOpts);
#ifdef __DEBUG__
    cout << comps[BP.comp] << "\t" << versions[BP.version] << "\t" << recos[BP.reco] << "\t" << runs[BP.run] << "\t" << opts[BP.opt] << endl;
#endif
    if (! Line.Contains("Failed")) {
      Int_t n = sscanf(Line.Data(),"%*s CPU/event = %f(sec)        for %i events",&BP.CPU,&BP.noEvents);
#ifdef __DEBUG__
      cout << "n = " << n << "\tCPU = " << BP.CPU << "\tnoEvents = " << BP.noEvents << endl;
#endif
    } 
    FitP->Fill((Float_t *) &BP.run);
    i++;
  }
  fclose(fp);
  f->Write();
  }
  TCanvas *c1 = new TCanvas("c1","c1",2,10,700,500);
  TH2F *frame = new TH2F("frame","CPU for different runs, libraries, compiler versions and options)", 200, -0.5, 4, kRuns, -0.5, kRuns-0.5);
  frame->SetXTitle("log_{10}(CPU[sec/event])");
  frame->SetStats(0);
  TAxis *y = frame->GetYaxis();
  Int_t nbins = y->GetNbins();
  for (Int_t bin = 1; bin <= nbins; bin++) {
    y->SetBinLabel(bin, runs[bin-1]);
  }
  y->SetLabelSize(2e-2);
  frame->Draw();
  TLegend *l = new TLegend(0.7,0.1,0.9,0.5);
  l->SetFillStyle(4000);
  for (Int_t reco = 0; reco < kRecos; reco++) {
    Int_t marker = 19;
    for (Int_t version = 0; version < kVersions; version++) {
      for (Int_t comp = 0; comp < kComps; comp++) {
	marker++;
	for (Int_t opt = 0; opt <  kOpts; opt++) {
	  Int_t nFound = FitP->Draw("run:TMath::Log10(CPU)",
				    Form("reco == %i && version == %i && comp == %i && opt == %i",reco,version,comp,opt), 
				    "goff");
	  if (nFound > 0) {
	    TGraph *gr = new TGraph(nFound,FitP->GetV2(), FitP->GetV1());
	    gr->SetMarkerStyle(marker);
	    gr->SetMarkerSize(1.6-0.3*reco);
	    gr->SetMarkerColor(opt+1);
	    gr->SetLineColor(0);
	    gr->SetLineStyle(0);
	    gr->SetLineWidth(0.1);
	    gr->SetFillColor(0);
	    gr->Draw("P");
	    l->AddEntry(gr,Form("%s %s %s %s", comps[comp], versions[version], recos[reco], opts[opt]));
	  }
	}
      }
    }
  }
  l->Draw();
  c1->Update();
}
