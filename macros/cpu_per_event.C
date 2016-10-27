/* 
cd ~/work/Nightlies/
foreach log (`ls -1d gcc*\/*\/*\/*\/*.log`)
  set d = `dirname ${log}`;
  cd ${d}; pwd;
  if ( -r cpu_per_event) rm cpu_per_event
  grep 'Done with Event' *.log |  grep -v 'no. 1/' | awk  'BEGIN {n=0;j = 0;}{j++; n += $17;} END {if (j > 0) print "\tCPU/event = "n/j "(sec) \tfor "j" events"; else print "Failed";}' | tee cpu_per_event
  cd -;
end
egrep '(CPU|Failed)' gcc*\/*\/*\/*\/cpu_per_event | tee cpu_per_event.data
   root.exe cpu_per_event.root cpu_per_event.C+
================================================================================
grep CPU gcc*\/*\/*\/*\/cpu_per_event | tee cpu_per_event.data
awk -F\/ '{print $3"|\t"$4"|\t"$2"|\t"$1"|\t"$6}' cpu_per_event.data | sed -e 's/event =//' -e 's/(sec) for /\//' -e 's/ events//' -e 's/\.DEV2.//' -e 's/x8664/-m64/' -e 's/NODEBUG/-O/' -e 's/_vfppvd/v/' | sort | tee cpu_per_event.data.sorted


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
  Int_t   run, reco, version, comp, opt, bits, deb, noEvents;
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
void cpu_per_event(const Char_t *FileName="./cpu_per_event.data") {
  enum {kRecos = 4, kRuns = 78, kLastRuns = 19, kVersions = 1, kComps = 4, kDeb = 2, kBits = 2, kOpts = kDeb*kBits};
  const Char_t *runs[kRuns]  = {
"year_2000/MC.stan", //
"year_2000/RC.centr", //
"year_2000/RC.minb", //
"year_2001d/RC.cent", //
"year_2001d/RC.minb", //
"year_2001d/RC.pp", //
"year_2001/MC.pp", //
"year_2001/MC.stan", //
"year_2001/RC.cent", //
"year_2001/RC.minb", //
"year_2001/RC.pp", //
"year_2001_vfppvd/RC.pp", //
"year_2003/MC.dau", //
"year_2003/RC.dau", //
"year_2003/RC.pp", //
"year_2004/MC.auau", //
"year_2004/MC.auauCtr", //
"year_2004/RC.auau", //
"year_2004/RC.auau.lo", //
"year_2004/RC.auau.ph", //
"year_2004/RC.pp", //
"year_2005/MC.cucu200", //
"year_2005/MC.cucu62", //
"year_2005/RC.cucu200", //
"year_2005/RC.cucu200.ht", //
"year_2005/RC.cucu22", //
"year_2005/RC.cucu62", //
"year_2005/RC.pp200", //
"year_2006/MC.pp200", //
"year_2006/RC.pp200.Long", //
"year_2006/RC.pp200.Trans", //
"year_2007/MC.auau200", //
"year_2007/RC.auau200", //
"year_2007/RC.auau200.MB", //
"year_2007/RC.dau200", //
"year_2008/MC.dau200", //
"year_2008/MC.pp200", //
"year_2008/RC.pp200", //
"year_2009/MC.pp200", //
"year_2009/MC.pp500", //
"year_2009/RC.pp200", //
"year_2009/RC.pp500", //
"year_2010/MC.auau11", //
"year_2010/MC.auau200", //
"year_2010/MC.auau39", //
"year_2010/MC.auau62", //
"year_2010/MC.auau7", //
"year_2010/RC.auau11", //
"year_2010/RC.auau200", //
"year_2010/RC.auau39", //
"year_2010/RC.auau62", //
"year_2010/RC.auau7", //
"year_2011/MC.auau200", //
"year_2011/MC.pp500", //
"year_2011/MC.pp500.pileup", //
"year_2011/RC.auau20", //
"year_2011/RC.auau200", //
"year_2011/RC.auau27", //
"year_2011/RC.pp500", //
"year_2012/MC.CuAu200", //
"year_2012/MC.pp200", //
"year_2012/MC.pp500", //
"year_2012/MC.UU200", //
"year_2012/RC.cuAu200", //
"year_2012/RC.pp200", //
"year_2012/RC.pp500", //
"year_2012/RC.UU193", //
"year_2013/RC.pp500", //
"year_2014/MC.AuAu200", //
"year_2014/RC.AuAu15", //
"year_2014/RC.AuAu200", //
"year_2014/RC.AuAu200.low", //
"year_2014/RC.AuAu200.mid", //
"year_2014/RC.He3Au200", //
"year_2015/RC.pp200long", //
"year_2015/RC.pp200long.NoHFT", //
"year_2016/RC.AuAu200.y2016", //
"undef"
  };
  const Char_t *recos[kRecos]  = {"Sti","StiCA","StiCAKF","undef"};
  const Char_t *recosS[kRecos] = {"Sti","CA","KFV","udef"};
  const Char_t *versions[kVersions]  = {".DEV2"};// ,"eval","devC","TFG16a","TFG16b"};
  const Char_t *comps[kComps] = {"gcc482","gcc492","gcc521","gcc620"};
  const Char_t *opts[kOpts]  = {"","NODEBUG","x8664","NODEBUG.x8664"}; // [kOpts] => [kBits][kDeb], kBits = 0 -> -m32, = 1 -> -m64; kDeb = 0 -> -g; kDeb = 1 => -O2;
  const Char_t *debs[kDeb] = {"-g","-O2"};
  const Char_t *bits[kBits] = {"-m32","-m64"};
  Double_t CPU[kRuns][kComps][kBits][kDeb][kVersions][kRecos] = {-1};
  TString  Lines[kRuns][kComps][kBits][kDeb][kVersions][kRecos];
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".data",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple*  FitP = new TNtuple("FitP","CPU per event","run/I:reco/I:version/I:comp/I:opt/I:bits/I:deb/I:noEvents/I:CPU");
  Char_t line[121];
  Int_t i = 0;
  while (fgets(&line[0],120,fp)) {
    BP.run = BP.reco = BP.version = BP.comp = BP.opt = BP.noEvents = -1;
    BP.CPU = 1e4;
    TString Line(line);
    //#define __DEBUG__
#ifdef __DEBUG__
    cout << Line.Data();
#endif
    BP.comp    = iucomp(Line, comps, kComps);
    BP.version = 0; // iucomp(Line, versions, kVersions);
    BP.reco = iucomp(Line, recos, kRecos);
    if (BP.reco < 0)  BP.reco = kRecos-1;
    BP.run     = iucomp(Line, runs, kRuns);

    BP.opt     = 0; // 
    if      (Line.Contains(opts[3])) BP.opt = 3;
    else if (Line.Contains(opts[2])) BP.opt = 2;
    else if (Line.Contains(opts[1])) BP.opt = 1;
    BP.bits = 0;
    if (BP.opt > 1) BP.bits = 1;
    BP.deb = 0;
    if (BP.opt%2 == 1) BP.deb = 1;
    //    BP.opt     = iucomp(Line, &opts[1], kOpts-1) + 1;
#ifdef __DEBUG__
    cout << comps[BP.comp] << "\t" << versions[BP.version] << "\t" << recos[BP.reco] << "\t" << runs[BP.run] << "\t" << opts[BP.opt] << endl;
#endif
    if (! Line.Contains("Failed")) {
      Int_t n = sscanf(Line.Data(),"%*s CPU/event = %f(sec)        for %i events",&BP.CPU,&BP.noEvents);
      if (BP.comp < 0 || BP.opt < 0 || BP.version < 0 || BP.reco < 0 || BP.run < 0) {
	cout << "wrong indexes comp = " << BP.comp << " opt = " <<  BP.opt << " version = " << BP.version << " reco = " <<  BP.reco << " run = " <<  BP.run << endl;
	continue;
      }
      if (CPU[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco] > 0) {
	cout << "old Line:" << Lines[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco].Data() << endl;
	cout << "new Line:" << Line.Data() << endl;
	cout << "CPU[" << BP.comp << "][" << BP.opt << "][" << BP.version<< "][" << BP.reco << "][" << BP.run << "] = " << CPU[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco] << " new value = " << BP.CPU << endl;
	continue;
      }
      CPU[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco] = BP.CPU;
      Lines[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco] = Line;
#ifdef __DEBUG__
      cout << "n = " << n << "\tCPU = " << BP.CPU << "\tnoEvents = " << BP.noEvents << endl;
      cout << "CPU[" << BP.comp << "][" <<BP.opt << "][" <<BP.version << "][" <<BP.run << "] = " << CPU[BP.run][BP.comp][BP.bits][BP.deb][BP.version][BP.reco] << endl;
#endif
    } 
    FitP->Fill((Float_t *) &BP.run);
    i++;
  }
  fclose(fp);
  f->Write();
  TString Ident("                                 |");
  for (Int_t run = -1; run < kRuns; run++) {
    if (run >= 0 && run < kRuns - kLastRuns ) continue;
    if (run == -1) {
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	cout << Form("%39s|",comps[comp]);
      }
      cout << endl;
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	for (Int_t bit = 0; bit < kBits; bit++) {
	  cout << Form("%19s|",bits[bit]);
	}
      }
      cout << endl;
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	for (Int_t bit = 0; bit < kBits; bit++) {
	  for (Int_t deb = 0; deb < kDeb; deb++) {
	    cout << Form("%9s|",debs[deb]);
	  }
	}
      }
      cout << "|" << endl;
      continue;
    }
    for (Int_t reco = 0; reco < kRecos; reco++) {
      if (reco == 0) 
	cout << Form("%29s|%3s|",runs[run],recosS[reco]);
      else 
	cout << Form("                             |%3s|",recosS[reco]);
	
      for (Int_t version = 0; version < kVersions; version++) {
	for (Int_t comp = 0; comp < kComps; comp++) {
	  for (Int_t bit = 0; bit < kBits; bit++) {
	    for (Int_t deb = 0; deb < kDeb; deb++) {
	      if (CPU[comp][bit][deb][version][reco][run] > 0) {
		cout << Form("%9.3f|",CPU[comp][bit][deb][version][reco][run]);
	      } else {
		cout << "         |";
	      }
	    }
	  }
	}
      }
      cout << endl;
    }
  }
  // Ratio to gcc482 -m32 -O2
  Double_t n[kComps][kBits][kDeb][kVersions] = {0};
  Double_t x[kComps][kBits][kDeb][kVersions] = {0};
  Double_t xx[kComps][kBits][kDeb][kVersions] = {0};
  cout << "================================================================================" << endl;
  cout << "Ratio to gcc482 -O2 -m42" << endl;
  cout << "================================================================================" << endl;
  for (Int_t run = -1; run <= kRuns; run++) {
    if (run >= 0 && run < kRuns - kLastRuns ) continue;
    if (run == -1) {
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	cout << Form("%39s|",comps[comp]);
      }
      cout << endl;
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	for (Int_t bit = 0; bit < kBits; bit++) {
	  cout << Form("%19s|",bits[bit]);
	}
      }
      cout << endl;
      cout << Ident.Data();
      for (Int_t comp = 0; comp < kComps; comp++) {
	for (Int_t bit = 0; bit < kBits; bit++) {
	  for (Int_t deb = 0; deb < kDeb; deb++) {
	    cout << Form("%9s|",debs[deb]);
	  }
	}
      }
      cout << "|" << endl;
      continue;
    }
    for (Int_t reco = 0; reco <= kRecos; reco++) {
      if (run == kRuns) {
	if (reco) continue;
 	cout <<      "Average(%)                   |   |";
      }
      else {
	if (reco == 0) 	        cout << Form("%29s|%3s|",runs[run],recosS[reco]);
	else 	       cout << Form("                             |%3s|",recosS[reco]);
	if (CPU[0][0][1][0][reco][run] <= 0.0) {cout << endl; continue;}
      }
      for (Int_t version = 0; version < kVersions; version++) {
	for (Int_t comp = 0; comp < kComps; comp++) {
	  for (Int_t bit = 0; bit < kBits; bit++) {
	    for (Int_t deb = 0; deb < kDeb; deb++) {
	      if (run == kRuns) {
		if (reco) continue;
		if (n[comp][bit][deb][version] < 2) cout << "         |";
		else {
		  Double_t N   = n[comp][bit][deb][version];
		  Double_t xav = x[comp][bit][deb][version]/N;
		  Double_t sig = TMath::Sqrt(xx[comp][bit][deb][version]/N - xav*xav);
		  cout << Form("%4i+/-%2i|",(Int_t) TMath::Nint(100*xav),(Int_t) TMath::Nint(100*sig));
		}
		continue;
	      }
	      Double_t cpu = CPU[comp][bit][deb][version][reco][run];
	      if (cpu > 0) {
		Double_t cpuRef = CPU[0][0][1][0][reco][run];
		Double_t r = cpu/cpuRef;
		cout << Form("%9.3f|",r);
		n[comp][bit][deb][version]++;
		x[comp][bit][deb][version] += r;
		xx[comp][bit][deb][version] += r*r;
	      } else {
		cout << "         |";
	      }
	    }
	  }
	}
      }
      cout << endl;
    }
  }
  
#if 0
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
#endif
}
