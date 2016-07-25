/*
  ln -s  /star/data90/reco/production2009_500Gev_c/ReversedFullField/2009/log_St* .
  foreach d (`ls -1d log*`)
  ParseLogFiles.pl ${d}/*.log > ${d}.data; root.exe -q -b 'MakeNTupleFromLogF.C+("'${d}.data'")'
  end

  root.exe AuAu200evalCANODEBUG.root  AuAu200evalNODEBUG.root
  .L MakeNTupleFromLogF.C+
  Plot();

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
#include "TH3.h"
#include "TStyle.h"
#include "TLegend.h"
#include "TMarker.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#endif
struct BPoint_t {
  //  Float_t event,tracks,good_tracks,tracksPr,good_tracksPr,tpcHits,tpcHitsUsed,mctracks,ast,cpu;
  Float_t event,tracks,good_tracks,tracksPr,good_tracksPr,tpcHits,tpcHitsUsed,mctracks,ast,cpu,SectRCA,TotalCA,PrepaCA,AveraCA,sumslCA,NeighCA,StarHCA,TrConCA,TrSelCA,WriteCA,mergeCA,TotaACA,node,group,cores,run,GHz;
};
//const Char_t *v = "event:tracks:good_tracks:tracksPr:good_tracksPr:tpcHits:tpcHitsUsed:mctracks:ast:cpu";
const Char_t *v = "event:tracks:good_tracks:tracksPr:good_tracksPr:tpcHits:tpcHitsUsed:mctracks:ast:cpu:SectRCA:TotalCA:PrepaCA:AveraCA:sumslCA:NeighCA:StarHCA:TrConCA:TrSelCA:WriteCA:mergeCA:TotaACA:node:group:cores:run:GHz";
BPoint_t BPoint;
void MakeNTupleFromLogF(const Char_t *FileName="ParseLogFiles.data") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  fName.ReplaceAll(".data",".root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("FitP","LogF",v);//:D_l:D_p:D_u:w:a:b:c");
  char line[601];
  Int_t i = 0;
  while (fgets(&line[0],600,fp)) {
    if (line[0] != 'E') continue;
    //                              1                         10                            20
    Int_t n = sscanf(&line[0],"Ev. %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
		     &BPoint.event,&BPoint.tracks,&BPoint.good_tracks,&BPoint.tracksPr,&BPoint.good_tracksPr,
		     &BPoint.tpcHits,&BPoint.tpcHitsUsed,&BPoint.mctracks,&BPoint.ast,&BPoint.cpu,
		     &BPoint.SectRCA,&BPoint.TotalCA,&BPoint.PrepaCA,&BPoint.AveraCA,&BPoint.sumslCA,
		     &BPoint.NeighCA,&BPoint.StarHCA,&BPoint.TrConCA,&BPoint.TrSelCA,&BPoint.WriteCA,
		     &BPoint.mergeCA,&BPoint.TotaACA,&BPoint.node,&BPoint.group,&BPoint.cores,&BPoint.run,&BPoint.GHz);
    if (i%1000 == 0) {
      printf("%i %s",n,line);
      if (! i) {//  |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |    
	printf("Ev. event tracks   good_tra tracksPr good_tPr tpcHits  tpcHitsU      mctracks ast      cpu    SectRCA  TotalCA  PrepaCA  AveraCA  sumslCA  NeighCA  StarHCA  TrConCA  TrSelCA  WriteCA  mergeCA TotaACA  node   group cores run GHz\n");
      }
      printf("Ev. %5.0f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f "
	     "%8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.0f %8.0f %8.0f %8.0f %8.2f\n",
	     BPoint.event,BPoint.tracks,BPoint.good_tracks,BPoint.tracksPr,BPoint.good_tracksPr,
	     BPoint.tpcHits,BPoint.tpcHitsUsed,BPoint.mctracks,BPoint.ast,BPoint.cpu,
	     BPoint.SectRCA,BPoint.TotalCA,BPoint.PrepaCA,BPoint.AveraCA,BPoint.sumslCA,
	     BPoint.NeighCA,BPoint.StarHCA,BPoint.TrConCA,BPoint.TrSelCA,BPoint.WriteCA,
	     BPoint.mergeCA,BPoint.TotaACA,BPoint.node,BPoint.group,BPoint.cores,BPoint.run, BPoint.GHz );
    }
    FitP->Fill(&BPoint.event);
    i++;
    //    if (i > 2000) break;
    //    if (i%10 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
//________________________________________________________________________________
void Plot(Int_t group = 0) {
  gStyle->SetOptDate(0);
  TFile *StiCA = 0, *Sti = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TNtuple *FitP;
  TFile *f;
  while ( (f = (TFile *) next()) ) { 
    FitP = (TNtuple *) f->Get("FitP");
    if (FitP) {
      TString Name(f->GetName());
      if (Name.Contains("CA")) {
	StiCA = f;
      } else {
	Sti = f;
      }
    }
  }
  if (! Sti || ! StiCA) return;
  Sti->cd();
  FitP = (TNtuple *) Sti->Get("FitP");
  FitP->SetMarkerStyle(1);
  FitP->SetMarkerColor(1);
  TLegend *leg = new TLegend(0.2,0.7,0.5,0.9);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else c1 = new TCanvas();
  Double_t xmin = 0;
  Double_t xmax = 4e5; // pp 1.6e5;
  Double_t ymin = 0;
  Double_t ymax = 5e2; // pp 150;
  //  TH1F *frame = c1->DrawFrame(2,-1.,5.5,2.5);
  TH1F *frame = c1->DrawFrame(xmin,ymin,xmax,ymax);
  if (group == 0) frame->SetTitle("40 cores Intel(R) Xeon(R) CPU E5-2660 v2 @ 2.20GHz");
  if (group == 1) frame->SetTitle("Pentium Xeon X5660 (six-core 12 MB L2) - Dell R410, 2.79 GHz");
  if (group == 3) frame->SetTitle("Pentium Xeon X5550 Nehalem (quad core 8MB L2) - Dell R710, 2.67 GHz");
  TString Cut(Form("group==%i",group));
  if (! group) Cut = "cores==40&&abs(GHz-2.2)<1e-3";
//   frame->SetXTitle("Log_{10} tpcHits");
//   frame->SetYTitle("Log_{10} cpu (seconds)");
  frame->SetXTitle("tpcHits");
  frame->SetYTitle("cpu (seconds)");
  TH2F *hSti = new TH2F("hSti","Sti cpu",100,xmin,xmax,100,ymin,ymax);
  hSti->SetMarkerStyle(1);
  hSti->SetMarkerColor(1);
  //  FitP->Draw("TMath::Log10(cpu):TMath::Log10(tpcHits)>>hSti",Cut,"same");
  FitP->Draw("cpu:tpcHits>>hSti",Cut,"same");
  TProfile *hSti_pfx = hSti->ProfileX();
  hSti_pfx->SetMarkerStyle(20);
  hSti_pfx->Draw("same");
  leg->AddEntry(hSti_pfx,"Sti tracker");
  StiCA->cd();
  FitP = (TNtuple *) StiCA->Get("FitP");
  FitP->SetMarkerStyle(1);
  FitP->SetMarkerColor(2);
  TH2F *hStiCA = new TH2F("hStiCA","StiCA cpu",100,xmin,xmax,100,ymin,ymax);
  hStiCA->SetMarkerStyle(1);
  hStiCA->SetMarkerColor(2);
  //  FitP->Draw("TMath::Log10(cpu):TMath::Log10(tpcHits)>>hStiCA",Cut,"same");
  FitP->Draw("cpu:tpcHits>>hStiCA",Cut,"same");
  TProfile *hStiCA_pfx = hStiCA->ProfileX();
  hStiCA_pfx->SetMarkerStyle(20);
  hStiCA_pfx->Draw("same");
  leg->AddEntry(hStiCA_pfx,"StiCA tracker");
  
#if 0
  //  hStiCA->Draw("same");
  //  leg->AddEntry(hStiCA,"CA+Sti tracker");
  m = new TMarker(-1.,-1., 20);
  //  m->SetMarkerStyle(20);
  m->SetMarkerColor(2);
  leg->AddEntry(m,"CA+Sti tracker","p");
  FitP->SetMarkerColor(3);
  TH2F *TotaACA = new TH2F("TotaACA","Total (sector+merge) avarage reconstuction",100,xmin,xmax,100,ymin,ymax);
  TotaACA->SetMarkerStyle(1);
  TotaACA->SetMarkerColor(3);
  //  FitP->Draw("TMath::Log10(1e-3*TotaACA):TMath::Log10(tpcHits)>>TotaACA",Cut,"same");
  FitP->Draw("TMath::Log10(1e-3*(SectRCA+mergeCA)):TMath::Log10(tpcHits)>>TotaACA",Cut,"same");
  //  TotaACA->Draw("same");
  //  leg->AddEntry(TotaACA,"Global CA tracker");
  m = new TMarker(-1.,-1., 20);
  //  m->SetMarkerStyle(20);
  m->SetMarkerColor(3);
  leg->AddEntry(m,"global CA tracker","P");
  
  FitP->SetMarkerColor(4);
  TH2F *SectRCA = new TH2F("SectRCA","Sector reconstruction",100,xmin,xmax,100,ymin,ymax);
  SectRCA->SetMarkerStyle(1);
  SectRCA->SetMarkerColor(4);
  FitP->Draw("TMath::Log10(1e-3*SectRCA):TMath::Log10(tpcHits)>>SectRCA",Cut,"same");
  //  SectRCA->Draw("same");
  //  leg->AddEntry(SectRCA,"sector CA tracker");
  m = new TMarker(-1.,-1., 20);
  //  m->SetMarkerStyle(20);
  m->SetMarkerColor(4);
  leg->AddEntry(m,"sector CA tracker","P");
#endif
  leg->Draw();
}
