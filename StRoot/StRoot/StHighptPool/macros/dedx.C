// real
#include "commonmacro/histutil.h"
void dedx()
{
  gSystem->Load("StHiMicroEvent");
  gSystem->Load("StHiMicroAnalysis");

  TChain* chain=new TChain("StHiMicroTree");
  StHiMicroEvent* event= new StHiMicroEvent;
  
  //  int doHalf=-1;

  char* dir="links/P01hi.central.2000.mdst";
  //char* dir="HIMERGEDCENTRAL";
  int nFile=20;

  IO io(dir);
  io.setNFile(nFile);
  io.chain(chain);

  char* basecut="mTracks.mFitPts>=10 && mTracks.mPtPr>1.5 && abs(mTracks.mEtaPr)<1";

  TCanvas* c1=new TCanvas("c1","c1",100,100,400,500); 
  //  gStyle->SetOptStat(0);

  char com[200],cut[500],title[100];
  TH2D* h2=new TH2D("h2","dedx v momentum",
		    80,1,3,
		    200,0,0.1e-4);
  h2->SetXTitle("momentum");
  h2->SetYTitle("dedx");
  
  float pmin=1.8,pmax=2;
  
  sprintf(title,"dedx (%.1f<p<%.1f)",pmin,pmax);
  TH1D* h1=new TH1D("h1",title,
		    100,0.,0.05e-4);
  h1->SetXTitle("dedx");

  TH1D* h1P=new TH1D("h1P","primary momentum",
		     20,1,3);
  //----------
  Divide(c1,1,2,"dedx",dir);
  //  c1.Divide(1,2);

  sprintf(com,"mTracks.mDedx:mTracks.mPPr");

  chain->Project("h2",com,basecut);
  c1.cd(1);
  h2->Draw();

  sprintf(cut,"mTracks.mPPr>%.1f && mTracks.mPPr<%.1f && %s",
	  pmin,pmax,basecut);
  chain->Project("h1","mTracks.mDedx",cut);
  c1.cd(2); 
  h1->Draw();

  c1.Print("dedx.ps");
}
    
