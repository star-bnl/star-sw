#include <stdio.h>
#include <Riostream.h>
#include <TFile.h>
#include <TTree.h>
#include <TH2F.h>
#include <TPostScript.h>
#include <TCanvas.h>
#include <TVector3.h>
#include <TPad.h>
#include <TStyle.h>
#include <TString.h>
#include <TPaveText.h>
#include <TMath.h>

#include <StEmcPool/StPhotonCommon/MyEvent.h>
#include <StEmcPool/StPhotonCommon/MyPoint.h>
#include <StEmcPool/StPhotonCommon/MyMcTrack.h>

#include "AnaCuts.h"

#include "dAuBackground.h"
using namespace std;

ClassImp(dAuBackground)

Int_t isBad[2400];

dAuBackground::dAuBackground()
{
    cuts = 0;

  gStyle->SetPadGridX(kFALSE);
  gStyle->SetPadGridY(kFALSE);

  for(Int_t i=0;i<2400;i++)
    {
      isBad[i]=0;
    }
  Int_t badIds[]={82,156,240,264,733,756,818,853,931,1289,1292,1355,1359,1527,1759,1943,2011,2025,2090,2152,2369,-1};
  Int_t j=0;
  while(badIds[j]>0)
    {
      isBad[badIds[j]-1]=1;
      j++;
    }

    cuts = new AnaCuts();
  cout<<"constructed!!"<<endl;
}
dAuBackground::~dAuBackground()
{
    if (cuts) delete cuts; cuts = 0;
  cout<<"destructed!!"<<endl;
}

void dAuBackground::run(const char *file)
{
  //0-20
  TH1F *ratio_mb_020=new TH1F("ratio_mb_020","2001&&2003, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *ratio_ht1_020=new TH1F("ratio_ht1_020","2201, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *ratio_ht2_020=new TH1F("ratio_ht2_020","2202, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *ratio_sn_020=new TH1F("ratio_sn_020","2002, FTPC-Au:0-20%", 25,0.,1.);

  //20-40
  TH1F *ratio_mb_2040=new TH1F("ratio_mb_2040","2001&&2003, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *ratio_ht1_2040=new TH1F("ratio_ht1_2040","2201, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *ratio_ht2_2040=new TH1F("ratio_ht2_2040","2202, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *ratio_sn_2040=new TH1F("ratio_sn_2040","2002, FTPC-Au:20-40%", 25,0.,1.);

  //40-100
  TH1F *ratio_mb_40100=new TH1F("ratio_mb_40100","2001&&2003, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *ratio_ht1_40100=new TH1F("ratio_ht1_40100","2201, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *ratio_ht2_40100=new TH1F("ratio_ht2_40100","2202, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *ratio_sn_40100=new TH1F("ratio_sn_40100","2002, FTPC-Au:40-100%", 25,0.,1.);

  TH1F *hfrac_mb=new TH1F("frac_mb","2001&&2003, I[0.8,1.0]/I[0.0,1.0], 40-100%",50,0.0,0.05);
  TH1F *hfrac_ht1=new TH1F("frac_ht1","2201, I[0.8,1.0]/I[0.0,1.0], 40-100%",50,0.0,0.5);
  TH1F *hfrac_ht2=new TH1F("frac_ht2","2202, I[0.8,1.0]/I[0.0,1.0], 40-100%",50,0.0,0.5);
  TH1F *hfrac_sn=new TH1F("frac_sn","2000, I[0.8,1.0]/I[0.0,1.0], 40-100%",50,0.0,0.05);

  //totals
  //0-20
  TH1F *Ratio_mb_020=new TH1F("Ratio_mb_020","2001&&2003, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *Ratio_ht1_020=new TH1F("Ratio_ht1_020","2201, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *Ratio_ht2_020=new TH1F("Ratio_ht2_020","2202, FTPC-Au:0-20%", 25,0.,1.);
  TH1F *Ratio_sn_020=new TH1F("Ratio_sn_020","2002, FTPC-Au:0-20%", 25,0.,1.);

  //20-40
  TH1F *Ratio_mb_2040=new TH1F("Ratio_mb_2040","2001&&2003, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *Ratio_ht1_2040=new TH1F("Ratio_ht1_2040","2201, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *Ratio_ht2_2040=new TH1F("Ratio_ht2_2040","2202, FTPC-Au:20-40%", 25,0.,1.);
  TH1F *Ratio_sn_2040=new TH1F("Ratio_sn_2040","2002, FTPC-Au:20-40%", 25,0.,1.);

  //40-100
  TH1F *Ratio_mb_40100=new TH1F("Ratio_mb_40100","2001&&2003, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *Ratio_ht1_40100=new TH1F("Ratio_ht1_40100","2201, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *Ratio_ht2_40100=new TH1F("Ratio_ht2_40100","2202, FTPC-Au:40-100%", 25,0.,1.);
  TH1F *Ratio_sn_40100=new TH1F("Ratio_sn_40100","2002, FTPC-Au:40-100%", 25,0.,1.);

  TH2F *EvsE_mb=new TH2F("EvsE_mb","2001&&2003, neutral Energy vs TPC pT",240,0.,80,240,0.,80.);
  TH2F *EvsE_ht1=new TH2F("EvsE_ht1","2201, neutral Energy vs TPC pT",124,0.,80,240,0.,80.);
  TH2F *EvsE_ht2=new TH2F("EvsE_ht2","2202, neutral Energy vs TPC pT",124,0.,80,240,0.,80.);
  TH2F *EvsE_sn=new TH2F("EvsE_sn","2002, neutral Energy vs TPC pT",240,0.,80,240,0.,80.);

  //spectra
  //0-20
  TH1F *ptspec_mb_020=new TH1F("ptspec_mb_020","2001&&2003, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_ht1_020=new TH1F("ptspec_ht1_020","2201, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_ht2_020=new TH1F("ptspec_ht2_020","2202, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_sn_020=new TH1F("ptspec_sn_020","2002, pT spectrum neutral points, 0-20%",30,0.,15.);

  TH1F *ptspec_mb_cut_020=new TH1F("ptspec_mb_cut_020","2001&&2003, pT spectrum neutral points after cut, 0-20%",30,0.,15.);
  TH1F *ptspec_ht1_cut_020=new TH1F("ptspec_ht1_cut_020","2201, pT spectrum neutral points after cut, 0-20%",30,0.,15.);
  TH1F *ptspec_ht2_cut_020=new TH1F("ptspec_ht2_cut_020","2202, pT spectrum neutral points after cut, 0-20%",30,0.,15.);
  TH1F *ptspec_sn_cut_020=new TH1F("ptspec_sn_cut_020","2002, pT spectrum neutral points after cut, 0-20%",30,0.,15.);

  TH1F *ptspec_mb_bg_020=new TH1F("ptspec_mb_bg_020","2001&&2003, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_ht1_bg_020=new TH1F("ptspec_ht1_bg_020","2201, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_ht2_bg_020=new TH1F("ptspec_ht2_bg_020","2202, pT spectrum neutral points, 0-20%",30,0.,15.);
  TH1F *ptspec_sn_bg_020=new TH1F("ptspec_sn_bg_020","2002, pT spectrum neutral points, 0-20%",30,0.,15.);

  //20-40
  TH1F *ptspec_mb_2040=new TH1F("ptspec_mb_2040","2001&&2003, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_ht1_2040=new TH1F("ptspec_ht1_2040","2201, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_ht2_2040=new TH1F("ptspec_ht2_2040","2202, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_sn_2040=new TH1F("ptspec_sn_2040","2002, pT spectrum neutral points, 20-40%",30,0.,15.);

  TH1F *ptspec_mb_cut_2040=new TH1F("ptspec_mb_cut_2040","2001&&2003, pT spectrum neutral points after cut, 20-40%",30,0.,15.);
  TH1F *ptspec_ht1_cut_2040=new TH1F("ptspec_ht1_cut_2040","2201, pT spectrum neutral points after cut, 20-40%",30,0.,15.);
  TH1F *ptspec_ht2_cut_2040=new TH1F("ptspec_ht2_cut_2040","2202, pT spectrum neutral points after cut, 20-40%",30,0.,15.);
  TH1F *ptspec_sn_cut_2040=new TH1F("ptspec_sn_cut_2040","2002, pT spectrum neutral points after cut, 20-40%",30,0.,15.);

  TH1F *ptspec_mb_bg_2040=new TH1F("ptspec_mb_bg_2040","2001&&2003, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_ht1_bg_2040=new TH1F("ptspec_ht1_bg_2040","2201, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_ht2_bg_2040=new TH1F("ptspec_ht2_bg_2040","2202, pT spectrum neutral points, 20-40%",30,0.,15.);
  TH1F *ptspec_sn_bg_2040=new TH1F("ptspec_sn_bg_2040","2002, pT spectrum neutral points, 20-40%",30,0.,15.);

  //40-100
  TH1F *ptspec_mb_40100=new TH1F("ptspec_mb_40100","2001&&2003, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_ht1_40100=new TH1F("ptspec_ht1_40100","2201, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_ht2_40100=new TH1F("ptspec_ht2_40100","2202, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_sn_40100=new TH1F("ptspec_sn_40100","2002, pT spectrum neutral points, 40-100%",30,0.,15.);

  TH1F *ptspec_mb_cut_40100=new TH1F("ptspec_mb_cut_40100","2001&&2003, pT spectrum neutral points after cut, 40-100%",30,0.,15.);
  TH1F *ptspec_ht1_cut_40100=new TH1F("ptspec_ht1_cut_40100","2201, pT spectrum neutral points after cut, 40-100%",30,0.,15.);
  TH1F *ptspec_ht2_cut_40100=new TH1F("ptspec_ht2_cut_40100","2202, pT spectrum neutral points after cut, 40-100%",30,0.,15.);
  TH1F *ptspec_sn_cut_40100=new TH1F("ptspec_sn_cut_40100","2002, pT spectrum neutral points after cut, 40-100%",30,0.,15.);

  TH1F *ptspec_mb_bg_40100=new TH1F("ptspec_mb_bg_40100","2001&&2003, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_ht1_bg_40100=new TH1F("ptspec_ht1_bg_40100","2201, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_ht2_bg_40100=new TH1F("ptspec_ht2_bg_40100","2202, pT spectrum neutral points, 40-100%",30,0.,15.);
  TH1F *ptspec_sn_bg_40100=new TH1F("ptspec_sn_bg_40100","2002, pT spectrum neutral points, 40-100%",30,0.,15.);


  TH1F *ftpc_mb=new TH1F("ftpc_mb","2001&&2003, ftpc-Au mult.",60,0.,60.);
  TH1F *ftpc_ht1=new TH1F("ftpc_ht1","2201, ftpc-Au mult.",60,0.,60.);
  TH1F *ftpc_ht2=new TH1F("ftpc_ht2","2202, ftpc-Au mult.",60,0.,60.);
  TH1F *ftpc_sn=new TH1F("ftpc_sn","2002, ftpc-Au mult.",60,0.,60.);

  TH1F *ftpc_mb_cut=new TH1F("ftpc_mb_cut","2001&&2003, ftpc-Au mult. for r<.8",60,0.,60.);
  TH1F *ftpc_ht1_cut=new TH1F("ftpc_ht1_cut","2201, ftpc-Au mult. for r<.8",60,0.,60.);
  TH1F *ftpc_ht2_cut=new TH1F("ftpc_ht2_cut","2202, ftpc-Au mult. for r<.8",60,0.,60.);
  TH1F *ftpc_sn_cut=new TH1F("ftpc_sn_cut","2002, ftpc-Au mult. for r<.8",60,0.,60.);

  TH1F *tpc_mb=new TH1F("tpc_mb","2001&&2003, tpc mult.",100,0.,100.);
  TH1F *tpc_ht1=new TH1F("tpc_ht1","2201, tpc mult.",100,0.,100.);
  TH1F *tpc_ht2=new TH1F("tpc_ht2","2202, tpc mult.",100,0.,100.);
  TH1F *tpc_sn=new TH1F("tpc_sn","2002, tpc mult.",100,0.,100.);

  TH1F *tpc_mb_cut=new TH1F("tpc_mb_cut","2001&&2003, tpc mult. for r<.8",100,0.,100.);
  TH1F *tpc_ht1_cut=new TH1F("tpc_ht1_cut","2201, tpc mult. for r<.8",100,0.,100.);
  TH1F *tpc_ht2_cut=new TH1F("tpc_ht2_cut","2202, tpc mult. for r<.8",100,0.,100.);
  TH1F *tpc_sn_cut=new TH1F("tpc_sn_cut","2002, tpc mult. for r<.8",100,0.,100.);

  TH1F *bemc_mb=new TH1F("bemc_mb","2001&&2003, bemc mult.",60,0.,60.);
  TH1F *bemc_ht1=new TH1F("bemc_ht1","2201, bemc mult.",60,0.,60.);
  TH1F *bemc_ht2=new TH1F("bemc_ht2","2202, bemc mult.",60,0.,60.);
  TH1F *bemc_sn=new TH1F("bemc_sn","2002, bemc mult.",60,0.,60.);

  TH1F *bemc_mb_cut=new TH1F("bemc_mb_cut","2001&&2003, bemc mult. for r<.8",60,0.,60.);
  TH1F *bemc_ht1_cut=new TH1F("bemc_ht1_cut","2201, bemc mult. for r<.8",60,0.,60.);
  TH1F *bemc_ht2_cut=new TH1F("bemc_ht2_cut","2202, bemc mult. for r<.8",60,0.,60.);
  TH1F *bemc_sn_cut=new TH1F("bemc_sn_cut","2002, bemc mult. for r<.8",60,0.,60.);

  TH2F *etaphi_fill3085_sn=new TH2F("etaphi_fill3085_sn","eta/phi for 3085 sn 40-100%",500,0.0,1.0,1000,-3.2,3.2);
  TH2F *etaphi_fill3085_ht2=new TH2F("etaphi_fill3085_ht2","eta/phi for 3085 ht-2 40-100%",500,0.0,1.0,1000,-3.2,3.2);
  TH2F *etaphi_fill3088_sn=new TH2F("etaphi_fill3088_sn","eta/phi for 3088 sn 40-100%",500,0.0,1.0,1000,-3.2,3.2);
  TH2F *etaphi_fill3088_ht2=new TH2F("etaphi_fill3088_ht2","eta/phi for 3088 ht-2 40-100%",500,0.0,1.0,1000,-3.2,3.2);
  TH2F *etaphi_fill3088b_ht2=new TH2F("etaphi_fill3088b_ht2","eta/phi for 3088 ht-2 40-100% and R<0.7",500,0.0,1.0,1000,-3.2,3.2);

  TH2F *etaphi_sn=new TH2F("etaphi_sn","eta/phi sn 40-100%",500,0.0,1.0,1000,-3.2,3.2);


  TH1F *h_trigger=new TH1F("h_trigger","trigger ids after cut",2400,0.5,2400.5);

  ofstream fout;
  fout.open("bgfrac.dat");
  fout<<"pagina\trunid\tmb\tnmb\tht1\tnht1\tht2\tnht2\tsn\tnsn"<<endl<<endl;

  TFile *inf=new TFile(file,"OPEN");
  TTree *eventTree=(TTree*)inf->Get("mEventTree");
  
  MyEvent *ev=new MyEvent();
  eventTree->SetBranchAddress("branch",&ev);

  TPostScript *ps=new TPostScript("dAu_bgperrun.ps",111);
  TCanvas *c9=new TCanvas("c9","c9",800,600);
  c9->Divide(4,4);

  Int_t entry=0;
  Int_t runprev=0;
  Int_t pagina=0;
  while(eventTree->GetEntry(entry))
    {
      //if(ev->runId()<4061003)
      //{
      //entry++;
      // continue;
      //}
      //if(ev->runId()>4061025)
      //{
      // cout<<"end of this fill!"<<endl;
      // break;
      //}

      if(ev->runId()!=runprev && runprev!=0)
	{
	  if(ev->runId()<runprev)
	    {
	      cout<<"Something wrong!"<<endl;
	      return;
	    }
	  cout<<endl<<"new run: "<<ev->runId()<<endl<<endl;
	  pagina++;

	  Float_t total_mb=ratio_mb_40100->Integral();
	  Float_t bg_mb=ratio_mb_40100->Integral(21,25);
	  Float_t total_ht1=ratio_ht1_40100->Integral();
          Float_t bg_ht1=ratio_ht1_40100->Integral(21,25);
	  Float_t total_ht2=ratio_ht2_40100->Integral();
          Float_t bg_ht2=ratio_ht2_40100->Integral(21,25);
	  Float_t total_sn=ratio_sn_40100->Integral();
          Float_t bg_sn=ratio_sn_40100->Integral(21,25);

	  Float_t frac_mb=total_mb>0 ? bg_mb/total_mb : 0.;
	  Float_t frac_ht1=total_ht1>0 ? bg_ht1/total_ht1 : 0.;
	  Float_t frac_ht2=total_ht2>0 ? bg_ht2/total_ht2 : 0.;
	  Float_t frac_sn=total_sn>0 ? bg_sn/total_sn : 0.;
	  fout<<pagina<<"\t"<<runprev<<"\t"<<frac_mb<<"\t"<<total_mb<<"\t"<<frac_ht1<<"\t"<<total_ht1<<"\t"<<frac_ht2<<"\t"<<total_ht2<<"\t"<<frac_sn<<"\t"<<total_sn<<endl;

	  hfrac_mb->Fill(frac_mb,total_mb);//use weight
	  hfrac_ht1->Fill(frac_ht1,total_ht1);
	  hfrac_ht2->Fill(frac_ht2,total_ht2);
	  hfrac_sn->Fill(frac_sn,total_sn);

	  //create new ps page and plot previous run
	  ps->NewPage();
	  c9->cd(1);
	  ratio_mb_020->Draw();
	  c9->cd(2);
	  ratio_ht1_020->Draw();
	  c9->cd(3);
	  ratio_ht2_020->Draw();
	  c9->cd(4);
	  ratio_sn_020->Draw();
	  c9->cd(5);
	  ratio_mb_2040->Draw();
	  c9->cd(6);
	  ratio_ht1_2040->Draw();
	  c9->cd(7);
	  ratio_ht2_2040->Draw();
	  c9->cd(8);
	  ratio_sn_2040->Draw();
	  c9->cd(9);
	  ratio_mb_40100->Draw();
	  c9->cd(10);
	  ratio_ht1_40100->Draw();
	  c9->cd(11);
	  ratio_ht2_40100->Draw();
	  c9->cd(12);
	  ratio_sn_40100->Draw();
	  c9->cd(13);
	  TPaveText *pave=new TPaveText(.1,.1,.9,.9);
	  char label[20];
	  sprintf(label,"run %d",runprev);
	  pave->AddText(label);
	  pave->Draw();
	  c9->cd(0);
	  c9->Update();
	  
	  //reset hists
	  ratio_mb_020->Reset();
	  ratio_ht1_020->Reset();
	  ratio_ht2_020->Reset();
	  ratio_sn_020->Reset();
	  ratio_mb_2040->Reset();
	  ratio_ht1_2040->Reset();
	  ratio_ht2_2040->Reset();
	  ratio_sn_2040->Reset();
	  ratio_mb_40100->Reset();
	  ratio_ht1_40100->Reset();
	  ratio_ht2_40100->Reset();
	  ratio_sn_40100->Reset();

	  //set runprev
	  runprev=ev->runId();
	  
	}
      else if(ev->runId()!=runprev)
	{
	  cout<<endl<<"first run: "<<ev->runId()<<endl<<endl;
	  runprev=ev->runId();	  
	}

      Int_t nTrig=0;
      if(ev->runId()==0&&ev->trigger()==0)
	{
	  nTrig=1;
	  if(ev->highTowerAdc()>cuts->ht1AdcCUT) nTrig+=2;
	  if(ev->highTowerAdc()>cuts->ht2AdcCUT) nTrig+=4;	  
	  ev->setTrigger(nTrig);
	}


      Bool_t eventOK=kTRUE;
      TVector3 vPos=ev->vertex();
      //apply EVENT cuts
      if(TMath::Abs(vPos.Z())>cuts->vertexzCUT) eventOK=kFALSE;
      if(TMath::Abs(vPos.X())>cuts->vertexxCUT) eventOK=kFALSE;
      if(TMath::Abs(vPos.Y())>cuts->vertexyCUT) eventOK=kFALSE;
      if(vPos.Mag()<.000001) eventOK=kFALSE;
      if(ev->trigger()<1 && ev->getMcTrack()->id()==0) eventOK=kFALSE;

      if(ev->trigger()&2||ev->trigger()&4)
	{
	  if(ev->highTowerEnergy()<0.0001) eventOK=kFALSE;
	  if(isBad[ev->highTowerId()-1]==1) eventOK=kFALSE;
	}


      if(!eventOK)
	{
	  entry++;
	  continue;
	}

      if(ev->trigger()&2||ev->trigger()&4) h_trigger->Fill(ev->highTowerId());

      Int_t t=ev->trigger();
      Float_t en=ev->energyInBarrel();
      Float_t pt=ev->momentumInTpc();
      Float_t ratio=TMath::Abs(en+pt)>0 ? en/(en+pt) : -.1;
            
      if(ev->refMult()>17)
	{
	  if(t&1) {ratio_mb_020->Fill(ratio); Ratio_mb_020->Fill(ratio);}
	  if(t&2) {ratio_ht1_020->Fill(ratio); Ratio_ht1_020->Fill(ratio);}
	  if(t&4) {ratio_ht2_020->Fill(ratio); Ratio_ht2_020->Fill(ratio);}
	  if(t&8) {ratio_sn_020->Fill(ratio); Ratio_sn_020->Fill(ratio);}
	}
      else if(ev->refMult()>11)
	{
	  if(t&1) {ratio_mb_2040->Fill(ratio); Ratio_mb_2040->Fill(ratio);}
	  if(t&2) {ratio_ht1_2040->Fill(ratio); Ratio_ht1_2040->Fill(ratio);}
	  if(t&4) {ratio_ht2_2040->Fill(ratio); Ratio_ht2_2040->Fill(ratio);}
	  if(t&8) {ratio_sn_2040->Fill(ratio); Ratio_sn_2040->Fill(ratio);}
	}
      else if(ev->refMult()>=0)
	{

	  if(t&1) {ratio_mb_40100->Fill(ratio); Ratio_mb_40100->Fill(ratio);}
          if(t&2) {ratio_ht1_40100->Fill(ratio); Ratio_ht1_40100->Fill(ratio);}
          if(t&4) {ratio_ht2_40100->Fill(ratio); Ratio_ht2_40100->Fill(ratio);}
          if(t&8) {ratio_sn_40100->Fill(ratio); Ratio_sn_40100->Fill(ratio);}
	}
      
      if(t&1){
	tpc_mb->Fill(ev->goodPrimaries());
	ftpc_mb->Fill(ev->refMult());
	bemc_mb->Fill(ev->numberOfPoints());
	EvsE_mb->Fill(ev->momentumInTpc(),ev->energyInBarrel());
	if(ratio<cuts->ratioCUT){
	  tpc_mb_cut->Fill(ev->goodPrimaries());
	  ftpc_mb_cut->Fill(ev->refMult());
	  bemc_mb_cut->Fill(ev->numberOfPoints());
	}
      }
      if(t&2){
	tpc_ht1->Fill(ev->goodPrimaries());
	ftpc_ht1->Fill(ev->refMult());
	bemc_ht1->Fill(ev->numberOfPoints());
	EvsE_ht1->Fill(ev->momentumInTpc(),ev->energyInBarrel());
	if(ratio<cuts->ratioCUT){
	  tpc_ht1_cut->Fill(ev->goodPrimaries());
	  ftpc_ht1_cut->Fill(ev->refMult());
	  bemc_ht1_cut->Fill(ev->numberOfPoints());
	}
      } 
      if(t&4){
	tpc_ht2->Fill(ev->goodPrimaries());
	ftpc_ht2->Fill(ev->refMult());
	bemc_ht2->Fill(ev->numberOfPoints());
	EvsE_ht2->Fill(ev->momentumInTpc(),ev->energyInBarrel());
	if(ratio<cuts->ratioCUT){
	  tpc_ht2_cut->Fill(ev->goodPrimaries());
	  ftpc_ht2_cut->Fill(ev->refMult());
	  bemc_ht2_cut->Fill(ev->numberOfPoints());
	}
      } 
      if(t&8){
	tpc_sn->Fill(ev->goodPrimaries());
	ftpc_sn->Fill(ev->refMult());
	bemc_sn->Fill(ev->numberOfPoints());
	EvsE_sn->Fill(ev->momentumInTpc(),ev->energyInBarrel());
	if(ratio<cuts->ratioCUT){
	  tpc_sn_cut->Fill(ev->goodPrimaries());
	  ftpc_sn_cut->Fill(ev->refMult());
	  bemc_sn_cut->Fill(ev->numberOfPoints());
	}
      }
 
      //get neutral points
      MyPoint *p;
      TClonesArray *clA=(TClonesArray*)ev->getPointArray();
      for(Int_t j=0;j<clA->GetEntries();j++)
        {
          p=(MyPoint*)clA->At(j);
          TVector3 pPos=p->position();
          TVector3 pMom=pPos-vPos;
          pMom.SetMag(p->energy());
	
	  if(p->distanceToTrack()<cuts->neutralCUT) continue;
          if(p->nHitsEta()<cuts->etaHitsCUT) continue;
          if(p->nHitsPhi()<cuts->phiHitsCUT) continue;
          if(pPos.PseudoRapidity()<cuts->etaMinCUT) continue;
          if(pPos.PseudoRapidity()>cuts->etaMaxCUT) continue;

          Float_t pT=pMom.Pt();
	  if(pMom.PseudoRapidity()>cuts->rapidityMinCUT && pMom.PseudoRapidity()<cuts->rapidityMaxCUT)
	    {
	      if(ev->refMult()>17)
		{
		  if(t&1)
		    {
		      ptspec_mb_020->Fill(pT);
		      if(ratio>=cuts->ratioCUT) 
			ptspec_mb_bg_020->Fill(pT);
		      else
			ptspec_mb_cut_020->Fill(pT);
		    }
		  if(t&2)
		    {
		      ptspec_ht1_020->Fill(pT);
		      if(ratio>=cuts->ratioCUT)
			ptspec_ht1_bg_020->Fill(pT);
		      else
			ptspec_ht1_cut_020->Fill(pT);
		    }
		  if(t&4)
		    {
		      ptspec_ht2_020->Fill(pT);
		      if(ratio>=cuts->ratioCUT)
			ptspec_ht2_bg_020->Fill(pT);
		      else
			ptspec_ht2_cut_020->Fill(pT);
		    }
		  if(t&8)
		    {
		      ptspec_sn_020->Fill(pT);
		      if(ratio>=cuts->ratioCUT)
			ptspec_sn_bg_020->Fill(pT);
		      else
			ptspec_sn_cut_020->Fill(pT);
		    }
		}
	      else if(ev->refMult()>11)
		{
		  if(t&1)
		    {
		      ptspec_mb_2040->Fill(pT);
		      if(ratio>=cuts->ratioCUT)
			ptspec_mb_bg_2040->Fill(pT);
		      else
			ptspec_mb_cut_2040->Fill(pT);
		    }
                  if(t&2)
                    {
                      ptspec_ht1_2040->Fill(pT);
                      if(ratio>=cuts->ratioCUT)
                        ptspec_ht1_bg_2040->Fill(pT);
                      else
                        ptspec_ht1_cut_2040->Fill(pT);
                    }
                  if(t&4)
                    {
                      ptspec_ht2_2040->Fill(pT);
                      if(ratio>=cuts->ratioCUT)
                        ptspec_ht2_bg_2040->Fill(pT);
                      else
                        ptspec_ht2_cut_2040->Fill(pT);
                    }
                  if(t&8)
                    {
                      ptspec_sn_2040->Fill(pT);
                      if(ratio>=cuts->ratioCUT)
                        ptspec_sn_bg_2040->Fill(pT);
                      else
                        ptspec_sn_cut_2040->Fill(pT);
                    }
		}
	      else if(ev->refMult()>=0)
                {
                  if(t&1)
		    {
		      ptspec_mb_40100->Fill(pT);
		      if(ratio>=cuts->ratioCUT)
			ptspec_mb_bg_40100->Fill(pT);
		      else
			ptspec_mb_cut_40100->Fill(pT);
		    }
                  if(t&2)
                    {
                      ptspec_ht1_40100->Fill(pT);
                      if(ratio>=cuts->ratioCUT)
                        ptspec_ht1_bg_40100->Fill(pT);
                      else
                        ptspec_ht1_cut_40100->Fill(pT);
                    }
                  if(t&4)
                    {
                      ptspec_ht2_40100->Fill(pT);
		      if(ratio<0.7) etaphi_fill3088b_ht2->Fill(pPos.PseudoRapidity(),pPos.Phi());
                      if(ratio>=cuts->ratioCUT)
			{
                        ptspec_ht2_bg_40100->Fill(pT);
			if(ev->runId()>=4060055&&ev->runId()<=4060088) etaphi_fill3085_ht2->Fill(pPos.PseudoRapidity(),pPos.Phi());
			if(ev->runId()>=4061003&&ev->runId()<=4061025) etaphi_fill3088_ht2->Fill(pPos.PseudoRapidity(),pPos.Phi());
			}
                      else
                        ptspec_ht2_cut_40100->Fill(pT);
                    }
                  if(t&8)
                    {
                      ptspec_sn_40100->Fill(pT);
                      if(ratio>=cuts->ratioCUT)
			{
			  ptspec_sn_bg_40100->Fill(pT);
			  etaphi_sn->Fill(pPos.PseudoRapidity(),pPos.Phi());
			  if(ev->runId()>=4060055&&ev->runId()<=4060088) etaphi_fill3085_sn->Fill(pPos.PseudoRapidity(),pPos.Phi());
			  if(ev->runId()>=4061003&&ev->runId()<=4061025) etaphi_fill3088_sn->Fill(pPos.PseudoRapidity(),pPos.Phi());
			}
                      else
                        ptspec_sn_cut_40100->Fill(pT);
                    }
                }

	    }

	}
      
      entry++;
    }
  
  fout.close();
  
  ps->Close();

  TString oname(file);
  oname.Prepend("outputBg113005_");
  TFile o(oname.Data(),"RECREATE");

  etaphi_fill3085_ht2->Write();
  etaphi_fill3088_ht2->Write();
  etaphi_fill3085_sn->Write();
  etaphi_fill3088_sn->Write();
  etaphi_fill3088b_ht2->Write();
  etaphi_sn->Write();

  hfrac_mb->Write();
  hfrac_ht1->Write();
  hfrac_ht2->Write();
  hfrac_sn->Write();  
  h_trigger->Write();

  Ratio_mb_020->Write();
  Ratio_ht1_020->Write();
  Ratio_ht2_020->Write();
  Ratio_sn_020->Write();
  Ratio_mb_2040->Write();
  Ratio_ht1_2040->Write();
  Ratio_ht2_2040->Write();
  Ratio_sn_2040->Write();
  Ratio_mb_40100->Write();
  Ratio_ht1_40100->Write();
  Ratio_ht2_40100->Write();
  Ratio_sn_40100->Write();

  EvsE_mb->Write();
  EvsE_ht1->Write();
  EvsE_ht2->Write();
  EvsE_sn->Write();

  ptspec_mb_020->Write();
  ptspec_mb_cut_020->Write();
  ptspec_mb_bg_020->Write();
  ptspec_ht1_020->Write();
  ptspec_ht1_cut_020->Write();
  ptspec_ht1_bg_020->Write();
  ptspec_ht2_020->Write();
  ptspec_ht2_cut_020->Write();
  ptspec_ht2_bg_020->Write();
  ptspec_sn_020->Write();
  ptspec_sn_cut_020->Write();
  ptspec_sn_bg_020->Write();

  ptspec_mb_2040->Write();
  ptspec_mb_cut_2040->Write();
  ptspec_mb_bg_2040->Write();
  ptspec_ht1_2040->Write();
  ptspec_ht1_cut_2040->Write();
  ptspec_ht1_bg_2040->Write();
  ptspec_ht2_2040->Write();
  ptspec_ht2_cut_2040->Write();
  ptspec_ht2_bg_2040->Write();
  ptspec_sn_2040->Write();
  ptspec_sn_cut_2040->Write();
  ptspec_sn_bg_2040->Write();

  ptspec_mb_40100->Write();
  ptspec_mb_cut_40100->Write();
  ptspec_mb_bg_40100->Write();
  ptspec_ht1_40100->Write();
  ptspec_ht1_cut_40100->Write();
  ptspec_ht1_bg_40100->Write();
  ptspec_ht2_40100->Write();
  ptspec_ht2_cut_40100->Write();
  ptspec_ht2_bg_40100->Write();
  ptspec_sn_40100->Write();
  ptspec_sn_cut_40100->Write();
  ptspec_sn_bg_40100->Write();

  tpc_mb->Write();
  tpc_mb_cut->Write();
  tpc_ht1->Write();
  tpc_ht1_cut->Write();
  tpc_ht2->Write();
  tpc_ht2_cut->Write();
  tpc_sn->Write();
  tpc_sn_cut->Write();
  ftpc_mb->Write();
  ftpc_mb_cut->Write();
  ftpc_ht1->Write();
  ftpc_ht1_cut->Write();
  ftpc_ht2->Write();
  ftpc_ht2_cut->Write();
  ftpc_sn->Write();
  ftpc_sn_cut->Write();
  bemc_mb->Write();
  bemc_mb_cut->Write();
  bemc_ht1->Write();
  bemc_ht1_cut->Write();
  bemc_ht2->Write();
  bemc_ht2_cut->Write();
  bemc_sn->Write();
  bemc_sn_cut->Write();

  o.Close();

}


void dAuBackground::runSim(const char *file)
{ 
  TH1F *ratio_pythia=new TH1F("ratio_pythia","pythia, E over pT", 25,0.,1.);
  TH1F *energy_pythia=new TH1F("energy_pythia","energy spectrum",200,0.,100.);
  TH1F *tpcpt_pythia=new TH1F("tpcpt_pythia","TPC pT spectrum",200,0.,100.);
  TH1F *spec_pythia=new TH1F("spec_pythia","pT spectrum neutral points",30,0.,15.);
  TH1F *spec_pythia_bg=new TH1F("spec_pythia_bg","pT spectrum neutral points",30,0.,15.);

  TH1F *ratio_pythia_ht1=new TH1F("ratio_pythia_ht1","pythia, E over pT ht1", 25,0.,1.);
  TH1F *energy_pythia_ht1=new TH1F("energy_pythia_ht1","energy spectrum ht1",200,0.,100.);
  TH1F *tpcpt_pythia_ht1=new TH1F("tpcpt_pythia_ht1","TPC pT spectrum ht1",200,0.,100.);
  TH1F *spec_pythia_ht1=new TH1F("spec_pythia_ht1","pT spectrum neutral points ht1",30,0.,15.);
  TH1F *spec_pythia_bg_ht1=new TH1F("spec_pythia_bg_ht1","pT spectrum neutral points ht1",30,0.,15.);
  TH1F *spec_pythia_ht1_cut=new TH1F("spec_pythia_ht1_cut","pT spectrum neutral points ht1 after cut",30,0.,15.);

  TH1F *pythia_tpc_ht1=new TH1F("pythia_tpc_ht1","tpc mult. ht-1 sim",100,0.,100.);
  TH1F *pythia_tpc_ht1_cut=new TH1F("pythia_tpc_ht1_cut","tpc mult. ht-1 sim after cut",100,0.,100.);
  TH1F *pythia_bemc_ht1=new TH1F("pythia_bemc_ht1","bemc mult. ht-1 sim",60,0.,60.);  
  TH1F *pythia_bemc_ht1_cut=new TH1F("pythia_bemc_ht1_cut","bemc mult. ht-1 sim after cut",60,0.,60.);

  TFile *inf=new TFile(file,"OPEN");
  TTree *eventTree=(TTree*)inf->Get("mEventTree");
  MyEvent *ev=new MyEvent();
  eventTree->SetBranchAddress("branch",&ev);

  TPostScript *ps=new TPostScript("dAu_bgPythia.ps",111);
  TCanvas *c9=new TCanvas("c9","c9",800,1000);
  c9->Divide(2,4);  

  Int_t entry=0;
  while(eventTree->GetEntry(entry))
    {
      if(entry%10000==0) cout<<entry<<endl;

      Bool_t eventOK=kTRUE;
      TVector3 vPos=ev->vertex();

      //apply EVENT cuts
      if(TMath::Abs(vPos.Z())>cuts->vertexzCUT) eventOK=kFALSE;
      if(TMath::Abs(vPos.X())>cuts->vertexxCUT) eventOK=kFALSE;
      if(TMath::Abs(vPos.Y())>cuts->vertexyCUT) eventOK=kFALSE;
      if(vPos.Mag()<.000001) eventOK=kFALSE;

      if(!eventOK)
        {
          entry++;
          continue;
        }
  
      Float_t en=ev->energyInBarrel();
      Float_t pt=ev->momentumInTpc();
      Float_t ratio=TMath::Abs(en+pt)>0 ? en/(en+pt) : 2.;

      ratio_pythia->Fill(ratio);
      energy_pythia->Fill(en);
      tpcpt_pythia->Fill(pt);
      if(ev->highTowerAdc()>cuts->ht1AdcCUT)
	{
	  pythia_tpc_ht1->Fill(ev->goodPrimaries());
	  pythia_bemc_ht1->Fill(ev->numberOfPoints());
	  ratio_pythia_ht1->Fill(ratio);
	  energy_pythia_ht1->Fill(en);
	  tpcpt_pythia_ht1->Fill(pt);
	  if(ratio<cuts->ratioCUT)
	    {
	      pythia_tpc_ht1_cut->Fill(ev->goodPrimaries());
	      pythia_bemc_ht1_cut->Fill(ev->numberOfPoints());
	    }
	}
      

      //get neutral points
      MyPoint *p;
      TClonesArray *clA=(TClonesArray*)ev->getPointArray();
      for(Int_t j=0;j<clA->GetEntries();j++)
        {
          p=(MyPoint*)clA->At(j);
          TVector3 pPos=p->position();
          TVector3 pMom=pPos-vPos;
          pMom.SetMag(p->energy());

          if(p->distanceToTrack()<cuts->neutralCUT) continue;
          if(p->nHitsEta()<cuts->etaHitsCUT) continue;
          if(p->nHitsPhi()<cuts->phiHitsCUT) continue;
          if(pPos.PseudoRapidity()<cuts->etaMinCUT) continue;
          if(pPos.PseudoRapidity()>cuts->etaMaxCUT) continue;

          Float_t pT=pMom.Pt();
          if(pMom.PseudoRapidity()>cuts->rapidityMinCUT && pMom.PseudoRapidity()<cuts->rapidityMaxCUT)
            {
	      spec_pythia->Fill(pT);
	      if(ratio>=cuts->ratioCUT) spec_pythia_bg->Fill(pT);
	      if(ev->highTowerAdc()>cuts->ht1AdcCUT)
		{
		  spec_pythia_ht1->Fill(pT);
		  if(ratio>=cuts->ratioCUT) spec_pythia_bg_ht1->Fill(pT);
		  else spec_pythia_ht1_cut->Fill(pT);
		}
	    }



	}
    
      entry++;
    }

  TFile f("hijing_tryout.root","RECREATE");
  spec_pythia_ht1->Write();
  spec_pythia_ht1_cut->Write();
  spec_pythia_bg_ht1->Write();
  ratio_pythia_ht1->Write();
  
  pythia_tpc_ht1->Write();
  pythia_tpc_ht1_cut->Write();
  pythia_bemc_ht1->Write();
  pythia_bemc_ht1_cut->Write();
  f.Close();

  ps->NewPage();
  c9->cd(1);
  ratio_pythia->Draw();
  c9->cd(2);
  ratio_pythia_ht1->Draw();
  c9->cd(3);
  energy_pythia->Draw();
  gPad->SetLogy();
  c9->cd(4);
  energy_pythia_ht1->Draw();
  gPad->SetLogy();
  c9->cd(5);
  tpcpt_pythia->Draw();
  gPad->SetLogy();
  c9->cd(6);
  tpcpt_pythia_ht1->Draw();
  gPad->SetLogy();
  c9->cd(7);
  spec_pythia->Draw();
  spec_pythia_bg->SetLineColor(6);
  spec_pythia_bg->Draw("same");
  gPad->SetLogy();
  c9->cd(8);
  spec_pythia_ht1->Draw();
  spec_pythia_bg_ht1->SetLineColor(6);
  spec_pythia_bg_ht1->Draw("same");
  gPad->SetLogy();
  c9->cd(0);
  c9->Update();
  ps->Close();
}
