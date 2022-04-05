// $Id: St2009pubJS_histo.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author :  Justin Stevens, IUCF

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009pubJSMaker.h"

//________________________________________________
//________________________________________________
void
St2009pubJSMaker::initHistos(){
  const float PI=TMath::Pi();
  TString core="JS"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file


  //...... data histograms
  memset(hA,0,sizeof(hA));
  memset(hB,0,sizeof(hB));
  TH1 *h; TH2 *h2;
  //TList *Lx;  TLine *ln; 
    

  //******* ETOW QA histos *********
  
  // beam background
  hB[0]=h2=new TH2F(core+"etowTotPostTr","All energy in ETOW, after track pT cut ; eta [1,2];phi",12,-0.5,11.5 ,60,-PI+.05,PI+.05);
  hA[0]=h=new TH2F(core+"etowHighPostTr",Form("Endcap Towers E>%.1f, after track pT cut ; eta [1,2];phi",par_highEtow),12,-0.5,11.5 ,60,-PI+.05,PI+.05);

  hB[1]=h2=new TH2F(core+"etowTotPreNear","All energy in ETOW, before 2x2/nearJet cut; eta [1,2];phi",12,-0.5,11.5 ,60,-PI+.05,PI+.05);
  hA[1]=h=new TH2F(core+"etowHighPreNear",Form("Endcap Towers E>%.1f, before 2x2/nearJet cut; eta [1,2];phi",par_highEtow),12,-0.5,11.5 ,60,-PI+.05,PI+.05);

  hB[2]=h2=new TH2F(core+"etowTotPreAway","All energy in ETOW, before awayET cut; eta [1,2];phi",12,-0.5,11.5 ,60,-PI+.05,PI+.05);
  hA[2]=h=new TH2F(core+"etowHighPreAway",Form("Endcap Towers E>%.1f, before awayET cut; eta [1,2];phi",par_highEtow),12,-0.5,11.5 ,60,-PI+.05,PI+.05);

  // vertZ dependence of gain
  hA[3]=h=new TH1F(core+"etowEneZ1","Endcap Tower E, Z vert [-100,-50]; Tower E (GeV)",300,0,30);
  hA[4]=h=new TH1F(core+"etowEneZ2","Endcap Tower E, Z vert [-50,0]; Tower E (GeV)",300,0,30);
  hA[5]=h=new TH1F(core+"etowEneZ3","Endcap Tower E, Z vert [0,50]; Tower E (GeV)",300,0,30);
  hA[6]=h=new TH1F(core+"etowEneZ4","Endcap Tower E, Z vert [50,100]; Tower E (GeV)",300,0,30);

  //****** Conditions on away side cuts ******** 
  hA[7]=h=new TH1F(core+"awayNTow",Form(" away # Towers with Et > %.1f on away side ; # towers fired",par_countTowEt),20,0,20);
  hA[8]=h=new TH1F(core+"awayNTr",Form(" away # Tracks with Pt > %.1f on away side ; # tracks",par_countTrPt),20,0,20);

  hA[9]=h=new TH1F(core+"awayCond1",Form("(# away side tracks w/ pT>%.1f) > %d & awayET > %.1f  ; 2x2 Cluster ET (GeV)",par_countTrPt,par_awayNTrCut,par_awayTotET), 100,0,100);
  hA[10]=h=new TH1F(core+"awayCond2",Form("(# away side tracks w/ pT>%.1f) <= %d & awayET > %.1f ; 2x2 Cluster  ET (GeV)",par_countTrPt,par_awayNTrCut,par_awayTotET), 100,0,100);

  hA[11]=h=new TH1F(core+"nearNTow",Form(" near # Towers with Et > %.1f on near side ; # towers fired",par_countTowEt),20,0,20);
  hA[12]=h=new TH1F(core+"nearNTr",Form(" near # Tracks with Pt > %.1f on near side ; # tracks",par_countTrPt),20,0,20);
  
  //Look at event info that pass all W cuts but have low E
  hA[13]=h=new TH1F(core+"vertex_LT25","Z vertex, after all cuts cluster ET < 10; Z vertex (cm)",100,-200,200);
  hA[14]=h=new TH1F(core+"awayEt_LT25","away ET, after all cuts cluster ET < 20; away ET",100,0,100);

  hA[15]=h=new TH1F(core+"nearNTow_LT25",Form(" near # Towers with Et > %.1f on near side ; # towers fired",par_countTowEt),20,0,20);
  hA[16]=h=new TH1F(core+"nearNTr_LT25",Form(" near # Tracks with Pt > %.1f on near side ; # tracks",par_countTrPt),20,0,20);
  
  hA[18]=h=new TH1F(core+"Zv_LT25",Form("Z vertex ; Z vertex (cm)"),100,-200,200);
  hA[19]=h=new TH1F(core+"VRf_LT25","PPV Vertex rank, funny X-axis; X=Log10(rank)+offset", 150, -9,25);
  hA[20]=h=new TH1F(core+"bX48_LT25","bx 48 ; bXing= raw bx48",128,-0.5,127.5);
  hA[21]=h=new TH1F(core+"bX7_LT25","bx 7 ; bXing= raw bx7",128,-0.5,127.5);
  //cluster histos
  hA[22]=new TH2F(core+"2D_LT25","2D (eta,phi) distribution of QCD background; detector eta; detector phi",10,-1.0,1.0,24,-PI,PI);
  hA[23]=h=new TH1F(core+"nTower_LT25",Form(" # towers with ADC>kSigPed in 2x2 cluster; # towers"),10,0,10);
  hA[24]=new TH2F(core+"nearRvEta_LT25","2x2 ET / NearJet ET; detector eta; 2x2/nearJet",10,-1.0,1.0,50,0.,1.);
  
  //Transvers mass plots from muDst
  hA[25]=new TH1F(core+"hadRecoilPt","Hadronic recoil pt; Hadronic recoil pt",50,0,50);
  hA[26]=h=new TH1F(core+"RecoilEta","Hadronic Recoil #eta; Hadronic Recoil #eta",50,-4,4);
  hA[27]=new TH1F(core+"electronRecoPt","Electron Reco pt ; 2x2 cluster ET",60,0,60);
  hA[28]=new TH1F(core+"neutrinoRecoPt","Neutrino pt (ie -(had. recoil - ele) pT); Reco Neutrino pT",60,0,60);
  hA[29]=new TH1F(core+"mT","Reco Transverse Mass; Reco m_{T}",100,0,100);
  hA[30]=h=new TH2F(core+"mTvsEleEt","Reco Electron ET vs Reco Transverse Mass; m_{T} GeV;2x2 Cluster ET GeV",100,0,100,60,0,60);
  
  //Kinematics
  hA[31]=h=new TH1F(core+"ElectronE_neg","Electron E for #eta < -0.8;  Reco Electron E",100,0,100);
  hA[32]=h=new TH1F(core+"ElectronE_pos","Electron E for #eta > 0.8;  Reco Electron E",100,0,100);
  hA[33]=h=new TH1F(core+"ElectronE_zero","Electron E for -0.1 < #eta < 0.1; Reco Electron E",100,0,100);
  
  hA[34]=h=new TH1F(core+"RecoPlusWpL","RecoPlus W pL; Reco W pL",100,-100,100);
  hA[35]=h=new TH1F(core+"RecoMinusWpL","RecoMinus W pL; RecoMinus W pL",100,-100,100);
  

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  for(int i=0;i<mxHB;i++) {
    if(  hB[i]==0) continue;
    HList->Add( hB[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}


// $Log: St2009pubJS_histo.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
