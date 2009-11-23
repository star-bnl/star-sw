// $Id: St2009pubMc_histo.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author :  Justin Stevens, IUCF

#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include "St2009pubMcMaker.h"

//________________________________________________
//________________________________________________
void
St2009pubMcMaker::initHistos(){
  //const float PI=TMath::Pi();
  TString core="MC"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TH1 *h; //TH2 *h2;
  //TList *Lx;  TLine *ln; 
    

  //Quantities from geant and correlations w/ reconstructed values 

  hA[1]=h=new TH1F(core+"Wpt","W pt from Geant; W pt",50,0,50);
  hA[2]=h=new TH1F(core+"WpL","W pL from Geant; W pL",100,-100,100);
  hA[3]=h=new TH1F(core+"WminusHadRecoilpt","W pt (from Geant) - Hadronic Recoil pt ; W - hadronic recoil pt",100,-20,20);
  hA[4]=h=new TH2F(core+"hadRec_Wpt","Hadronic Recoil pt vs W pt from Geant; W pT; hadronic recoil pt",100,0,20,100,0,20); 
  hA[5]=h=new TH1F(core+"hadRecoilPt","Hadronic recoil pt; Hadronic recoil pt",50,0,50);
  hA[6]=h=new TH2F(core+"delPhi_Wpt","#Delta #phi (W + hadronic recoil) vs W pT from Geant; W pT; #Delta #phi",50,0,50,100,-4,4);
  hA[7]=h=new TH2F(core+"WptminusHad_Wpt","W pt (from Geant) - Hadronic Recoil pt vs W pt; W pt; W - hadronic recoil pt",50,0,50,100,-20,20);
  hA[8]=h=new TH2F(core+"delPhi_Recoilpt","#Delta #phi (W + hadronic recoil) vs Hadronic Recoil pt; Hadronic Recoil pT; #Delta #phi",50,0,50,100,-4,4);
  
  //electron plots
  hA[9]=h=new TH1F(core+"electronGeantPt","Electron pt from Geant; Geant electron pt",60,0,60);
  hA[10]=h=new TH1F(core+"electronRecoPt","Electron Reco pt ; 2x2 cluster ET",60,0,60);
  hA[11]=h=new TH2F(core+"electronRecovsGeant","Electron Reco pt vs Geant pt; Geant electron pt; 2x2 cluster Et",60,0,60,60,0,60);
  hA[12]=h=new TH2F(core+"diffElectronPtvsGeantpt","Electron pt (Geant - Reco) vs Geant; Geant Electron pT; Geant - Reco Electron pT",60,0,60,100,-20,20); 
  
  //neutrino plots
  hA[13]=h=new TH1F(core+"neutrinoRecoPt","Neutrino pt (ie -(had. recoil - ele) pT); Reco Neutrino pT",60,0,60);
  hA[14]=h=new TH1F(core+"neutrinoGeantPt","Neutrino pt from Geant; Geant neutrino pT",60,0,60);
  hA[15]=h=new TH2F(core+"neutrinoRecovsGeant","Neutrino Reco pt vs Geant pt; Geant neutrino pt; Reco neutrino Et",60,0,60,60,0,60);
  hA[16]=h=new TH1F(core+"diffNeutrinoPt","Neutrino pt (Geant - Reco); Geant - Reco Neutrino pT",100,-20,20);
  hA[17]=h=new TH2F(core+"eleG_neutrinoG","Electron Geant pt vs Neutrino Geant pt; Geant Neutrino pt; Geant Electron pt",60,0,60,60,0,60);
  
  //Transvers Mass Reco
  hA[18]=h=new TH2F(core+"delPhiGeant_Reco","Reco #phi_{e,#nu} vs Geant #phi_{e,#nu}; #phi_{e,#nu} Reco ; #phi_{e,#nu} Geant",50,-6,6,50,-6,6);  
  hA[19]=h=new TH1F(core+"delPhiGeantminusReco","Reco cos(#phi_{e,#nu}) minus Geant cos(#phi_{e,#nu}); Geant-Reco ",50,-.2,.2);
  hA[20]=h=new TH1F(core+"mT","Reco Transverse Mass; Reco m_{T}",100,0,100);
  hA[21]=h=new TH1F(core+"gMT","Geant Transverse Mass; Geant m_{T}",100,0,100);
  hA[22]=h=new TH2F(core+"mTvsEleEt","Reco Electron ET vs Reco Transverse Mass; m_{T} GeV;2x2 Cluster ET GeV",100,0,100,60,0,60);
  hA[23]=h=new TH1F(core+"GmTminusmT","Geant - Reco Transverse Mass; Geant - Reco mT (GeV)",100,-20,20);

  //Hadronic Recoil Eta
  hA[24]=h=new TH1F(core+"Weta","W #eta from Geant; W #eta from Geant",100,-4,4);
  hA[25]=h=new TH1F(core+"RecoilEta_all","Hadronic Recoil #eta (for all W #eta); Hadronic Recoil #eta",50,-4,4);
  hA[26]=h=new TH2F(core+"RecoilEtaAll_Wpt","Hadronic Recoil #eta (for all W #eta) vs W pt (from Geant); W pt; Hadronic Recoil #eta",50,0,50,50,-4,4);
  hA[27]=h=new TH1F(core+"RecoilEta_WetaPos","Hadronic Recoil #eta (for W #eta > 0); Hadronic Recoil #eta",50,-4,4);
  hA[28]=h=new TH1F(core+"RecoilEta_WetaNeg","Hadronic Recoil #eta (for W #eta < 0); Hadronic Recoil #eta",50,-4,4);
    

  //Reconstructing W pL
  hA[29]=h=new TH1F(core+"RecoPlusWpL","Reco W pL; RecoPlus W pL",100,-200,200);
  hA[30]=h=new TH1F(core+"GeantMinusRecoPlusWpL","Geant W pL - Reco W pL; Geant - RecoPlus W pL",100,-200,200);
  hA[31]=h=new TH2F(core+"RecoPlusvsGeantWpL","RecoPlus W pL vs Geant W pL ; Geant W pL;Reco W pL",100,-200,200,200,-200,200);
  hA[32]=h=new TH1F(core+"RecoMinusWpL","RecoMinus W pL; RecoMinus W pL",100,-200,200);
  hA[33]=h=new TH1F(core+"GeantMinusRecoMinusWpL","Geant W pL - RecoMinus W pL; Geant - RecoMinus W pL",100,-200,200);
  hA[34]=h=new TH2F(core+"RecoMinusvsGeantWpL","RecoMinus W pL vs Geant W pL ; Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);

  hA[35]=h=new TH2F(core+"RecoMinusvsGeantWpL_neg","RecoMinus W pL vs Geant W pL (for ele #eta < -0.8); Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);
  hA[36]=h=new TH2F(core+"RecoMinusvsGeantWpL_pos","RecoMinus W pL vs Geant W pL (for ele #eta > 0.8); Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);
  hA[37]=h=new TH2F(core+"RecoPlusvsGeantWpL_neg","RecoPlus W pL vs Geant W pL (for ele #eta < -0.8); Geant W pL;RecoPlus W pL",100,-200,200,100,-200,200);
  hA[38]=h=new TH2F(core+"RecoPlusvsGeantWpL_pos","RecoPlus W pL vs Geant W pL (for ele #eta > 0.8); Geant W pL;RecoPlus W pL",100,-200,200,100,-200,200);
  
  //ele E vs W pL for different thetas
  hA[39]=h=new TH2F(core+"ElectronEvsWpL_neg","Electron E vs W pL (from Geant) for #eta < -0.8; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[40]=h=new TH2F(core+"ElectronEvsWpL_pos","Electron E vs W pL (from Geant) for #eta > 0.8; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[41]=h=new TH2F(core+"ElectronEvsWpL_zero","Electron E vs W pL (from Geant) for -0.1 < #eta < 0.1; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[42]=h=new TH1F(core+"ElectronE_neg","Electron E for #eta < -0.8;  Reco Electron E",100,0,100);
  hA[43]=h=new TH1F(core+"ElectronE_pos","Electron E for #eta > 0.8;  Reco Electron E",100,0,100);
  hA[44]=h=new TH1F(core+"ElectronE_zero","Electron E for -0.1 < #eta < 0.1; Reco Electron E",100,0,100);

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}

// $Log: St2009pubMc_histo.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
