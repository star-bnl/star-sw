// $Id: St2009pubMc_histo.cxx,v 1.4 2011/09/14 14:23:21 stevens4 Exp $
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
  memset(hB,0,sizeof(hB));
  TH1 *h; TH2 *h2;
  //TList *Lx;  TLine *ln; 
  const float PI=TMath::Pi();
    

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

  //free 45-49

  //W efficiency histos
  hA[50]=h=new TH1F(core+"eleETall","pt of all leptons ; lepton pt (from Geant)",100,1,101);
  hA[51]=h=new TH1F(core+"eleETtrig","pt of leptons that satisfy trigger ; lepton pt (from Geant)",100,1,101);
  hA[52]=h=new TH1F(core+"eleETvert","pt of leptons that w/ good vertex ; lepton pt (from Geant)",100,1,101);
  hA[53]=h=new TH1F(core+"eleETreco","pt of leptons that pass W cuts ; lepton pt (from Geant)",100,1,101);
  hA[54]=h=new TH1F(core+"eleEtaAll","#eta of all leptons ; lepton #eta (from Geant)",300,-1.5,1.5);
  hA[55]=h=new TH1F(core+"eleEtaTrig","#eta of leptons that satisfy trigger ; lepton #eta (from Geant)",300,-1.5,1.5);
  hA[56]=h=new TH1F(core+"eleEtaVert","#eta of leptons that w/ good vertex ; lepton #eta (from Geant)",300,-1.5,1.5);
  hA[57]=h=new TH1F(core+"eleEtaReco","#eta of leptons that pass W cuts ; lepton #eta (from Geant)",300,-1.5,1.5);
  hA[58]=h=new TH1F(core+"eleZvertAll","zVertex of all events ; zVertex (from Geant)",100,-200,200);
  hA[59]=h=new TH1F(core+"eleZvertTrig","zVertex of events that satisfy trigger ; zVertex (from Geant)",100,-200,200);
  hA[60]=h=new TH1F(core+"eleZvertVert","zVertex of events that w/ good vertex ; zVertex (from Geant)",100,-200,200);
  hA[61]=h=new TH1F(core+"eleZvertReco","zVertex of events that pass W cuts ; zVertex (from Geant)",100,-200,200);
  hA[62]=h=new TH1F(core+"elePhiAll","#phi of all leptons; lepton #phi (from Geant)",12,(-1.+15./180.)*PI,(1.+15./180.)*PI);
  hA[63]=h=new TH1F(core+"elePhiTrig","#phi of leptons that satisfy trigger ; lepton #phi (from Geant)",12,(-1.+15./180.)*PI,(1.+15./180.)*PI);
  hA[64]=h=new TH1F(core+"elePhiVert","#phi of leptons that w/ good vertex ; lepton #phi (from Geant)",12,(-1.+15./180.)*PI,(1.+15./180.)*PI);
  hA[65]=h=new TH1F(core+"elePhiReco","#phi of leptons that pass W cuts ; lepton #phi (from Geant)",12,(-1.+15./180.)*PI,(1.+15./180.)*PI);
  
  //different binning for ET histograms
  hA[68]=h=new TH1F(core+"eleETallJoe","pt of all leptons ; lepton pt (from Geant)",100,1,101);
  hA[69]=h=new TH1F(core+"eleETtrigJoe","pt of leptons with good trigger; lepton pt (from Geant)",100,1,101);
  hA[70]=h=new TH1F(core+"eleETvertJoe","pt of leptons with good vertex; lepton pt (from Geant)",100,1,101);
  hA[71]=h=new TH1F(core+"eleETrecoJoe","pt of reconstructed leptons; lepton pt (from Geant)",100,1,101);

  //plot for Scott to look at trigger effic W+ vs W-
  hB[66]=h2=new TH2F(core+"eleEta_ptPreTrig","Reconstructed lepton pt vs lepton detector #eta from Geant (before trig); lepton detector #eta; lepton pt (from Geant)",300,-1.5,1.5,100,0,100);
  hB[67]=h2=new TH2F(core+"eleEta_ptPostTrig","Reconstructed lepton pt vs lepton detector #eta from Geant (after trig); lepton detector #eta; lepton pt (from Geant)",300,-1.5,1.5,100,0,100);
  
  //x1 and x2 distributions
  hA[72]=h=new TH1F(core+"wRapid","Rapidity of W; W rapidity",100,-2,2);
  hA[73]=h=new TH1F(core+"x1","x1 distribution; x1",100,0,1);
  hA[74]=h=new TH1F(core+"x2","x2 distribution; x2",100,0,1);
  hA[75]=h=new TH2F(core+"x2_x1","x2 vs x1 ; x1; x2",100,0,1,100,0,1);
  hA[76]=h=new TH1F(core+"x1minusx2","x1 - x2; x1-x2",100,-1,1);

  //wrap phi on pair of barrel modules
  hA[77]=h=new TH1F(core+"elePhiModulePairAll","lepton #phi (modulo 12 degrees ie. pair of modules); #phi % 12 (degrees)",240,0.0,12.0);
  hA[78]=h=new TH1F(core+"elePhiModulePairTrig","lepton #phi with good trigger (modulo 12 degrees ie. pair of modules); #phi % 12 (degrees)",240,0.0,12.0);
  
  //detector eta plots for effic
  hA[79]=h=new TH1F(core+"eleDetEtaAll","Detector #eta; lepton detector #eta (from Geant)",300,-1.5,1.5);
  hA[80]=h=new TH1F(core+"eleDetEtaTrig","Detector #eta of leptons that satisfy trigger ; lepton detector #eta (from Geant)",300,-1.5,1.5);
  hA[81]=h=new TH1F(core+"eleDetEtaAllEtLT35","Detector #eta; lepton detector #eta (from Geant)",300,-1.5,1.5);
  hA[82]=h=new TH1F(core+"eleDetEtaTrigEtLT35","Detector #eta of leptons that satisfy trigger ; lepton detector #eta (from Geant)",300,-1.5,1.5);

  hB[83]=h2=new TH2F(core+"eleET_EtaAll","ET vs #eta of leptons that satisfy trigger ; lepton #eta (from Geant); lepton pt (from Geant)",300,-1.5,1.5,100,0,100);
  hB[84]=h2=new TH2F(core+"eleET_EtaTrig","lepton ET vs #eta ; lepton #eta (from Geant); lepton pt (from Geant)",300,-1.5,1.5,100,0,100);
  

  //tracking efficiency
  hA[85]=h=new TH1F(core+"eleETTrack","pt of leptons with tr > 10 ; lepton pt (from Geant)",100,1,101);
  hA[86]=h=new TH1F(core+"eleEtaTrack","#eta of all leptons with tr > 10 ; lepton #eta (from Geant)",300,-1.5,1.5);
  hA[87]=h=new TH1F(core+"eleZvertTrack","zVertex of events with tr > 10 ; zVertex (from Geant)",100,-200,200);
  hA[88]=h=new TH1F(core+"elePhiTrack","#phi of all leptons with tr > 10 ; lepton #phi (from Geant)",12,(-1.+15./180.)*PI,(1.+15./180.)*PI);


  //efficiency vs zdcRate
  hA[89]=h=new TH1F(core+"eleZdcAll","zdc coinc. rate for all leptons; ZDC coincidence rate",100,0,200000);
  hA[90]=h=new TH1F(core+"eleZdcTrig","zdc coinc. rate for leptons that satisfy trigger; ZDC coincidence rate",100,0,200000);
  hA[91]=h=new TH1F(core+"eleZdcVert","zdc coinc. rate for leptons with a good vertex; ZDC coincidence rate",100,0,200000);
  hA[92]=h=new TH1F(core+"eleZdcTrack","zdc coinc. rate for leptons with tr > 10 ; ZDC coincidence rate",100,0,200000);
  hA[93]=h=new TH1F(core+"eleZdcReco","zdc coinc. rate for leptons that pass W cuts; ZDC coincidence rate",100,0,200000);


  //Z efficiency histos
  hA[100]=h=new TH1F(core+"zMassAll","Geant Z mass all; Z mass (from Geant)",100,0,200);
  hA[101]=h=new TH1F(core+"zMassTrig","Geant Z mass for events with good trig; Z mass (from Geant)",100,0,200);
  hA[102]=h=new TH1F(core+"zMassVert","Geant Z mass for events with good vertex ; Z mass (from Geant)",100,0,200);
  hA[103]=h=new TH1F(core+"zMassTrack","Geant Z mass for events with 2 reco tracks ; Z mass (from Geant)",100,0,200);
  hA[104]=h=new TH1F(core+"zMassReco","Geant Z mass for events passing all Z cuts ; Z mass (from Geant)",100,0,200);
  
  hA[105]=h=new TH1F(core+"zZdcAll","zdc coinc. rate for all; ZDC coincidence rate",100,0,200000);
  hA[106]=h=new TH1F(core+"zZdcTrig","zdc coinc. rate for events with good trig; ZDC coincidence rate",100,0,200000);
  hA[107]=h=new TH1F(core+"zZdcVert","zdc coinc. rate for events with good vertex; ZDC coincidence rate",100,0,200000);
  hA[108]=h=new TH1F(core+"zZdcTrack","zdc coinc. rate for for events with 2 reco tracks; ZDC coincidence rate",100,0,200000);
  hA[109]=h=new TH1F(core+"zZdcReco","zdc coinc. rate for events passing all Z cuts; ZDC coincidence rate",100,0,200000);

  //check geant and MuDst events match correctly
  hB[68]=h2=new TH2F(core+"electronRecovsGeantAll","Electron Reco pt vs Geant pt; Geant electron pt; 2x2 cluster Et",60,0,60,60,0,60);
    
  // btow scale investigation histos
  hA[110]=h=new TH2F(core+"ThrownEt_eta","Geant Thrown Electron ET vs detector eta; detector eta; electron ET",40,-1.,1.,100.,0,100.);
  hA[111]=h=new TH2F(core+"ThrownEt_etaWcuts","Geant Thrown Electron ET vs detector eta passing all W cuts; detector eta; electron ET",40,-1.,1.,100.,0,100.);
  hA[112]=h=new TH2F(core+"SmearEt_eta","Geant Smeared Electron ET vs detector eta; detector eta; electron ET",40,-1.,1.,100.,0,100.);
  hA[113]=h=new TH2F(core+"SmearEt_etaWcuts","Geant Smeared Electron ET vs detector eta passing all W cuts; detector eta; electron ET",40,-1.,1.,100.,0,100.);

  hA[115]=h=new TH1F(core+"recoRthrown","Reconstructed ET / Thrown ET; Reco ET / Thrown ET",1000,0.5,1.5);
  hA[116]=h=new TH1F(core+"recoRthrownE","Reconstructed E / Thrown E; Reco E / Thrown E",1000,0.5,1.5);

  hA[117]=h=new TH1F(core+"thrownEt_etaLT1","Geant Thrown Electron ET, |#eta| < 1; electron ET",120.,0,60.);
  hA[118]=h=new TH1F(core+"thrownEtwieghted_etaLT1","Geant Thrown Electron ET weighted by RHICBOS, |#eta| < 1; electron ET",120.,0,60.);
  hA[119]=h=new TH1F(core+"thrownEtwieghted_etaLT1_passCuts","Geant Thrown Electron ET weighted by RHICBOS, |#eta| < 1 passing W cuts; electron ET",120.,0,60.);
  hA[120]=h=new TH2F(core+"ptDiff_recoPt","Geant-Reco Pt vs Reco Pt for tracks matching to electron; Reco Pt; Geant-Reco Pt",160.,0.,80.,200.,-20.,20.);
  hA[121]=h=new TH2F(core+"delEta_delPhi","Delta eta vs Delta phi for events passing all W cuts; Delta phi; Delta eta",100.,-0.1,0.1,100.,-0.1,0.1);

  hA[122]=h=new TH2F(core+"bx7i","7 bit bXing of input events",100,0,200000,128.,-0.5,127.5);
  hA[123]=h=new TH2F(core+"bx7a","7 bit bXing of accepted events",100,0,200000,128.,-0.5,127.5);

  hA[124]=h=new TH1F(core+"recoTrPti","Input reconstructed track pt associated with ele",160,0,80);
  hA[125]=h=new TH1F(core+"recoTrPta","Accepted reconstructed track pt associated with ele",160,0,80);
  hA[126]=h=new TH1F(core+"recoTrInvPti","Input reconstructed track 1/pt associated with ele",100,0,0.1);
  hA[127]=h=new TH1F(core+"recoTrInvPta","Accepted reconstructed track 1/pt associated with ele",100,0,0.1);
  hA[128]=h=new TH2F(core+"nHitAssoc_pt","Associated track N hits vs reco Pt",160,0.,80.,50,0,50);
  hA[129]=h=new TH2F(core+"fitFracAssoc_pt","Associated track fitFrac vs reco Pt",160,0.,80.,50,0,1.1);
  hA[130]=h=new TH2F(core+"RxyInAssoc_pt","prim tr 1st hit vs reco Pt",160,0.,80.,60,50,170.);
  hA[131]=h=new TH2F(core+"RxyOutAssoc_pt","prim tr last hit vs reco Pt",160,0.,80.,60,100,220.);
  hA[132]=h=new TH2F(core+"nHitAssoc_InvPt","Associated track N hits vs reco 1/Pt",100,0,0.1,50,0,50);
  hA[133]=h=new TH2F(core+"fitFracAssoc_InvPt","Associated track fitFrac vs reco 1/Pt",100,0,0.1,50,0,1.1);
  hA[134]=h=new TH2F(core+"RxyInAssoc_InvPt","prim tr 1st hit vs reco 1/Pt",100,0,0.1,60,50,170.);
  hA[135]=h=new TH2F(core+"RxyOutAssoc_InvPt","prim tr last hit vs reco 1/Pt",100,0,0.1,60,100,220.);
  hA[136]=h=new TH1F(core+"flagAssoc","prim tr flag",400,0,400);

  hA[140]=h=new TH1F(core+"eleETwTag","pt of wTag'd leptons ; lepton pt (from Geant)",100,1,101);

  //likelihood templates
  for(int itemp=0; itemp<10; itemp++){
    hB[itemp+110]=h2=new TH2F(core+Form("tempFuncRes%d",itemp),Form("Template function w/ resolution = %.02f",itemp/100.),101,0,101,100,0,100);
    hB[itemp+120]=h2=new TH2F(core+Form("tempFuncRes%dWcuts",itemp),Form("Template function w/ resolution = %.02f (W cuts applied)",itemp/100.),101,0,101,100,0,100);
    hB[itemp+130]=h2=new TH2F(core+Form("tempFuncRes%dWcuts_innerEta",itemp),Form("Template function w/ resolution = %.02f (W cuts applied) |eta|=[0,0.5]",itemp/100.),101,0,101,100,0,100);
    hB[itemp+140]=h2=new TH2F(core+Form("tempFuncRes%dWcuts_outerEta",itemp),Form("Template function w/ resolution = %.02f (W cuts applied) |eta|=[0.5,1.0]",itemp/100.),101,0,101,100,0,100);
    hB[itemp+150]=h2=new TH2F(core+Form("tempFuncRes%dWcuts_toshow",itemp),Form("Template function w/ resolution = %.02f (W cuts applied)",itemp/100.),101,0.798,1.202,100,0,100);
  }

  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    hA[i]->Sumw2(); //need since weighting effic histos
    HList->Add( hA[i]);
  }
  for(int i=0;i<mxHB;i++) {
    if(  hB[i]==0) continue;
    hB[i]->Sumw2(); //need since weighting effic histos
    HList->Add( hB[i]);
  }
  //HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;

}

// $Log: St2009pubMc_histo.cxx,v $
// Revision 1.4  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.3  2010/02/18 19:48:10  stevens4
// add more effic histograms and cleanup
//
// Revision 1.2  2010/01/21 17:54:31  stevens4
// add effic histos and charge seperated background plots
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
