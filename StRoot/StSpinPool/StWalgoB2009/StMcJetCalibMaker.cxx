// $Id: StMcJetCalibMaker.cxx,v 1.1 2010/05/01 01:31:45 balewski Exp $
//
//*-- Author : Jan Balewski, MIT
// 
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>

//need these to get MC record
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"


#include "St2009WMaker.h"
#include "St2009WjjMaker.h" // for jetEcorr
#include "StMcJetCalibMaker.h"
#include <StSpinPool/StJets/StJet.h>

ClassImp(StMcJetCalibMaker)
 
//________________________________________________________
//
StMcJetCalibMaker::StMcJetCalibMaker(const char *name):StMaker(name){
  wMK=0; wjjMK=0;HList=0;
  core=name;
  par_vertexZ=70;// cm
  par_verZerr=1;//cm
  par_delRmax=0.1;// match gen & reco jets
  par_corLevel=0; // level of jet energy correction
  par_jetEtaLow=-1.5; par_jetEtaHigh=2; 
  isMC=1; 
}


//_____________________________________________________________________________
//
Int_t 
StMcJetCalibMaker::Init(){
  assert(wMK);
  if( par_corLevel)  assert(wjjMK);
  assert(HList);
  initHistos();

  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t 
StMcJetCalibMaker::FinishRun  (int runNo){
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
StMcJetCalibMaker::InitRun  (int runNo){


  LOG_INFO<<GetName()<<Form("::InitRun(%d),calib params: |geant vertZ|<%.0f cm  |zG-zR|<%.1f cm, jet: eta=[%.1f,%1f],  delR(reco-gen)<%.2f \n  JES corr level=%d",
			    runNo,par_vertexZ ,par_verZerr,par_jetEtaLow,par_jetEtaHigh, 
			   par_delRmax,
			    par_corLevel
		 )<<endm;	 
  assert(isMC);
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
StMcJetCalibMaker::Make(){
  calibrate();

  return kStOK;
}

//_____________________________________________________________________________
//
void 
StMcJetCalibMaker::calibrate(){
  StMcEvent* mMcEvent = 0;
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
  if(mMcEvent==0) return; // STARNGE - fix it
  assert(mMcEvent);
  

  //........ restrict M-C vertex .........

  StMcVertex *V=mMcEvent->primaryVertex();
  TVector3 mcV=TVector3(V->position().x(),V->position().y(),V->position().z());
  //printf("MC ver=%f %f %f\n", mcV.x(), mcV.y(), mcV.z());
  
  hA[0]->Fill("inp",1.);
  hA[1]->Fill(mcV.z());
  if(fabs(mcV.z()) > par_vertexZ) return;
  hA[0]->Fill("verG",1.);

#if 0 
  for(uint i=0;i<mMcEvent->tracks().size();i++){//loop tracks
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    const StLorentzVectorF &p4=mcTrack-> fourMomentum();
    printf("i=%d pdgId=%d  p=%f %f %f  m=%f\n",i,pdgId,p4.x(),p4.y(),p4.z(),p4.m());
    if(i>15) break;
  }
#endif
 
   //........ select Pythia particles .........
  int type=0; // QCD process
  int pdgId=mMcEvent->tracks()[6]->pdgId(); // boson?
  if(pdgId==23) type=1; //Z
  if( pdgId==-24 || pdgId==24 ) type=2;// W+, W-

  // A+B --> C+D  or  A+B-->W --> C+D
 
  if(mMcEvent->tracks().size()<10) return;
  // access 2:2 kinematics for W, Z production or QCD scattering
  //  StMcTrack* mctA= mMcEvent->tracks()[4]; // q or qbar 1
  // StMcTrack* mctB= mMcEvent->tracks()[5]; //  q or qbar 2

  StMcTrack* mctW=0, *mctC=0, *mctD=0;
  if(type) {// Z,W+, or W-
    mctW=mMcEvent->tracks()[6]; // boson
    mctC= mMcEvent->tracks()[7];// decay1 (lepton or q or qbar)
    mctD= mMcEvent->tracks()[8];// decay 2 product
    hA[0]->Fill("W,Z",1.);
  } else { // QCD process
    mctC= mMcEvent->tracks()[6];// paron1 
    mctD= mMcEvent->tracks()[7];// parton 2
  }

  //... define matching array for gen vs. thrown jets
  const int mxGJ=2, mxRJ=4;
  int nGJ=0,nRJ=0 , mrJI[mxGJ];;
  TLorentzVector gJA[mxGJ], rJA[mxRJ];

  float delRA[mxGJ][mxRJ],delRAc[mxGJ][mxRJ];
  memset(mrJI,-1,sizeof(mrJI)); // clear all gen-jets as not-matched

  for(int k=0;k<2;k++) { // find parton in the barrel
    const StLorentzVectorF &ttC=mctC-> fourMomentum();
    const StLorentzVectorF &ttD=mctD-> fourMomentum();
    TLorentzVector gJ;
    if(k==0) gJ=TLorentzVector(ttC.px(),ttC.py(),ttC.pz(),ttC.e());
    if(k==1) gJ=TLorentzVector(ttD.px(),ttD.py(),ttD.pz(),ttD.e());
    hA[11]->Fill(gJ.Eta(),gJ.Phi());
    if(gJ.Eta()<par_jetEtaLow )  continue;
    if(gJ.Eta()>par_jetEtaHigh ) continue;
    assert(nGJ<mxGJ);
    gJA[nGJ++]=gJ;
    hA[4]->Fill(gJ.Pt(),gJ.Eta());
  }
  
  if(!nGJ) return;
  hA[0]->Fill("etaG",1.);

  if(type) {
    const StLorentzVectorF &pW=mctW-> fourMomentum();
    hA[10]->Fill(pW.m());
  }


  //....... find  reco 1st vertex .....

  int nInpPrimV=wMK->mMuDstMaker->muDst()->numberOfPrimaryVertices();
  int nVer=0;
  for(int iv=0;iv<nInpPrimV;iv++) {
    if(nVer) break; // consider only 1st valid vertex
    StMuPrimaryVertex* V= wMK->mMuDstMaker->muDst()->primaryVertex(iv);
    assert(V);
    wMK->mMuDstMaker->muDst()->setVertexIndex(iv);
    float rank=V->ranking();
    if (rank<=0) continue;
    const StThreeVectorF &r=V->position();
    hA[2]->Fill(r.z()-mcV.z());
    if(fabs(r.z()-mcV.z()) > par_verZerr) continue;
    nVer++; // count valid vertices
    //printf("  reco ver=%f %f %f\n", r.x(), r.y(), r.z());
  }

  if(nVer<=0) return; 
  hA[0]->Fill("verR",1.);


  //....... select reco jets  ........

  TClonesArray* jets = wMK->getJets("ConeJets12_100"); // select jet-tree type 
  int nJets= wMK->nJets;
  //printf("nJets=%d  n2J=%.0f\n",nJets, hA[0]->GetBinContent(1)); 

  if(nJets<1) return;
  hA[0]->Fill("anyJ",1.);
  if(nJets>=2)  hA[0]->Fill("mulJ",1.);
  
  // ..... compute R-distance for all possible pairs (gen_i,reco_j) ...
  float par_recJetPtMin=5.; 

  for (int ir=0; ir< nJets; ir++){
     StJet* jet = (StJet*)jets->At(ir);
     TLorentzVector rJ = *jet;
     if(par_corLevel) rJ=wjjMK->trueJet(rJ);// apply JES correction

     if(rJ.Pt()<par_recJetPtMin) continue;
     if(nRJ>=mxRJ) continue; // skip 
     for(int ig=0;ig<nGJ;ig++)  // compute R
       delRA[ig][nRJ]=gJA[ig].DrEtaPhi(rJ);
     rJA[nRJ++]=rJ; // store jet
  }
  //  printf("aa nRJ=%d\n",nRJ);
  

  memcpy(delRAc,delRA,sizeof(delRA));// just for QA & printouts

  //...... select best matching starting from lowest minimum in R
  int nMJ=0;
  while(nMJ<nRJ) {
    float minR=1e37;
    int irm=-1,igm=-1;
    for (int ir=0; ir< nRJ; ir++)      
      for(int ig=0;ig<nGJ;ig++) {
	if(delRA[ig][ir]>minR) continue;
	minR= delRA[ig][ir];
	igm=ig; irm=ir;
      }
    mrJI[igm]=irm;
    nMJ++;
    // disable this  this pair
    for (int ir=0; ir< nRJ; ir++)  delRA[igm][ir]=99999; 
    for(int ig=0;ig<nGJ;ig++) delRA[ig][irm]=99999;

  }// end of while

  //.... work only with matched jets
  for(int ig=0;ig<nGJ;ig++) {
    if(mrJI[ig]<0)  continue;
    int ir=mrJI[ig];
    float delR=delRAc[ig][ir];
    TLorentzVector gJ=gJA[ig], rJ=rJA[ ir];
    
    hA[3]->Fill(delR);
    if(delR>par_delRmax) continue;
    hA[0]->Fill("delR",1.);

 

    hA[15]->Fill(gJ.Pt());
    hA[12]->Fill(gJ.Pt(),gJ.Eta());
    hA[21]->Fill(gJ.E(),rJ.E());
    hA[22]->Fill(gJ.Pt(),rJ.Pt());


    int iEta=(rJ.Eta()+1.2)/0.4;
    if(iEta<0) iEta=0;
    if(iEta>=mxEta) iEta=mxEta-1;
    hA[40+iEta]->Fill(gJ.Pt(),rJ.Pt());

    float ratioPT=rJ.Pt()/gJ.Pt();
    //float neutFrac=->neutralFraction();
    //float chargFrac=jet->chargedFraction();
    hA[16]->Fill(ratioPT);

    //... neutral....
    //hA[26]->Fill(ratio*neutFrac);
 
    //.... charged....
    //hA[36]->Fill(ratio*chargFrac);

  }

#if 0  // pppppppppp test printing
  TLorentzVector pG;
    printf("gen type=%d  pdgId=%d  eveID=%d\n",type,pdgId,wMK->mMuDstMaker->muDst()->event()->eventId());
    if(type && par_corLevel==0) {
      const StLorentzVectorF &ttW=mctW-> fourMomentum();
      pG=TLorentzVector(ttW.px(),ttW.py(),ttW.pz(),ttW.e());
      printf("gen W,  P=%.1f %.1f %.1f   E=%.1f  M=%.1f  PT=%.1f eta=%.2f  phi/deg=%.0f\n",pG.X(),pG.Y(),pG.Z(),pG.E(), pG.M(),pG.Pt(),pG.Eta(), pG.Phi()/3.14*180);
    }
    for(int k=0;k<2;k++) { // find parton in the barrel
      const StLorentzVectorF &ttC=mctC-> fourMomentum();
      const StLorentzVectorF &ttD=mctD-> fourMomentum();
      if(k==0) pG=TLorentzVector(ttC.px(),ttC.py(),ttC.pz(),ttC.e());
      if(k==1) pG=TLorentzVector(ttD.px(),ttD.py(),ttD.pz(),ttD.e());
      printf("gen q=%c,  P=%.1f %.1f %.1f   E=%.1f  M=%.1f  PT=%.1f eta=%.2f  phi/deg=%.0f\n",'C'+k,pG.X(),pG.Y(),pG.Z(),pG.E(), pG.M(),pG.Pt(),pG.Eta(), pG.Phi()/3.14*180);
    }
 

    printf("reco inp nJ=%d eveID=%d: selected nGJ=%d nRJ=%d\n",nJets,wMK->mMuDstMaker->muDst()->event()->eventId(),nGJ,nRJ);
    for (int i_jet=0; i_jet< nJets; i_jet++){
      StJet* jet = (StJet*)jets->At(i_jet);
      TLorentzVector J = *jet;
      float neutPt=jet->neutralFraction()*J.Pt();
      if(J.Pt()<5) continue;
      printf("recJ=%d,  P=%.1f %.1f %.1f   E=%.1f  M=%.1f  PT=%.1f neutPt=%.1f  eta=%.2f  phi/deg=%.0f\n",i_jet,J.X(),J.Y(),J.Z(),J.E(), J.M(),J.Pt(),neutPt,J.Eta(), J.Phi()/3.14*180);
    }

    printf("matching gen-->reco :\n");
    for(int ig=0;ig<nGJ;ig++) {
      printf("gen J PT=%.1f : delR(ir)= ",gJA[ig].Pt());
      for (int ir=0; ir< nRJ; ir++){
	if( mrJI[ig]==ir) printf("*");
	else printf(" ");
	printf("%.2f, ", delRAc[ig][ir]);
      }
      if(mrJI[ig]<0) 	printf("no match");
      else printf(" rec PT=%.1f", rJA[mrJI[ig]].Pt());
      printf("\n");
    }
    printf("\n---------\n");

#endif



#if 0




    //...... compute correction .......
    TLorentzVector pJ=pJreco; //1:1
    float cor1=1./St2009WjjMaker::jetEcorr(pJreco.E());

    // apply all corrections
    if(par_corLevel) pJ=cor1*pJ;

    float neutFrac=jet->neutralFraction();
    float chargFrac=jet->chargedFraction();
    
    float frac=pJ.E()/pG.E();
    if(fabs(frac-1.)> par_ptMargin) continue;
    
    if(delR>par_delRmargin) continue;
    hA[3]->Fill(delR);
    if(delR>par_delRmax) continue;
    hA[0]->Fill("delR",1.);
    hA[12]->Fill(pG.Pt(),pG.Eta());
    
    //..... study energy resolution.....
    //printf("%d %f %f %f \n",par_corLevel,frac,pJ.E(),pG.E());

    //...... full jet energy .....
    hA[16]->Fill(frac);
    hA[17]->Fill(frac,pG.Eta());
    hA[18]->Fill(pG.Eta()-pJ.Eta(),pG.Eta());
    hA[19]->Fill(pG.DeltaPhi(pJ)/3.1416*180 ,pG.Eta());
    hA[21]->Fill(pG.E(),pJ.E());
    hA[22]->Fill(pG.Pt(),pJ.Pt());

    int iEta=(pJ.Eta()+1.)/0.5;
    if(iEta<0) iEta=0;
    if(iEta>=mxEta) iEta=mxEta-1;
    hA[40+iEta]->Fill(pG.Pt(),pJ.Pt());

    //... neutral....
    hA[26]->Fill(frac*neutFrac);
    hA[27]->Fill(pJ.Eta(),pJ.E()*neutFrac);
 
    //.... charged....
    hA[36]->Fill(frac*chargFrac);
    hA[37]->Fill(pJ.Eta(),pJ.E()*chargFrac);
    break;
  }

#endif

  /*   TLorentzVector (px,py,pz,E). 
       TLorentzVector v4(TVector3(1., 2., 3.),4.);
       v.SetVect(TVector3(1,2,3)); 
       v.SetXYZT(x,y,z,t); 
       v.SetPxPyPzE(px,py,pz,e); 
       v.SetXYZM(x,y,z,m);   //   ->  v=(x,y,z,e=Sqrt(x*x+y*y+z*z+m*m))
  */
  
  
  
  //::::::::::::::::::::::::::::::::::::::::::::::::
  //:::::accepted W events for x-section :::::::::::
  //::::::::::::::::::::::::::::::::::::::::::::::::
  
}


// $Log: StMcJetCalibMaker.cxx,v $
// Revision 1.1  2010/05/01 01:31:45  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//


