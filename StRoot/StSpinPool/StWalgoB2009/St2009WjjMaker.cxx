// $Id: St2009WjjMaker.cxx,v 1.1 2010/04/16 01:04:43 balewski Exp $
//
//*-- Author : Jan Balewski, MIT
// 
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>
//#include <StEvent/StEventInfo.h> // just to get time
#include <StSpinPool/StSpinDbMaker/StSpinDbMaker.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>

#include "St2009WMaker.h"

#include "St2009WjjMaker.h"
#include <StSpinPool/StJets/StJet.h>

ClassImp(St2009WjjMaker)

//_____________________________________________________________________________
//
St2009WjjMaker::St2009WjjMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
  core=name;
 
  par_jetPtLow=10.;// GeV/c
  par_jetPtHigh=40.;// GeV/c
  par_jetEtaLow=-0.8;
  par_jetEtaHigh=1.1;

  par_djPtHigh=30;
  par_spinSort=false;
  par_vertexZ=100;// cm
  isMC=0;
 }


//_____________________________________________________________________________
//
Int_t 
St2009WjjMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  nRun=0;

  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t 
St2009WjjMaker::FinishRun  (int runNo){
  char txt[1000];

  if(par_spinSort) {
    sprintf(txt,"events T= %d %d",Tfirst,Tlast);
    printf("Finish run=%d , events time range %s\n",runNo,txt);
    hbxIdeal->GetYaxis()->SetTitle(txt);
  }
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
St2009WjjMaker::InitRun  (int runNo){

  if(par_spinSort) {
    assert(runNo>= 10081007); // F10407, first pp500 long fill with defined pol pattern
    assert(runNo<=10103046); // F10536, last pp500 fill in run9
    
    char txt[1000],txt0[100];
    sprintf(txt0,"bxIdeal%d",nRun);
    sprintf(txt,"intended fill pattern  R%d-%d vs. bXing; %s", runNo,nRun,spinDb->getV124comment());
    nRun++;
    Tfirst=int(2e9); Tlast=-Tfirst;
    hbxIdeal=new TH1F(core+txt0,txt,128,-0.5,127.5);
    hbxIdeal->SetFillColor(kYellow);
    HList->Add(hbxIdeal);
  
    spinDb->print(0); // 0=short, 1=huge
    for(int bx=0;bx<120;bx++){
      if(spinDb->isBXfilledUsingInternalBX(bx))  hbxIdeal->Fill(bx);   
    }
    

    sprintf(txt,"bXing= bx7+off=%d",spinDb->BX7offset());
    hA[4]->GetXaxis()->SetTitle(txt);
  } // end of spin sort
    

  LOG_INFO<<Form("::InitRun(%d) done, W->jet+jet sorting  params: spinSort=%d |vertZ|<%.0f, jetPt=[%.1f,%.1f] GeV/c, jetEta=[%.1f,%.1f]\n   DJ: pT<%.1f GeV/c",
		 runNo,par_spinSort,par_vertexZ ,par_jetPtLow,par_jetPtHigh,par_jetEtaLow,par_jetEtaHigh,par_djPtHigh
		 )<<endm;	 
  return kStOK;
}

//_____________________________________________________________________________
//
Int_t 
St2009WjjMaker::Make(){
  int T=wMK->mMuDstMaker->muDst()->event()->eventInfo().time();
  if(Tlast<T) Tlast=T;
  if(Tfirst>T) Tfirst=T;

  bXingSort();
  return kStOK;
}

//_____________________________________________________________________________
//
void 
St2009WjjMaker::bXingSort(){
  //has access to whole W-algo-maker data via pointer 'wMK'
  
  hA[0]->Fill("inp",1.);

  //...... trigger .........

  if(!isMC){
    StMuEvent* muEve = wMK->mMuDstMaker->muDst()->event();
    StMuTriggerIdCollection *tic=&(muEve->triggerIdCollection());
    assert(tic);
    const StTriggerId &l1=tic->l1();
    vector<unsigned int> idL=l1.triggerIds();
    //  printf("nTrig=%d, trigID: ",idL.size());
    int trgOK=0;
    for(unsigned int i=0;i<idL.size(); i++){
      if(idL[i]==230420) trgOK+=1; // AJP
      if(idL[i]==230411) trgOK+=2; // JP2
    }
    if(!trgOK) return;
    hA[0]->Fill("trig",1.);
  }
  
  //....... find vertex .....
  int nInpPrimV=wMK->mMuDstMaker->muDst()->numberOfPrimaryVertices();
  int nVer=0;
  for(int iv=0;iv<nInpPrimV;iv++) {
    if(iv) break; // tmp - consider only 1st vertex, since jets are made only for the 1st vertex
    StMuPrimaryVertex* V= wMK->mMuDstMaker->muDst()->primaryVertex(iv);
    assert(V);
    wMK->mMuDstMaker->muDst()->setVertexIndex(iv);
    float rank=V->ranking();
    if (rank<=0) continue;
    const StThreeVectorF &r=V->position();
    if(fabs(r.z()) > par_vertexZ) continue;
    nVer++; // count valid vertices
  }

  if(nVer<=0) return; 
  hA[0]->Fill("vert",1.);

  //......... require: L2W-trig (ET or rnd) & vertex is reasonable .......
  

  //****loop over branch with EEMC****
  TClonesArray* jets = wMK->getJets("ConeJets12_100"); // select specific jet-tree type 
  int nJets= wMK->nJets;
  //printf("nJets=%d  n2J=%.0f\n",nJets, hA[0]->GetBinContent(1)); 
  if(nJets<1) return;
  hA[0]->Fill("anyJ",1.);
  if(nJets<2) return;
  hA[0]->Fill("mulJ",1.);


  TLorentzVector jet[2], Jsum; int kJ=0; 
  for (int i_jet=0; i_jet< nJets; i_jet++){// try to find 2 jets passing cuts
    // printf("ij=%d\n", i_jet);
    TLorentzVector J = *((StJet*)jets->At(i_jet));

    Jsum+=J;
    if(kJ==2 &&  J.Pt()>2) hA[0]->Fill("J3",1.);
    if(kJ>=2)  continue;

    hA[10+kJ]->Fill(J.Pt(), J.Eta());
    if(J.Pt()<par_jetPtLow) continue;
    if(J.Pt()>par_jetPtHigh) continue;
    if(J.Eta()<par_jetEtaLow) continue;
    if(J.Eta()>par_jetEtaHigh) continue;
    // printf("kJ=%d,  px=%f %f %f   PT=%f=%f=ET\n",kJ,jet[kJ].X(),jet[kJ].Y(),jet[kJ].Z(),jet[kJ].Pt(),jet[kJ].Et());
    jet[kJ++]=J;
    if(kJ==1)hA[0]->Fill("J1",1.);
    if(kJ==2)hA[0]->Fill("J2",1.);
  }


  if(kJ<2) return;
  TLorentzVector diJet=jet[0]+jet[1];
  
  if(diJet.Pt()>par_djPtHigh) return;
  hA[0]->Fill("DjPt",1.);

  float invM=sqrt(diJet*diJet);

  // printf("kJ12,  px=%f %f %f   %f=%f  invM=%.1f\n",diJet.X(),diJet.Y(),diJet.Z(),diJet.Pt(),diJet.Et(),invM);

  hA[12]->Fill(diJet.Pt(), diJet.Eta());

  hA[13]->Fill(invM, fabs(jet[0].Eta()+jet[1].Eta()));
  hA[14]->Fill(invM);
  hA[15]->Fill(invM, fabs(diJet.Z()));
  hA[16]->Fill(invM, diJet.Pt());

  //... various correlations
  hA[20]->Fill( diJet.Z(),jet[0].Eta()+jet[1].Eta() );
  if(nJets>2) hA[21]->Fill( Jsum.Z(),diJet.Eta());
  hA[22]->Fill( diJet.Z(),diJet.Pt());
  

  /*   TLorentzVector (px,py,pz,E). 
       TLorentzVector v4(TVector3(1., 2., 3.),4.);
       v.SetVect(TVector3(1,2,3)); 
       v.SetXYZT(x,y,z,t); 
       v.SetPxPyPzE(px,py,pz,e); 
       v.SetXYZM(x,y,z,m);   //   ->  v=(x,y,z,e=Sqrt(x*x+y*y+z*z+m*m))
  */


#if 0
  for (int i_jet=1; i_jet< nJetsWE; i_jet++){//loop over jets
    StJet* jet = wMK->getJet(i_jet);
    TVector3 jetVec; //vector for jet momentum
	//vary neutral and charged et in jets for systematic
	float neutral=jet->neutralFraction()*jet->Pt();
	float charged=jet->chargedFraction()*jet->Pt();
	neutral=neutral*par_mcJetNeutScale;
	charged=charged*par_mcJetChrgScale;
	float sum=neutral+charged;
	jetVec.SetPtEtaPhi(sum,jet->Eta(),jet->Phi());
	if(jetVec.DeltaR(T.primP) > par_nearDeltaR)
              T.ptBalance+=jetVec;
      }
      TVector3 clustPt(T.primP.X(),T.primP.Y(),0);
      clustPt.SetMag(T.cluster.ET);
      T.ptBalance+=clustPt;
      T.sPtBalance = T.ptBalance.Perp();
      if(T.ptBalance.Dot(clustPt)<0) T.sPtBalance *=-1.;
      

#endif

  if(par_spinSort) {
    assert(spinDb->isValid());  // all 3 DB records exist 
    assert(spinDb->isPolDirLong());  // you do not want mix Long & Trans by accident  
    int bx48=wMK->wEve.bx48;
    int bx7=wMK->wEve.bx7;
    if(spinDb->offsetBX48minusBX7(bx48,bx7)) {
      printf("BAD bx7=%d bx48=%d del=%d\n",bx7,bx48,spinDb->offsetBX48minusBX7(bx48,bx7));
      hA[0]->Fill("badBx48",1.);  
      return;   
    }
        
    hA[2]->Fill(bx7);
    
    int bxStar7=spinDb->BXstarUsingBX7(bx7);
    
    hA[4]->Fill(bxStar7);
    
    int spin4=spinDb->spin4usingBX48(bx48); 
    hA[5]->Fill(bxStar7,spin4);
  }// end of spin processing  
    
#if 0
  StMuEvent* muEve = wMK->mMuDstMaker->muDst()->event();
  int max=0;
    for (int m=0;m<300;m++)  {    // access L0-HT data
      int val=muEve->emcTriggerDetector().highTower(m);
      if(max<val) max=val;
    }
#endif


      //::::::::::::::::::::::::::::::::::::::::::::::::
      //:::::accepted W events for x-section :::::::::::
      //::::::::::::::::::::::::::::::::::::::::::::::::

}


// $Log: St2009WjjMaker.cxx,v $
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//

