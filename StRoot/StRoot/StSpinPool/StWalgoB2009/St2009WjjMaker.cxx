// $Id: St2009WjjMaker.cxx,v 1.5 2010/06/25 15:42:09 balewski Exp $
//
//*-- Author : Jan Balewski, MIT
// 
#include <math.h>

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>

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
 
  par_jetPtLow=5.; par_jetPtHigh=60.;// GeV/c
  par_jetEtaLow=1.0;  par_jetEtaHigh=1.8;

  par_djPtLow=1.;  par_djPtHigh=40;// GeV/c
  par_djPzLow=3; par_djPzHigh=90; // GeV/c
  par_djEtaMin=0.1;
  par_etaSumLow=0.01; par_etaSumHigh=2.5;

  par_spinSort=false;
  par_vertexZ=100;// cm
  isMC=0;
  spinDb=0;
  par_corLevel=0; // default no corrections
  mJEScorrFile="fixMe"; memset(mJEScorrH,0,sizeof(mJEScorrH));
 }


//_____________________________________________________________________________
//
Int_t 
St2009WjjMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  nRun=0;
  
  LOG_INFO<<GetName()<<Form("::Init JES corr=%d, input=%s",par_corLevel,mJEScorrFile.Data())<<endl;;
  if(par_corLevel) {
    TFile *fd=new TFile(mJEScorrFile); assert(fd);
    for(int i=0;i<mxJESeta;i++) {
      TString tit= "jesCorr_iEta"; tit+=i;
      mJEScorrH[i]=(TH1F*)fd->Get(tit); assert(mJEScorrH[i]);
      mJEScorrH[i]->Print();

      // add derivate for interpolation    
      TH1F *h=mJEScorrH[i];      
      TAxis *ax=h->GetXaxis();      int nb=ax->GetNbins(); 
      for(int k=1;k<=nb-1;k++) {
	float x1=ax->GetBinCenter(k);	
	float x2=ax->GetBinCenter(k+1);	
	float y1=h->GetBinContent(k);	
	float y2=h->GetBinContent(k+1);	
	float tg=(y2-y1)/(x2-x1);	
	h->SetBinError(k,tg);     
      }    
    } // end of loop over eta bins
  }// end of JES initialialization

  return StMaker::Init();
}
  
//_____________________________________________________________________________
//
TLorentzVector  
St2009WjjMaker::trueJet( TLorentzVector rJ) {
  if (par_corLevel==0) return rJ;
  int iEta=(rJ.Eta()+1.2)/0.4;
  if(iEta<0) iEta=0;
  if(iEta>=mxJESeta ) iEta=mxJESeta-1;
  TH1F* h=mJEScorrH[iEta];
  float rPt=rJ.Pt();
  int bin=h->FindBin(rPt);
  float x1=h->GetBinCenter(bin);

  if(x1>rPt && bin>1) { // use lower pT bin for interpolation
    bin--;
    x1=h->GetBinCenter(bin);
  }
  float y1=h->GetBinContent(bin);
  float tg=h->GetBinError(bin);
  
  float truePt=y1+ tg*(rPt-x1);
  
  float fac=truePt/rJ.Pt();
  //printf(" rPt=%.2f  tPt=%.2f  ratio=%.3f\n",rJ.Pt(),truePt ,fac);

  
  return fac*rJ;
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
    assert(spinDb);
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
    

  LOG_INFO<<GetName()<<Form("::InitRun(%d) done, W->jet+jet sorting  params: doSpinSort=%d |vertZ|<%.0f cm,\n   jetPt=[%.1f,%.1f] GeV/c, jetEta=[%.1f,%.1f]\n   DJ: pT=[%.1f,%.1f] GeV/c, |Pz|=[%.1f,%.1f] GeV/c, eta1+2=[%.1f,%.1f]",
		 runNo,par_spinSort,par_vertexZ ,par_jetPtLow,par_jetPtHigh,par_jetEtaLow,par_jetEtaHigh,
		 par_djPtLow,par_djPtHigh,par_djPzLow,par_djPzHigh,par_etaSumLow, par_etaSumHigh
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

  if(!isMC ){ // fixed n the middle of processing
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
    if(trgOK)    hA[0]->Fill("trig",1.);
    // tmp, do not abort events here, now chain is trigger filtering
  }
  
  //....... find vertex .....
  int nInpPrimV=wMK->mMuDstMaker->muDst()->numberOfPrimaryVertices();
  int nVer=0;
  for(int iv=0;iv<nInpPrimV;iv++) {
    if(nVer) break; // tmp - consider only 1st vertex, since jets are made only for the 1st vertex
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

  //......... require:  vertex is reasonable .......
  

  //****loop over jet branch with B+EEMC****
  if(GetMaker("JetReader")==0) return; // most likely jet-maker was not in the chain
  TClonesArray* jets = wMK->getJets("ConeJets12_100"); // select specific jet-tree type 
  if(jets==0) return; // most likely jet-maker was not in the chain
  int nJets= wMK->nJets;
  //printf("nJets=%d  n2J=%.0f\n",nJets, hA[0]->GetBinContent(1)); 
  if(nJets<1) return;
  hA[0]->Fill("anyJ",1.);
  if(nJets<2) return;
  hA[0]->Fill("mulJ",1.);

  const int mxJ=2;
  TLorentzVector jet[mxJ]; 
  int nJ=0;
  for (int i_jet=0; i_jet< nJets; i_jet++){// try to find 2 jets passing cuts
    TLorentzVector Jreco = *((StJet*)jets->At(i_jet));

    //...... compute correction .......
    TLorentzVector J=Jreco; //1:1
    J=trueJet(J);

    if(J.Pt()<par_jetPtLow) continue;

    if(nJ>=mxJ) { // 3rd jet, kill for now 
      hA[0]->Fill("J3",1.);
      hA[6]->Fill(fabs(jet[0].DeltaPhi(J)),fabs(jet[1].DeltaPhi(J)));
      return;
    }
    jet[nJ++]=J;
    if(nJ==1)hA[0]->Fill("J1",1.);
  }
  
  if(nJ<mxJ) return;
  hA[0]->Fill("J2",1.);

  // 2 jet-event, process it
  assert(nJ<=mxJ);

  // decide which is jet #1
  if( fabs(jet[0].Eta()) > fabs(jet[1].Eta()) ) {// swap
    TLorentzVector J=jet[0];
    jet[0]=jet[1]; jet[1]=J;
  }

  //  now jet1 has smaller |eta|
  hA[10]->Fill(jet[0].Et(),jet[0].Eta());
  hA[11]->Fill(jet[1].Et(),jet[1].Eta());
  hA[17]->Fill(jet[0].E(),jet[1].E());
  hA[18]->Fill(jet[0].Pt(),jet[1].Pt());
  hA[19]->Fill(jet[0].Eta(),jet[1].Eta());
  float phiCDdeg=jet[0].DeltaPhi(jet[1])/3.1416*180.;
  if(phiCDdeg<-90) phiCDdeg+=360; // choose different phi range
  hA[13]->Fill(phiCDdeg); 

  
  TLorentzVector diJet=jet[0]+jet[1];
  float invM=sqrt(diJet*diJet);
 
  hA[14]->Fill(invM);
  hA[16]->Fill(invM, diJet.Pt());
  hA[15]->Fill( diJet.Z(),diJet.Pt());
 

  // for Pavel N.
  if(invM<60) {
      hA[21]->Fill(diJet.Pt());
      hA[22]->Fill(fabs(jet[0].DeltaPhi(jet[1])));
      hA[23]->Fill(jet[0].Eta()-jet[1].Eta());
      hA[24]->Fill(jet[0].Eta(),jet[1].Eta());
  }

  if(par_spinSort) {//........ do spin sorting
    
    assert(spinDb->isValid());  // all 3 DB records exist 
    assert(spinDb->isPolDirLong());  // you do not want mix Long & Trans by accident
    int bx48=wMK->wEve.bx48;
    int bx7=wMK->wEve.bx7;
    if(spinDb->offsetBX48minusBX7(bx48,bx7)) {
      printf("BAD bx7=%d bx48=%d del=%d\n",bx7,bx48,spinDb->offsetBX48minusBX7(bx48,bx7));
      hA[0]->Fill("badBx48",1.);
      return; // both counters must be in sync
    }
    hA[2]->Fill(bx7);
    int bxStar7=spinDb->BXstarUsingBX7(bx7);
    hA[4]->Fill(bxStar7);
    
    int spin4=spinDb->spin4usingBX48(bx48); 
    hA[5]->Fill(bxStar7,spin4);
    
    hA[20]->Fill(invM,spin4);
    
  } // end of spin sorting

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
// Revision 1.5  2010/06/25 15:42:09  balewski
// more plots for Pavel
//
// Revision 1.4  2010/05/04 12:14:35  balewski
// runs now w/o jet tree
//
// Revision 1.3  2010/05/03 17:24:37  balewski
// added spin sorting of di-jets
//
// Revision 1.2  2010/05/01 01:31:44  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//

