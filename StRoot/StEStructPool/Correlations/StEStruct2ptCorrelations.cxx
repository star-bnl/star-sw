/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.cxx,v 1.3 2004/07/01 00:34:52 porter Exp $
 *
 * Author: Jeff Porter adaptation of Aya's 2pt-analysis
 *
 **********************************************************************
 *
 * Description:  Analysis code for 2pt-analysis. 
 *    The analysis runs as follows:
 *       1D and 2D arrays (in yt,eta,phi) are setup
 *       and filled for each of the 6 pair types:
 *       Sibling (++,+- & -+, --)
 *       Mixed   (++,+- & -+, --)
 *       The 2D versions are additionally divided into yt1,yt2 slices
 *       and (via the StEStructBuffer) z-vertex
 *       After arrays are filled (looped over all events/job), Histograms are 
 *       created, filled, and written out to the data file for further
 *       processing.
 *
 *
 ***********************************************************************/
#include "StEStruct2ptCorrelations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StTimer.hh"
#include "StEStructCutBin.h"


ClassImp(StEStruct2ptCorrelations)

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::StEStruct2ptCorrelations(int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false) , mInit(false), mDeleted(false) {  init();  }

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::StEStruct2ptCorrelations(const char* cutFileName, int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mInit(false), mDeleted(false), mPair(cutFileName) {  init(); }

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::~StEStruct2ptCorrelations(){ cleanUp(); };


void StEStruct2ptCorrelations::init(){

  mCurrentEvent=NULL;
  mtimer=NULL;

  if(manalysisMode & 1) {
     mskipPairCuts=true;
  } else if(manalysisMode & 2){
    mdoPairCutHistograms=true;
  }
  for(int i=0;i<6;i++)numPairs[i]=numPairsProcessed[i]=mpossiblePairs[i]=0;

  for(int i=0;i<10;i++)mbuffCounter[i]=0;
  initArraysAndHistograms();

  /* Event count via Nch distribution */
mHNEvents[0]=new TH1F("NEventsSame","NEventsSame",1001,-0.5,1000.5);
mHNEvents[1]=new TH1F("NEventsMixed","NEventsMixed",1001,-0.5,1000.5);

  mInit=true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::finish(){

  if(!moutFileName){
    cout<<" NO OUTPUTFILE TO WRITE TO ..... giving up ...."<<endl;
    return;
  }

  fillHistograms();
  TFile * tf=new TFile(moutFileName,"RECREATE");
  writeHistograms(tf);
  tf->Close();
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::cleanUp(){ 
  if(mDeleted) return;
  deleteArraysAndHistograms(); 
  mDeleted=true;
}



//
//-------  Analysis Function ------//
//
//--------------------------------------------------------------------------
bool StEStruct2ptCorrelations::doEvent(StEStructEvent* event){
  if(!event) return false;

  if(2>event->Ntrack()){
    delete event;
    return true;
  }

  moveEvents();
  mCurrentEvent=event;
  mHNEvents[0]->Fill(event->Ntrack());
  return makeSiblingAndMixedPairs();
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::moveEvents(){

  if(!mCurrentEvent) return;
  int i=(int) floor((mCurrentEvent->VertexZ()+25.)/5);
  if(i<0 || i>9) return;
  mbuffer[i].addEvent(mCurrentEvent);
  mbuffCounter[i]++;

}

//--------------------------------------------------------------------------
bool StEStruct2ptCorrelations::makeSiblingAndMixedPairs(){

  if(!mCurrentEvent) return false; // logic problem!
  int i=(int)floor((mCurrentEvent->VertexZ()+25.)/5.);
  if(i<0 || i>9) return false;

  makePairs(mCurrentEvent,mCurrentEvent,0);
  makePairs(mCurrentEvent,mCurrentEvent,1);
  makePairs(mCurrentEvent,mCurrentEvent,2);

  mbuffer[i].resetCounter();
  while(1){
    mMixingEvent=mbuffer[i].nextEvent();
    if(!mMixingEvent) break;
      mHNEvents[1]->Fill(mMixingEvent->Ntrack());
      makePairs(mCurrentEvent,mMixingEvent,3);
      makePairs(mCurrentEvent,mMixingEvent,4);
      makePairs(mMixingEvent,mCurrentEvent,4);
      makePairs(mCurrentEvent,mMixingEvent,5);
  }

  return true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::makePairs(StEStructEvent* e1, StEStructEvent* e2, int j){

  if(j>=6) return;
  StEStructTrackCollection* t1;
  StEStructTrackCollection* t2;

  StEStructBinning* b=StEStructBinning::Instance();
  StEStructCutBin* cb=StEStructCutBin::Instance();

  mtBins**  mtmt   = mMtMt[j];
  etaBins** etaeta = mEtaEta[j];
  phiBins** phiphi = mPhiPhi[j];

  dmtBins**  atmtmt   = mAtSMtDMt[j];
  detaBins** atetaeta = mAtSEtaDEta[j];
  dphiBins** atphiphi = mAtSPhiDPhi[j];

  dphiBins** jtdmtdphi  = mJtDMtDPhi[j];
  dmtBins**  jtdetadmt  = mJtDEtaDMt[j];
  dphiBins** jtdetadphi = mJtDEtaDPhi[j];

  sphiBins** jtsmtsphi  = mJtSMtSPhi[j];
  smtBins**  jtsetasmt  = mJtSEtaSMt[j];
  sphiBins** jtsetasphi = mJtSEtaSPhi[j];


  switch(j) {
    case 0:
      {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionP();
        mPair.setPairType(0);
        mpossiblePairs[j]+=floor(0.5*(t1->getEntries()*(t2->getEntries()-1)));
        break;
      }
        
  case 1:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionM();
        mPair.setPairType(1);
        mpossiblePairs[j]+=(t1->getEntries()*t2->getEntries());
        break;
    }       
  case 2:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(0);
        mpossiblePairs[j]+=floor(0.5*(t1->getEntries()*(t2->getEntries()-1)));
        break;
    }
  case 3:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionP();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(t1->getEntries()*t2->getEntries());
        break;
    }
  case 4:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionM();
        mPair.setPairType(3);
        mpossiblePairs[j]+=(t1->getEntries()*t2->getEntries());
        break;
    }
  case 5:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(t1->getEntries()*t2->getEntries());
        break;
    }
  }


  StEStructTrackIterator Iter1;
  StEStructTrackIterator Iter2;

  int imt1,imt2,idmt,ismt;
  int ieta1,ieta2,ideta,iseta;
  int iphi1,iphi2,idphi,isphi;
  int ideta2,idmt2,idphi2;

  if(mtimer)mtimer->start();

  for(Iter1=t1->begin(); Iter1!=t1->end();++Iter1){

    mPair.SetTrack1(*Iter1);
    imt1  = b->imt(mPair.Track1()->Yt());
    ieta1 = b->ieta(mPair.Track1()->Eta());
    iphi1 = b->iphi(mPair.Track1()->Phi());
  
    if(j==0 || j==2) { 
       Iter2=Iter1+1; 
    } else { 
       Iter2=t2->begin(); 
    }

    for(; Iter2!=t2->end(); ++Iter2){

      numPairs[j]++;
      mPair.SetTrack2(*Iter2);
      if( mskipPairCuts || mPair.cutPair(mdoPairCutHistograms)==0){
	numPairsProcessed[j]++;
        int iy=cb->getCutBin(&mPair);
        //     cout<<"iy="<<iy<<endl;
        float weight=1.0;//mPair.SigmaPt();
         imt2  = b->imt(mPair.Track2()->Yt());
         ieta2 = b->ieta(mPair.Track2()->Eta());
         iphi2 = b->iphi(mPair.Track2()->Phi());

         mDeltaMt[j].dmt[b->iDeltaMt(mPair.DeltaMt())]+=weight; 

	 //-> X vs X
         mtmt[iy][imt1].mt[imt2]+=weight; 
         etaeta[iy][ieta1].eta[ieta2]+=weight; 
         phiphi[iy][iphi1].phi[iphi2]+=weight;

	 //	 if(j==0 || j==2){
 	 //-> X vs X (symmetry)
           mtmt[iy][imt2].mt[imt1]+=weight; 
           etaeta[iy][ieta2].eta[ieta1]+=weight; 
           phiphi[iy][iphi2].phi[iphi1]+=weight;
	   //	 }

         //-> delta y vs delta x          
         jtdmtdphi[iy][idmt=b->idmt(mPair.DeltaYt())].dphi[idphi=b->idphi(mPair.DeltaPhi())]+=weight; 
         jtdetadmt[iy][ideta=b->ideta(mPair.DeltaEta())].dmt[idmt]+=weight; 
         jtdetadphi[iy][ideta].dphi[idphi]+=weight; 

         //--- symmetry ---

         jtdmtdphi[iy][idmt2=b->idmt(-1.0*mPair.DeltaYt())].dphi[idphi]+=weight;
         jtdmtdphi[iy][idmt].dphi[idphi2=b->idphi(-1.0*mPair.DeltaPhi())]+=weight;
         jtdmtdphi[iy][idmt2].dphi[idphi2]+=weight;

         jtdetadmt[iy][ideta2=b->ideta(-1.0*mPair.DeltaEta())].dmt[idmt]+=weight; 
         jtdetadmt[iy][ideta].dmt[idmt2]+=weight; 
         jtdetadmt[iy][ideta2].dmt[idmt2]+=weight;
 
         jtdetadphi[iy][ideta2].dphi[idphi]+=weight; 
         jtdetadphi[iy][ideta].dphi[idphi2]+=weight; 
         jtdetadphi[iy][ideta2].dphi[idphi2]+=weight; 

	 //-> Sum y vs delta x
         atmtmt[iy][ismt=b->ismt(mPair.SigmaYt())].dmt[idmt]+=weight; 
         atetaeta[iy][iseta=b->iseta(mPair.SigmaEta())].deta[ideta]+=weight; 
         atphiphi[iy][isphi=b->isphi(mPair.SigmaPhi())].dphi[idphi]+=weight; 
         atmtmt[iy][ismt].dmt[idmt2]+=weight; 
         atetaeta[iy][iseta].deta[ideta2]+=weight; 
         atphiphi[iy][isphi].dphi[idphi2]+=weight; 

	 //-> Sum y vs sum x
         jtsmtsphi[iy][ismt].sphi[isphi]+=weight; 
         jtsetasmt[iy][iseta].smt[ismt]+=weight; 
         jtsetasphi[iy][iseta].sphi[isphi]+=weight; 
         
         msmts[j].smt[ismt]+=weight;
         msetas[j].seta[iseta]+=weight;
         msphis[j].sphi[isphi]+=weight;

         mdmts[j].dmt[idmt]+=weight;
         mdetas[j].deta[ideta]+=weight;
         mdphis[j].dphi[idphi]+=weight;
         mdmts[j].dmt[idmt2]+=weight;
         mdetas[j].deta[ideta2]+=weight;
         mdphis[j].dphi[idphi2]+=weight;


      };// pair cut
    };// iter2 loop
  };// iter 1 loop

  if(mtimer)mtimer->stop();
}


//
//------------ Below are init, delete, write functions -------///
//

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::fillHistograms(){

  StEStructBinning* b=StEStructBinning::Instance();
  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0; i<6; i++){

    for(int k=0;k<b->deltaMtBins();k++)
      mHDeltaMt[i]->Fill(b->deltaMtVal(k),mDeltaMt[i].dmt[k]);

    for(int k=0;k<b->dmtBins();k++)
      mHdmts[i]->Fill(b->dmtVal(k),mdmts[i].dmt[k]);
    for(int k=0;k<b->smtBins();k++)
      mHsmts[i]->Fill(b->smtVal(k),msmts[i].smt[k]);

    for(int k=0;k<b->detaBins();k++)
      mHdetas[i]->Fill(b->detaVal(k),mdetas[i].deta[k]);
    for(int k=0;k<b->setaBins();k++)
      mHsetas[i]->Fill(b->setaVal(k),msetas[i].seta[k]);

    for(int k=0;k<b->dphiBins();k++)
      mHdphis[i]->Fill(b->dphiVal(k),mdphis[i].dphi[k]);
    for(int k=0;k<b->sphiBins();k++)
      mHsphis[i]->Fill(b->sphiVal(k),msphis[i].sphi[k]);


    mtBins**  mtmt   = mMtMt[i];
    etaBins** etaeta = mEtaEta[i];
    phiBins** phiphi = mPhiPhi[i];

    dmtBins**  atmtmt   = mAtSMtDMt[i];
    detaBins** atetaeta = mAtSEtaDEta[i];
    dphiBins** atphiphi = mAtSPhiDPhi[i];

    dphiBins** jtdmtdphi  = mJtDMtDPhi[i];
    dmtBins**  jtdetadmt  = mJtDEtaDMt[i];
    dphiBins** jtdetadphi = mJtDEtaDPhi[i];

    sphiBins** jtsmtsphi  = mJtSMtSPhi[i];
    smtBins**  jtsetasmt  = mJtSEtaSMt[i];
    sphiBins** jtsetasphi = mJtSEtaSPhi[i];

    for(int y=0;y<numCutBins;y++){

    for(int k=0;k<b->mtBins();k++)
      for(int j=0;j<b->mtBins();j++)
        mHMtMt[i][y]->Fill(b->mtVal(k),b->mtVal(j),mtmt[y][k].mt[j]);

    for(int k=0;k<b->phiBins();k++)
      for(int j=0;j<b->phiBins();j++)
        mHPhiPhi[i][y]->Fill(b->phiVal(k),b->phiVal(j),phiphi[y][k].phi[j]);

    for(int k=0;k<b->etaBins();k++)
      for(int j=0;j<b->etaBins();j++)
        mHEtaEta[i][y]->Fill(b->etaVal(k),b->etaVal(j),etaeta[y][k].eta[j]);


    for(int k=0;k<b->dmtBins();k++)
      for(int j=0;j<b->dphiBins();j++)
        mHJtDMtDPhi[i][y]->Fill(b->dmtVal(k),b->dphiVal(j),jtdmtdphi[y][k].dphi[j]);

    for(int k=0;k<b->detaBins();k++)
      for(int j=0;j<b->dmtBins();j++)
        mHJtDEtaDMt[i][y]->Fill(b->detaVal(k),b->dmtVal(j),jtdetadmt[y][k].dmt[j]);

    for(int k=0;k<b->detaBins();k++)
      for(int j=0;j<b->dphiBins();j++)
        mHJtDEtaDPhi[i][y]->Fill(b->detaVal(k),b->dphiVal(j),jtdetadphi[y][k].dphi[j]);

    for(int k=0;k<b->smtBins();k++)
      for(int j=0;j<b->dmtBins();j++)
        mHAtSMtDMt[i][y]->Fill(b->smtVal(k),b->dmtVal(j),atmtmt[y][k].dmt[j]);

    for(int k=0;k<b->sphiBins();k++)
      for(int j=0;j<b->dphiBins();j++)
        mHAtSPhiDPhi[i][y]->Fill(b->sphiVal(k),b->dphiVal(j),atphiphi[y][k].dphi[j]);

    for(int k=0;k<b->setaBins();k++)
      for(int j=0;j<b->detaBins();j++)
        mHAtSEtaDEta[i][y]->Fill(b->setaVal(k),b->detaVal(j),atetaeta[y][k].deta[j]);

    for(int k=0;k<b->smtBins();k++)
      for(int j=0;j<b->sphiBins();j++)
        mHJtSMtSPhi[i][y]->Fill(b->smtVal(k),b->sphiVal(j),jtsmtsphi[y][k].sphi[j]);

    for(int k=0;k<b->setaBins();k++)
      for(int j=0;j<b->smtBins();j++)
        mHJtSEtaSMt[i][y]->Fill(b->setaVal(k),b->smtVal(j),jtsetasmt[y][k].smt[j]);

    for(int k=0;k<b->setaBins();k++)
      for(int j=0;j<b->sphiBins();j++)
        mHJtSEtaSPhi[i][y]->Fill(b->setaVal(k),b->sphiVal(j),jtsetasphi[y][k].sphi[j]);


    }
  }

}


//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::writeHistograms(TFile* tf){

  tf->cd();
  for(int j=0;j<2;j++)mHNEvents[j]->Write(); 
 
  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int j=0;j<numCutBins;j++){
  for(int i=0;i<6;i++){
     mHMtMt[i][j]->Write();
     mHPhiPhi[i][j]->Write();
     mHEtaEta[i][j]->Write();
  }
  }

  for(int i=0;i<6;i++){
      mHDeltaMt[i]->Write();
      mHdmts[i]->Write();
      mHsmts[i]->Write();
      mHdetas[i]->Write();
      mHsetas[i]->Write();
      mHdphis[i]->Write();
      mHsphis[i]->Write();
  };

  for(int j=0;j<numCutBins;j++){
  for(int i=0;i<6;i++){
      mHJtDMtDPhi[i][j]->Write();
      mHJtDEtaDMt[i][j]->Write();
      mHJtDEtaDPhi[i][j]->Write();
  }
  }

  for(int j=0;j<numCutBins;j++){
  for(int i=0;i<6;i++){
     mHAtSMtDMt[i][j]->Write();
     mHAtSPhiDPhi[i][j]->Write();
     mHAtSEtaDEta[i][j]->Write();
  }
  }

  for(int j=0;j<numCutBins;j++){
  for(int i=0;i<6;i++){
     mHJtSMtSPhi[i][j]->Write();
     mHJtSEtaSMt[i][j]->Write();
     mHJtSEtaSPhi[i][j]->Write();
  }
  }
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::initArraysAndHistograms(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<6;i++){
     mMtMt[i]=new mtBins*[numCutBins];
     mEtaEta[i]=new etaBins*[numCutBins];
     mPhiPhi[i]=new phiBins*[numCutBins];
     mAtSMtDMt[i]=new dmtBins*[numCutBins];
     mAtSEtaDEta[i]=new detaBins*[numCutBins];
     mAtSPhiDPhi[i]=new dphiBins*[numCutBins];
     mJtDMtDPhi[i]=new dphiBins*[numCutBins];
     mJtDEtaDMt[i]=new dmtBins*[numCutBins];
     mJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mJtSMtSPhi[i]=new sphiBins*[numCutBins];
     mJtSEtaSMt[i]=new smtBins*[numCutBins];
     mJtSEtaSPhi[i]=new sphiBins*[numCutBins];    
  }

  for(int j=0;j<numCutBins;j++){
   for(int i=0;i<6;i++){
     mMtMt[i][j]=new mtBins[EBYE_MT_BINS];
     memset(mMtMt[i][j],0,EBYE_MT_BINS*sizeof(mtBins));

     mEtaEta[i][j]=new etaBins[EBYE_ETA_BINS];
     memset(mEtaEta[i][j],0,EBYE_ETA_BINS*sizeof(etaBins));

     mPhiPhi[i][j]=new phiBins[EBYE_PHI_BINS];
     memset(mPhiPhi[i][j],0,EBYE_PHI_BINS*sizeof(phiBins));

     mAtSMtDMt[i][j]=new dmtBins[EBYE_SMT_BINS];
     memset(mAtSMtDMt[i][j],0,EBYE_SMT_BINS*sizeof(dmtBins));

     mAtSEtaDEta[i][j]=new detaBins[EBYE_SETA_BINS];
     memset(mAtSEtaDEta[i][j],0,EBYE_SETA_BINS*sizeof(detaBins));

     mAtSPhiDPhi[i][j]=new dphiBins[EBYE_SPHI_BINS];
     memset(mAtSPhiDPhi[i][j],0,EBYE_SPHI_BINS*sizeof(dphiBins));

     mJtDMtDPhi[i][j]=new dphiBins[EBYE_DMT_BINS];
     memset(mJtDMtDPhi[i][j],0,EBYE_DMT_BINS*sizeof(dphiBins));

     mJtDEtaDMt[i][j]=new dmtBins[EBYE_DETA_BINS];
     memset(mJtDEtaDMt[i][j],0,EBYE_DETA_BINS*sizeof(dmtBins));

     mJtDEtaDPhi[i][j]=new dphiBins[EBYE_DETA_BINS];
     memset(mJtDEtaDPhi[i][j],0,EBYE_DETA_BINS*sizeof(dphiBins));

     mJtSMtSPhi[i][j]=new sphiBins[EBYE_SMT_BINS];
     memset(mJtSMtSPhi[i][j],0,EBYE_SMT_BINS*sizeof(sphiBins));

     mJtSEtaSMt[i][j]=new smtBins[EBYE_SETA_BINS];
     memset(mJtSEtaSMt[i][j],0,EBYE_SETA_BINS*sizeof(smtBins));

     mJtSEtaSPhi[i][j]=new sphiBins[EBYE_SETA_BINS];    
     memset(mJtSEtaSPhi[i][j],0,EBYE_SETA_BINS*sizeof(sphiBins));

   }
  }

  memset(mdmts,0,6*sizeof(dmtBins));
  memset(msmts,0,6*sizeof(smtBins));
  memset(mdetas,0,6*sizeof(detaBins));
  memset(msetas,0,6*sizeof(setaBins));
  memset(mdphis,0,6*sizeof(dphiBins));
  memset(msphis,0,6*sizeof(sphiBins));


  StEStructBinning* b=StEStructBinning::Instance();

  char* bName[]={"Sibpp","Sibpm","Sibmm","Mixpp","Mixpm","Mixmm"};
  char* bTitle[]={"Sibling : +.+","Sibling : +.- + -.+","Sibling : -.-","Mixed : +.+","Mixed : +.- + -.+","Mixed : -.-"};

  for(int i=0; i<6; i++){
    TString hnDeltaMt(bName[i]); hnDeltaMt+="DeltaMt";
    TString htDeltaMt("DeltaMt"); htDeltaMt+=bTitle[i];
    mHDeltaMt[i]=new TH1F(hnDeltaMt.Data(),htDeltaMt.Data(),b->deltaMtBins(),b->deltaMtMin(),b->deltaMtMax());

    TString hnDMt(bName[i]); hnDMt+="DMt";
    TString htDMt("DMt"); htDMt+=bTitle[i];
    mHdmts[i]=new TH1F(hnDMt.Data(),htDMt.Data(),b->dmtBins(),b->dmtMin(),b->dmtMax());


    TString hnSMt(bName[i]); hnSMt+="SMt";
    TString htSMt("SMt"); htSMt+=bTitle[i];
    mHsmts[i]=new TH1F(hnSMt.Data(),htSMt.Data(),b->smtBins(),b->smtMin(),b->smtMax());


    TString hnDEta(bName[i]); hnDEta+="DEta";
    TString htDEta("DEta"); htDEta+=bTitle[i];
    mHdetas[i]=new TH1F(hnDEta.Data(),htDEta.Data(),b->detaBins(),b->detaMin(),b->detaMax());


    TString hnSEta(bName[i]); hnSEta+="SEta";
    TString htSEta("SEta"); htSEta+=bTitle[i];
    mHsetas[i]=new TH1F(hnSEta.Data(),htSEta.Data(),b->setaBins(),b->setaMin(),b->setaMax());


    TString hnDPhi(bName[i]); hnDPhi+="DPhi";
    TString htDPhi("DPhi"); htDPhi+=bTitle[i];
    mHdphis[i]=new TH1F(hnDPhi.Data(),htDPhi.Data(),b->dphiBins(),b->dphiMin(),b->dphiMax());


    TString hnSPhi(bName[i]); hnSPhi+="SPhi";
    TString htSPhi("SPhi"); htSPhi+=bTitle[i];
    mHsphis[i]=new TH1F(hnSPhi.Data(),htSPhi.Data(),b->sphiBins(),b->sphiMin(),b->sphiMax());

    mHMtMt[i]=new TH2F*[numCutBins];
    mHEtaEta[i]=new TH2F*[numCutBins];
    mHPhiPhi[i]=new TH2F*[numCutBins];
    mHAtSMtDMt[i]=new TH2F*[numCutBins];
    mHAtSEtaDEta[i]=new TH2F*[numCutBins];
    mHAtSPhiDPhi[i]=new TH2F*[numCutBins];
    mHJtDMtDPhi[i]=new TH2F*[numCutBins];
    mHJtDEtaDMt[i]=new TH2F*[numCutBins];
    mHJtDEtaDPhi[i]=new TH2F*[numCutBins];
    mHJtSMtSPhi[i]=new TH2F*[numCutBins];
    mHJtSEtaSMt[i]=new TH2F*[numCutBins];
    mHJtSEtaSPhi[i]=new TH2F*[numCutBins];

    bool ytcuts=false;
    if(numCutBins>1)ytcuts=true;

    for(int j=0;j<numCutBins;j++){
    TString hnMtMt(bName[i]); hnMtMt+="MtMt"; if(ytcuts)hnMtMt+=j;
    TString htMtMt("MtMt"); htMtMt+=bTitle[i]; 
    mHMtMt[i][j]=new TH2F(hnMtMt.Data(),htMtMt.Data(),b->mtBins(),b->mtMin(),b->mtMax(),b->mtBins(),b->mtMin(),b->mtMax());

    TString hnEtaEta(bName[i]); hnEtaEta+="EtaEta"; if(ytcuts)hnEtaEta+=j;
    TString htEtaEta("EtaEta"); htEtaEta+=bTitle[i];
    mHEtaEta[i][j]=new TH2F(hnEtaEta.Data(),htEtaEta.Data(),b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());

    TString hnPhiPhi(bName[i]); hnPhiPhi+="PhiPhi"; if(ytcuts)hnPhiPhi+=j;
    TString htPhiPhi("PhiPhi"); htPhiPhi+=bTitle[i];
    mHPhiPhi[i][j]=new TH2F(hnPhiPhi.Data(),htPhiPhi.Data(),b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());

    TString hnSMtDMt(bName[i]); hnSMtDMt+="SMtDMt"; if(ytcuts)hnSMtDMt+=j;
    TString htSMtDMt("Auto SMtDMt"); htSMtDMt+=bTitle[i];
    mHAtSMtDMt[i][j]=new TH2F(hnSMtDMt.Data(),htSMtDMt.Data(),b->smtBins(),b->smtMin(),b->smtMax(),b->dmtBins(),b->dmtMin(),b->dmtMax());

    TString hnSEtaDEta(bName[i]); hnSEtaDEta+="SEtaDEta"; if(ytcuts)hnSEtaDEta+=j;
    TString htSEtaDEta("Auto SEtaDEta"); htSEtaDEta+=bTitle[i];
    mHAtSEtaDEta[i][j]=new TH2F(hnSEtaDEta.Data(),htSEtaDEta.Data(),b->setaBins(),b->setaMin(),b->setaMax(),b->detaBins(),b->detaMin(),b->detaMax());

    TString hnSPhiDPhi(bName[i]); hnSPhiDPhi+="SPhiDPhi"; if(ytcuts)hnSPhiDPhi+=j;
    TString htSPhiDPhi("Auto SPhiDPhi"); htSPhiDPhi+=bTitle[i];
    mHAtSPhiDPhi[i][j]=new TH2F(hnSPhiDPhi.Data(),htSPhiDPhi.Data(),b->sphiBins(),b->sphiMin(),b->sphiMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

    TString hnDMtDPhi(bName[i]); hnDMtDPhi+="DMtDPhi"; if(ytcuts)hnDMtDPhi+=j;
    TString htDMtDPhi("Joint DMtDPhi"); htDMtDPhi+=bTitle[i];
    mHJtDMtDPhi[i][j]=new TH2F(hnDMtDPhi.Data(),htDMtDPhi.Data(),b->dmtBins(),b->dmtMin(),b->dmtMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

    TString hnDEtaDMt(bName[i]); hnDEtaDMt+="DEtaDMt"; if(ytcuts)hnDEtaDMt+=j;
    TString htDEtaDMt("Joint DEtaDMt"); htDEtaDMt+=bTitle[i];
    mHJtDEtaDMt[i][j]=new TH2F(hnDEtaDMt.Data(),htDEtaDMt.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dmtBins(),b->dmtMin(),b->dmtMax());

    TString hnDEtaDPhi(bName[i]); hnDEtaDPhi+="DEtaDPhi"; if(ytcuts)hnDEtaDPhi+=j;
    TString htDEtaDPhi("Joint DEtaDPhi"); htDEtaDPhi+=bTitle[i];
    mHJtDEtaDPhi[i][j]=new TH2F(hnDEtaDPhi.Data(),htDEtaDPhi.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

    TString hnSMtSPhi(bName[i]); hnSMtSPhi+="SMtSPhi"; if(ytcuts)hnSMtSPhi+=j;
    TString htSMtSPhi("Joint SMtSPhi"); htSMtSPhi+=bTitle[i];
    mHJtSMtSPhi[i][j]=new TH2F(hnSMtSPhi.Data(),htSMtSPhi.Data(),b->smtBins(),b->smtMin(),b->smtMax(),b->sphiBins(),b->sphiMin(),b->sphiMax());


    TString hnSEtaSMt(bName[i]); hnSEtaSMt+="SEtaSMt"; if(ytcuts)hnSEtaSMt+=j; 
    TString htSEtaSMt("Joint SEtaSMt"); htSEtaSMt+=bTitle[i];
    mHJtSEtaSMt[i][j]=new TH2F(hnSEtaSMt.Data(),htSEtaSMt.Data(),b->setaBins(),b->setaMin(),b->setaMax(),b->smtBins(),b->smtMin(),b->smtMax());

    TString hnSEtaSPhi(bName[i]); hnSEtaSPhi+="SEtaSPhi"; if(ytcuts)hnSEtaSPhi+=j;
    TString htSEtaSPhi("Joint SEtaSPhi"); htSEtaSPhi+=bTitle[i];
    mHJtSEtaSPhi[i][j]=new TH2F(hnSEtaSPhi.Data(),htSEtaSPhi.Data(),b->setaBins(),b->setaMin(),b->setaMax(),b->sphiBins(),b->sphiMin(),b->sphiMax());

    }
  }

}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::deleteArraysAndHistograms(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<6;i++){

   for(int j=0;j<numCutBins;j++){

   delete [] mMtMt[i][j];
   delete []  mEtaEta[i][j];
   delete []  mPhiPhi[i][j];
   delete []  mAtSMtDMt[i][j];
   delete []  mAtSEtaDEta[i][j];
   delete []  mAtSPhiDPhi[i][j];
   delete []  mJtDMtDPhi[i][j];
   delete []  mJtDEtaDMt[i][j];
   delete []  mJtDEtaDPhi[i][j];
   delete []  mJtSMtSPhi[i][j];
   delete []  mJtSEtaSMt[i][j];
   delete []  mJtSEtaSPhi[i][j];
   }   
   delete []  mMtMt[i];
   delete []  mEtaEta[i];
   delete []  mPhiPhi[i];
   delete []  mAtSMtDMt[i];
   delete []  mAtSEtaDEta[i];
   delete []  mAtSPhiDPhi[i];
   delete []  mJtDMtDPhi[i];
   delete []  mJtDEtaDMt[i];
   delete []  mJtDEtaDPhi[i];
   delete []  mJtSMtSPhi[i];
   delete []  mJtSEtaSMt[i];
   delete []  mJtSEtaSPhi[i];    

  }

  for(int i=0;i<6;i++){
    delete mHdmts[i];
    delete mHsmts[i];
    delete mHdetas[i];
    delete mHsetas[i];
    delete mHdphis[i];
    delete mHsphis[i];
    for(int j=0;j<numCutBins;j++){
   delete mHMtMt[i][j];
   delete mHEtaEta[i][j];
   delete mHPhiPhi[i][j];
   delete mHAtSMtDMt[i][j];
   delete mHAtSEtaDEta[i][j];
   delete mHAtSPhiDPhi[i][j];
   delete mHJtDMtDPhi[i][j];
   delete mHJtDEtaDMt[i][j];
   delete mHJtDEtaDPhi[i][j];
   delete mHJtSMtSPhi[i][j];
   delete mHJtSEtaSMt[i][j];
   delete mHJtSEtaSPhi[i][j];
    }
   delete [] mHMtMt[i];
   delete [] mHEtaEta[i];
   delete [] mHPhiPhi[i];
   delete [] mHAtSMtDMt[i];
   delete [] mHAtSEtaDEta[i];
   delete [] mHAtSPhiDPhi[i];
   delete [] mHJtDMtDPhi[i];
   delete [] mHJtDEtaDMt[i];
   delete [] mHJtDEtaDPhi[i];
   delete [] mHJtSMtSPhi[i];
   delete [] mHJtSEtaSMt[i];
   delete [] mHJtSEtaSPhi[i];

  }


}

/***********************************************************************
 *
 * $Log: StEStruct2ptCorrelations.cxx,v $
 * Revision 1.3  2004/07/01 00:34:52  porter
 * correct accounting for possible pairs in stats files
 *
 * Revision 1.2  2004/06/25 03:11:48  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


