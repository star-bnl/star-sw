/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.cxx,v 1.5 2005/03/03 01:30:43 porter Exp $
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
StEStruct2ptCorrelations::StEStruct2ptCorrelations(int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false) , mInit(false), mDeleted(false) {   }

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::StEStruct2ptCorrelations(const char* cutFileName, int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mInit(false), mDeleted(false), mPair(cutFileName) {  }

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::~StEStruct2ptCorrelations(){ cleanUp(); };


void StEStruct2ptCorrelations::init(){

  mCurrentEvent=NULL;
  mtimer=NULL;

  //--> code to simplify hist-creation via class held name defs
  char* _tmpName[]={"Sibpp","Sibpm","Sibmm","Mixpp","Mixpm","Mixmm"};
  char* _tmpTitle[]={"Sibling : +.+","Sibling : +.- + -.+","Sibling : -.-","Mixed : +.+","Mixed : +.- + -.+","Mixed : -.-"};
  for(int i=0;i<6;i++){
    bName[i]=new char[6];
    strcpy(bName[i],_tmpName[i]);
    bTitle[i]=new char[20];
    strcpy(bTitle[i],_tmpTitle[i]);
  }

  if(manalysisMode & 1) {
     mskipPairCuts=true;
  } else if(manalysisMode & 2){
    mdoPairCutHistograms=true;
  }
  for(int i=0;i<6;i++)numPairs[i]=numPairsProcessed[i]=mpossiblePairs[i]=0;

  for(int i=0;i<10;i++)mbuffCounter[i]=0;
  initArraysAndHistograms();

  /* Event count via Nch distribution */
  mHNEvents[0]=new TH1F("NEventsSame","NEventsSame",1000,0.,2000.);
  mHNEvents[1]=new TH1F("NEventsMixed","NEventsMixed",1000,0.,2000.);

  StEStructCutBin* cb = StEStructCutBin::Instance();
  int ncutbins=cb->getNumBins();
  mHpt = new TH1F*[ncutbins];  

  for(int i=0;i<ncutbins;i++){
    TString hname("pt");
    if(ncutbins>1)hname+=i;
    mHpt[i] = new TH1F(hname.Data(),hname.Data(),500,0,10.);
  }

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
  if(mInit == false) init();

  if(2>event->Ntrack()){
    delete event;
    return true;
  }

  moveEvents();
  mCurrentEvent=event;
  mHNEvents[0]->Fill(event->Ntrack());

  // inclusive pt distribution
  StEStructCutBin* cb = StEStructCutBin::Instance();
  int ncutbins=cb->getNumBins();
  StEStructTrackCollection *tp = mCurrentEvent->TrackCollectionP();
  float ptval;
  for(StEStructTrackIterator iter = tp->begin(); iter != tp->end(); iter++) {
    int* index_ids=cb->getPtBins(ptval=(*iter)->Pt());
    int j=0;
    while(index_ids[j]>=0){
      mHpt[index_ids[j]]->Fill(ptval);
      j++;
      if(j==ncutbins)break;
    }
  }

  StEStructTrackCollection *tm = mCurrentEvent->TrackCollectionM();
  for(StEStructTrackIterator iter = tm->begin(); iter != tm->end(); iter++) {
    int* index_ids=cb->getPtBins(ptval=(*iter)->Pt());
    int j=0;
    while(index_ids[j]>=0){
      mHpt[index_ids[j]]->Fill(ptval);
      j++;
      if(j==ncutbins)break;
    }
  }


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

  StEStructBinning* b = StEStructBinning::Instance();
  StEStructCutBin* cb = StEStructCutBin::Instance();

  ytBins**  ytyt        = mYtYt[j];
  ptBins**  ptpt        = mPtPt[j];
  etaBins** etaeta      = mEtaEta[j];
  phiBins** phiphi      = mPhiPhi[j];

  etaBins** pretaeta      = mPrEtaEta[j];
  phiBins** prphiphi      = mPrPhiPhi[j];
  etaBins** suetaeta      = mSuEtaEta[j];
  phiBins** suphiphi      = mSuPhiPhi[j];

  dytBins**  atytyt     = mAtSYtDYt[j];
  dptBins**  atptpt     = mAtSPtDPt[j];

  dphiBins** jtdytdphi = mJtDYtDPhi[j];
  detaBins** jtdytdeta = mJtDYtDEta[j];
  dphiBins** jtdetadphi = mJtDEtaDPhi[j];
  dphiBins** jtsetadphi = mJtSEtaDPhi[j];

  dphiBins** prjtdetadphi = mPrJtDEtaDPhi[j];
  dphiBins** prjtsetadphi = mPrJtSEtaDPhi[j];
  dphiBins** sujtdetadphi = mSuJtDEtaDPhi[j];
  dphiBins** sujtsetadphi = mSuJtSEtaDPhi[j];

  qBins*  qinv = mQinv[j];

  switch(j) {
    case 0:
      {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionP();
        mPair.setPairType(0);
        mpossiblePairs[j]+=(int)floor(0.5*(t1->getEntries()*(t2->getEntries()-1)));
        break;
      }
        
  case 1:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionM();
        mPair.setPairType(1);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }       
  case 2:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(0);
        mpossiblePairs[j]+=(int)floor(0.5*(t1->getEntries()*(t2->getEntries()-1)));
        break;
    }
  case 3:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionP();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  case 4:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionM();
        mPair.setPairType(3);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  case 5:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  }


  StEStructTrackIterator Iter1;
  StEStructTrackIterator Iter2;

  int iyt1,iyt2,idyt1,idyt2,isyt;
  int ipt1,ipt2,idpt1,idpt2,ispt;
  int ieta1,ieta2,ideta1,ideta2,iseta;
  int iphi1,iphi2,idphi1,idphi2;
  float pt1, pt2;


  float nwgt=1.0;//mPair.SigmaPt();

  if(mtimer)mtimer->start();

  for(Iter1=t1->begin(); Iter1!=t1->end();++Iter1){

    mPair.SetTrack1(*Iter1);
    iyt1  = b->iyt(mPair.Track1()->Yt());
    ipt1  = b->ipt(pt1=mPair.Track1()->Pt());
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

         iyt2  = b->iyt(mPair.Track2()->Yt());
         ipt2  = b->ipt(pt2=mPair.Track2()->Pt());
         ieta2 = b->ieta(mPair.Track2()->Eta());
         iphi2 = b->iphi(mPair.Track2()->Phi());

         float pwgt=pt1*pt2;
         float swgt=pt1+pt2;

	 //-> X vs X
         ytyt[iy][iyt1].yt[iyt2]+=nwgt; 
         ptpt[iy][ipt1].pt[ipt2]+=nwgt; 
         etaeta[iy][ieta1].eta[ieta2]+=nwgt; 
         phiphi[iy][iphi1].phi[iphi2]+=nwgt;

         pretaeta[iy][ieta1].eta[ieta2]+=pwgt; 
         prphiphi[iy][iphi1].phi[iphi2]+=pwgt;
         suetaeta[iy][ieta1].eta[ieta2]+=swgt; 
         suphiphi[iy][iphi1].phi[iphi2]+=swgt;

 	 //-> X vs X (symmetry)
         ytyt[iy][iyt2].yt[iyt1]+=nwgt; 
         ptpt[iy][ipt2].pt[ipt1]+=nwgt; 
         etaeta[iy][ieta2].eta[ieta1]+=nwgt; 
         phiphi[iy][iphi2].phi[iphi1]+=nwgt;

         pretaeta[iy][ieta2].eta[ieta1]+=pwgt; 
         prphiphi[iy][iphi2].phi[iphi1]+=pwgt;
         suetaeta[iy][ieta2].eta[ieta1]+=swgt; 
         suphiphi[iy][iphi2].phi[iphi1]+=swgt;

         //-> delta y vs delta x          
         jtdetadphi[iy][ideta1=b->ideta(mPair.DeltaEta())].dphi[idphi1=b->idphi(mPair.DeltaPhi())]+=nwgt; 

         //--- symmetry ---

       jtdetadphi[iy][ideta2=b->ideta(-1.*mPair.DeltaEta())].dphi[idphi1]+=nwgt;
       jtdetadphi[iy][ideta1].dphi[idphi2=b->idphi(-1.*mPair.DeltaPhi())]+=nwgt;
       jtdetadphi[iy][ideta2].dphi[idphi2]+=nwgt; 

       jtdytdphi[iy][idyt1=b->idyt(mPair.DeltaYt())].dphi[idphi1]+=nwgt;
       jtdytdphi[iy][idyt2=b->idyt(-1.*mPair.DeltaYt())].dphi[idphi1]+=nwgt;
       jtdytdphi[iy][idyt1].dphi[idphi2]+=nwgt;
       jtdytdphi[iy][idyt2].dphi[idphi2]+=nwgt;

       jtdytdeta[iy][idyt1].deta[ideta1]+=nwgt;
       jtdytdeta[iy][idyt2].deta[ideta1]+=nwgt;
       jtdytdeta[iy][idyt1].deta[ideta2]+=nwgt;
       jtdytdeta[iy][idyt2].deta[ideta2]+=nwgt;

         prjtdetadphi[iy][ideta1].dphi[idphi1]  += pwgt;
         prjtdetadphi[iy][ideta2].dphi[idphi1] += pwgt;
         prjtdetadphi[iy][ideta1].dphi[idphi2] += pwgt;
         prjtdetadphi[iy][ideta2].dphi[idphi2]+= pwgt;

         sujtdetadphi[iy][ideta1].dphi[idphi1]  += swgt;
         sujtdetadphi[iy][ideta2].dphi[idphi1] += swgt;
         sujtdetadphi[iy][ideta1].dphi[idphi2] += swgt;
         sujtdetadphi[iy][ideta2].dphi[idphi2]+= swgt;


	 //-> Sum y vs delta x
         atytyt[iy][isyt=b->isyt(mPair.SigmaYt())].dyt[idyt1]+=nwgt; 
         atytyt[iy][isyt].dyt[idyt2]+=nwgt; 
         atptpt[iy][ispt=b->ispt(mPair.SigmaPt())].dpt[idpt1=b->idpt(mPair.DeltaPt())]+=nwgt; 
         atptpt[iy][ispt].dpt[idpt2=b->idpt(-1.*mPair.DeltaPt())]+=nwgt; 

         jtsetadphi[iy][iseta=b->iseta(mPair.SigmaEta())].dphi[idphi1]+=nwgt; 
         jtsetadphi[iy][iseta].dphi[idphi2]+=nwgt; 
         prjtsetadphi[iy][iseta].dphi[idphi1] += pwgt; 
         prjtsetadphi[iy][iseta].dphi[idphi2]+= pwgt; 
         sujtsetadphi[iy][iseta].dphi[idphi1] += swgt; 
         sujtsetadphi[iy][iseta].dphi[idphi2]+= swgt; 


         qinv[iy].q[b->iq(mPair.qInv())]+=nwgt;


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

    ytBins**  ytyt   = mYtYt[i];
    ptBins**  ptpt   = mPtPt[i];

    etaBins** etaeta = mEtaEta[i];
    phiBins** phiphi = mPhiPhi[i];
    qBins*   qinv = mQinv[i];

    etaBins** pretaeta = mPrEtaEta[i];
    phiBins** prphiphi = mPrPhiPhi[i];
    etaBins** suetaeta = mSuEtaEta[i];
    phiBins** suphiphi = mSuPhiPhi[i];

    dytBins**  atytyt   = mAtSYtDYt[i];
    dptBins**  atptpt   = mAtSPtDPt[i];

    dphiBins** jtdytdphi = mJtDYtDPhi[i];
    detaBins** jtdytdeta = mJtDYtDEta[i];
    dphiBins** jtdetadphi = mJtDEtaDPhi[i];
    dphiBins** jtsetadphi = mJtSEtaDPhi[i];

    dphiBins** prjtdetadphi = mPrJtDEtaDPhi[i];
    dphiBins** prjtsetadphi = mPrJtSEtaDPhi[i];
    dphiBins** sujtdetadphi = mSuJtDEtaDPhi[i];
    dphiBins** sujtsetadphi = mSuJtSEtaDPhi[i];


    for(int y=0;y<numCutBins;y++){

    for(int k=0;k<b->ytBins();k++)
      for(int j=0;j<b->ytBins();j++)
        mHYtYt[i][y]->Fill(b->ytVal(k),b->ytVal(j),ytyt[y][k].yt[j]);

    for(int k=0;k<b->ptBins();k++)
      for(int j=0;j<b->ptBins();j++)
        mHPtPt[i][y]->Fill(b->ptVal(k),b->ptVal(j),ptpt[y][k].pt[j]);

    float xv,yv;
    for(int k=0;k<b->phiBins();k++){
      for(int j=0;j<b->phiBins();j++){
        mHPhiPhi[i][y]->Fill(xv=b->phiVal(k),yv=b->phiVal(j),phiphi[y][k].phi[j]);
        mHPrPhiPhi[i][y]->Fill(xv,yv,prphiphi[y][k].phi[j]);
        mHSuPhiPhi[i][y]->Fill(xv,yv,suphiphi[y][k].phi[j]);
      }
    }

    for(int k=0;k<b->etaBins();k++){
      for(int j=0;j<b->etaBins();j++){
        mHEtaEta[i][y]->Fill(xv=b->etaVal(k),yv=b->etaVal(j),etaeta[y][k].eta[j]);
        mHPrEtaEta[i][y]->Fill(xv,yv,pretaeta[y][k].eta[j]);
        mHSuEtaEta[i][y]->Fill(xv,yv,suetaeta[y][k].eta[j]);
      }
    }


    for(int k=0;k<b->detaBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtDEtaDPhi[i][y]->Fill(xv=b->detaVal(k),yv=b->dphiVal(j),jtdetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(xv,yv,prjtdetadphi[y][k].dphi[j]);
        mHSuJtDEtaDPhi[i][y]->Fill(xv,yv,sujtdetadphi[y][k].dphi[j]);
      }
    }

    for(int k=0;k<b->sytBins();k++)
      for(int j=0;j<b->dytBins();j++)
        mHAtSYtDYt[i][y]->Fill(b->sytVal(k),b->dytVal(j),atytyt[y][k].dyt[j]);

    for(int k=0;k<b->sptBins();k++)
      for(int j=0;j<b->dptBins();j++)
        mHAtSPtDPt[i][y]->Fill(b->sptVal(k),b->dptVal(j),atptpt[y][k].dpt[j]);

    for(int k=0;k<b->setaBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtSEtaDPhi[i][y]->Fill(xv=b->setaVal(k),yv=b->dphiVal(j),jtsetadphi[y][k].dphi[j]);
        mHPrJtSEtaDPhi[i][y]->Fill(xv,yv,prjtsetadphi[y][k].dphi[j]);
        mHSuJtSEtaDPhi[i][y]->Fill(xv,yv,sujtsetadphi[y][k].dphi[j]);
      }
    }

    for(int k=0;k<b->dytBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtDYtDPhi[i][y]->Fill(xv=b->dytVal(k),yv=b->dphiVal(j),jtdytdphi[y][k].dphi[j]);
      }      
      for(int j=0;j<b->detaBins();j++){
        mHJtDYtDEta[i][y]->Fill(xv=b->dytVal(k),yv=b->detaVal(j),jtdytdeta[y][k].deta[j]);
      }      
    }

    for(int k=0;k<b->qBins();k++)mHQinv[i][y]->Fill(b->qVal(k),qinv[y].q[k]);

    }
  }

}


//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::writeHistograms(TFile* tf){

  tf->cd();

  for(int j=0;j<2;j++)mHNEvents[j]->Write(); 

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int j=0;j<numCutBins;j++){
    mHpt[j]->Write();
   for(int i=0;i<6;i++){

     mHYtYt[i][j]->Write();
     mHPtPt[i][j]->Write();

     mHPhiPhi[i][j]->Write();
     mHEtaEta[i][j]->Write();
     mHPrPhiPhi[i][j]->Write();
     mHPrEtaEta[i][j]->Write();
     mHSuPhiPhi[i][j]->Write();
     mHSuEtaEta[i][j]->Write();

     mHAtSYtDYt[i][j]->Write();
     mHAtSPtDPt[i][j]->Write();

     mHJtDYtDPhi[i][j]->Write();
     mHJtDYtDEta[i][j]->Write();
     mHJtDEtaDPhi[i][j]->Write();
     mHPrJtDEtaDPhi[i][j]->Write();
     mHSuJtDEtaDPhi[i][j]->Write();
     mHJtSEtaDPhi[i][j]->Write();
     mHPrJtSEtaDPhi[i][j]->Write();
     mHSuJtSEtaDPhi[i][j]->Write();

     mHQinv[i][j]->Write();

   }
  }


}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::initArraysAndHistograms(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  // storage arrays first
  for(int i=0;i<6;i++){
     mYtYt[i]=new ytBins*[numCutBins];
     mAtSYtDYt[i]=new dytBins*[numCutBins];
     mPtPt[i]=new ptBins*[numCutBins];
     mAtSPtDPt[i]=new dptBins*[numCutBins];

     mEtaEta[i]=new etaBins*[numCutBins];
     mPhiPhi[i]=new phiBins*[numCutBins];
     mJtDYtDPhi[i]=new dphiBins*[numCutBins];
     mJtDYtDEta[i]=new detaBins*[numCutBins];
     mJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mJtSEtaDPhi[i]=new dphiBins*[numCutBins];

     mSuEtaEta[i]=new etaBins*[numCutBins];
     mSuPhiPhi[i]=new phiBins*[numCutBins];
     mSuJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mSuJtSEtaDPhi[i]=new dphiBins*[numCutBins];

     mPrEtaEta[i]=new etaBins*[numCutBins];
     mPrPhiPhi[i]=new phiBins*[numCutBins];
     mPrJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mPrJtSEtaDPhi[i]=new dphiBins*[numCutBins];
     
     /*  --- I cut out the ql,qo,qs
     if(mPair.doHbt3D()){
       mQlQs[i]=new qBins*[numCutBins];
       mQoQop[i]=new qBins*[numCutBins];
     }
     */

     mQinv[i]=new qBins[numCutBins];
     memset(mQinv[i],0,numCutBins*sizeof(qBins));
  }

  for(int j=0;j<numCutBins;j++){
   for(int i=0;i<6;i++){

     mYtYt[i][j]=new ytBins[ESTRUCT_YT_BINS];
     memset(mYtYt[i][j],0,ESTRUCT_YT_BINS*sizeof(ytBins));
     mAtSYtDYt[i][j]=new dytBins[ESTRUCT_SYT_BINS];
     memset(mAtSYtDYt[i][j],0,ESTRUCT_SYT_BINS*sizeof(dytBins));
     mPtPt[i][j]=new ptBins[ESTRUCT_PT_BINS];
     memset(mPtPt[i][j],0,ESTRUCT_PT_BINS*sizeof(ptBins));
     mAtSPtDPt[i][j]=new dptBins[ESTRUCT_SPT_BINS];
     memset(mAtSPtDPt[i][j],0,ESTRUCT_SPT_BINS*sizeof(dptBins));


     mEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mJtDYtDPhi[i][j]=new dphiBins[ESTRUCT_DYT_BINS];
     memset(mJtDYtDPhi[i][j],0,ESTRUCT_DYT_BINS*sizeof(dphiBins));
     mJtDYtDEta[i][j]=new detaBins[ESTRUCT_DYT_BINS];
     memset(mJtDYtDEta[i][j],0,ESTRUCT_DYT_BINS*sizeof(detaBins));
     mJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));

     mSuEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mSuEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mSuPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mSuPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mSuJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mSuJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mSuJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mSuJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));

     mPrEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mPrEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPrPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPrPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mPrJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mPrJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mPrJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mPrJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));


   }
  }


  StEStructBinning* b=StEStructBinning::Instance();

  // histograms second

  for(int i=0; i<6; i++){

    mHYtYt[i]=new TH2F*[numCutBins];
    mHAtSYtDYt[i]=new TH2F*[numCutBins];
    mHPtPt[i]=new TH2F*[numCutBins];
    mHAtSPtDPt[i]=new TH2F*[numCutBins];

    mHEtaEta[i]=new TH2F*[numCutBins];
    mHPhiPhi[i]=new TH2F*[numCutBins];
    mHPrEtaEta[i]=new TH2F*[numCutBins];
    mHPrPhiPhi[i]=new TH2F*[numCutBins];
    mHSuEtaEta[i]=new TH2F*[numCutBins];
    mHSuPhiPhi[i]=new TH2F*[numCutBins];

    mHJtDYtDPhi[i]=new TH2F*[numCutBins];
    mHJtDYtDEta[i]=new TH2F*[numCutBins];
    mHJtDEtaDPhi[i]=new TH2F*[numCutBins];
    mHJtSEtaDPhi[i]=new TH2F*[numCutBins];
    mHPrJtDEtaDPhi[i]=new TH2F*[numCutBins];
    mHPrJtSEtaDPhi[i]=new TH2F*[numCutBins];
    mHSuJtDEtaDPhi[i]=new TH2F*[numCutBins];
    mHSuJtSEtaDPhi[i]=new TH2F*[numCutBins];

    mHQinv[i]=new TH1F*[numCutBins];

    int ncb=numCutBins;

    for(int j=0;j<numCutBins;j++){

      createHist2D(mHYtYt,"YtYt",i,j,ncb,b->ytBins(),b->ytMin(),b->ytMax(),b->ytBins(),b->ytMin(),b->ytMax());
      createHist2D(mHAtSYtDYt,"SYtDYt",i,j,ncb,b->sytBins(),b->sytMin(),b->sytMax(),b->dytBins(),b->dytMin(),b->dytMax());

      createHist2D(mHPtPt,"PtPt",i,j,ncb,b->ptBins(),b->ptMin(),b->ptMax(),b->ptBins(),b->ptMin(),b->ptMax());
      createHist2D(mHAtSPtDPt,"SPtDPt",i,j,ncb,b->sptBins(),b->sptMin(),b->sptMax(),b->dptBins(),b->dptMin(),b->dptMax());

      createHist2D(mHEtaEta,"EtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      createHist2D(mHPhiPhi,"PhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      createHist2D(mHJtDYtDPhi,"DYtDPhi",i,j,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      createHist2D(mHJtDYtDEta,"DYtDEta",i,j,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->detaBins(),b->detaMin(),b->detaMax());
      createHist2D(mHJtDEtaDPhi,"DEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      createHist2D(mHJtSEtaDPhi,"SEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      createHist2D(mHPrEtaEta,"PrEtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      createHist2D(mHPrPhiPhi,"PrPhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      createHist2D(mHPrJtDEtaDPhi,"PrDEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      createHist2D(mHPrJtSEtaDPhi,"PrSEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      createHist2D(mHSuEtaEta,"SuEtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      createHist2D(mHSuPhiPhi,"SuPhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      createHist2D(mHSuJtDEtaDPhi,"SuDEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      createHist2D(mHSuJtSEtaDPhi,"SuSEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      createHist1D(mHQinv,"Qinv",i,j,ncb,b->qBins(),b->qMin(),b->qMax());

    }
  }

}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::deleteArraysAndHistograms(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<6;i++){

   for(int j=0;j<numCutBins;j++){

   delete []  mYtYt[i][j];
   delete []  mPtPt[i][j];

   delete []  mEtaEta[i][j];
   delete []  mPhiPhi[i][j];
   delete []  mPrEtaEta[i][j];
   delete []  mPrPhiPhi[i][j];
   delete []  mSuEtaEta[i][j];
   delete []  mSuPhiPhi[i][j];

   delete []  mAtSYtDYt[i][j];
   delete []  mAtSPtDPt[i][j];

   delete []  mJtDYtDPhi[i][j];
   delete []  mJtDYtDEta[i][j];
   delete []  mJtDEtaDPhi[i][j];
   delete []  mJtSEtaDPhi[i][j];

   delete []  mPrJtDEtaDPhi[i][j];
   delete []  mPrJtSEtaDPhi[i][j];
   delete []  mSuJtDEtaDPhi[i][j];
   delete []  mSuJtSEtaDPhi[i][j];

   }   
   delete []  mYtYt[i];
   delete []  mPtPt[i];

   delete []  mEtaEta[i];
   delete []  mPhiPhi[i];
   delete []  mPrEtaEta[i];
   delete []  mPrPhiPhi[i];
   delete []  mSuEtaEta[i];
   delete []  mSuPhiPhi[i];

   delete []  mAtSYtDYt[i];
   delete []  mAtSPtDPt[i];

   delete []  mJtDYtDPhi[i];
   delete []  mJtDYtDEta[i];
   delete []  mJtDEtaDPhi[i];
   delete []  mJtSEtaDPhi[i];
   delete []  mPrJtDEtaDPhi[i];
   delete []  mPrJtSEtaDPhi[i];
   delete []  mSuJtDEtaDPhi[i];
   delete []  mSuJtSEtaDPhi[i];

   delete []  mQinv[i];
  }

  for(int i=0;i<6;i++){
    for(int j=0;j<numCutBins;j++){
     delete mHYtYt[i][j];
     delete mHPtPt[i][j];

     delete mHEtaEta[i][j];
     delete mHPhiPhi[i][j];
     delete mHPrEtaEta[i][j];
     delete mHPrPhiPhi[i][j];
     delete mHSuEtaEta[i][j];
     delete mHSuPhiPhi[i][j];

     delete mHAtSYtDYt[i][j];
     delete mHAtSPtDPt[i][j];

     delete mHJtDYtDPhi[i][j];
     delete mHJtDYtDEta[i][j];
     delete mHJtDEtaDPhi[i][j];
     delete mHJtSEtaDPhi[i][j];
     delete mHPrJtDEtaDPhi[i][j];
     delete mHPrJtSEtaDPhi[i][j];
     delete mHSuJtDEtaDPhi[i][j];
     delete mHSuJtSEtaDPhi[i][j];

     delete mHQinv[i][j];
    }
   delete [] mHYtYt[i];
   delete [] mHPtPt[i];

   delete [] mHEtaEta[i];
   delete [] mHPhiPhi[i];
   delete [] mHPrEtaEta[i];
   delete [] mHPrPhiPhi[i];
   delete [] mHSuEtaEta[i];
   delete [] mHSuPhiPhi[i];

   delete [] mHAtSYtDYt[i];
   delete [] mHAtSPtDPt[i];

   delete [] mHJtDYtDPhi[i];
   delete [] mHJtDYtDEta[i];
   delete [] mHJtDEtaDPhi[i];
   delete [] mHJtSEtaDPhi[i];
   delete [] mHPrJtDEtaDPhi[i];
   delete [] mHPrJtSEtaDPhi[i];
   delete [] mHSuJtDEtaDPhi[i];
   delete [] mHSuJtSEtaDPhi[i];

   delete [] mHQinv[i];

  }


}

//-----------------------------------------------------------------
void StEStruct2ptCorrelations::createHist2D(TH2F*** h, const char* name, int iknd, int icut, int ncut, int nx, float xmin, float xmax, int ny, float ymin, float ymax){

  TString hname(bName[iknd]);
  hname+=name;
  if(ncut>1)hname+=icut;
  TString htitle(bTitle[iknd]);
  htitle+=name;
  h[iknd][icut]=new TH2F(hname.Data(),htitle.Data(),nx,xmin,xmax,ny,ymin,ymax);

}
//-----------------------------------------------------------------
void StEStruct2ptCorrelations::createHist1D(TH1F*** h, const char* name, int iknd, int icut, int ncut, int nx, float xmin, float xmax){

  TString hname(bName[iknd]);
  hname+=name;
  if(ncut>1)hname+=icut;
  TString htitle(bTitle[iknd]);
  htitle+=name;
  h[iknd][icut]=new TH1F(hname.Data(),htitle.Data(),nx,xmin,xmax);

}


/***********************************************************************
 *
 * $Log: StEStruct2ptCorrelations.cxx,v $
 * Revision 1.5  2005/03/03 01:30:43  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.4  2004/09/16 23:44:05  chunhuih
 *
 * call init() from doEvent() instead of the constructor. This is needed
 * because we want to use the polymorphic feature of this virtual function.
 * The constructor would always call the local version of a virtual function.
 *
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


