/**********************************************************************
 *
 * $Id: StEStruct2ptPtNbar.cxx,v 1.1 2004/09/16 23:49:21 chunhuih Exp $
 *
 * Author: Chunhui Han adaptation of Aya's 2pt-analysis for pt
 *
 **********************************************************************
 */

#include "StEStruct2ptPtNbar.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructCutBin.h"
#include "StTimer.hh"

ClassImp(StEStruct2ptPtNbar)
//--------------------------------------------------------------------------
StEStruct2ptPtNbar::~StEStruct2ptPtNbar() { cleanUp(); }

//--------------------------------------------------------------------------
// Override doEvent() function in order to calculate pt distribution
bool StEStruct2ptPtNbar::doEvent(StEStructEvent *event) {
  if(!event) return false;
  if(mInit == false) init();
  if(2 > event->Ntrack()) {
    delete event;
    return true;
  }
  moveEvents();
  mCurrentEvent = event;
  mHNEvents[0]->Fill(event->Ntrack());

  // inclusive pt distribution
  StEStructTrackCollection *tp = mCurrentEvent->TrackCollectionP();
  for(StEStructTrackIterator iter = tp->begin(); iter != tp->end(); iter++) {
    mHpt->Fill( (*iter)->Pt() );
  }
  StEStructTrackCollection *tm = mCurrentEvent->TrackCollectionM();
  for(StEStructTrackIterator iter = tm->begin(); iter != tm->end(); iter++) {
    mHpt->Fill( (*iter)->Pt());
  }
  return makeSiblingAndMixedPairs();
}

//--------------------------------------------------------------------------
void StEStruct2ptPtNbar::makePairs(StEStructEvent* e1, StEStructEvent* e2, int j) {

  if(j>=6) return;
  StEStructTrackCollection* t1;
  StEStructTrackCollection* t2;

  StEStructBinning* b = StEStructBinning::Instance();
  StEStructCutBin* cb = StEStructCutBin::Instance();

  mtBins**   nmtmt     = mnMtMt[j];
  etaBins**  netaeta   = mnEtaEta[j];
  phiBins**  nphiphi   = mnPhiPhi[j];
  dphiBins** ndetadphi = mnJtDEtaDPhi[j];

  mtBins**   amtmt     = maMtMt[j];
  etaBins**  aetaeta   = maEtaEta[j];
  phiBins**  aphiphi   = maPhiPhi[j];
  dphiBins** adetadphi = maJtDEtaDPhi[j];

  mtBins**   bmtmt     = mbMtMt[j];
  etaBins**  betaeta   = mbEtaEta[j];
  phiBins**  bphiphi   = mbPhiPhi[j];
  dphiBins** bdetadphi = mbJtDEtaDPhi[j];

  mtBins**   cmtmt     = mcMtMt[j];
  etaBins**  cetaeta   = mcEtaEta[j];
  phiBins**  cphiphi   = mcPhiPhi[j];
  dphiBins** cdetadphi = mcJtDEtaDPhi[j];

  const int numCutBins = cb->getNumBins();

  switch(j) {
  case 0:
    {
      t1=e1->TrackCollectionP();
      t2=e2->TrackCollectionP();
      mPair.setPairType(0);
      break;
    }
  case 1:
    {
      t1=e1->TrackCollectionP();
      t2=e2->TrackCollectionM();
      mPair.setPairType(1);
      break;
    }
  case 2:
    {
      t1=e1->TrackCollectionM();
      t2=e2->TrackCollectionM();
      mPair.setPairType(0);
      break;
    }
  case 3:
    {
      t1=e1->TrackCollectionP();
      t2=e2->TrackCollectionP();
      mPair.setPairType(2);
      break;
    }
  case 4:
    {
      t1=e1->TrackCollectionP();
      t2=e2->TrackCollectionM();
      mPair.setPairType(3);
      break;
    }
  case 5:
    {
      t1=e1->TrackCollectionM();
      t2=e2->TrackCollectionM();
      mPair.setPairType(2);
      break;
    }
  }

  StEStructTrackIterator Iter1;
  StEStructTrackIterator Iter2;

  int imt1,imt2;
  int ieta1,ieta2,ideta;
  int iphi1,iphi2,idphi;
  int ideta2,idphi2;
  float pt1, pt2;
  if(mtimer)mtimer->start();
  for(Iter1=t1->begin(); Iter1!=t1->end();++Iter1) {
    mPair.SetTrack1(*Iter1);
    imt1  = b->imt(mPair.Track1()->Yt());
    ieta1 = b->ieta(mPair.Track1()->Eta());
    iphi1 = b->iphi(mPair.Track1()->Phi());
    pt1 = (mPair.Track1())->Pt();

    if(j==0 || j==2) {
      Iter2=Iter1+1;
    }
    else {
      Iter2=t2->begin();
    }
    for(; Iter2!=t2->end(); ++Iter2) {
      numPairs[j]++;
      mPair.SetTrack2(*Iter2);
 
      if( mskipPairCuts || mPair.cutPair(mdoPairCutHistograms)==0) {
	numPairsProcessed[j]++;
        int iyt = cb->getCutBin(&mPair);
        imt2  = b->imt(mPair.Track2()->Yt());
        ieta2 = b->ieta(mPair.Track2()->Eta());
        iphi2 = b->iphi(mPair.Track2()->Phi());
	pt2 = mPair.Track2()->Pt();
        float weight = pt1 * pt2;

	nmtmt[iyt][imt1].mt[imt2] += 1;
	nmtmt[iyt][imt2].mt[imt1] += 1;
	amtmt[iyt][imt1].mt[imt2] += weight;
	amtmt[iyt][imt2].mt[imt1] += weight;
	bmtmt[iyt][imt1].mt[imt2] += pt1 + pt2;
	bmtmt[iyt][imt2].mt[imt1] += pt1 + pt2;

	netaeta[iyt][ieta1].eta[ieta2] += 1;
	netaeta[iyt][ieta2].eta[ieta1] += 1;
	aetaeta[iyt][ieta1].eta[ieta2] += weight;
	aetaeta[iyt][ieta2].eta[ieta1] += weight;
	betaeta[iyt][ieta1].eta[ieta2] += pt1 + pt2;
	betaeta[iyt][ieta2].eta[ieta1] += pt1 + pt2;

	nphiphi[iyt][iphi1].phi[iphi2] += 1;
	nphiphi[iyt][iphi2].phi[iphi1] += 1;
	aphiphi[iyt][iphi1].phi[iphi2] += weight;
	aphiphi[iyt][iphi2].phi[iphi1] += weight;
	bphiphi[iyt][iphi1].phi[iphi2] += pt1 + pt2;
	bphiphi[iyt][iphi2].phi[iphi1] += pt1 + pt2;

	ideta = b->ideta(mPair.DeltaEta());
	idphi = b->idphi(mPair.DeltaPhi());
	ideta2 = b->ideta(-1.0 * mPair.DeltaEta());
	idphi2 = b->idphi(-1.0 * mPair.DeltaPhi());
	ndetadphi[iyt][ideta].dphi[idphi] += 1;
	ndetadphi[iyt][ideta].dphi[idphi2] += 1;
	ndetadphi[iyt][ideta2].dphi[idphi] += 1;
	ndetadphi[iyt][ideta2].dphi[idphi2] += 1;
	adetadphi[iyt][ideta].dphi[idphi] += weight;
	adetadphi[iyt][ideta].dphi[idphi2] += weight;
	adetadphi[iyt][ideta2].dphi[idphi] += weight;
	adetadphi[iyt][ideta2].dphi[idphi2] += weight;
	bdetadphi[iyt][ideta].dphi[idphi] += pt1 + pt2;
	bdetadphi[iyt][ideta].dphi[idphi2] += pt1 + pt2;
	bdetadphi[iyt][ideta2].dphi[idphi] += pt1 + pt2;
	bdetadphi[iyt][ideta2].dphi[idphi2] += pt1 + pt2;
      } // pair cut
    } // iter 2 loop
  } // iter 1 loop
}

//
//------------ Below are init, delete, write functions -------///
//

//--------------------------------------------------------------------------
void StEStruct2ptPtNbar::fillHistograms(){
  if(mInit == false) init();

  StEStructBinning* b=StEStructBinning::Instance();
  const int numCutBins = StEStructCutBin::Instance()->getNumBins();

  for(int i=0; i<6; i++) {
    mtBins**   nmtmt     = mnMtMt[i];
    etaBins**  netaeta   = mnEtaEta[i];
    phiBins**  nphiphi   = mnPhiPhi[i];
    dphiBins** ndetadphi = mnJtDEtaDPhi[i];

    mtBins**   amtmt     = maMtMt[i];
    etaBins**  aetaeta   = maEtaEta[i];
    phiBins**  aphiphi   = maPhiPhi[i];
    dphiBins** adetadphi = maJtDEtaDPhi[i];

    mtBins**   bmtmt     = mbMtMt[i];
    etaBins**  betaeta   = mbEtaEta[i];
    phiBins**  bphiphi   = mbPhiPhi[i];
    dphiBins** bdetadphi = mbJtDEtaDPhi[i];

    mtBins**   cmtmt     = mcMtMt[i];
    etaBins**  cetaeta   = mcEtaEta[i];
    phiBins**  cphiphi   = mcPhiPhi[i];
    dphiBins** cdetadphi = mcJtDEtaDPhi[i];

    for(int iyt=0;iyt<numCutBins;iyt++){
      float x,y;
      for(int k=0;k<b->mtBins();k++){
	for(int j=0;j<b->mtBins();j++){
	  mnHMtMt[i][iyt]->Fill(x=b->mtVal(k),y=b->mtVal(j),nmtmt[iyt][k].mt[j]);
	  maHMtMt[i][iyt]->Fill(x,y,amtmt[iyt][k].mt[j]);
	  mbHMtMt[i][iyt]->Fill(x,y,bmtmt[iyt][k].mt[j]);
	  mcHMtMt[i][iyt]->Fill(x,y,cmtmt[iyt][k].mt[j]);
	}
      }
      for(int k=0;k<b->phiBins();k++){
	for(int j=0;j<b->phiBins();j++){
	  mnHPhiPhi[i][iyt]->Fill(x=b->phiVal(k),y=b->phiVal(j),nphiphi[iyt][k].phi[j]);
	  maHPhiPhi[i][iyt]->Fill(x,y,aphiphi[iyt][k].phi[j]);
	  mbHPhiPhi[i][iyt]->Fill(x,y,bphiphi[iyt][k].phi[j]);
	  mcHPhiPhi[i][iyt]->Fill(x,y,cphiphi[iyt][k].phi[j]);
	}
      }
      for(int k=0;k<b->etaBins();k++){
	for(int j=0;j<b->etaBins();j++){
	  mnHEtaEta[i][iyt]->Fill(x=b->etaVal(k),y=b->etaVal(j),netaeta[iyt][k].eta[j]);
	  maHEtaEta[i][iyt]->Fill(x,y,aetaeta[iyt][k].eta[j]);
	  mbHEtaEta[i][iyt]->Fill(x,y,betaeta[iyt][k].eta[j]);
	  mcHEtaEta[i][iyt]->Fill(x,y,cetaeta[iyt][k].eta[j]);
	}
      }
      for(int k=0;k<b->detaBins();k++){
	for(int j=0;j<b->dphiBins();j++){
	  mnHJtDEtaDPhi[i][iyt]->Fill(x=b->detaVal(k),y=b->dphiVal(j),ndetadphi[iyt][k].dphi[j]);
	  maHJtDEtaDPhi[i][iyt]->Fill(x,y,adetadphi[iyt][k].dphi[j]);
	  mbHJtDEtaDPhi[i][iyt]->Fill(x,y,bdetadphi[iyt][k].dphi[j]);
	  mcHJtDEtaDPhi[i][iyt]->Fill(x,y,cdetadphi[iyt][k].dphi[j]);
	}
      }
    }// y-loop
  }
}

//---------------------------------------------------------------------------
void StEStruct2ptPtNbar::writeHistograms(TFile* tf){
  tf->cd();
  for(int j=0;j<2;j++)mHNEvents[j]->Write();
  mHpt->Write();

  const int numCutBins = StEStructCutBin::Instance()->getNumBins();

  // write inclusive pt distribution
  for(int j=0;j<numCutBins;j++){
    for(int i=0;i<6;i++){
      mnHMtMt[i][j]->Write();
      mnHPhiPhi[i][j]->Write();
      mnHEtaEta[i][j]->Write();
      mnHJtDEtaDPhi[i][j]->Write();

      maHMtMt[i][j]->Write();
      maHPhiPhi[i][j]->Write();
      maHEtaEta[i][j]->Write();
      maHJtDEtaDPhi[i][j]->Write();

      mbHMtMt[i][j]->Write();
      mbHPhiPhi[i][j]->Write();
      mbHEtaEta[i][j]->Write();
      mbHJtDEtaDPhi[i][j]->Write();

      mcHMtMt[i][j]->Write();
      mcHPhiPhi[i][j]->Write();
      mcHEtaEta[i][j]->Write();
      mcHJtDEtaDPhi[i][j]->Write();
    }
  }
}

//--------------------------------------------------------------------------
void StEStruct2ptPtNbar::initArraysAndHistograms(){

  const int numCutBins = StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<6;i++) {
    mnMtMt[i]=new mtBins*[numCutBins];
    mnEtaEta[i]=new etaBins*[numCutBins];
    mnPhiPhi[i]=new phiBins*[numCutBins];
    mnJtDEtaDPhi[i]=new dphiBins*[numCutBins];
    maMtMt[i]=new mtBins*[numCutBins];
    maEtaEta[i]=new etaBins*[numCutBins];
    maPhiPhi[i]=new phiBins*[numCutBins];
    maJtDEtaDPhi[i]=new dphiBins*[numCutBins];
    mbMtMt[i]=new mtBins*[numCutBins];
    mbEtaEta[i]=new etaBins*[numCutBins];
    mbPhiPhi[i]=new phiBins*[numCutBins];
    mbJtDEtaDPhi[i]=new dphiBins*[numCutBins];
    mcMtMt[i]=new mtBins*[numCutBins];
    mcEtaEta[i]=new etaBins*[numCutBins];
    mcPhiPhi[i]=new phiBins*[numCutBins];
    mcJtDEtaDPhi[i]=new dphiBins*[numCutBins];
    for(int j = 0; j < numCutBins; j++) {
      mnMtMt[i][j]=new mtBins[EBYE_MT_BINS];
      memset(mnMtMt[i][j],0,EBYE_MT_BINS*sizeof(mtBins));
      maMtMt[i][j]=new mtBins[EBYE_MT_BINS];
      memset(maMtMt[i][j],0,EBYE_MT_BINS*sizeof(mtBins));
      mbMtMt[i][j]=new mtBins[EBYE_MT_BINS];
      memset(mbMtMt[i][j],0,EBYE_MT_BINS*sizeof(mtBins));
      mcMtMt[i][j]=new mtBins[EBYE_MT_BINS];
      memset(mcMtMt[i][j],0,EBYE_MT_BINS*sizeof(mtBins));

      mnEtaEta[i][j]=new etaBins[EBYE_ETA_BINS];
      memset(mnEtaEta[i][j],0,EBYE_ETA_BINS*sizeof(etaBins));
      maEtaEta[i][j]=new etaBins[EBYE_ETA_BINS];
      memset(maEtaEta[i][j],0,EBYE_ETA_BINS*sizeof(etaBins));
      mbEtaEta[i][j]=new etaBins[EBYE_ETA_BINS];
      memset(mbEtaEta[i][j],0,EBYE_ETA_BINS*sizeof(etaBins));
      mcEtaEta[i][j]=new etaBins[EBYE_ETA_BINS];
      memset(mcEtaEta[i][j],0,EBYE_ETA_BINS*sizeof(etaBins));

      mnPhiPhi[i][j]=new phiBins[EBYE_PHI_BINS];
      memset(mnPhiPhi[i][j],0,EBYE_PHI_BINS*sizeof(phiBins));
      maPhiPhi[i][j]=new phiBins[EBYE_PHI_BINS];
      memset(maPhiPhi[i][j],0,EBYE_PHI_BINS*sizeof(phiBins));
      mbPhiPhi[i][j]=new phiBins[EBYE_PHI_BINS];
      memset(mbPhiPhi[i][j],0,EBYE_PHI_BINS*sizeof(phiBins));
      mcPhiPhi[i][j]=new phiBins[EBYE_PHI_BINS];
      memset(mcPhiPhi[i][j],0,EBYE_PHI_BINS*sizeof(phiBins));

      mnJtDEtaDPhi[i][j]=new dphiBins[EBYE_DETA_BINS];
      memset(mnJtDEtaDPhi[i][j],0,EBYE_DETA_BINS*sizeof(dphiBins));
      maJtDEtaDPhi[i][j]=new dphiBins[EBYE_DETA_BINS];
      memset(maJtDEtaDPhi[i][j],0,EBYE_DETA_BINS*sizeof(dphiBins));
      mbJtDEtaDPhi[i][j]=new dphiBins[EBYE_DETA_BINS];
      memset(mbJtDEtaDPhi[i][j],0,EBYE_DETA_BINS*sizeof(dphiBins));
      mcJtDEtaDPhi[i][j]=new dphiBins[EBYE_DETA_BINS];
      memset(mcJtDEtaDPhi[i][j],0,EBYE_DETA_BINS*sizeof(dphiBins));
    }
  }
  StEStructBinning* b=StEStructBinning::Instance();
  char* bName[]={"Sibpp","Sibpm","Sibmm","Mixpp","Mixpm","Mixmm"};
  char* bTitle[]={"Sibling : +.+","Sibling : +.- + -.+","Sibling : -.-","Mixed : +.+","Mixed : +.- + -.+","Mixed : -.-"};
  for(int i = 0; i < 6; i++) {
    mnHMtMt[i]       = new TH2F*[numCutBins];
    mnHEtaEta[i]     = new TH2F*[numCutBins];
    mnHPhiPhi[i]     = new TH2F*[numCutBins];
    mnHJtDEtaDPhi[i] = new TH2F*[numCutBins];
    maHMtMt[i]       = new TH2F*[numCutBins];
    maHEtaEta[i]     = new TH2F*[numCutBins];
    maHPhiPhi[i]     = new TH2F*[numCutBins];
    maHJtDEtaDPhi[i] = new TH2F*[numCutBins];
    mbHMtMt[i]       = new TH2F*[numCutBins];
    mbHEtaEta[i]     = new TH2F*[numCutBins];
    mbHPhiPhi[i]     = new TH2F*[numCutBins];
    mbHJtDEtaDPhi[i] = new TH2F*[numCutBins];
    mcHMtMt[i]       = new TH2F*[numCutBins];
    mcHEtaEta[i]     = new TH2F*[numCutBins];
    mcHPhiPhi[i]     = new TH2F*[numCutBins];
    mcHJtDEtaDPhi[i] = new TH2F*[numCutBins];

    for(int j = 0; j < numCutBins; j++) {
      TString nnMtMt(bName[i]); nnMtMt += "nMtMt";
      if(numCutBins > 1) nnMtMt += j;
      TString ntMtMt("nMtMt"); ntMtMt += bTitle[i];
      mnHMtMt[i][j]=new TH2F(nnMtMt.Data(),ntMtMt.Data(),b->mtBins(),b->mtMin(),b->mtMax(),b->mtBins(),b->mtMin(),b->mtMax());

      TString nnEtaEta(bName[i]); nnEtaEta += "nEtaEta";
      if(numCutBins > 1) nnEtaEta += j;
      TString ntEtaEta("nEtaEta"); ntEtaEta += bTitle[i];
      mnHEtaEta[i][j]=new TH2F(nnEtaEta.Data(),ntEtaEta.Data(),b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());

      TString nnPhiPhi(bName[i]); nnPhiPhi += "nPhiPhi";
      if(numCutBins > 1) nnPhiPhi += j;
      TString ntPhiPhi("nPhiPhi"); ntPhiPhi += bTitle[i];
      mnHPhiPhi[i][j]=new TH2F(nnPhiPhi.Data(),ntPhiPhi.Data(),b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());

      TString nnDEtaDPhi(bName[i]); nnDEtaDPhi += "nDEtaDPhi";
      if(numCutBins > 1) nnDEtaDPhi += j;
      TString ntDEtaDPhi("Joint nDEtaDPhi"); ntDEtaDPhi += bTitle[i];
      mnHJtDEtaDPhi[i][j]=new TH2F(nnDEtaDPhi.Data(),ntDEtaDPhi.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      // first set
      TString anMtMt(bName[i]); anMtMt+="aMtMt";
      if(numCutBins>1)anMtMt+=j;
      TString atMtMt("aMtMt"); atMtMt+=bTitle[i];
      maHMtMt[i][j]=new TH2F(anMtMt.Data(),atMtMt.Data(),b->mtBins(),b->mtMin(),b->mtMax(),b->mtBins(),b->mtMin(),b->mtMax());

      TString anEtaEta(bName[i]); anEtaEta+="aEtaEta"; 
      if(numCutBins>1)anEtaEta+=j;
      TString atEtaEta("aEtaEta"); atEtaEta+=bTitle[i];
      maHEtaEta[i][j]=new TH2F(anEtaEta.Data(),atEtaEta.Data(),b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());

      TString anPhiPhi(bName[i]); anPhiPhi+="aPhiPhi"; 
      if(numCutBins>1)anPhiPhi+=j;
      TString atPhiPhi("aPhiPhi"); atPhiPhi+=bTitle[i];
      maHPhiPhi[i][j]=new TH2F(anPhiPhi.Data(),atPhiPhi.Data(),b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());

      TString anDEtaDPhi(bName[i]); anDEtaDPhi+="aDEtaDPhi"; 
      if(numCutBins>1)anDEtaDPhi+=j;
      TString atDEtaDPhi("Joint aDEtaDPhi"); atDEtaDPhi+=bTitle[i];
      maHJtDEtaDPhi[i][j]=new TH2F(anDEtaDPhi.Data(),atDEtaDPhi.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      //second set
      TString bnMtMt(bName[i]); bnMtMt+="bMtMt";
      if(numCutBins>1)bnMtMt+=j;
      TString btMtMt("bMtMt"); btMtMt+=bTitle[i]; 
      mbHMtMt[i][j]=new TH2F(bnMtMt.Data(),btMtMt.Data(),b->mtBins(),b->mtMin(),b->mtMax(),b->mtBins(),b->mtMin(),b->mtMax());

      TString bnEtaEta(bName[i]); bnEtaEta+="bEtaEta"; 
      if(numCutBins>1)bnEtaEta+=j;
      TString btEtaEta("bEtaEta"); btEtaEta+=bTitle[i];
      mbHEtaEta[i][j]=new TH2F(bnEtaEta.Data(),btEtaEta.Data(),b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());

      TString bnPhiPhi(bName[i]); bnPhiPhi+="bPhiPhi"; 
      if(numCutBins>1)bnPhiPhi+=j;
      TString btPhiPhi("bPhiPhi"); btPhiPhi+=bTitle[i];
      mbHPhiPhi[i][j]=new TH2F(bnPhiPhi.Data(),btPhiPhi.Data(),b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());

      TString bnDEtaDPhi(bName[i]); bnDEtaDPhi+="bDEtaDPhi"; 
      if(numCutBins>1)bnDEtaDPhi+=j;
      TString btDEtaDPhi("Joint bDEtaDPhi"); btDEtaDPhi+=bTitle[i];
      mbHJtDEtaDPhi[i][j]=new TH2F(bnDEtaDPhi.Data(),btDEtaDPhi.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      //third set
      TString cnMtMt(bName[i]); cnMtMt+="cMtMt";
      if(numCutBins>1)cnMtMt+=j;
      TString ctMtMt("cMtMt"); ctMtMt+=bTitle[i]; 
      mcHMtMt[i][j]=new TH2F(cnMtMt.Data(),ctMtMt.Data(),b->mtBins(),b->mtMin(),b->mtMax(),b->mtBins(),b->mtMin(),b->mtMax());

      TString cnEtaEta(bName[i]); cnEtaEta+="cEtaEta"; 
      if(numCutBins>1)cnEtaEta+=j;
      TString ctEtaEta("cEtaEta"); ctEtaEta+=bTitle[i];
      mcHEtaEta[i][j]=new TH2F(cnEtaEta.Data(),ctEtaEta.Data(),b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());

      TString cnPhiPhi(bName[i]); cnPhiPhi+="cPhiPhi"; 
      if(numCutBins>1)cnPhiPhi+=j;
      TString ctPhiPhi("cPhiPhi"); ctPhiPhi+=bTitle[i];
      mcHPhiPhi[i][j]=new TH2F(cnPhiPhi.Data(),ctPhiPhi.Data(),b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());

      TString cnDEtaDPhi(bName[i]); cnDEtaDPhi+="cDEtaDPhi"; 
      if(numCutBins>1)cnDEtaDPhi+=j;
      TString ctDEtaDPhi("Joint cDEtaDPhi"); ctDEtaDPhi+=bTitle[i];
      mcHJtDEtaDPhi[i][j]=new TH2F(cnDEtaDPhi.Data(),ctDEtaDPhi.Data(),b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    }
  }
  // pt distribution
  mHpt = new TH1F("pt","pt",100,0,3);
}

//--------------------------------------------------------------------------
void StEStruct2ptPtNbar::deleteArraysAndHistograms(){

  const int numCutBins = StEStructCutBin::Instance()->getNumBins();

  for(int i = 0; i < 6; i++) {
    for(int j = 0; j < numCutBins; j++) {
      delete [] mnMtMt[i][j];
      delete [] mnEtaEta[i][j];
      delete [] mnPhiPhi[i][j];
      delete [] mnJtDEtaDPhi[i][j];
      delete [] maMtMt[i][j];
      delete [] maEtaEta[i][j];
      delete [] maPhiPhi[i][j];
      delete [] maJtDEtaDPhi[i][j];
      delete [] mbMtMt[i][j];
      delete [] mbEtaEta[i][j];
      delete [] mbPhiPhi[i][j];
      delete [] mbJtDEtaDPhi[i][j];
      delete [] mcMtMt[i][j];
      delete [] mcEtaEta[i][j];
      delete [] mcPhiPhi[i][j];
      delete [] mcJtDEtaDPhi[i][j];
    }
    delete [] mnMtMt[i];
    delete [] mnEtaEta[i];
    delete [] mnPhiPhi[i];
    delete [] mnJtDEtaDPhi[i];
    delete [] maMtMt[i];
    delete [] maEtaEta[i];
    delete [] maPhiPhi[i];
    delete [] maJtDEtaDPhi[i];
    delete [] mbMtMt[i];
    delete [] mbEtaEta[i];
    delete [] mbPhiPhi[i];
    delete [] mbJtDEtaDPhi[i];
    delete [] mcMtMt[i];
    delete [] mcEtaEta[i];
    delete [] mcPhiPhi[i];
    delete [] mcJtDEtaDPhi[i];
  }
  for(int i = 0; i < 6; i++) {
    for(int j = 0; j < numCutBins; j++) {
      delete mnHMtMt[i][j];
      delete mnHEtaEta[i][j];
      delete mnHPhiPhi[i][j];
      delete mnHJtDEtaDPhi[i][j];
      delete maHMtMt[i][j];
      delete maHEtaEta[i][j];
      delete maHPhiPhi[i][j];
      delete maHJtDEtaDPhi[i][j];
      delete mbHMtMt[i][j];
      delete mbHEtaEta[i][j];
      delete mbHPhiPhi[i][j];
      delete mbHJtDEtaDPhi[i][j];
      delete mcHMtMt[i][j];
      delete mcHEtaEta[i][j];
      delete mcHPhiPhi[i][j];
      delete mcHJtDEtaDPhi[i][j];
    }
    delete [] mnHMtMt[i];
    delete [] mnHEtaEta[i];
    delete [] mnHPhiPhi[i];
    delete [] mnHJtDEtaDPhi[i];
    delete [] maHMtMt[i];
    delete [] maHEtaEta[i];
    delete [] maHPhiPhi[i];
    delete [] maHJtDEtaDPhi[i];
    delete [] mbHMtMt[i];
    delete [] mbHEtaEta[i];
    delete [] mbHPhiPhi[i];
    delete [] mbHJtDEtaDPhi[i];
    delete [] mcHMtMt[i];
    delete [] mcHEtaEta[i];
    delete [] mcHPhiPhi[i];
    delete [] mcHJtDEtaDPhi[i];
  }
  delete [] mHNEvents;
  delete mHpt;
}

/***********************************************************************
 *
 * $Log: StEStruct2ptPtNbar.cxx,v $
 * Revision 1.1  2004/09/16 23:49:21  chunhuih
 * Initial storage of the 2-point Pt correlation to the EStruct CVS repository.
 * Nbar means that we use N_bar in the denominator in the calculation.
 *
 *
 *********************************************************************/
