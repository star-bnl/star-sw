/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.cxx,v 1.17 2006/05/03 17:52:11 msd Exp $
 *
 * Author: Jeff Porter adaptation of Aya's 2pt-analysis
 *
 **********************************************************************
 *
 * Description:  Analysis code for 2pt-analysis. 
 *    The analysis runs as follows:
 *       1D and 2D arrays (in yt,eta,phi) are setup
 *       and filled for each of the 8 pair types:
 *       Sibling (++, +-, -+, --)
 *       Mixed   (++, +-, -+, --)
 *       Particle id is done via dEdx and introduced into the analysis via cut-bins.
 *       Order particles so pi always come before K before p and plus comes before
 *       minus. 2D histograms will not be guaranteed to be symmetric.
 *       The 2D versions are additionally divided into z-vertex (via the StEStructBuffer)
 *       After arrays are filled (looped over all events/job), Histograms are 
 *       created, filled, and written out to the data file for further
 *       processing.
 *
 *       Note that if we find charge symmetry we can form LS, US, CD and CI
 *       combinations using these histograms.
 *
 *
 ***********************************************************************/
#include "StEStruct2ptCorrelations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/Correlations/StEStructMaxB.h"  
#include "StEStructPool/Correlations/StEStructBuffer.h"  // to read some constants  

#include "StTimer.hh"
#include "StEStructCutBin.h"

#include "Stiostream.h"

#include "StMessMgr.h"

#include <iostream>
using namespace std;

ClassImp(StEStruct2ptCorrelations);

//--------------------------------------------------------------------------
StEStruct2ptCorrelations::StEStruct2ptCorrelations(int mode) {
  initInternalData();
  manalysisMode=mode;
  mPairCuts = new StEStructPairCuts; 
}


//--------------------------------------------------------------------------
StEStruct2ptCorrelations::StEStruct2ptCorrelations(StEStructPairCuts* pcuts, int mode) {
  initInternalData();
  manalysisMode=mode;
  mPairCuts=pcuts;

}

//----------------------------------------------------------
void  StEStruct2ptCorrelations::initInternalData(){

  mPairCuts=NULL;
  mQAHists = NULL;
  mCurrentEvent = NULL;
  mMixingEvent = NULL;
  moutFileName = NULL;
  mqaoutFileName = NULL;

  mskipPairCuts            = false;
  mdoPairCutHistograms     = false;
  mdoPairDensityHistograms = false;
  mskipEtaDeltaWeight      = false;

  mInit=false;
  mDeleted=false;
  mHistosWritten = false;
  mlocalQAHists = false;
  manalysisIndex = -1;

  kZBuffMin   = -75.0;
  kZBuffMax   = +75.0;
  kBuffWidth  = 5.0;
  kNumBuffers = int( (kZBuffMax-kZBuffMin)/kBuffWidth );  // Better be 30.
  //  for(int i=0;i<kNumBuffers;i++)mbuffCounter[i]=0;
}



//--------------------------------------------------------------------------
StEStruct2ptCorrelations::~StEStruct2ptCorrelations(){ cleanUp(); };


void StEStruct2ptCorrelations::init(){

  cout << "Initializing with analysis mode " << manalysisMode << endl;

  mCurrentEvent=NULL;
  mtimer=NULL;

   //--> code to simplify hist-creation via class held name defs
   char* _tmpName[]={"Sibpp","Sibpm","Sibmp","Sibmm","Mixpp","Mixpm","Mixmp","Mixmm"};
   char* _tmpTitle[]={"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-",
                      "Mixed : +.+","Mixed : +.-","Mixed : -.+","Mixed : -.-"};

   for(int i=0;i<8;i++){
     bName[i]=new char[6];
     strcpy(bName[i],_tmpName[i]);
     bTitle[i]=new char[20];
     strcpy(bTitle[i],_tmpTitle[i]);
   }

  if(manalysisMode & 1) {
     mskipPairCuts=true;
  } 
  if(manalysisMode & 2){
    mdoPairCutHistograms=true;
  }
  if(manalysisMode & 4){
    mdoPairDensityHistograms=true;
  }
  if(manalysisMode & 8){
    mskipEtaDeltaWeight = true;
  }

  cout << "  Skip Pair Cuts = " << mskipPairCuts << endl;
  cout << "  Do Pair Cut Hists = " << mdoPairCutHistograms << endl;
  cout << "  Do Pair Density Hists = " << mdoPairDensityHistograms << endl;
  cout << "  Skip EtaDelta weights = " << mskipEtaDeltaWeight << endl;

  for(int i=0;i<8;i++)numPairs[i]=numPairsProcessed[i]=mpossiblePairs[i]=0;

//
  initArrays();
// Try allocating histograms at start of job.
// If we don't add histograms to directory we don't get the obnoxious
//   Potential memory leak error.
  TH1::AddDirectory(kFALSE);
  initHistograms();

  /* Event count via Nch distribution */
  mHNEvents[0]=new TH1D("NEventsSame","NEventsSame",1000,0.,2000.);
  mHNEvents[1]=new TH1D("NEventsMixed","NEventsMixed",1000,0.,2000.);
  

  StEStructCutBin* cb = StEStructCutBin::Instance();
  int ncutbins=cb->getNumBins();
  int nQAbins=cb->getNumQABins();


  /* QA histograms */
  if(!mQAHists) {
    mlocalQAHists = true;
    cout<<"createing qa hists"<<endl;
    mQAHists = new StEStructQAHists(); // if not set .. assume data
    mQAHists->initTrackHistograms(nQAbins,analysisIndex());
  } else {
    cout<<"init QA Hists"<<endl;
    mQAHists->initTrackHistograms(nQAbins);  // will do it for only 1 analysis
  }

  /* Inclusive pt distribution (per cutbin) */
  mHptAll = new TH1D("ptAll","ptAll",500,0,10.);
  mHpt[0] = new TH1D*[ncutbins];
  mHpt[1] = new TH1D*[ncutbins];
  for(int i=0;i<ncutbins;i++){
    TString hnamea("pta");
    if(ncutbins>1)hnamea+=i;
    mHpt[0][i] = new TH1D(hnamea.Data(),hnamea.Data(),500,0,10.);
    TString hnameb("ptb");
    if(ncutbins>1)hnameb+=i;
    mHpt[1][i] = new TH1D(hnameb.Data(),hnameb.Data(),500,0,10.);
  }

  /* Event Mixing Parameters */
  mHmix = new TH2D("EventMixing","Event Mixing: delta-Z vs delta-N",50,0,10, 50,0,200);  // deltaZ vs deltaN
  TH1::AddDirectory(kTRUE);

  mInit=true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::finish(){

  if(!moutFileName){
    cout<<" NO OUTPUTFILE TO WRITE TO ..... giving up ...."<<endl;
    return;
  }

  if(!mInit){  // TEST 
    cout<<" WARNING: init=false"<<endl;
    return;
  }

  // We call finish explicitly in doEStruct, but it also gets called when
  // we tell root to quit. I don't know the details of why. If we have
  // already written histograms don't bother a second time.
  // (Also I think finish() will invoke cleanUp() so second time here
  //  all the arrays will be gone and we get a core dump if we try
  //  touching them.)
  if (mHistosWritten) {
      return;
  }
//  initHistograms();
  fillHistograms();
  TFile * tf=new TFile(moutFileName,"RECREATE");
  tf->cd();
  writeHistograms();
  tf->Close();
  // When we don't delete tf it gets reported as a memory leak
  // (which it is, but we are quitting anyway so why bother with it?)
  mHistosWritten = true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::cleanUp(){ 
  if(mDeleted) return;
  deleteHistograms();
  deleteArrays(); 

  delete mHNEvents[0];
  delete mHNEvents[1];
  delete mHptAll;
  StEStructCutBin* cb = StEStructCutBin::Instance();
  if (cb) {
      int ncutbins=cb->getNumBins();
      for(int i=0;i<ncutbins;i++){
        delete mHpt[0][i];
        delete mHpt[1][i];
      }
      delete [] mHpt[0];
      delete [] mHpt[1];
  }
  mDeleted=true;
}

//--------------------------------------------------------------------------
// Parse cuts file for limits on vertex position.
// Want 5cm wide buffers.
void StEStruct2ptCorrelations::setZBuffLimits(StEStructCuts* ecut) {

  kZBuffMin = ecut->minVal("primaryVertexZ");
  kZBuffMax = ecut->maxVal("primaryVertexZ");
  kNumBuffers = int( (kZBuffMax-kZBuffMin)/kBuffWidth ); 

  if(kZBuffMin==kZBuffMax){  // no z vertex cut ...
    kZBuffMin=-100.;
    kZBuffMax=100.;
    kNumBuffers = 1;
    kBuffWidth= (kZBuffMax - kZBuffMin) / kNumBuffers; // adjust widths
  }

  cout<<"Setting ZBuffers:  Max="<<kZBuffMax<<" Min="<<kZBuffMin<<" NumBuff="<<kNumBuffers<<endl;
  if(kNumBuffers<=_MAX_ZVBINS_) return; // we're good to go

  kNumBuffers=_MAX_ZVBINS_;  
  kBuffWidth= (kZBuffMax - kZBuffMin) / kNumBuffers; // adjust widths
  if(kBuffWidth>6.5) { 
     gMessMgr->Warning()<<" Zvertex Width ="<<kBuffWidth<<" gt 6.5 cm"<<endm;
  }
}
//---------------------------------------------------------------------
void StEStruct2ptCorrelations::writeDiagnostics(){

  int nIn=0;
  int nOut=0;
  for(int i=0;i<kNumBuffers;i++){
    nIn  += mbuffer[i].numEventsIn();
    nOut += mbuffer[i].numEventsDeleted();
  }
  cout<<" Analysis["<<analysisIndex()<<"] recieved "<<nIn<<" useful events, deleted "<<nOut<<" of them"<<endl;

}

//
//-------  Analysis Function ------//
//
//--------------------------------------------------------------------------
bool StEStruct2ptCorrelations::doEvent(StEStructEvent* event){

  if(!event) {
   mCurrentEvent=NULL;
   return false;
  }

  if(mInit == false) init();

  if(2>event->Ntrack()){
    delete event;
    return true;
  }

  //cout << "Doing event " << event->EventID() << ":  " << event->Ntrack() << " tracks." << endl;

  moveEvents();
  mCurrentEvent=event;
  mHNEvents[0]->Fill(event->Ntrack());

  // inclusive pt distribution
  // Note that cuts are done in yt-yt space and bins are
  // set up before pid is done (so assume pion mass.)
  // I think these are just diagnostic histograms. Even so
  // I am uncomfortable with this pt histogramming.
  // (dEdx histogramming is ok though.)
  // Assign mass according to dEdx pid here.
  // djp 11/28/2005

  float Mass[] = {0.1396, 0.1396, 0.497, 0.9383};

  StEStructCutBin* cb = StEStructCutBin::Instance();

  StEStructTrackCollection *tc;
  for(int ich=0;ich<2;ich++){
    if(ich==0){
       tc=mCurrentEvent->TrackCollectionP();
    } else {
       tc=mCurrentEvent->TrackCollectionM();
    }

    for(StEStructTrackIterator iter = tc->begin(); iter != tc->end(); iter++) {

     int i = cb->getdEdxPID((*iter));
     mQAHists->fillTrackHistograms(*iter,i);

    // Choose mass according to dEdx (in which case transverse and longitudinal
    // rapidities will be calculated as actual rapidities) or set mass to 0
    // in which case we will use the quantity Jeff was using for transverse rapidity
    // (a mid-rapidity approximation for pions) and eta for logitudinal rapidity.
    // Sould really have a switch accessible from macro level instead of
    // using cutMode.
    if (cb->getMode() == 5) {
        (*iter)->SetMassAssignment(Mass[i]);
    } else {
        // 0 should be default, but just to be explicit here/
        (*iter)->SetMassAssignment(0);
    }
    // Always use psuedo-rapidity for now.
    // We are used to looking at the eta-phi plots (more or less.)
         (*iter)->SetMassAssignment(0);
    }
  }

  return makeSiblingAndMixedPairs();
}


//-----------------------------------------------------------------------
int StEStruct2ptCorrelations::bufferIndex(){
  if(!mCurrentEvent || kBuffWidth==0.) return -1;

  // this should only be in 1 place 
  return (int) floor((mCurrentEvent->VertexZ()-kZBuffMin)/kBuffWidth); 
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::moveEvents(){

  if(!mCurrentEvent) return;
  if (mCurrentEvent->VertexZ() > kZBuffMax) {
      return;
  }

  int i=bufferIndex();

  if(i<0 || i>kNumBuffers-1) return;                              
  mbuffer[i].addEvent(mCurrentEvent);
  //  mbuffCounter[i]++;

}

//--------------------------------------------------------------------------
bool StEStruct2ptCorrelations::makeSiblingAndMixedPairs(){

  if(!mCurrentEvent) return false; // logic problem!
  if (mCurrentEvent->VertexZ() > kZBuffMax) {
      return false;
  }

  int i=bufferIndex();

  if(i<0 || i>kNumBuffers-1) return false;

  makePairs(mCurrentEvent,mCurrentEvent,0);
  makePairs(mCurrentEvent,mCurrentEvent,1);
  makePairs(mCurrentEvent,mCurrentEvent,2);
  makePairs(mCurrentEvent,mCurrentEvent,3);

  mbuffer[i].resetCounter();
  int mult = mCurrentEvent->Ntrack();
  //cout << "Event " << mCurrentEvent->EventID() << ": " << mult << " tracks;   Z = " << mCurrentEvent->VertexZ() << ", i = " << i << endl;
  //cout << i <<"\t"; mbuffer[i].Print();
  while(1){
    mMixingEvent=mbuffer[i].nextEvent(mult);
    if(!mMixingEvent) break;
    //cout << "\t  mixing " << mMixingEvent->EventID() << ": " << mMixingEvent->Ntrack()  << " tracks." << endl;
    mHNEvents[1]->Fill(mMixingEvent->Ntrack());
    float deltaZ = abs(mCurrentEvent->VertexZ() - mMixingEvent->VertexZ());
    float deltaN = abs(mCurrentEvent->Ntrack() -  mMixingEvent->Ntrack());
    mHmix->Fill(deltaZ,deltaN);
    makePairs(mCurrentEvent,mMixingEvent,4);
    makePairs(mCurrentEvent,mMixingEvent,5);
    makePairs(mCurrentEvent,mMixingEvent,6);
    makePairs(mMixingEvent,mCurrentEvent,5);
    makePairs(mMixingEvent,mCurrentEvent,6);
    makePairs(mCurrentEvent,mMixingEvent,7);
  }

  return true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::makePairs(StEStructEvent* e1, StEStructEvent* e2, int j){

  if(j>=8) return;
  StEStructTrackCollection* t1;
  StEStructTrackCollection* t2;

  StEStructPairCuts& mPair = *mPairCuts;

  StEStructBinning* b = StEStructBinning::Instance();
  StEStructCutBin* cb = StEStructCutBin::Instance();

  ytBins**  ytyt        = mYtYt[j];
  ytBins**  nytyt       = mNYtYt[j];
  ptBins**  ptpt        = mPtPt[j];
  etaBins** etaeta      = mEtaEta[j];
  phiBins** phiphi      = mPhiPhi[j];
  phiBins** nphiphi     = mNPhiPhi[j];

  etaBins** pretaeta      = mPrEtaEta[j];
  phiBins** prphiphi      = mPrPhiPhi[j];
  etaBins** paetaeta      = mPaEtaEta[j];
  phiBins** paphiphi      = mPaPhiPhi[j];
  etaBins** pbetaeta      = mPbEtaEta[j];
  phiBins** pbphiphi      = mPbPhiPhi[j];

  dytBins**  atytyt      = mAtSYtDYt[j];
  dytBins**  atnytyt     = mAtNSYtDYt[j];

  dphiBins** jtdetadphi  = mJtDEtaDPhi[j];
  dphiBins** jtsetadphi  = mJtSEtaDPhi[j];
  dphiBins** jtndetadphi = mJtNDEtaDPhi[j];
  dphiBins** jtnsetadphi = mJtNSEtaDPhi[j];

  dphiBins** prjtdetadphi = mPrJtDEtaDPhi[j];
  dphiBins** prjtsetadphi = mPrJtSEtaDPhi[j];
  dphiBins** pajtdetadphi = mPaJtDEtaDPhi[j];
  dphiBins** pajtsetadphi = mPaJtSEtaDPhi[j];
  dphiBins** pbjtdetadphi = mPbJtDEtaDPhi[j];
  dphiBins** pbjtsetadphi = mPbJtSEtaDPhi[j];

  qBins*  qinv  = mQinv[j];
  qBins*  nqinv = mNQinv[j];

  /* --> rjp 4/24/2006 
     --> I removed these set when having to add both N and nwgt histograms
     --> to handle the errors correctly            

  xtBins**  xtxt        = mXtXt[j];
  dphiBins** jtdytdphi  = mJtDYtDPhi[j];
  detaBins** jtdytdeta  = mJtDYtDEta[j];
  dptBins**  atptpt     = mAtSPtDPt[j];
  dptBins**  atnptpt    = mAtNSPtDPt[j];

  */

  TPCSepBins* avgtsep = mTPCAvgTSep[j];
  TPCSepBins* avgzsep = mTPCAvgZSep[j];
  TPCSepBins* enttsep = mTPCEntTSep[j];
  TPCSepBins* entzsep = mTPCEntZSep[j];
  TPCSepBins* midtsep = mTPCMidTSep[j];
  TPCSepBins* midzsep = mTPCMidZSep[j];
  TPCSepBins* exittsep = mTPCExitTSep[j];
  TPCSepBins* exitzsep = mTPCExitZSep[j];
  TPCSepBins* midtp   = mTPCMidTdptP[j];
  TPCSepBins* midtn   = mTPCMidTdptN[j];
  TPCSepBins* midzp   = mTPCMidZdptP[j];
  TPCSepBins* midzn   = mTPCMidZdptN[j];

  TPCSepBins** avgtz =  mTPCAvgTZ[j];
  TPCSepBins** enttz =  mTPCEntTZ[j];
  TPCSepBins** midtz =  mTPCMidTZ[j];
  TPCSepBins** exittz =  mTPCExitTZ[j];
  dptBins** enttd =  mTPCEntTdpt[j];
  dptBins** midtd =  mTPCMidTdpt[j];
  dptBins** exittd =  mTPCExitTdpt[j];


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
        t2=e2->TrackCollectionP();
        mPair.setPairType(1);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }       
  case 3:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(0);
        mpossiblePairs[j]+=(int)floor(0.5*(t1->getEntries()*(t2->getEntries()-1)));
        break;
    }
  case 4:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionP();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  case 5:
    {
        t1=e1->TrackCollectionP();
        t2=e2->TrackCollectionM();
        mPair.setPairType(3);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  case 6:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionP();
        mPair.setPairType(3);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  case 7:
    {
        t1=e1->TrackCollectionM();
        t2=e2->TrackCollectionM();
        mPair.setPairType(2);
        mpossiblePairs[j]+=(int)(t1->getEntries()*t2->getEntries());
        break;
    }
  }

  // for pair cut analysis, in mixed events you need to correct for the offset from different primary vertex Z values 
  if(j>=4 && mdoPairDensityHistograms) mPair.SetZoffset(e2->VertexZ() - e1->VertexZ());
  else mPair.SetZoffset(0);

  StEStructTrackIterator Iter1;
  StEStructTrackIterator Iter2;

  int iyt1,iyt2,idyt1,idyt2,isyt;
  int ixt1,ixt2;
  int ipt1,ipt2,idpt1,idpt2,ispt;
  int ieta1,ieta2,ideta1,ideta2,iseta;
  int iphi1,iphi2,idphi1,idphi2;
  int isavgt, isavgz, isentt, isentz;
  int ismidt, ismidz, isexitt, isexitz;
  float pt1, pt2;

  float nwgt=1.0;//mPair.SigmaPt();

  if(mtimer)mtimer->start();

  float mass1, mass2;
  // Need to calculate mean p_t for tracks within each cut bin.
  // Don't want to weight track by how many pairs it is included in, so
  // we check that track has not been included in histogram before
  // adding it.
  // (Note that we also need mean p_t for all used tracks to calculate
  //  the "all" correlations we can use for comparison with other
  //  analyses. Cut bins should span the space.)
  int ncutbins = cb->getNumBins();
  int ntracks;
  t1->getEntries()>t2->getEntries() ? ntracks=t1->getEntries() : ntracks=t2->getEntries();
  int **itpt[2];
  for (int it=0;it<2;it++) {
    itpt[it] = new int*[ncutbins];
    for (int icb=0;icb<ncutbins;icb++) {
      itpt[it][icb] = new int[ntracks];
      for (int i=0;i<ntracks;i++) {
        itpt[it][icb][i] = 0;
      }
    }
  }

  int it1 = -1;
  for(Iter1=t1->begin();Iter1!=t1->end();++Iter1){
    it1++;

    mPair.SetTrack1(*Iter1);
    int it2;
    if(j==0 || j==3) { 
      Iter2=Iter1+1; 
      it2 = it1;
    } else { 
      Iter2=t2->begin(); 
      it2 = -1;
    }

    mHptAll->Fill(mPair.Track1()->Pt());

    for(; Iter2!=t2->end(); ++Iter2){
      it2++;
      numPairs[j]++;
      mPair.SetTrack2(*Iter2);
      if( mskipPairCuts || mPair.cutPair(mdoPairCutHistograms)==0){
        numPairsProcessed[j]++;
        int icb=cb->getCutBin(&mPair);

        int jt1, jt2, iSwitch;
        // Note that we use mass in calculation of rapidities. These
        // go with the tracks (which we don't switch within the pair)
        // so we keep track of the sign of Delta (is -1 when track order
        // should be switched.)
        // (Up to now (3/28/2006) we are using mass1=mass2=0 so this
        // detail shouldn't have caused any problems.)
        mass1 = mPair.Track1()->AssignedMass();
        mass2 = mPair.Track2()->AssignedMass();
        if (cb->switchBins(&mPair)) {
          iSwitch = -1;
          jt1   = it2;
          iyt1  = b->iyt(mPair.Track2()->Yt(mass2));
          ixt1  = b->ixt(mPair.Track2()->Xt());
          pt1   = mPair.Track2()->Pt();
          ipt1  = b->ipt(pt1);
          ieta1 = b->ieta(mPair.Track2()->Eta(mass2));
          iphi1 = b->iphi(mPair.Track2()->Phi());
          jt2   = it1;
          iyt2  = b->iyt(mPair.Track1()->Yt(mass1));
          ixt2  = b->ixt(mPair.Track1()->Xt());
          pt2   = mPair.Track1()->Pt();
          ipt2  = b->ipt(pt2);
          ieta2 = b->ieta(mPair.Track1()->Eta(mass1));
          iphi2 = b->iphi(mPair.Track1()->Phi());
        } else {
          iSwitch = +1;
          jt1   = it1;
          iyt1  = b->iyt(mPair.Track1()->Yt(mass1));
          ixt1  = b->ixt(mPair.Track1()->Xt());
          pt1   = mPair.Track1()->Pt();
          ipt1  = b->ipt(pt1);
          ieta1 = b->ieta(mPair.Track1()->Eta(mass1));
          iphi1 = b->iphi(mPair.Track1()->Phi());
          jt2   = it2;
          iyt2  = b->iyt(mPair.Track2()->Yt(mass2));
          ixt2  = b->ixt(mPair.Track2()->Xt());
          pt2   = mPair.Track2()->Pt();
          ipt2  = b->ipt(pt2);
          ieta2 = b->ieta(mPair.Track2()->Eta(mass2));
          iphi2 = b->iphi(mPair.Track2()->Phi());
        }
        int symmetrize = cb->symmetrizeBins(&mPair);
        if (itpt[0][icb][jt1] == 0) {
            mHpt[0][icb]->Fill(pt1);
            itpt[0][icb][jt1] = 1;
        }
        if (itpt[1][icb][jt2] == 0) {
            mHpt[1][icb]->Fill(pt2);
            itpt[1][icb][jt2] = 1;
        }
        if (symmetrize) {
            if (itpt[0][icb][jt2] == 0) {
                mHpt[0][icb]->Fill(pt2);
                itpt[0][icb][jt2] = 1;
            }
            if (itpt[1][icb][jt1] == 0) {
                mHpt[1][icb]->Fill(pt1);
                itpt[1][icb][jt1] = 1;
            }
        }

        float pwgt=pt1*pt2;
        float spt1=pt1;
        float spt2=pt2;
        float wgt = 1.0;
        nwgt = wgt;
        if( !mskipEtaDeltaWeight ) {
           nwgt = b->getDEtaWeight(mPair.DeltaEta());
           pwgt*=nwgt;
           spt1*=nwgt;
           spt2*=nwgt;
	}

        //-> X vs X
        ytyt[icb][iyt1].yt[iyt2]+=nwgt;
        nytyt[icb][iyt1].yt[iyt2]+=1;
	//        xtxt[icb][ixt1].xt[ixt2]+=nwgt;
        ptpt[icb][ipt1].pt[ipt2]+=nwgt;
        etaeta[icb][ieta1].eta[ieta2]+=1; // nwgt;
        phiphi[icb][iphi1].phi[iphi2]+=nwgt;
        nphiphi[icb][iphi1].phi[iphi2]+=1;

        pretaeta[icb][ieta1].eta[ieta2]+=pwgt/nwgt;
        prphiphi[icb][iphi1].phi[iphi2]+=pwgt;

        paetaeta[icb][ieta1].eta[ieta2]+=spt1/nwgt;
        paphiphi[icb][iphi1].phi[iphi2]+=spt1;

        pbetaeta[icb][ieta1].eta[ieta2]+=spt2/nwgt;
        pbphiphi[icb][iphi1].phi[iphi2]+=spt2;

        //-> X vs X (symmetry)
        if (symmetrize) {
          ytyt[icb][iyt2].yt[iyt1]+=nwgt;
          nytyt[icb][iyt2].yt[iyt1]+=1;
	  //          xtxt[icb][ixt2].xt[ixt1]+=nwgt;
          ptpt[icb][ipt2].pt[ipt1]+=nwgt;
          etaeta[icb][ieta2].eta[ieta1]+=1; // nwgt;
          phiphi[icb][iphi2].phi[iphi1]+=nwgt;
          nphiphi[icb][iphi2].phi[iphi1]+=1;

          pretaeta[icb][ieta2].eta[ieta1]+=pwgt/nwgt;
          prphiphi[icb][iphi2].phi[iphi1]+=pwgt;

          paetaeta[icb][ieta2].eta[ieta1]+=spt1/nwgt;
          paphiphi[icb][iphi2].phi[iphi1]+=spt1;

          pbetaeta[icb][ieta2].eta[ieta1]+=spt2/nwgt;
          pbphiphi[icb][iphi2].phi[iphi1]+=spt2;
        }

        //-> delta y vs delta x
        idyt1 = b->idyt(iSwitch*mPair.DeltaYt(mass1,mass2));
        idpt1 = b->idpt(iSwitch*mPair.DeltaPt());
        ideta1= b->ideta(iSwitch*mPair.DeltaEta(mass1,mass2));
        idphi1= b->idphi(iSwitch*mPair.DeltaPhi());

        isyt = b->isyt(mPair.SigmaYt(mass1,mass2));
        ispt = b->ispt(mPair.SigmaPt());
        iseta= b->iseta(mPair.SigmaEta(mass1,mass2));
        
        //--- symmetry ---
        // Note that the differences, dyt, dpt, deta and dphi,
        // actually fill different parts of the histograms.
//>>>>> Is this true? I'm sure I saw in a test job that when I only
//      filled ideta1,idphi1 I got 1/4 of the \eta_\Delta - \phi_\Delta
//      space, but I don't see why this should be.
//
//      methods idyt, idpt, ideta and idphi return fabs( difference ).
//      When we get rid of the fabs() we can do away with idyt2, idpt2, ideta2 and idphi2.
//      (put them into an if (symmetrize ) block.
        jtdetadphi[icb][ideta1].dphi[idphi1] +=nwgt;
        jtndetadphi[icb][ideta1].dphi[idphi1] +=1;
	//        jtdytdphi[icb][idyt1].dphi[idphi1]   +=nwgt;
	//        jtdytdeta[icb][idyt1].deta[ideta1]   +=nwgt;

        prjtdetadphi[icb][ideta1].dphi[idphi1] += pwgt;
        pajtdetadphi[icb][ideta1].dphi[idphi1] += spt1;
        pbjtdetadphi[icb][ideta1].dphi[idphi1] += spt2;

        if (symmetrize) {
            idyt2 = b->idyt(-1.*iSwitch*mPair.DeltaYt(mass1,mass2));
            idpt2 = b->idpt(-1.*iSwitch*mPair.DeltaPt());
            ideta2= b->ideta(-1.*iSwitch*mPair.DeltaEta(mass1,mass2));
            idphi2= b->idphi(-1.*iSwitch*mPair.DeltaPhi());

            jtdetadphi[icb][ideta1].dphi[idphi2] +=nwgt;
            jtdetadphi[icb][ideta2].dphi[idphi1] +=nwgt;
            jtdetadphi[icb][ideta2].dphi[idphi2] +=nwgt;
            jtndetadphi[icb][ideta1].dphi[idphi2] +=1;
            jtndetadphi[icb][ideta2].dphi[idphi1] +=1;
            jtndetadphi[icb][ideta2].dphi[idphi2] +=1;

	    /* 
            jtdytdphi[icb][idyt1].dphi[idphi2] +=nwgt;
	    jtdytdphi[icb][idyt2].dphi[idphi1] +=nwgt;
	    jtdytdphi[icb][idyt2].dphi[idphi2] +=nwgt;

            jtdytdeta[icb][idyt1].deta[ideta2] +=nwgt;
            jtdytdeta[icb][idyt2].deta[ideta1] +=nwgt;
            jtdytdeta[icb][idyt2].deta[ideta2] +=nwgt;
	    */
            prjtdetadphi[icb][ideta1].dphi[idphi2] += pwgt;
            prjtdetadphi[icb][ideta2].dphi[idphi1] += pwgt;
            prjtdetadphi[icb][ideta2].dphi[idphi2] += pwgt;

            pajtdetadphi[icb][ideta1].dphi[idphi2] += spt1;
            pajtdetadphi[icb][ideta2].dphi[idphi1] += spt1;
            pajtdetadphi[icb][ideta2].dphi[idphi2] += spt1;

            pbjtdetadphi[icb][ideta1].dphi[idphi2] += spt2;
            pbjtdetadphi[icb][ideta2].dphi[idphi1] += spt2;
            pbjtdetadphi[icb][ideta2].dphi[idphi2] += spt2;
        }

       //-> Sum y vs delta x
       // For symmetry only reflect around the delta axis.
       atytyt[icb][isyt].dyt[idyt1] +=nwgt;
       atnytyt[icb][isyt].dyt[idyt1] +=1;
//       atptpt[icb][ispt].dpt[idpt1] +=nwgt;
       if (symmetrize) {
            atytyt[icb][isyt].dyt[idyt2] +=nwgt;
            atnytyt[icb][isyt].dyt[idyt2] +=1;
//          atptpt[icb][ispt].dpt[idpt2] +=nwgt;
        }

        jtsetadphi[icb][iseta].dphi[idphi1]+=nwgt;
        jtnsetadphi[icb][iseta].dphi[idphi1]+=1;
        prjtsetadphi[icb][iseta].dphi[idphi1] += pwgt;
        pajtsetadphi[icb][iseta].dphi[idphi1] += spt1;
        pbjtsetadphi[icb][iseta].dphi[idphi1] += spt2;
        if (symmetrize) {
            jtsetadphi[icb][iseta].dphi[idphi2]+=nwgt;
            jtnsetadphi[icb][iseta].dphi[idphi2]+=1;
            prjtsetadphi[icb][iseta].dphi[idphi2]+= pwgt;
            pajtsetadphi[icb][iseta].dphi[idphi2]+= spt1;
            pbjtsetadphi[icb][iseta].dphi[idphi2]+= spt2;
        }

        qinv[icb].q[b->iq(mPair.qInv())]+=nwgt;
        nqinv[icb].q[b->iq(mPair.qInv())]+=1;
    
        if(mdoPairDensityHistograms) {
          avgtsep[icb].sep[isavgt=b->isep(mPair.NominalTpcAvgXYSeparation())]+=nwgt;
          avgzsep[icb].sep[isavgz=b->isep(mPair.NominalTpcAvgZSeparation())]+=nwgt;
          enttsep[icb].sep[isentt=b->isep(mPair.NominalTpcXYEntranceSeparation())]+=nwgt;
          entzsep[icb].sep[isentz=b->isep(mPair.NominalTpcZEntranceSeparation())]+=nwgt;
          midtsep[icb].sep[ismidt=b->isep(mPair.MidTpcXYSeparation())]+=nwgt;
          midzsep[icb].sep[ismidz=b->isep(mPair.MidTpcZSeparation())]+=nwgt;
          exittsep[icb].sep[isexitt=b->isep(mPair.NominalTpcXYExitSeparation())]+=nwgt;
          exitzsep[icb].sep[isexitz=b->isep(mPair.NominalTpcZExitSeparation())]+=nwgt;

          avgtz[icb][isavgt].sep[isavgz]+=nwgt;
          enttz[icb][isentt].sep[isentz]+=nwgt;
          midtz[icb][ismidt].sep[ismidz]+=nwgt;
          exittz[icb][isexitt].sep[isexitz]+=nwgt;     

          // need to rearrange pair so that deltaPhi>0
          float delpt; // my delta pt 
          int idelpt;  // bin index
          if (mPair.Track1()->Phi() - mPair.Track2()->Phi() >= 0) { 
            delpt = mPair.DeltaPt();  // here delta=1-2, can use standard defs
            idelpt = idpt1;
          } else {  // redefine delta=2-1
            delpt = -1.*mPair.DeltaPt();
            if (symmetrize) idelpt = idpt2; 
	    else  idelpt = b->idpt(-1.*iSwitch*mPair.DeltaPt()); // idpt2 undefined 
          }
          enttd[icb][isentt].dpt[idelpt]+=nwgt;  
          midtd[icb][ismidt].dpt[idelpt]+=nwgt;
          exittd[icb][isexitt].dpt[idelpt]+=nwgt;
          if (delpt>=0) {                                     
            midtp[icb].sep[b->isep(mPair.MidTpcXYSeparation())]+=nwgt;  
            midzp[icb].sep[b->isep(mPair.MidTpcZSeparation()) ]+=nwgt;
          } else {
            midtn[icb].sep[b->isep(mPair.MidTpcXYSeparation())]+=nwgt;
            midzn[icb].sep[b->isep(mPair.MidTpcZSeparation()) ]+=nwgt;
          }

        } // pair density
      };// pair cut
    };// iter2 loop
  };// iter 1 loop

  for (int it=0;it<2;it++) {
    for (int icb=0; icb<ncutbins; icb++) {
      delete [] itpt[it][icb];
    }
    delete [] itpt[it];
  }


  if(mtimer)mtimer->stop();
}


//
//------------ Below are init, delete, write functions -------///
//

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::fillHistograms(){

  float xv,yv;
  StEStructBinning* b=StEStructBinning::Instance();
  int numCutBins=StEStructCutBin::Instance()->getNumBins();
  for(int i=0; i<8; i++){

    ytBins**  ytyt   = mYtYt[i];
    ytBins**  nytyt   = mNYtYt[i];
    //    xtBins**  xtxt   = mXtXt[i];
    ptBins**  ptpt   = mPtPt[i];

    etaBins** etaeta  = mEtaEta[i];
    phiBins** phiphi  = mPhiPhi[i];
    qBins*    qinv    = mQinv[i];
    phiBins** nphiphi = mNPhiPhi[i];
    qBins*    nqinv   = mNQinv[i];

    etaBins** pretaeta = mPrEtaEta[i];
    phiBins** prphiphi = mPrPhiPhi[i];
    etaBins** paetaeta = mPaEtaEta[i];
    phiBins** paphiphi = mPaPhiPhi[i];
    etaBins** pbetaeta = mPbEtaEta[i];
    phiBins** pbphiphi = mPbPhiPhi[i];

    dytBins**  atytyt   = mAtSYtDYt[i];
    dytBins**  atnytyt   = mAtNSYtDYt[i];
    //    dptBins**  atptpt   = mAtSPtDPt[i];

    //   dphiBins** jtdytdphi = mJtDYtDPhi[i];
    //    detaBins** jtdytdeta = mJtDYtDEta[i];
    dphiBins** jtdetadphi = mJtDEtaDPhi[i];
    dphiBins** jtsetadphi = mJtSEtaDPhi[i];
    dphiBins** jtndetadphi = mJtNDEtaDPhi[i];
    dphiBins** jtnsetadphi = mJtNSEtaDPhi[i];

    dphiBins** prjtdetadphi = mPrJtDEtaDPhi[i];
    dphiBins** prjtsetadphi = mPrJtSEtaDPhi[i];
    dphiBins** pajtdetadphi = mPaJtDEtaDPhi[i];
    dphiBins** pajtsetadphi = mPaJtSEtaDPhi[i];
    dphiBins** pbjtdetadphi = mPbJtDEtaDPhi[i];
    dphiBins** pbjtsetadphi = mPbJtSEtaDPhi[i];
  
    int ncb=numCutBins;
    TH1::AddDirectory(kFALSE);
    for(int y=0;y<numCutBins;y++){

    createHist2D(mHYtYt,"YtYt",i,y,ncb,b->ytBins(),b->ytMin(),b->ytMax(),b->ytBins(),b->ytMin(),b->ytMax());
    createHist2D(mHNYtYt,"NYtYt",i,y,ncb,b->ytBins(),b->ytMin(),b->ytMax(),b->ytBins(),b->ytMin(),b->ytMax());
    for(int k=0;k<b->ytBins();k++){
      for(int j=0;j<b->ytBins();j++){
        mHYtYt[i][y]->Fill(b->ytVal(k),b->ytVal(j),ytyt[y][k].yt[j]);
        mHNYtYt[i][y]->Fill(b->ytVal(k),b->ytVal(j),nytyt[y][k].yt[j]);
      }
    }
    delete [] ytyt[y];
    delete [] nytyt[y];

    /*
    createHist2D(mHXtXt,"XtXt",i,y,ncb,b->xtBins(),b->xtMin(),b->xtMax(),b->xtBins(),b->xtMin(),b->xtMax());
    for(int k=0;k<b->xtBins();k++)
      for(int j=0;j<b->xtBins();j++)
        mHXtXt[i][y]->Fill(b->xtVal(k),b->xtVal(j),xtxt[y][k].xt[j]);

    delete [] xtxt[y];
    */

    createHist2D(mHPtPt,"PtPt",i,y,ncb,b->ptBins(),b->ptMin(),b->ptMax(),b->ptBins(),b->ptMin(),b->ptMax());
    for(int k=0;k<b->ptBins();k++)
      for(int j=0;j<b->ptBins();j++)
        mHPtPt[i][y]->Fill(b->ptVal(k),b->ptVal(j),ptpt[y][k].pt[j]);

    delete [] ptpt[y];

    createHist2D(mHPhiPhi,"PhiPhi",i,y,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
    createHist2D(mHNPhiPhi,"NPhiPhi",i,y,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
    createHist2D(mHPrPhiPhi,"PrPhiPhi",i,y,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
    createHist2D(mHPaPhiPhi,"PaPhiPhi",i,y,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
    createHist2D(mHPbPhiPhi,"PbPhiPhi",i,y,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
    for(int k=0;k<b->phiBins();k++){
      for(int j=0;j<b->phiBins();j++){
        mHPhiPhi[i][y]->Fill(xv=b->phiVal(k),yv=b->phiVal(j),phiphi[y][k].phi[j]);
        mHNPhiPhi[i][y]->Fill(xv=b->phiVal(k),yv=b->phiVal(j),nphiphi[y][k].phi[j]);
        mHPrPhiPhi[i][y]->Fill(xv,yv,prphiphi[y][k].phi[j]);
        mHPaPhiPhi[i][y]->Fill(xv,yv,paphiphi[y][k].phi[j]);
        mHPbPhiPhi[i][y]->Fill(xv,yv,pbphiphi[y][k].phi[j]);
      }
    }

    delete [] phiphi[y];
    delete [] nphiphi[y];
    delete [] prphiphi[y];
    delete [] paphiphi[y];
    delete [] pbphiphi[y];

    createHist2D(mHEtaEta,"EtaEta",i,y,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
    createHist2D(mHPrEtaEta,"PrEtaEta",i,y,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
    createHist2D(mHPaEtaEta,"PaEtaEta",i,y,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
    createHist2D(mHPbEtaEta,"PbEtaEta",i,y,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
    for(int k=0;k<b->etaBins();k++){
      for(int j=0;j<b->etaBins();j++){
        mHEtaEta[i][y]->Fill(xv=b->etaVal(k),yv=b->etaVal(j),etaeta[y][k].eta[j]);
        mHPrEtaEta[i][y]->Fill(xv,yv,pretaeta[y][k].eta[j]);
        mHPaEtaEta[i][y]->Fill(xv,yv,paetaeta[y][k].eta[j]);
        mHPbEtaEta[i][y]->Fill(xv,yv,pbetaeta[y][k].eta[j]);
      }
    }

    delete [] etaeta[y];
    delete [] pretaeta[y];
    delete [] paetaeta[y];
    delete [] pbetaeta[y];

    createHist2D(mHJtDEtaDPhi,"DEtaDPhi",i,y,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHJtNDEtaDPhi,"NDEtaDPhi",i,y,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPrJtDEtaDPhi,"PrDEtaDPhi",i,y,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPaJtDEtaDPhi,"PaDEtaDPhi",i,y,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPbJtDEtaDPhi,"PbDEtaDPhi",i,y,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    for(int k=0;k<b->detaBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtDEtaDPhi[i][y]->Fill(xv=b->detaVal(k),yv=b->dphiVal(j),jtdetadphi[y][k].dphi[j]);
        mHJtNDEtaDPhi[i][y]->Fill(xv=b->detaVal(k),yv=b->dphiVal(j),jtndetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(xv,yv,prjtdetadphi[y][k].dphi[j]);
        mHPaJtDEtaDPhi[i][y]->Fill(xv,yv,pajtdetadphi[y][k].dphi[j]);
        mHPbJtDEtaDPhi[i][y]->Fill(xv,yv,pbjtdetadphi[y][k].dphi[j]);
      }
    }

    delete [] jtdetadphi[y];
    delete [] jtndetadphi[y];
    delete [] prjtdetadphi[y];
    delete [] pajtdetadphi[y];
    delete [] pbjtdetadphi[y];

    createHist2D(mHAtSYtDYt,"SYtDYt",i,y,ncb,b->sytBins(),b->sytMin(),b->sytMax(),b->dytBins(),b->dytMin(),b->dytMax());
    createHist2D(mHAtNSYtDYt,"NSYtDYt",i,y,ncb,b->sytBins(),b->sytMin(),b->sytMax(),b->dytBins(),b->dytMin(),b->dytMax());
    for(int k=0;k<b->sytBins();k++){
      for(int j=0;j<b->dytBins();j++){
        mHAtSYtDYt[i][y]->Fill(b->sytVal(k),b->dytVal(j),atytyt[y][k].dyt[j]);
        mHAtNSYtDYt[i][y]->Fill(b->sytVal(k),b->dytVal(j),atnytyt[y][k].dyt[j]);
      }
    }

   delete [] atytyt[y];
   delete [] atnytyt[y];

   /*
   createHist2D(mHAtSPtDPt,"SPtDPt",i,y,ncb,b->sptBins(),b->sptMin(),b->sptMax(),b->dptBins(),b->dptMin(),b->dptMax());
   for(int k=0;k<b->sptBins();k++)
      for(int j=0;j<b->dptBins();j++)
        mHAtSPtDPt[i][y]->Fill(b->sptVal(k),b->dptVal(j),atptpt[y][k].dpt[j]);
 
   delete [] atptpt[y];
   */
    createHist2D(mHJtSEtaDPhi,"SEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHJtNSEtaDPhi,"NSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPrJtSEtaDPhi,"PrSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPaJtSEtaDPhi,"PaSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPbJtSEtaDPhi,"PbSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    for(int k=0;k<b->setaBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtSEtaDPhi[i][y]->Fill(xv=b->setaVal(k),yv=b->dphiVal(j),jtsetadphi[y][k].dphi[j]);
        mHJtNSEtaDPhi[i][y]->Fill(xv=b->setaVal(k),yv=b->dphiVal(j),jtnsetadphi[y][k].dphi[j]);
        mHPrJtSEtaDPhi[i][y]->Fill(xv,yv,prjtsetadphi[y][k].dphi[j]);
        mHPaJtSEtaDPhi[i][y]->Fill(xv,yv,pajtsetadphi[y][k].dphi[j]);
        mHPbJtSEtaDPhi[i][y]->Fill(xv,yv,pbjtsetadphi[y][k].dphi[j]);
      }
    }

    delete [] jtsetadphi[y];
    delete [] jtnsetadphi[y];
    delete [] prjtsetadphi[y];
    delete [] pbjtsetadphi[y];   
    delete [] pajtsetadphi[y];   

    /*
    createHist2D(mHJtDYtDPhi,"DYtDPhi",i,y,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHJtDYtDEta,"DYtDEta",i,y,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->detaBins(),b->detaMin(),b->detaMax());
    for(int k=0;k<b->dytBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        mHJtDYtDPhi[i][y]->Fill(xv=b->dytVal(k),yv=b->dphiVal(j),jtdytdphi[y][k].dphi[j]);
      }      
      for(int j=0;j<b->detaBins();j++){
        mHJtDYtDEta[i][y]->Fill(xv=b->dytVal(k),yv=b->detaVal(j),jtdytdeta[y][k].deta[j]);
      }      
    }
    delete [] jtdytdphi[y];
    delete [] jtdytdeta[y];
    */

    createHist1D(mHQinv,"Qinv",i,y,ncb,b->qBins(),b->qMin(),b->qMax());
    createHist1D(mHNQinv,"NQinv",i,y,ncb,b->qBins(),b->qMin(),b->qMax());
    for(int k=0;k<b->qBins();k++){
        mHQinv[i][y]->Fill(b->qVal(k),qinv[y].q[k]);
        mHNQinv[i][y]->Fill(b->qVal(k),nqinv[y].q[k]);
    }
    //    delete [] qinv[y];

    } // for y
  } // for i
  
  if(mdoPairDensityHistograms) {
    for(int i=0; i<8; i++){
      for(int y=0;y<numCutBins;y++){
        TPCSepBins* avgtsep = mTPCAvgTSep[i];
        TPCSepBins* avgzsep = mTPCAvgZSep[i];
        TPCSepBins* enttsep = mTPCEntTSep[i];
        TPCSepBins* entzsep = mTPCEntZSep[i];
        TPCSepBins* midtsep = mTPCMidTSep[i];
        TPCSepBins* midzsep = mTPCMidZSep[i];
        TPCSepBins* exittsep= mTPCExitTSep[i];
        TPCSepBins* exitzsep= mTPCExitZSep[i];
        TPCSepBins* midtp =   mTPCMidTdptP[i];
        TPCSepBins* midtn =   mTPCMidTdptN[i];
        TPCSepBins* midzp =   mTPCMidZdptP[i];
        TPCSepBins* midzn =   mTPCMidZdptN[i];
        TPCSepBins** avgtz =  mTPCAvgTZ[i];
        TPCSepBins** enttz =  mTPCEntTZ[i];
        TPCSepBins** midtz =  mTPCMidTZ[i];
        TPCSepBins** exittz = mTPCExitTZ[i];
        dptBins** enttd =  mTPCEntTdpt[i];
        dptBins** midtd =  mTPCMidTdpt[i];
        dptBins** exittd = mTPCExitTdpt[i];

        for(int k=0;k<b->TPCSepBins();k++) {
          mHTPCAvgTSep[i][y]->Fill(xv=b->sepVal(k),avgtsep[y].sep[k]);
          mHTPCAvgZSep[i][y]->Fill(xv,avgzsep[y].sep[k]);
          mHTPCEntTSep[i][y]->Fill(xv,enttsep[y].sep[k]);
          mHTPCEntZSep[i][y]->Fill(xv,entzsep[y].sep[k]);
          mHTPCMidTSep[i][y]->Fill(xv,midtsep[y].sep[k]);
          mHTPCMidZSep[i][y]->Fill(xv,midzsep[y].sep[k]);
          mHTPCExitTSep[i][y]->Fill(xv,exittsep[y].sep[k]);
          mHTPCExitZSep[i][y]->Fill(xv,exitzsep[y].sep[k]);
          mHTPCMidTdptP[i][y]->Fill(xv,midtp[y].sep[k]);
          mHTPCMidTdptN[i][y]->Fill(xv,midtn[y].sep[k]);
          mHTPCMidZdptP[i][y]->Fill(xv,midzp[y].sep[k]);
          mHTPCMidZdptN[i][y]->Fill(xv,midzn[y].sep[k]);
          for(int j=0;j<b->TPCSepBins();j++) {
            mHTPCAvgTZ[i][y]->Fill(xv,yv=b->sepVal(j),avgtz[y][k].sep[j]);
            mHTPCEntTZ[i][y]->Fill(xv,yv,enttz[y][k].sep[j]);
            mHTPCMidTZ[i][y]->Fill(xv,yv,midtz[y][k].sep[j]);
            mHTPCExitTZ[i][y]->Fill(xv,yv,exittz[y][k].sep[j]);
          }
          for(int j=0;j<b->dptBins();j++) {
            mHTPCEntTdpt[i][y]->Fill(xv,yv=b->dptVal(j),enttd[y][k].dpt[j]);
            mHTPCMidTdpt[i][y]->Fill(xv,yv,midtd[y][k].dpt[j]);
            mHTPCExitTdpt[i][y]->Fill(xv,yv,exittd[y][k].dpt[j]);    
          }
        }
      } 
    } 
  } // if pair density

}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::writeHistograms() {

  for(int j=0;j<2;j++)mHNEvents[j]->Write(); 
  mHmix->Write();

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  mHptAll->Write();
  for(int j=0;j<numCutBins;j++){
    mHpt[0][j]->Write();
    mHpt[1][j]->Write();
    for(int i=0;i<8;i++){

     mHYtYt[i][j]->Write();
     mHNYtYt[i][j]->Write();
     //    mHXtXt[i][j]->Write();
     mHPtPt[i][j]->Write();

     mHPhiPhi[i][j]->Write();
     mHNPhiPhi[i][j]->Write();
     mHEtaEta[i][j]->Write();
     mHPrPhiPhi[i][j]->Write();
     mHPrEtaEta[i][j]->Write();
     mHPaPhiPhi[i][j]->Write();
     mHPaEtaEta[i][j]->Write();
     mHPbPhiPhi[i][j]->Write();
     mHPbEtaEta[i][j]->Write();

     mHAtSYtDYt[i][j]->Write();
     mHAtNSYtDYt[i][j]->Write();
     //     mHAtSPtDPt[i][j]->Write();
     /*
     mHJtDYtDPhi[i][j]->Write();
     mHJtDYtDEta[i][j]->Write();
     */
     mHJtDEtaDPhi[i][j]->Write();
     mHJtNDEtaDPhi[i][j]->Write();
     mHPrJtDEtaDPhi[i][j]->Write();
     mHPaJtDEtaDPhi[i][j]->Write();
     mHPbJtDEtaDPhi[i][j]->Write();

     mHJtSEtaDPhi[i][j]->Write();
     mHJtNSEtaDPhi[i][j]->Write();
     mHPrJtSEtaDPhi[i][j]->Write();
     mHPaJtSEtaDPhi[i][j]->Write();
     mHPbJtSEtaDPhi[i][j]->Write();

     mHQinv[i][j]->Write();
     mHNQinv[i][j]->Write();

     if(mdoPairDensityHistograms) {
     mHTPCAvgTSep[i][j]->Write();
     mHTPCAvgZSep[i][j]->Write();
     mHTPCEntTSep[i][j]->Write();
     mHTPCEntZSep[i][j]->Write();
     mHTPCMidTSep[i][j]->Write();
     mHTPCMidZSep[i][j]->Write();
     mHTPCExitTSep[i][j]->Write();
     mHTPCExitZSep[i][j]->Write();

     mHTPCMidTdptP[i][j]->Write();
     mHTPCMidTdptN[i][j]->Write();
     mHTPCMidZdptP[i][j]->Write();
     mHTPCMidZdptN[i][j]->Write();

     mHTPCAvgTZ[i][j]->Write();
     mHTPCEntTZ[i][j]->Write();
     mHTPCMidTZ[i][j]->Write();
     mHTPCExitTZ[i][j]->Write();
     mHTPCEntTdpt[i][j]->Write();
     mHTPCMidTdpt[i][j]->Write();
     mHTPCExitTdpt[i][j]->Write();
     }
     
   }
  }
  
  
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::writeQAHists(TFile *qtf) {

  if(!mlocalQAHists) return;

  if(!qtf){
    cout<<" NO QA OUTPUT TFile TO WRITE TO ... giving up ..."<<endl;
    return;
  }

  mQAHists->writeTrackHistograms(qtf);


}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::initArrays(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<8;i++){

     mYtYt[i]=new ytBins*[numCutBins];
     mAtSYtDYt[i]=new dytBins*[numCutBins];
     mNYtYt[i]=new ytBins*[numCutBins];
     mAtNSYtDYt[i]=new dytBins*[numCutBins];
     //    mXtXt[i]=new xtBins*[numCutBins];
     mPtPt[i]=new ptBins*[numCutBins];
     //     mAtSPtDPt[i]=new dptBins*[numCutBins];

     mEtaEta[i]=new etaBins*[numCutBins];
     mPhiPhi[i]=new phiBins*[numCutBins];
     mNPhiPhi[i]=new phiBins*[numCutBins];
     //     mJtDYtDPhi[i]=new dphiBins*[numCutBins];
     //     mJtDYtDEta[i]=new detaBins*[numCutBins];
     mJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mJtSEtaDPhi[i]=new dphiBins*[numCutBins];
     mJtNDEtaDPhi[i]=new dphiBins*[numCutBins];
     mJtNSEtaDPhi[i]=new dphiBins*[numCutBins];

     mPaEtaEta[i]=new etaBins*[numCutBins];
     mPaPhiPhi[i]=new phiBins*[numCutBins];
     mPaJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mPaJtSEtaDPhi[i]=new dphiBins*[numCutBins];
     mPbEtaEta[i]=new etaBins*[numCutBins];
     mPbPhiPhi[i]=new phiBins*[numCutBins];
     mPbJtDEtaDPhi[i]=new dphiBins*[numCutBins];
     mPbJtSEtaDPhi[i]=new dphiBins*[numCutBins];

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
     mNQinv[i]=new qBins[numCutBins];
     memset(mQinv[i],0,numCutBins*sizeof(qBins)); // do the memset here
     memset(mNQinv[i],0,numCutBins*sizeof(qBins)); // do the memset here

     if(mdoPairDensityHistograms) {     
       mTPCAvgTSep[i]=new TPCSepBins[numCutBins];  //1D  
       mTPCAvgZSep[i]=new TPCSepBins[numCutBins];
       mTPCEntTSep[i]=new TPCSepBins[numCutBins];
       mTPCEntZSep[i]=new TPCSepBins[numCutBins];
       mTPCMidTSep[i]=new TPCSepBins[numCutBins];
       mTPCMidZSep[i]=new TPCSepBins[numCutBins];
       mTPCExitTSep[i]=new TPCSepBins[numCutBins];
       mTPCExitZSep[i]=new TPCSepBins[numCutBins];
       mTPCMidTdptP[i]=new TPCSepBins[numCutBins];
       mTPCMidTdptN[i]=new TPCSepBins[numCutBins];
       mTPCMidZdptP[i]=new TPCSepBins[numCutBins];
       mTPCMidZdptN[i]=new TPCSepBins[numCutBins];
       memset(mTPCAvgTSep[i], 0,numCutBins*sizeof(TPCSepBins)); 
       memset(mTPCAvgZSep[i], 0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCEntTSep[i], 0,numCutBins*sizeof(TPCSepBins)); 
       memset(mTPCEntZSep[i], 0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCMidTSep[i], 0,numCutBins*sizeof(TPCSepBins)); 
       memset(mTPCMidZSep[i], 0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCExitTSep[i], 0,numCutBins*sizeof(TPCSepBins)); 
       memset(mTPCExitZSep[i], 0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCMidTdptP[i],0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCMidTdptN[i],0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCMidZdptP[i],0,numCutBins*sizeof(TPCSepBins));
       memset(mTPCMidZdptN[i],0,numCutBins*sizeof(TPCSepBins));

       mTPCAvgTZ[i]=new TPCSepBins*[numCutBins];  //2D
       mTPCEntTZ[i]=new TPCSepBins*[numCutBins];  
       mTPCMidTZ[i]=new TPCSepBins*[numCutBins];  
       mTPCExitTZ[i]=new TPCSepBins*[numCutBins]; 
       mTPCEntTdpt[i]=new dptBins*[numCutBins];
       mTPCMidTdpt[i]=new dptBins*[numCutBins];
       mTPCExitTdpt[i]=new dptBins*[numCutBins]; 
     }

  }

  for(int j=0;j<numCutBins;j++){
   for(int i=0;i<8;i++){

     mYtYt[i][j]=new ytBins[ESTRUCT_YT_BINS];
     memset(mYtYt[i][j],0,ESTRUCT_YT_BINS*sizeof(ytBins));
     mAtSYtDYt[i][j]=new dytBins[ESTRUCT_SYT_BINS];
     memset(mAtSYtDYt[i][j],0,ESTRUCT_SYT_BINS*sizeof(dytBins));
     mNYtYt[i][j]=new ytBins[ESTRUCT_YT_BINS];
     memset(mNYtYt[i][j],0,ESTRUCT_YT_BINS*sizeof(ytBins));
     mAtNSYtDYt[i][j]=new dytBins[ESTRUCT_SYT_BINS];
     memset(mAtNSYtDYt[i][j],0,ESTRUCT_SYT_BINS*sizeof(dytBins));
     mPtPt[i][j]=new ptBins[ESTRUCT_PT_BINS];
     memset(mPtPt[i][j],0,ESTRUCT_PT_BINS*sizeof(ptBins));

     /*
     mXtXt[i][j]=new xtBins[ESTRUCT_XT_BINS];
     memset(mXtXt[i][j],0,ESTRUCT_XT_BINS*sizeof(xtBins));
     mAtSPtDPt[i][j]=new dptBins[ESTRUCT_SPT_BINS];
     memset(mAtSPtDPt[i][j],0,ESTRUCT_SPT_BINS*sizeof(dptBins));
     */

     mEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mNPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mNPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     /*
     mJtDYtDPhi[i][j]=new dphiBins[ESTRUCT_DYT_BINS];
     memset(mJtDYtDPhi[i][j],0,ESTRUCT_DYT_BINS*sizeof(dphiBins));
     mJtDYtDEta[i][j]=new detaBins[ESTRUCT_DYT_BINS];
     memset(mJtDYtDEta[i][j],0,ESTRUCT_DYT_BINS*sizeof(detaBins));
     */
     mJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mJtNDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mJtNDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));
     mJtNSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mJtNSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));

     mPaEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mPaEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPaPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPaPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mPaJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mPaJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mPaJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mPaJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));
     mPbEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mPbEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPbPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPbPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mPbJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mPbJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mPbJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mPbJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));

     mPrEtaEta[i][j]=new etaBins[ESTRUCT_ETA_BINS];
     memset(mPrEtaEta[i][j],0,ESTRUCT_ETA_BINS*sizeof(etaBins));
     mPrPhiPhi[i][j]=new phiBins[ESTRUCT_PHI_BINS];
     memset(mPrPhiPhi[i][j],0,ESTRUCT_PHI_BINS*sizeof(phiBins));
     mPrJtDEtaDPhi[i][j]=new dphiBins[ESTRUCT_DETA_BINS];
     memset(mPrJtDEtaDPhi[i][j],0,ESTRUCT_DETA_BINS*sizeof(dphiBins));
     mPrJtSEtaDPhi[i][j]=new dphiBins[ESTRUCT_SETA_BINS];
     memset(mPrJtSEtaDPhi[i][j],0,ESTRUCT_SETA_BINS*sizeof(dphiBins));

     if(mdoPairDensityHistograms) {
       mTPCAvgTZ[i][j]=new TPCSepBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCAvgTZ[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(TPCSepBins));
       mTPCEntTZ[i][j]=new TPCSepBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCEntTZ[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(TPCSepBins));
       mTPCMidTZ[i][j]=new TPCSepBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCMidTZ[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(TPCSepBins));
       mTPCExitTZ[i][j]=new TPCSepBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCExitTZ[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(TPCSepBins));
       
       mTPCEntTdpt[i][j]=new dptBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCEntTdpt[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(dptBins));
       mTPCMidTdpt[i][j]=new dptBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCMidTdpt[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(dptBins));
       mTPCExitTdpt[i][j]=new dptBins[ESTRUCT_TPCSEP_BINS];
       memset(mTPCExitTdpt[i][j],0,ESTRUCT_TPCSEP_BINS*sizeof(dptBins));
     }
     
   }
  }
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::deleteArrays(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<8;i++){
    
    for(int j=0;j<numCutBins;j++){
      
      //      delete []  mYtYt[i][j];
      //      delete []  mXtXt[i][j];
      //      delete []  mPtPt[i][j];
      
      //      delete []  mEtaEta[i][j];
      //      delete []  mPhiPhi[i][j];
      //      delete []  mPrEtaEta[i][j];
      //      delete []  mPrPhiPhi[i][j];
      //      delete []  mPaEtaEta[i][j];
      //      delete []  mPaPhiPhi[i][j];
      //      delete []  mPbEtaEta[i][j];
      //      delete []  mPbPhiPhi[i][j];
      
      //      delete []  mAtSYtDYt[i][j];
      //      delete []  mAtSPtDPt[i][j];
      
      //      delete []  mJtDYtDPhi[i][j];
      //      delete []  mJtDYtDEta[i][j];
      //      delete []  mJtDEtaDPhi[i][j];
      //      delete []  mJtSEtaDPhi[i][j];
      
      //      delete []  mPrJtDEtaDPhi[i][j];
      //      delete []  mPrJtSEtaDPhi[i][j];
      //      delete []  mPaJtDEtaDPhi[i][j];
      //      delete []  mPaJtSEtaDPhi[i][j];
      //      delete []  mPbJtDEtaDPhi[i][j];
      //      delete []  mPbJtSEtaDPhi[i][j];
      
      if(mdoPairDensityHistograms)  {
	delete []  mTPCAvgTZ[i][j];
	delete []  mTPCEntTZ[i][j];
	delete []  mTPCMidTZ[i][j];
	delete []  mTPCExitTZ[i][j];
	delete []  mTPCEntTdpt[i][j];
        delete []  mTPCMidTdpt[i][j];
        delete []  mTPCExitTdpt[i][j];
      }
    }   
    
    delete []  mYtYt[i];
    delete []  mXtXt[i];
    delete []  mPtPt[i];
    
    delete []  mEtaEta[i];
    delete []  mPhiPhi[i];
    delete []  mPrEtaEta[i];
    delete []  mPrPhiPhi[i];
    delete []  mPaEtaEta[i];
    delete []  mPaPhiPhi[i];
    delete []  mPbEtaEta[i];
    delete []  mPbPhiPhi[i];

    delete []  mAtSYtDYt[i];
    delete []  mAtSPtDPt[i];

    delete []  mJtDYtDPhi[i];
    delete []  mJtDYtDEta[i];
    delete []  mJtDEtaDPhi[i];
    delete []  mJtSEtaDPhi[i];
    delete []  mPrJtDEtaDPhi[i];
    delete []  mPrJtSEtaDPhi[i];
    delete []  mPaJtDEtaDPhi[i];
    delete []  mPaJtSEtaDPhi[i];
    delete []  mPbJtDEtaDPhi[i];
    delete []  mPbJtSEtaDPhi[i];
    
    delete []  mQinv[i];

    if(mdoPairDensityHistograms) {
      delete [] mTPCAvgTSep[i];
      delete [] mTPCAvgZSep[i];
      delete [] mTPCEntTSep[i];
      delete [] mTPCEntZSep[i];
      delete [] mTPCMidTSep[i];
      delete [] mTPCMidZSep[i];
      delete [] mTPCExitTSep[i];
      delete [] mTPCExitZSep[i];
      delete [] mTPCMidTdptP[i];
      delete [] mTPCMidTdptN[i];
      delete [] mTPCMidZdptP[i];
      delete [] mTPCMidZdptN[i];
      delete [] mTPCAvgTZ[i];
      delete [] mTPCEntTZ[i];
      delete [] mTPCMidTZ[i];
      delete [] mTPCExitTZ[i];
      delete [] mTPCEntTdpt[i];
      delete [] mTPCMidTdpt[i];
      delete [] mTPCExitTdpt[i]; 
    }
  }

}

//--------------------------------------------------------------------------

void StEStruct2ptCorrelations::initHistograms(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();
  StEStructBinning* b=StEStructBinning::Instance();


  for(int i=0; i<8; i++){

    mHYtYt[i]=new TH2D*[numCutBins];
    mHAtSYtDYt[i]=new TH2D*[numCutBins];
    mHNYtYt[i]=new TH2D*[numCutBins];
    mHAtNSYtDYt[i]=new TH2D*[numCutBins];
    //    mHXtXt[i]=new TH2D*[numCutBins];
    mHPtPt[i]=new TH2D*[numCutBins];
    //    mHAtSPtDPt[i]=new TH2D*[numCutBins];

    mHEtaEta[i]=new TH2D*[numCutBins];
    mHPhiPhi[i]=new TH2D*[numCutBins];
    mHNPhiPhi[i]=new TH2D*[numCutBins];
    mHPrEtaEta[i]=new TH2D*[numCutBins];
    mHPrPhiPhi[i]=new TH2D*[numCutBins];
    mHPaEtaEta[i]=new TH2D*[numCutBins];
    mHPaPhiPhi[i]=new TH2D*[numCutBins];
    mHPbEtaEta[i]=new TH2D*[numCutBins];
    mHPbPhiPhi[i]=new TH2D*[numCutBins];

    /*
    mHJtDYtDPhi[i]=new TH2D*[numCutBins];
    mHJtDYtDEta[i]=new TH2D*[numCutBins];
    */
 
    mHJtDEtaDPhi[i]=new TH2D*[numCutBins];
    mHJtSEtaDPhi[i]=new TH2D*[numCutBins];
    mHJtNDEtaDPhi[i]=new TH2D*[numCutBins];
    mHJtNSEtaDPhi[i]=new TH2D*[numCutBins];
    mHPrJtDEtaDPhi[i]=new TH2D*[numCutBins];
    mHPrJtSEtaDPhi[i]=new TH2D*[numCutBins];
    mHPaJtDEtaDPhi[i]=new TH2D*[numCutBins];
    mHPaJtSEtaDPhi[i]=new TH2D*[numCutBins];
    mHPbJtDEtaDPhi[i]=new TH2D*[numCutBins];
    mHPbJtSEtaDPhi[i]=new TH2D*[numCutBins];

    mHQinv[i]=new TH1D*[numCutBins];
    mHNQinv[i]=new TH1D*[numCutBins];

    if(mdoPairDensityHistograms) {
      mHTPCAvgTSep[i]=new TH1D*[numCutBins];
      mHTPCAvgZSep[i]=new TH1D*[numCutBins];
      mHTPCEntTSep[i]=new TH1D*[numCutBins];
      mHTPCEntZSep[i]=new TH1D*[numCutBins];
      mHTPCMidTSep[i]=new TH1D*[numCutBins];
      mHTPCMidZSep[i]=new TH1D*[numCutBins];
      mHTPCExitTSep[i]=new TH1D*[numCutBins];
      mHTPCExitZSep[i]=new TH1D*[numCutBins];
      mHTPCMidTdptP[i]=new TH1D*[numCutBins];
      mHTPCMidTdptN[i]=new TH1D*[numCutBins];
      mHTPCMidZdptP[i]=new TH1D*[numCutBins];
      mHTPCMidZdptN[i]=new TH1D*[numCutBins];
      mHTPCAvgTZ[i]=new TH2D*[numCutBins];
      mHTPCEntTZ[i]=new TH2D*[numCutBins];
      mHTPCMidTZ[i]=new TH2D*[numCutBins];
      mHTPCExitTZ[i]=new TH2D*[numCutBins];
      mHTPCEntTdpt[i]=new TH2D*[numCutBins];
      mHTPCMidTdpt[i]=new TH2D*[numCutBins];
      mHTPCExitTdpt[i]=new TH2D*[numCutBins];
    }
    
    int ncb=numCutBins;

    for(int j=0;j<numCutBins;j++){

      //      createHist2D(mHYtYt,"YtYt",i,j,ncb,b->ytBins(),b->ytMin(),b->ytMax(),b->ytBins(),b->ytMin(),b->ytMax());
      //      createHist2D(mHAtSYtDYt,"SYtDYt",i,j,ncb,b->sytBins(),b->sytMin(),b->sytMax(),b->dytBins(),b->dytMin(),b->dytMax());

      //      createHist2D(mHXtXt,"XtXt",i,j,ncb,b->xtBins(),b->xtMin(),b->xtMax(),b->xtBins(),b->xtMin(),b->xtMax());

      //      createHist2D(mHPtPt,"PtPt",i,j,ncb,b->ptBins(),b->ptMin(),b->ptMax(),b->ptBins(),b->ptMin(),b->ptMax());
      //      createHist2D(mHAtSPtDPt,"SPtDPt",i,j,ncb,b->sptBins(),b->sptMin(),b->sptMax(),b->dptBins(),b->dptMin(),b->dptMax());

      //      createHist2D(mHEtaEta,"EtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      //      createHist2D(mHPhiPhi,"PhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      //      createHist2D(mHJtDYtDPhi,"DYtDPhi",i,j,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHJtDYtDEta,"DYtDEta",i,j,ncb,b->dytBins(),b->dytMin(),b->dytMax(),b->detaBins(),b->detaMin(),b->detaMax());
      //      createHist2D(mHJtDEtaDPhi,"DEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHJtSEtaDPhi,"SEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      //      createHist2D(mHPrEtaEta,"PrEtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      //      createHist2D(mHPrPhiPhi,"PrPhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      //      createHist2D(mHPrJtDEtaDPhi,"PrDEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHPrJtSEtaDPhi,"PrSEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      //      createHist2D(mHPaEtaEta,"PaEtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      //      createHist2D(mHPaPhiPhi,"PaPhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      //      createHist2D(mHPaJtDEtaDPhi,"PaDEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHPaJtSEtaDPhi,"PaSEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHPbEtaEta,"PbEtaEta",i,j,ncb,b->etaBins(),b->etaMin(),b->etaMax(),b->etaBins(),b->etaMin(),b->etaMax());
      //      createHist2D(mHPbPhiPhi,"PbPhiPhi",i,j,ncb,b->phiBins(),b->phiMin(),b->phiMax(),b->phiBins(),b->phiMin(),b->phiMax());
      //      createHist2D(mHPbJtDEtaDPhi,"PbDEtaDPhi",i,j,ncb,b->detaBins(),b->detaMin(),b->detaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());
      //      createHist2D(mHPbJtSEtaDPhi,"PbSEtaDPhi",i,j,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->dphiBins(),b->dphiMin(),b->dphiMax());

      //      createHist1D(mHQinv,"Qinv",i,j,ncb,b->qBins(),b->qMin(),b->qMax());
      
      if(mdoPairDensityHistograms) {
	createHist1D(mHTPCAvgTSep,"TPCAvgTSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCAvgZSep,"TPCAvgZSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCEntTSep,"TPCEntTSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCEntZSep,"TPCEntZSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidTSep,"TPCMidTSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidZSep,"TPCMidZSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCExitTSep,"TPCExitTSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCExitZSep,"TPCExitZSep",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidTdptP,"TPCMidTdptP",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidTdptN,"TPCMidTdptN",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidZdptP,"TPCMidZdptP",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist1D(mHTPCMidZdptN,"TPCMidZdptN",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());

	createHist2D(mHTPCAvgTZ, "TPCAvgTZ", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist2D(mHTPCEntTZ, "TPCEntTZ", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist2D(mHTPCMidTZ, "TPCMidTZ", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist2D(mHTPCExitTZ,"TPCExitTZ",i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax());
	createHist2D(mHTPCEntTdpt, "TPCEntTdpt", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->dptBins(),b->dptMin(),b->dptMax());
	createHist2D(mHTPCMidTdpt, "TPCMidTdpt", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->dptBins(),b->dptMin(),b->dptMax());
	createHist2D(mHTPCExitTdpt, "TPCExitTdpt", i,j,ncb,b->TPCSepBins(),b->TPCSepMin(),b->TPCSepMax(),b->dptBins(),b->dptMin(),b->dptMax());
      }
    }
  }
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::deleteHistograms(){
  
  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  for(int i=0;i<8;i++){
    for(int j=0;j<numCutBins;j++){
      delete mHYtYt[i][j];
      delete mHNYtYt[i][j];
      //      delete mHXtXt[i][j];
      delete mHPtPt[i][j];
      
      delete mHEtaEta[i][j];
      delete mHPhiPhi[i][j];
      delete mHNPhiPhi[i][j];
      delete mHPrEtaEta[i][j];
      delete mHPrPhiPhi[i][j];
      delete mHPaEtaEta[i][j];
      delete mHPaPhiPhi[i][j];
      delete mHPbEtaEta[i][j];
      delete mHPbPhiPhi[i][j];

      delete mHAtSYtDYt[i][j];
      delete mHAtNSYtDYt[i][j];
      //      delete mHAtSPtDPt[i][j];

      /*
      delete mHJtDYtDPhi[i][j];
      delete mHJtDYtDEta[i][j];
      */
      delete mHJtDEtaDPhi[i][j];
      delete mHJtSEtaDPhi[i][j];
      delete mHJtNDEtaDPhi[i][j];
      delete mHJtNSEtaDPhi[i][j];
      delete mHPrJtDEtaDPhi[i][j];
      delete mHPrJtSEtaDPhi[i][j];
      delete mHPaJtDEtaDPhi[i][j];
      delete mHPaJtSEtaDPhi[i][j];
      delete mHPbJtDEtaDPhi[i][j];
      delete mHPbJtSEtaDPhi[i][j];

      delete mHQinv[i][j];
      delete mHNQinv[i][j];

      if(mdoPairDensityHistograms) {
	delete mHTPCAvgTSep[i][j];
	delete mHTPCAvgZSep[i][j];
	delete mHTPCEntTSep[i][j];
	delete mHTPCEntZSep[i][j];
	delete mHTPCMidTSep[i][j];
	delete mHTPCMidZSep[i][j];
	delete mHTPCExitTSep[i][j];
	delete mHTPCExitZSep[i][j];
	delete mHTPCMidTdptP[i][j];
	delete mHTPCMidTdptN[i][j];
	delete mHTPCMidZdptP[i][j];
	delete mHTPCMidZdptN[i][j];
	delete mHTPCAvgTZ[i][j];
	delete mHTPCEntTZ[i][j];
	delete mHTPCMidTZ[i][j];
	delete mHTPCExitTZ[i][j];
	delete mHTPCEntTdpt[i][j];
	delete mHTPCMidTdpt[i][j];
	delete mHTPCExitTdpt[i][j];       
      }
    
    }

    delete [] mHYtYt[i];
    delete [] mHNYtYt[i];
    //    delete [] mHXtXt[i];
    delete [] mHPtPt[i];

    delete [] mHEtaEta[i];
    delete [] mHPhiPhi[i];
    delete [] mHNPhiPhi[i];
    delete [] mHPrEtaEta[i];
    delete [] mHPrPhiPhi[i];
    delete [] mHPaEtaEta[i];
    delete [] mHPaPhiPhi[i];
    delete [] mHPbEtaEta[i];
    delete [] mHPbPhiPhi[i];

    delete [] mHAtSYtDYt[i];
    delete [] mHAtNSYtDYt[i];
    //    delete [] mHAtSPtDPt[i];
    /*
    delete [] mHJtDYtDPhi[i];
    delete [] mHJtDYtDEta[i];
    */
    delete [] mHJtDEtaDPhi[i];
    delete [] mHJtSEtaDPhi[i];
    delete [] mHJtNDEtaDPhi[i];
    delete [] mHJtNSEtaDPhi[i];
    delete [] mHPrJtDEtaDPhi[i];
    delete [] mHPrJtSEtaDPhi[i];
    delete [] mHPaJtDEtaDPhi[i];
    delete [] mHPaJtSEtaDPhi[i];
    delete [] mHPbJtDEtaDPhi[i];
    delete [] mHPbJtSEtaDPhi[i];

    delete [] mHQinv[i];
    delete [] mHNQinv[i];

    if(mdoPairDensityHistograms) {
      delete [] mHTPCAvgTSep[i];
      delete [] mHTPCAvgZSep[i];
      delete [] mHTPCEntTSep[i];
      delete [] mHTPCEntZSep[i];
      delete [] mHTPCMidTSep[i];
      delete [] mHTPCMidZSep[i];
      delete [] mHTPCExitTSep[i];
      delete [] mHTPCExitZSep[i];
      delete [] mHTPCMidTdptP[i];
      delete [] mHTPCMidTdptN[i];
      delete [] mHTPCMidZdptP[i];
      delete [] mHTPCMidZdptN[i];
      delete [] mHTPCAvgTZ[i];
      delete [] mHTPCEntTZ[i];
      delete [] mHTPCMidTZ[i];
      delete [] mHTPCExitTZ[i];
      delete [] mHTPCEntTdpt[i];
      delete [] mHTPCMidTdpt[i];
      delete [] mHTPCExitTdpt[i];
    }

  }

}

//-----------------------------------------------------------------
void StEStruct2ptCorrelations::createHist2D(TH2D*** h, const char* name, int iknd, int icut, int ncut, int nx, float xmin, float xmax, int ny, float ymin, float ymax ){

  TString hname(bName[iknd]);
  hname+=name;
  if(ncut>1)hname+=icut;
  TString htitle(bTitle[iknd]);
  htitle+=name;
  h[iknd][icut]=new TH2D(hname.Data(),htitle.Data(),nx,xmin,xmax,ny,ymin,ymax);

}
//-----------------------------------------------------------------
void StEStruct2ptCorrelations::createHist1D(TH1D*** h, const char* name, int iknd, int icut, int ncut, int nx, float xmin, float xmax){

  TString hname(bName[iknd]);
  hname+=name;
  if(ncut>1)hname+=icut;
  TString htitle(bTitle[iknd]);
  htitle+=name;
  h[iknd][icut]=new TH1D(hname.Data(),htitle.Data(),nx,xmin,xmax);

}


/***********************************************************************
 *
 * $Log: StEStruct2ptCorrelations.cxx,v $
 * Revision 1.17  2006/05/03 17:52:11  msd
 * Fixed pair density plots broken by recent symmetry changes
 *
 * Revision 1.16  2006/04/27 22:40:35  porter
 * 3 changes: 1) added npair hists for errors needed with eta_delta weighting
 * 2) commented out a few histograms to trim memory usage
 * 3) changed all hists to double precision (reflected in createHists member functions)
 *
 * Revision 1.15  2006/04/13 23:02:35  prindle
 * Added comment about not deleting output root file (which gets reported
 * as a memory leak but this is just before the program quits anayway.)
 *
 * Revision 1.14  2006/04/12 19:09:07  porter
 * added logic should z-vertex cut be ommitted (i.e. analysis of event generators)
 *
 * Revision 1.13  2006/04/10 23:42:32  porter
 * Added sameSide() & awaySide() methods to PairCut (so only defined in 1 place)
 * and added the eta_delta weighting as a binned correctin defined by the eta-limits in
 * the StEStructBinning object
 *
 * Revision 1.12  2006/04/06 01:01:11  prindle
 *
 *   New mode in CutBin, 5, to do pid correlations. There is still an issue
 * of how to set the pt ranges allowed for the different particle types.
 * For data we probably want to restrict p to below 1GeV for pi and K, but
 * for Hijing and Pythia we can have perfect pid. Currently cuts are type
 * into the code (so you have to re-compile to change them.)
 *
 *   In the Correlations code I split -+ from +- and am keeping track of
 * pt for each cut bin. These required changes in the Support code.
 *
 * Revision 1.11  2006/04/04 22:10:07  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.10  2006/02/22 22:05:10  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
 * Revision 1.8  2005/10/10 16:22:51  msd
 * fixing bug in buffer initialization
 *
 * Revision 1.7  2005/09/14 17:14:17  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.6  2005/09/07 20:21:13  prindle
 *
 *   2ptCorrelations: Rearranged array/histogram initialization/destruction.
 *                    Now histograms are only allocated at end of job,
 *                    just before they are filled then written.
 *
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


