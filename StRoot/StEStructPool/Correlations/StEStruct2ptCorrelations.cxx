/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.cxx,v 1.10 2006/02/22 22:05:10 prindle Exp $
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
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/Correlations/StEStructMaxB.h"  
#include "StEStructPool/Correlations/StEStructBuffer.h"  // to read some constants  

#include "StTimer.hh"
#include "StEStructCutBin.h"

#include "Stiostream.h"

#include <iostream>
using namespace std;

ClassImp(StEStruct2ptCorrelations);

//--------------------------------------------------------------------------
//StEStruct2ptCorrelations::StEStruct2ptCorrelations(int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mdoPairDensityHistograms(false), mInit(false), mDeleted(false) {   }

StEStruct2ptCorrelations::StEStruct2ptCorrelations(int mode) {
  manalysisMode=mode; 
  mskipPairCuts=false; 
  mdoPairCutHistograms=false; 
  mdoPairDensityHistograms=false; 
  mInit=false; 
  mDeleted=false;
  mHistosWritten = false;

  kZBuffMin   = -75.0;
  kZBuffMax   = +75.0;
  kBuffWidth  = 5.0;
  kNumBuffers = int( (kZBuffMax-kZBuffMin)/kBuffWidth );  // Better be 30.
  for(int i=0;i<kNumBuffers;i++)mbuffCounter[i]=0;
}


//--------------------------------------------------------------------------
//StEStruct2ptCorrelations::StEStruct2ptCorrelations(const char* cutFileName, int mode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mdoPairDensityHistograms(false), mInit(false), mDeleted(false), mPair(cutFileName) {  }
StEStruct2ptCorrelations::StEStruct2ptCorrelations(const char* cutFileName, int mode):  mPair(cutFileName) {
  manalysisMode=mode;
  mskipPairCuts=false;
  mdoPairCutHistograms=false;
  mdoPairDensityHistograms=false;
  mInit=false;
  mDeleted=false;
  mHistosWritten = false;
  //mPair(cutFileName);

  kZBuffMin   = -75.0;
  kZBuffMax   = +75.0;
  kBuffWidth  = 5.0;
  kNumBuffers = int( (kZBuffMax-kZBuffMin)/kBuffWidth );  // Better be 30.
  for(int i=0;i<kNumBuffers;i++)mbuffCounter[i]=0;
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
  cout << "  Skip Pair Cuts = " << mskipPairCuts << endl;
  cout << "  Do Pair Cut Hists = " << mdoPairCutHistograms << endl;
  cout << "  Do Pair Density Hists = " << mdoPairDensityHistograms << endl;

  for(int i=0;i<8;i++)numPairs[i]=numPairsProcessed[i]=mpossiblePairs[i]=0;

  initArrays();
// Try allocating histograms at start of job.
// If we don't add histograms to directory we don't get the obnoxious
//   Potential memory leak error.
  TH1::AddDirectory(kFALSE);
  initHistograms();

  /* Event count via Nch distribution */
  mHNEvents[0]=new TH1F("NEventsSame","NEventsSame",1000,0.,2000.);
  mHNEvents[1]=new TH1F("NEventsMixed","NEventsMixed",1000,0.,2000.);
  
  /* Inclusive pt distribution (per cutbin) */
  StEStructCutBin* cb = StEStructCutBin::Instance();
  int ncutbins=cb->getNumBins();
  mHpt = new TH1F*[ncutbins];  
  for(int i=0;i<ncutbins;i++){
    TString hname("pt");
    if(ncutbins>1)hname+=i;
    mHpt[i] = new TH1F(hname.Data(),hname.Data(),500,0,10.);
  }

  /* Event Mixing Parameters */
  mHmix = new TH2F("EventMixing","Event Mixing: delta-Z vs delta-N",50,0,10, 50,0,200);  // deltaZ vs deltaN
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
  mHistosWritten = true;
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::cleanUp(){ 
  if(mDeleted) return;
  deleteHistograms();
  deleteArrays(); 
  mDeleted=true;
}

//--------------------------------------------------------------------------
// Parse cuts file for limits on vertex position.
// Want 5cm wide buffers.
void StEStruct2ptCorrelations::setZBuffLimits( const char* cutFileName ) {
    ifstream from(cutFileName);

    if(!from){ 
        cout<<" Cut file Not Found while looking for Z vertex limits in StEStruct2ptCorrelations::setZBuffLimits"<<endl; 
    }

    char line[256], lineRead[256];
    char* puteol;
    char** val = new char*[100];
    int ival = 0;

    bool done = false;
    while(!done) {
        if(from.eof()){
            cout<<" Did not find Z vertex limits in StEStruct2ptCorrelations::setZBuffLimits, file "<< cutFileName <<endl; 
            done=true;
        } else {
            from.getline(lineRead,256);
            strcpy(line,lineRead);
            if ( (line[0]=='#') ) {
                continue;
            }
            if ( !strstr(line,"primaryVertexZ") ) {
                continue;
            }
            if ((puteol=strstr(line,"#"))) {
                *puteol='\0';
            }
            ival=0;
            val[ival]=line;
            char* fcomma;
            while ((fcomma=strstr(val[ival],","))) {
                *fcomma='\0';
                fcomma++;
                ival++;
                val[ival]=fcomma;
            }
            if (ival==2) {
                done=true;
                break;
            }
        }
    }
    if (ival != 2) {
        cout << " Did not find a line containing primaryVertexZ and two numbers in file " << cutFileName <<endl; 
        cout<<" Using default limits kZBuffMin = " << kZBuffMin << ", kZBuffMax = " << kZBuffMax <<endl; 
    } else {
        kZBuffMin = strtof(val[1],0);
        kZBuffMax = strtof(val[2],0);
        kNumBuffers = int( (kZBuffMax-kZBuffMin)/kBuffWidth );  // Need to check this is 30 or less.
        if (kNumBuffers > 30) {
            kNumBuffers = 30;
            kBuffWidth = (kZBuffMax - kZBuffMin) / kNumBuffers;
            cout << "   Too many buffers needed to cover vertex range." << endl;
            cout << "   Increasing width of z slice to " << kBuffWidth << endl;
            if (kBuffWidth > 7) {
              cout << "**WARNING**: Jeff Porter says z-slice bins larger than about 6.5cm causes systematice effects on correlations**" << endl;
            }
        }
        for(int i=0;i<kNumBuffers;i++)mbuffCounter[i]=0;
        cout << "  Mixing events with deltaZ = " << kBuffWidth << " and deltaN = " << DELTANMAX;
        cout << ", " << _MAXEBYEBUFFER_ << " mixed events for each event." << endl;
    }
    from.close();
    delete [] val;  val = 0;
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
  StEStructBinning* b = StEStructBinning::Instance();
  StEStructCutBin* cb = StEStructCutBin::Instance();
  int ncutbins=cb->getNumBins();
  StEStructTrackCollection *tp = mCurrentEvent->TrackCollectionP();
  float ptval;
  QAEtaBins *eta;
  QAPhiBins *phi;
  QAPtBins  *pt;
  PtotBins  *dedx;
  for(StEStructTrackIterator iter = tp->begin(); iter != tp->end(); iter++) {
    int* index_ids=cb->getPtBins(ptval=(*iter)->Pt());
    int j=0;
    while(index_ids[j]>=0){
      mHpt[index_ids[j]]->Fill(ptval);
      j++;
      if(j==ncutbins)break;
    }
    int iptot = b->iptot((*iter)->Ptot());
    int idedx = b->idedx((*iter)->Dedx());
    int i = cb->getdEdxPID((*iter));
    dedx = mdEdxPtot[0][i];
    dedx[idedx].Ptot[iptot] += 1;

    int ieta = b->iqaeta((*iter)->Eta());
    eta = &mQAEta[0][i];
    eta->Eta[ieta] += 1;
    int iphi = b->iqaphi((*iter)->Phi());
    phi = &mQAPhi[0][i];
    phi->Phi[iphi] += 1;
    int ipt = b->iqapt((*iter)->Pt());
    pt = &mQAPt[0][i];
    pt->Pt[ipt] += 1;
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

  StEStructTrackCollection *tm = mCurrentEvent->TrackCollectionM();
  for(StEStructTrackIterator iter = tm->begin(); iter != tm->end(); iter++) {
    int* index_ids=cb->getPtBins(ptval=(*iter)->Pt());
    int j=0;
    while(index_ids[j]>=0){
      mHpt[index_ids[j]]->Fill(ptval);
      j++;
      if(j==ncutbins)break;
    }
    int iptot = b->iptot((*iter)->Ptot());
    int idedx = b->idedx((*iter)->Dedx());
    int i = cb->getdEdxPID((*iter));
    dedx = mdEdxPtot[1][i];
    dedx[idedx].Ptot[iptot] += 1;

    int ieta = b->iqaeta((*iter)->Eta());
    eta = &mQAEta[1][i];
    eta->Eta[ieta] += 1;
    int iphi = b->iqaphi((*iter)->Phi());
    phi = &mQAPhi[1][i];
    phi->Phi[iphi] += 1;
    int ipt = b->iqapt((*iter)->Pt());
    pt = &mQAPt[1][i];
    pt->Pt[ipt] += 1;
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

  return makeSiblingAndMixedPairs();
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::moveEvents(){

  if(!mCurrentEvent) return;
  if (mCurrentEvent->VertexZ() > kZBuffMax) {
      return;
  }
  int i=(int) floor((mCurrentEvent->VertexZ()-kZBuffMin)/kBuffWidth); // eventually this should be moved up to the buffer class...
  if(i<0 || i>kNumBuffers-1) return;                              
  mbuffer[i].addEvent(mCurrentEvent);
  mbuffCounter[i]++;

}

//--------------------------------------------------------------------------
bool StEStruct2ptCorrelations::makeSiblingAndMixedPairs(){

  if(!mCurrentEvent) return false; // logic problem!
  if (mCurrentEvent->VertexZ() > kZBuffMax) {
      return false;
  }
  int i=(int) floor((mCurrentEvent->VertexZ()-kZBuffMin)/kBuffWidth);
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

  StEStructBinning* b = StEStructBinning::Instance();
  StEStructCutBin* cb = StEStructCutBin::Instance();

  ytBins**  ytyt        = mYtYt[j];
  xtBins**  xtxt        = mXtXt[j];
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
  int symmetrizeYt;

  if(mtimer)mtimer->start();

  float mass1, mass2;
  for(Iter1=t1->begin(); Iter1!=t1->end();++Iter1){

    mPair.SetTrack1(*Iter1);
    if(j==0 || j==3) { 
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

        if (cb->switchYtBins(&mPair)) {
          mass1 = mPair.Track2()->AssignedMass();
          iyt1  = b->iyt(mPair.Track2()->Yt(mass1));
          ixt1  = b->ixt(mPair.Track2()->Xt());
          pt1   = mPair.Track2()->Pt();
          ipt1  = b->ipt(pt1);
          ieta1 = b->ieta(mPair.Track2()->Eta(mass1));
          iphi1 = b->iphi(mPair.Track2()->Phi());
          mass2 = mPair.Track1()->AssignedMass();
          iyt2  = b->iyt(mPair.Track1()->Yt(mass2));
          ixt2  = b->ixt(mPair.Track1()->Xt());
          pt2   = mPair.Track1()->Pt();
          ipt2  = b->ipt(pt2);
          ieta2 = b->ieta(mPair.Track1()->Eta(mass2));
          iphi2 = b->iphi(mPair.Track1()->Phi());
        } else {
          mass1 = mPair.Track1()->AssignedMass();
          iyt1  = b->iyt(mPair.Track1()->Yt(mass1));
          ixt1  = b->ixt(mPair.Track1()->Xt());
          pt1   = mPair.Track1()->Pt();
          ipt1  = b->ipt(pt1);
          ieta1 = b->ieta(mPair.Track1()->Eta(mass1));
          iphi1 = b->iphi(mPair.Track1()->Phi());
          mass2 = mPair.Track2()->AssignedMass();
          iyt2  = b->iyt(mPair.Track2()->Yt(mass2));
          ixt2  = b->ixt(mPair.Track2()->Xt());
          pt2   = mPair.Track2()->Pt();
          ipt2  = b->ipt(pt2);
          ieta2 = b->ieta(mPair.Track2()->Eta(mass2));
          iphi2 = b->iphi(mPair.Track2()->Phi());
        }
        symmetrizeYt = cb->symmetrizeYtBins(&mPair);

        float pwgt=pt1*pt2;
        float swgt=pt1+pt2;

        //-> X vs X
        ytyt[iy][iyt1].yt[iyt2]+=nwgt; 
        xtxt[iy][ixt1].xt[ixt2]+=nwgt; 
        ptpt[iy][ipt1].pt[ipt2]+=nwgt; 
        etaeta[iy][ieta1].eta[ieta2]+=nwgt; 
        phiphi[iy][iphi1].phi[iphi2]+=nwgt;

        pretaeta[iy][ieta1].eta[ieta2]+=pwgt; 
        prphiphi[iy][iphi1].phi[iphi2]+=pwgt;
        suetaeta[iy][ieta1].eta[ieta2]+=swgt; 
        suphiphi[iy][iphi1].phi[iphi2]+=swgt;

        //-> X vs X (symmetry)
        if (symmetrizeYt) {
          ytyt[iy][iyt2].yt[iyt1]+=nwgt;
          xtxt[iy][ixt2].xt[ixt1]+=nwgt; 
          ptpt[iy][ipt2].pt[ipt1]+=nwgt; 
          etaeta[iy][ieta2].eta[ieta1]+=nwgt; 
          phiphi[iy][iphi2].phi[iphi1]+=nwgt;

          pretaeta[iy][ieta2].eta[ieta1]+=pwgt; 
          prphiphi[iy][iphi2].phi[iphi1]+=pwgt;
          suetaeta[iy][ieta2].eta[ieta1]+=swgt; 
          suphiphi[iy][iphi2].phi[iphi1]+=swgt;
        }

        //-> delta y vs delta x
        idyt1 = b->idyt(mPair.DeltaYt(mass1,mass2));
        idpt1 = b->idpt(mPair.DeltaPt());
        ideta1= b->ideta(mPair.DeltaEta(mass1,mass2));
        idphi1= b->idphi(mPair.DeltaPhi());

        idyt2 = b->idyt(-1.*mPair.DeltaYt(mass1,mass2));
        idpt2 = b->idpt(-1.*mPair.DeltaPt());
        ideta2= b->ideta(-1.*mPair.DeltaEta(mass1,mass2));
        idphi2= b->idphi(-1.*mPair.DeltaPhi());

        isyt = b->isyt(mPair.SigmaYt(mass1,mass2));
        ispt = b->ispt(mPair.SigmaPt());
        iseta= b->iseta(mPair.SigmaEta(mass1,mass2));
        

        //--- symmetry ---
        // Note that the differences, dyt, dpt, deta and dphi,
        // actually fill different parts of the histograms.

        jtdetadphi[iy][ideta1].dphi[idphi1] +=nwgt; 
        jtdetadphi[iy][ideta2].dphi[idphi1] +=nwgt;
        jtdetadphi[iy][ideta1].dphi[idphi2] +=nwgt;
        jtdetadphi[iy][ideta2].dphi[idphi2] +=nwgt; 

        jtdytdphi[iy][idyt1].dphi[idphi1] +=nwgt;
        jtdytdphi[iy][idyt1].dphi[idphi2] +=nwgt;
        jtdytdphi[iy][idyt2].dphi[idphi1] +=nwgt;
        jtdytdphi[iy][idyt2].dphi[idphi2] +=nwgt;

        jtdytdeta[iy][idyt1].deta[ideta1] +=nwgt;
        jtdytdeta[iy][idyt1].deta[ideta2] +=nwgt;
        jtdytdeta[iy][idyt2].deta[ideta1] +=nwgt;
        jtdytdeta[iy][idyt2].deta[ideta2] +=nwgt;

        prjtdetadphi[iy][ideta1].dphi[idphi1] += pwgt;
        prjtdetadphi[iy][ideta2].dphi[idphi1] += pwgt;
        prjtdetadphi[iy][ideta1].dphi[idphi2] += pwgt;
        prjtdetadphi[iy][ideta2].dphi[idphi2] += pwgt;

        sujtdetadphi[iy][ideta1].dphi[idphi1] += swgt;
        sujtdetadphi[iy][ideta2].dphi[idphi1] += swgt;
        sujtdetadphi[iy][ideta1].dphi[idphi2] += swgt;
        sujtdetadphi[iy][ideta2].dphi[idphi2] += swgt;

        //-> Sum y vs delta x
        // For symmetry only reflect around the delta axis.
        atytyt[iy][isyt].dyt[idyt1] +=nwgt;
        atytyt[iy][isyt].dyt[idyt2] +=nwgt;

        atptpt[iy][ispt].dpt[idpt1] +=nwgt;
        atptpt[iy][ispt].dpt[idpt2] +=nwgt;

        jtsetadphi[iy][iseta].dphi[idphi1]+=nwgt;
        jtsetadphi[iy][iseta].dphi[idphi2]+=nwgt;

        prjtsetadphi[iy][iseta].dphi[idphi1] += pwgt;
        prjtsetadphi[iy][iseta].dphi[idphi2]+= pwgt;

        sujtsetadphi[iy][iseta].dphi[idphi1] += swgt;
        sujtsetadphi[iy][iseta].dphi[idphi2]+= swgt;

        qinv[iy].q[b->iq(mPair.qInv())]+=nwgt;
    
        if(mdoPairDensityHistograms) {
          avgtsep[iy].sep[isavgt=b->isep(mPair.NominalTpcAvgXYSeparation())]+=nwgt;
          avgzsep[iy].sep[isavgz=b->isep(mPair.NominalTpcAvgZSeparation())]+=nwgt;
          enttsep[iy].sep[isentt=b->isep(mPair.NominalTpcXYEntranceSeparation())]+=nwgt;
          entzsep[iy].sep[isentz=b->isep(mPair.NominalTpcZEntranceSeparation())]+=nwgt;
          midtsep[iy].sep[ismidt=b->isep(mPair.MidTpcXYSeparation())]+=nwgt;
          midzsep[iy].sep[ismidz=b->isep(mPair.MidTpcZSeparation())]+=nwgt;
          exittsep[iy].sep[isexitt=b->isep(mPair.NominalTpcXYExitSeparation())]+=nwgt;
          exitzsep[iy].sep[isexitz=b->isep(mPair.NominalTpcZExitSeparation())]+=nwgt;

          avgtz[iy][isavgt].sep[isavgz]+=nwgt;
          enttz[iy][isentt].sep[isentz]+=nwgt;
          midtz[iy][ismidt].sep[ismidz]+=nwgt;
          exittz[iy][isexitt].sep[isexitz]+=nwgt;     

          // need to rearrange pair so that deltaPhi>0
          float delpt; // my delta pt 
          int idelpt;  // bin index
          if (mPair.Track1()->Phi() - mPair.Track2()->Phi() >= 0) { 
            delpt = mPair.DeltaPt();  // here delta=1-2, can use standard defs
            idelpt = idpt1;
          } else {  // redefine delta=2-1
            delpt = -1.*mPair.DeltaPt();
            idelpt = idpt2; 
          }
          enttd[iy][isentt].dpt[idelpt]+=nwgt;  
          midtd[iy][ismidt].dpt[idelpt]+=nwgt;
          exittd[iy][isexitt].dpt[idelpt]+=nwgt;
          if (delpt>=0) {                                     
            midtp[iy].sep[b->isep(mPair.MidTpcXYSeparation())]+=nwgt;  
            midzp[iy].sep[b->isep(mPair.MidTpcZSeparation()) ]+=nwgt;
          } else {
            midtn[iy].sep[b->isep(mPair.MidTpcXYSeparation())]+=nwgt;
            midzn[iy].sep[b->isep(mPair.MidTpcZSeparation()) ]+=nwgt;
          }

        } // pair density
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

  float xv,yv;
  StEStructBinning* b=StEStructBinning::Instance();
  int numCutBins=StEStructCutBin::Instance()->getNumBins();
  for(int i=0; i<8; i++){

    ytBins**  ytyt   = mYtYt[i];
    xtBins**  xtxt   = mXtXt[i];
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

    for(int k=0;k<b->xtBins();k++)
      for(int j=0;j<b->xtBins();j++)
        mHXtXt[i][y]->Fill(b->xtVal(k),b->xtVal(j),xtxt[y][k].xt[j]);

    for(int k=0;k<b->ptBins();k++)
      for(int j=0;j<b->ptBins();j++)
        mHPtPt[i][y]->Fill(b->ptVal(k),b->ptVal(j),ptpt[y][k].pt[j]);

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

  for(int j=0;j<numCutBins;j++){
    mHpt[j]->Write();
   for(int i=0;i<8;i++){

     mHYtYt[i][j]->Write();
     mHXtXt[i][j]->Write();
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
void StEStruct2ptCorrelations::writeQAHists() {
  if(!mqaoutFileName){
    cout<<" NO QA OUTPUTFILE TO WRITE TO ..... giving up ...."<<endl;
    return;
  }

  StEStructBinning* b=StEStructBinning::Instance();
  for(int j=0; j<2; j++) {
    for(int i=0; i<4; i++) {
      PtotBins*  dedx   = mdEdxPtot[j][i];
      for(int k=0;k<b->dEdxBins();k++) {
        for(int l=0;l<b->PtotBins();l++) {
          mHdEdxPtot[j][i]->Fill(b->ptotVal(l),b->dedxVal(k),dedx[k].Ptot[l]);
        }
      }
      QAEtaBins  *eta   = &mQAEta[j][i];
      for(int k=0;k<b->QAEtaBins();k++) {
        mHQAEta[j][i]->Fill(b->qaetaVal(k),eta->Eta[k]);
      }
      QAPhiBins  *phi   = &mQAPhi[j][i];
      for(int k=0;k<b->QAPhiBins();k++) {
        mHQAPhi[j][i]->Fill(b->qaphiVal(k),phi->Phi[k]);
      }
      QAPtBins  *pt   = &mQAPt[j][i];
      for(int k=0;k<b->QAPtBins();k++) {
        mHQAPt[j][i]->Fill(b->qaptVal(k),pt->Pt[k]);
      }
    }
  }

  TFile * tf=new TFile(mqaoutFileName,"RECREATE");
  tf->cd();
  for(int j=0;j<2;j++) {
      for(int i=0;i<4;i++){
          mHdEdxPtot[j][i]->Write();
          mHQAEta[j][i]->Write();
          mHQAPhi[j][i]->Write();
          mHQAPt[j][i]->Write();
      }
  }
  tf->Close();
}

//--------------------------------------------------------------------------
void StEStruct2ptCorrelations::initArrays(){

  int numCutBins=StEStructCutBin::Instance()->getNumBins();

  // storage arrays
  QAPhiBins *phi;
  QAEtaBins *eta;
  QAPtBins  *pt;
  for (int j=0;j<2;j++) {
      for (int i=0;i<4;i++) {
          mdEdxPtot[j][i] = new PtotBins[ESTRUCT_DEDX_BINS];
          memset(mdEdxPtot[j][i],0,ESTRUCT_DEDX_BINS*sizeof(PtotBins));
          phi = &mQAPhi[j][i];
          memset(phi,0,ESTRUCT_QAPHI_BINS*sizeof(float));
          eta = &mQAEta[j][i];
          memset(eta,0,ESTRUCT_QAETA_BINS*sizeof(float));
          pt  = &mQAPt[j][i];
          memset(pt,0,ESTRUCT_QAPT_BINS*sizeof(float));
      }
  }

  for(int i=0;i<8;i++){

     mYtYt[i]=new ytBins*[numCutBins];
     mAtSYtDYt[i]=new dytBins*[numCutBins];
     mXtXt[i]=new xtBins*[numCutBins];
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
     memset(mQinv[i],0,numCutBins*sizeof(qBins)); // do the memset here

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
     mXtXt[i][j]=new xtBins[ESTRUCT_XT_BINS];
     memset(mXtXt[i][j],0,ESTRUCT_XT_BINS*sizeof(xtBins));
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

  for(int j=0;j<2;j++) {
      for(int i=0;i<4;i++){
          delete []  mdEdxPtot[j][i];
      }
  }

  for(int i=0;i<8;i++){
    
    for(int j=0;j<numCutBins;j++){
      
      delete []  mYtYt[i][j];
      delete []  mXtXt[i][j];
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

  // histograms second

  int nx = b->PtotBins();
  int ny = b->dEdxBins();
  float xmin = b->PtotMin();
  float xmax = b->PtotMax();
  float ymin = b->dEdxMin();
  float ymax = b->dEdxMax();
  mHdEdxPtot[0][0] = new TH2F("unidentified+_dEdx","unidentified+_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[0][1] = new TH2F("pion+_dEdx","pion+_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[0][2] = new TH2F("Kaon+_dEdx","Kaon+_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[0][3] = new TH2F("proton+_dEdx","proton+_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[1][0] = new TH2F("unidentified-_dEdx","unidentified-_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[1][1] = new TH2F("pion-_dEdx","pion-_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[1][2] = new TH2F("Kaon-_dEdx","Kaon-_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  mHdEdxPtot[1][3] = new TH2F("proton-_dEdx","proton-_dEdx",nx,xmin,xmax,ny,ymin,ymax);
  nx = b->QAEtaBins();
  xmin = b->QAEtaMin();
  xmax = b->QAEtaMax();
  mHQAEta[0][0] = new TH1F("unidentified+_Eta","unidentified+_Eta",nx,xmin,xmax);
  mHQAEta[0][1] = new TH1F("pion+_Eta","pion+_Eta",nx,xmin,xmax);
  mHQAEta[0][2] = new TH1F("Kaon+_Eta","Kaon+_Eta",nx,xmin,xmax);
  mHQAEta[0][3] = new TH1F("proton+_Eta","proton+_Eta",nx,xmin,xmax);
  mHQAEta[1][0] = new TH1F("unidentified-_Eta","unidentified-_Eta",nx,xmin,xmax);
  mHQAEta[1][1] = new TH1F("pion-_Eta","pion-_Eta",nx,xmin,xmax);
  mHQAEta[1][2] = new TH1F("Kaon-_Eta","Kaon-_Eta",nx,xmin,xmax);
  mHQAEta[1][3] = new TH1F("proton-_Eta","proton-_Eta",nx,xmin,xmax);
  nx = b->QAPhiBins();
  xmin = b->QAPhiMin();
  xmax = b->QAPhiMax();
  mHQAPhi[0][0] = new TH1F("unidentified+_Phi","unidentified+_Phi",nx,xmin,xmax);
  mHQAPhi[0][1] = new TH1F("pion+_Phi","pion+_Phi",nx,xmin,xmax);
  mHQAPhi[0][2] = new TH1F("Kaon+_Phi","Kaon+_Phi",nx,xmin,xmax);
  mHQAPhi[0][3] = new TH1F("proton+_Phi","proton+_Phi",nx,xmin,xmax);
  mHQAPhi[1][0] = new TH1F("unidentified-_Phi","unidentified-_Phi",nx,xmin,xmax);
  mHQAPhi[1][1] = new TH1F("pion-_Phi","pion-_Phi",nx,xmin,xmax);
  mHQAPhi[1][2] = new TH1F("Kaon-_Phi","Kaon-_Phi",nx,xmin,xmax);
  mHQAPhi[1][3] = new TH1F("proton-_Phi","proton-_Phi",nx,xmin,xmax);
  nx = b->QAPtBins();
  xmin = b->QAPtMin();
  xmax = b->QAPtMax();
  mHQAPt[0][0] = new TH1F("unidentified+_Pt","unidentified+_Pt",nx,xmin,xmax);
  mHQAPt[0][1] = new TH1F("pion+_Pt","pion+_Pt",nx,xmin,xmax);
  mHQAPt[0][2] = new TH1F("Kaon+_Pt","Kaon+_Pt",nx,xmin,xmax);
  mHQAPt[0][3] = new TH1F("proton+_Pt","proton+_Pt",nx,xmin,xmax);
  mHQAPt[1][0] = new TH1F("unidentified-_Pt","unidentified-_Pt",nx,xmin,xmax);
  mHQAPt[1][1] = new TH1F("pion-_Pt","pion-_Pt",nx,xmin,xmax);
  mHQAPt[1][2] = new TH1F("Kaon-_Pt","Kaon-_Pt",nx,xmin,xmax);
  mHQAPt[1][3] = new TH1F("proton-_Pt","proton-_Pt",nx,xmin,xmax);


  for(int i=0; i<8; i++){

    mHYtYt[i]=new TH2F*[numCutBins];
    mHAtSYtDYt[i]=new TH2F*[numCutBins];
    mHXtXt[i]=new TH2F*[numCutBins];
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

    if(mdoPairDensityHistograms) {
      mHTPCAvgTSep[i]=new TH1F*[numCutBins];
      mHTPCAvgZSep[i]=new TH1F*[numCutBins];
      mHTPCEntTSep[i]=new TH1F*[numCutBins];
      mHTPCEntZSep[i]=new TH1F*[numCutBins];
      mHTPCMidTSep[i]=new TH1F*[numCutBins];
      mHTPCMidZSep[i]=new TH1F*[numCutBins];
      mHTPCExitTSep[i]=new TH1F*[numCutBins];
      mHTPCExitZSep[i]=new TH1F*[numCutBins];
      mHTPCMidTdptP[i]=new TH1F*[numCutBins];
      mHTPCMidTdptN[i]=new TH1F*[numCutBins];
      mHTPCMidZdptP[i]=new TH1F*[numCutBins];
      mHTPCMidZdptN[i]=new TH1F*[numCutBins];
      mHTPCAvgTZ[i]=new TH2F*[numCutBins];
      mHTPCEntTZ[i]=new TH2F*[numCutBins];
      mHTPCMidTZ[i]=new TH2F*[numCutBins];
      mHTPCExitTZ[i]=new TH2F*[numCutBins];
      mHTPCEntTdpt[i]=new TH2F*[numCutBins];
      mHTPCMidTdpt[i]=new TH2F*[numCutBins];
      mHTPCExitTdpt[i]=new TH2F*[numCutBins];
    }
    
    int ncb=numCutBins;

    for(int j=0;j<numCutBins;j++){

      createHist2D(mHYtYt,"YtYt",i,j,ncb,b->ytBins(),b->ytMin(),b->ytMax(),b->ytBins(),b->ytMin(),b->ytMax());
      createHist2D(mHAtSYtDYt,"SYtDYt",i,j,ncb,b->sytBins(),b->sytMin(),b->sytMax(),b->dytBins(),b->dytMin(),b->dytMax());

      createHist2D(mHXtXt,"XtXt",i,j,ncb,b->xtBins(),b->xtMin(),b->xtMax(),b->xtBins(),b->xtMin(),b->xtMax());

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

  for(int j=0;j<2;j++){
      for(int i=0;i<4;i++){
          delete mHdEdxPtot[j][i];
          delete mHQAEta[j][i];
          delete mHQAPhi[j][i];
          delete mHQAPt[j][i];
      }
  }

  for(int i=0;i<8;i++){
    for(int j=0;j<numCutBins;j++){
      delete mHYtYt[i][j];
      delete mHXtXt[i][j];
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
    delete [] mHXtXt[i];
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


