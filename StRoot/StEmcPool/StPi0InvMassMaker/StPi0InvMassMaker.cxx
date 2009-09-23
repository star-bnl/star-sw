/*******************************************************************************
 * 
 * $Id: StPi0InvMassMaker.cxx,v 1.3 2009/09/23 03:46:36 fine Exp $
 *
 * Author: Andre Mischke, Utrecht, Jan 2005 (a.mischke@phys.uu.nl)
 *******************************************************************************
 *  
 * Description: StPi0InvMassMaker: 
 *
 * calculates minv(gamma,gamma) tower by tower. The tower is assigned by
 * the neutral bemc point with the highest energy.
 *
 ******************************************************************************/

#include <vector>
#include <algorithm>

#include "TFile.h"
#include "TH2F.h"
#include "TObjArray.h"

// ROOT
#include "TROOT.h" // for gROOT variable
#include "TNtuple.h"
#include "TTree.h"

// StEvent
//#include "StChain.h"
#include "StEvent.h"
#include "StEventTypes.h"

// EMC related
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

// for dedx
#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
#include "StDedxPidTraits.h"
#include "StTrackFitTraits.h"
#include "StTrackDetectorInfo.h"
#include "StTrackGeometry.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StZdcTriggerDetector.h"
#include "StEventTypes.h"
#include "StMemoryInfo.hh"

#include "StMaker.h"

#include "TEventMixer.h"
#include "TMixer.h"
#include "StPi0InvMassMaker.h"

ClassImp(StPi0InvMassMaker)


//________________________________________________________________________________________________________
StPi0InvMassMaker::StPi0InvMassMaker(const char *name, Bool_t doTracks, const char *outfile):StMaker(name)
{
  // constructor 
  mDoTracks = doTracks;
  mFileName = outfile;

  mPi = 3.1416;
  mPi0Mass = 0.13498;
 
  debug = false; // debug mode  
  fnummixed = 5; // number of events for event-mixing  

  // initialize counter values
  ievtot=0;
/*
  ievaccep=0;
  inochainpt=0; 
  inoevpt=0;
  ibadrun=0;
  iemc=0;
  inoprimvert=0;
  itrig=0;
  */
}

//________________________________________________________________________________________________________
StPi0InvMassMaker::~StPi0InvMassMaker()
{
  // destructor	
}

//________________________________________________________________________________________________________
Int_t StPi0InvMassMaker::Init()
{
  cout << "++++++++++++ StPi0InvMassMaker::Init()" << endl;

  mSMDSummary = new TH1F("mSMDSummary","",5,-0.5,4.5);
  mEventSummary = new TH1F("mEventSummary","0: all, 1: acc1, 2: acc2",10,-0.5,9.5);

  mTriggerSummary = new TH1F("mTriggerSummary","0:MB, 1:HT1, 2:HT2, 3:all accepted", 19,-0.25,9.25);
  mTriggerSummaryCut1 = new TH1F("mTriggerSummaryCut1","0:MB, 1:HT1, 2:HT2, 3:all accepted", 19,-0.25,9.25);
  mTriggerSummaryCut2 = new TH1F("mTriggerSummaryCut2","0:MB, 1:HT1, 2:HT2, 3:all accepted", 19,-0.25,9.25);
  mTriggerSummaryZeroBemc = new TH1F("mTriggerSummaryZeroBemc","0:MB, 1:HT1, 2:HT2, 3:all accepted", 19,-0.25,9.25);
  
  //mPointCluster = new TH1F("mPointCluster","",10,0,10);
  mTrackPtHistMinBias = new TH1F("mTrackPtHistMinBias","Global tracks P_{T} for MinBias",100,0,30);
  mTrackPtHistHiTower = new TH1F("mTrackPtHistHiTower","Global tracks P_{T} for HiTower",100,0,30);
  mPointPtHistMinBias = new TH1F("mPointPtHistMinBias","EMC point P_{T} for MinBias",100,0,40);
  mPointPtHistHiTower = new TH1F("mPointPtHistHiTower","EMC point P_{T} for HiTower",100,0,40);
      
  
  mPhotonSpectra_MB  = new TH1F("mPhotonSpectra_MB","",400,0.,40.);
  mPhotonSpectra_HT1 = new TH1F("mPhotonSpectra_HT1","",400,0.,40.);
  mPhotonSpectra_HT2 = new TH1F("mPhotonSpectra_HT2","",400,0.,40.);
  mPhotonSpectraSel_MB  = new TH1F("mPhotonSpectraSel_MB","",400,0.,40.);
  mPhotonSpectraSel_HT1 = new TH1F("mPhotonSpectraSel_HT1","",400,0.,40.);
  mPhotonSpectraSel_HT2 = new TH1F("mPhotonSpectraSel_HT2","",400,0.,40.);
  mHiTowerEtSpectra_MB  = new TH1F("mHiTowerEtSpectra_MB","",400,0.,40.);  
  mHiTowerEtSpectra_HT1 = new TH1F("mHiTowerEtSpectra_HT1","",400,0.,40.);
  mHiTowerEtSpectra_HT2 = new TH1F("mHiTowerEtSpectra_HT2","",400,0.,40.);
  
  mZvertex = new TH1F("mZvertex","",100,-215,215);
  mZvertexCut2 = new TH1F("mZvertexCut2","",100,-215,215);
  
  mPrimaryTracks = new TH1F("mPrimaryTracks","",261,-10.5,250.5);
  mPrimaryTracksCut1 = new TH1F("mPrimaryTracksCut1","",261,-10.5,250.5);
  mPrimaryTracks_BemcHits = new TH2F("mPrimaryTracks_BemcHits","",100,-0.5,99.5, 261,-10.5,250.5);		  
  
  // check for bad runs!
  mBemcPoints  = new TH1F("mBemcPoints","Number of Bemc points, all",100,-0.5,99.5);
  mBemcPointsCut1  = new TH1F("mBemcPointsCut1","Number of Bemc points, cut1",100,-0.5,99.5); 
  mBemcPointsCut12 = new TH2F("mBemcPointsCut12","Number of Bemc points, cut12",100,0.5,100000.5, 100,-0.5,99.5);
  mBemcPointsCut2 = new TH1F("mBemcPointsCut2","Number of Bemc points, cut2",100,-0.5,99.5);
  mBemcPointsAfterCuts2Photons = new TH1F("mBemcPointsAfterCuts2Photons","Number of Bemc points after cuts",100,-0.5,99.5);
    
  mTowerIdhit = new TH1F("mTowerIdhit","",2400,0.5,2400.5);
  mTowerIdhit2 = new TH2F("mTowerIdhit2","",100,0.5,100000.5, 2400,0.5,2400.5);
  mTowerIdTw  = new TH1F("mTowerIdTw","",2400,0.5,2400.5);

  for (Int_t i=1; i<=ntower; i++)
  {
    Char_t name[20]; sprintf(name,"minv_tower%d",i);
    mInvMass[i] = new TH1F(name,"", 100,0.,2.);
  }
  mPiZeroNtuple    = new TNtuple("mPiZeroNtuple","EMC Reconstructed Pi0 ntuple", "pt:m");
  //"trigger:pt:m:energy1:energy2:t1:t2:phi:eta");  "pt:m:n1:n2:t1:t2:asym");
  mPiZeroMixNtuple = new TNtuple("mPiZeroMixNtuple", "EMC Reconstructed Pi0 ntuple", "pt:m");

  //mPointsNtuple = new TNtuple("mPointsNtuple","EMC Points ntuple", "trigger:energy:t:id");
  //mRawHitNtuple = new TNtuple("mRawHitNtuple","","adc:runid:tid");  
  //HiPointEnergy - energy of highest point in the event


  // particle lists and mixers   CUTSS
  fEventMixer = new TEventMixer();
  fEventMixer->SetMixVariable(fnummixed,16,-80,80, 5,0.5,20.5, 7,-0.25,3.25);
  fTMixer = new TMixer(fnummixed);

  photonlist = new TObjArray();
  pipluslist = new TObjArray();


  // read bad tower list
  FILE *file = fopen("/star/u/amischke/pi0ana/corr_factor_towerwise.dat","r"); 
  for (Int_t i=1; i<=ntower; i++)
  {
    fscanf(file,"%d %f", &tower[i], &gain[i]); 
    //cout <<" +++ tower "<<tower[i]<<"     gain "<<gain[i]<<endl;
  }
  fclose(file);
		  
  return StMaker::Init();
}

//________________________________________________________________________________________________________
Int_t StPi0InvMassMaker::Make()
{
  ievtot++;
  if (debug) cout <<"+++++++++++++++++++++++++++++++++++++++++ event: "<<ievtot<<endl;
  mEventSummary->Fill(0);
 
  /*
  if (debug) cout << "--- check: mChain" << endl; 
  if (mChain==0) 
  {
    cout <<"++++++++++++ StPi0InvMassMaker::Make() Bad pointer to StChain object!"<<endl;
    //inochainpt++;
    //mEventSummary->Fill(2);
    return kStWarn;
  }
  */
  StEvent *event = (StEvent*)GetInputDS("StEvent");
  if (!event)
  {
    cout <<"++++++++++++ StPi0InvMassMaker::Make() Cannot get the event pointer"<<endl;
    //inoevpt++;
    mEventSummary->Fill(3);
    return kStOK;
  }

  readBadRunList();
  Int_t runN = event->runInfo()->runId();
  Int_t flag_run=0;
  for (Int_t k=0; k<=badrunidmax; k++)
  {
    //if (debug) cout <<"-------------> runid: "<<k<<"  "<<badrunid[k]<<"  "<<runN<<endl;
    if (runN == badrunid[k])
      flag_run=1;    
  }
  if (flag_run==1)
  {
      if (debug) cout <<"++++++++++++ StPi0InvMassMaker::Make() Bad run_id: "<<runN<<endl;
      //ibadrun++;
      mEventSummary->Fill(4);
      return kStOk;
  }

  // check for trigger condidtions
  Float_t trigger = 9;
  mb = true;
  ht1 = false;
  ht2 = false;

  if (event->triggerIdCollection() && event->triggerIdCollection()->nominal())
  {  
    if (debug) cout <<"++++++++++++ StPi0InvMassMaker::Make() not MinBias or High Tower triggered events"<<endl;
    //itrig++;
    mEventSummary->Fill(7);
    return kStOk;
    
    if (event->triggerIdCollection()->nominal()->isTrigger(2001) || 
        event->triggerIdCollection()->nominal()->isTrigger(2003)) 
    {
      mb = true;
    }
    if (event->triggerIdCollection()->nominal()->isTrigger(2201)) 
    {
      ht1 = true;
    }
    if (event->triggerIdCollection()->nominal()->isTrigger(2202)) 
    {
      ht2 = true;
    }
  }

  //new trigger selection
  if (mb && !(mb&&ht1) && !(mb&&ht2))
    trigger=0;
  if (mb&&ht1 && !(mb&&ht1&&ht2))
    trigger=0.5;
  if (ht1 && !(mb&&ht1) && !(ht1&&ht2))
    trigger=1.;
  if (ht1&&ht2 && !(mb&&ht1&&ht2))
    trigger=1.5;
  if (ht2 && !(ht1&&ht2) && !(mb&&ht2))
    trigger=2.;
  if (mb&&ht2 &&!(mb&&ht1&&ht2))
    trigger=2.5;
  if (mb&&ht1&&ht2)
    trigger=3.;
  
  
  StPrimaryVertex* primaryVertex = event->primaryVertex(); 
  if (!primaryVertex)
  {
    cout <<"++++++++++++ StPi0InvMassMaker::Make() Event w/o primary vertex"<<endl;
    //inoprimvert++;
    mEventSummary->Fill(6);
    return kStOk;
  }

  StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
  if (!emccol)
  {
    cout <<"++++++++++++ StPi0InvMassMaker::Make() No EMC Collection"<<endl;
    //iemc++;
    mEventSummary->Fill(5);
    return kStOk;
  }
  StEmcDetector* bemc = emccol->detector(kBarrelEmcTowerId);
  if (!bemc)
  {
    cout <<"++++++++++++ StPi0InvMassMaker::Make() No BEMC detector"<<endl;
    return kStOk;
  }
  StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
  mBemcPoints->Fill(bEmcPoints.size());
    
  // get primary tracks
  StSPtrVecPrimaryTrack& primaryTracks = primaryVertex->daughters();
  long numberOfPrimaryTracks = primaryTracks.size();
  mPrimaryTracks->Fill(numberOfPrimaryTracks);
  mPrimaryTracks_BemcHits->Fill(bEmcPoints.size(), numberOfPrimaryTracks);
  mTriggerSummary->Fill(4);
  mTriggerSummary->Fill(trigger);
      	
  mEventSummary->Fill(1);


  // reject z_vertex==0 events 
  StThreeVectorF vPos;
  vPos = primaryVertex->position();
  mZvertex->Fill(vPos.z());
  if (vPos.z()==0)
  {
    if (debug) cout <<"++++++++++++ StPi0InvMassMaker::Make() z_vertex==0"<<endl;
    //inochainpt++;
    mEventSummary->Fill(8);	
    return kStOk;
  }
  
  // reject events with no BEMC hits  
  if (bEmcPoints.size()==0)
  {
    if (debug) cout <<"++++++++++++ StPi0InvMassMaker::Make() No BEMC points"<<endl;
    //inochainpt++;
    mEventSummary->Fill(9);
    mTriggerSummaryZeroBemc->Fill(trigger);
    //return kStOk;
  }	  
  
  //ievaccep++;     
  mEventSummary->Fill(2);

  // event characteristics
  mPrimaryTracksCut1->Fill(numberOfPrimaryTracks);
   if (debug) cout <<"--- Number of primary tracks in this event: "<<numberOfPrimaryTracks<<endl;

  mBemcPointsCut1->Fill(bEmcPoints.size());
   // get reduced run number
   Int_t runnm = runN-5000000;
   if (debug) cout <<"--- event accepted... RUN_ID: "<<runN<<" ("<<runnm<<")"<<endl;
  mBemcPointsCut12->Fill(runnm, bEmcPoints.size());
  mTriggerSummaryCut1->Fill(4);
  mTriggerSummaryCut1->Fill(trigger);
	    

  if (ievtot%300==0) saveHistograms();
  
  
  // read EMC point list
  if (!readPointList())
  {
    if (debug) cout <<"+++  ERROR read readPointList()"<<endl;
    return kStOk;
  }


  // get high tower Et
  Float_t hiTowerEt = getHiTowerEt(emccol);
  if (mb)  mHiTowerEtSpectra_MB->Fill(hiTowerEt);
  if (ht1) mHiTowerEtSpectra_HT1->Fill(hiTowerEt);
  if (ht2) mHiTowerEtSpectra_HT2->Fill(hiTowerEt);
	    
 	  
  // look for track-point (TPC-BEMC) association
  if (mDoTracks) associateTracksWithEmcPoints(this);
  
  
  // check for point list	EVENTS CUTSS
  if (photonlist->GetEntries()>0 &&
      bEmcPoints.size()<=100 &&
      vPos.z()>-80 && vPos.z()<80)  //25 for AuAu
  {
    if (debug) cout<< "--- new photon " << photonlist->GetEntries() << endl;

    //if (trigger==0) // take Minbias events only for event-mixing
    fEventMixer->AddEvent(photonlist, photonlist, vPos.z(), bEmcPoints.size(), trigger);
   

    // event characteristics
    mBemcPointsCut2->Fill(bEmcPoints.size());
    mTriggerSummaryCut2->Fill(4);
    mTriggerSummaryCut2->Fill(trigger);
    mZvertexCut2->Fill(vPos.z());

    
    if (debug) cout<<"--- accepted event: trigger "<<trigger<< endl;
    getPhotonSpectra(photonlist, trigger, runnm, vPos.z(), bEmcPoints.size());
   
    getInvMass(0, photonlist, photonlist, trigger, vPos.z());     


    if (debug) 
      cout <<"++++++++++++++++++++++++++++++++++++++++++++++\n +++ event mixing procedure"<<endl;
    ievmix = 0;
    while (ievmix<fEventMixer->GetNumMixedEvent() && (fEventMixer->IsItReady()==1))
    {
      //cout <<"+++ event mixing: fEventMixer->GetNumMixedEvent() "<<fEventMixer->GetNumMixedEvent()<<endl;
	    
      startphoton1 = ievmix;
      startphoton2 = (fEventMixer->GetNumMixedEvent()-1)/2;
      
      if (debug) 
	cout <<"+++ event mixing: start "<<startphoton1<<"  "<<startphoton2<<endl;
      
      if (startphoton1 != startphoton2)
      {
	fEventMixer->GoToEvent(startphoton1, startphoton2);
	//ifEventMixer->GoToEvent(startphoton2, 1);
	mixedphoton1list = fEventMixer->GetPartList1();
	mixedphoton2list = fEventMixer->GetPartList2();
	
        getInvMass(1, mixedphoton1list, mixedphoton2list, trigger, vPos.z());

        // clear lists
	mixedphoton1list->Clear();
	mixedphoton2list->Clear();
      }
      ievmix++;     
    }
  }


  /*
  // reset list  
  for(ii=0; ii<photonlist->GetEntries(); ii++)
  {
    //delete (StEmcPoint *) photonlist->At(ii);
    delete photonlist->RemoveAt(ii);
  }
  */
  photonlist->Clear();
  pipluslist->Clear();
  if (debug) cout <<"--- reset photon/charged_pion list done"<<endl;

  return kStOK;
}

//________________________________________________________________________________________________________
Int_t StPi0InvMassMaker::Finish()
{
  saveHistograms();
  
  cout <<"\n ---------------------------------------" << endl;
  cout <<"\t Event Summary: "<< endl; 
  cout <<" ---------------------------------------" << endl;
  cout <<" total number of events:    "<< mEventSummary->GetBinContent(1) << endl;
  cout <<" accepted events:           "<< mEventSummary->GetBinContent(2) << endl;
  cout <<" # events after CUT SET 1   "<< mEventSummary->GetBinContent(3) <<"\n"<< endl;
  
  cout <<" no event pointer:          "<< mEventSummary->GetBinContent(4) << endl;
  cout <<" bad run:                   "<< mEventSummary->GetBinContent(5) << endl;
  cout <<" bad trigger:               "<< mEventSummary->GetBinContent(8) << endl;
  cout <<" no emc collection:         "<< mEventSummary->GetBinContent(6) << endl;
  cout <<" no primary vertex:         "<< mEventSummary->GetBinContent(7) <<"\n"<< endl;

  cout <<" z_vertex = 0:              "<< mEventSummary->GetBinContent(9) << endl;
  cout <<" bemc hits = 0:             "<< mEventSummary->GetBinContent(10) << endl;  
  
  Char_t output_name[40];
  sprintf(output_name,"finished_%s_%d_events.dat",mFileName,ievtot);
  FILE *output = fopen(output_name,"w");
  fclose(output);
  
  return kStOk; 
}

//________________________________________________________________________________________________________
void StPi0InvMassMaker::saveHistograms()
{
  cout <<"++++++++++ StPi0InvMassMaker::saveHistograms()"<<endl;
	
  TFile *hfile = (TFile*) gROOT->FindObject(mFileName); 
  if (hfile) hfile->Close();

  hfile = new TFile(mFileName,"RECREATE");
  //cout <<"TFile "<<hfile->GetName()<<" IsOpen = "<<(int) hfile->IsOpen()<< endl;

  mSMDSummary->Write();
  mEventSummary->Write();
  mPrimaryTracks->Write();
  mPrimaryTracksCut1->Write();
  mPrimaryTracks_BemcHits->Write();
  
  mTowerIdhit->Write();
  mTowerIdhit2->Write();
  mTowerIdTw->Write();
                                                                                                                        
  mZvertex->Write();
  mZvertexCut2->Write();

  mTriggerSummary->Write();
  mTriggerSummaryCut1->Write();
  mTriggerSummaryCut2->Write();
  mTriggerSummaryZeroBemc->Write();
  //mPointCluster->Write();
  mTrackPtHistMinBias->Write();
  mTrackPtHistHiTower->Write();
  mPointPtHistMinBias->Write();
  mPointPtHistHiTower->Write();
   
  mBemcPoints->Write(); 
  mBemcPointsCut1->Write();
  mBemcPointsCut12->Write();
  mBemcPointsCut2->Write();
  mBemcPointsAfterCuts2Photons->Write();
  
  mPhotonSpectra_MB->Write();
  mPhotonSpectra_HT1->Write();
  mPhotonSpectra_HT2->Write();
  mPhotonSpectraSel_MB->Write();
  mPhotonSpectraSel_HT1->Write();
  mPhotonSpectraSel_HT2->Write();
  mHiTowerEtSpectra_MB->Write();
  mHiTowerEtSpectra_HT1->Write();
  mHiTowerEtSpectra_HT2->Write();
  
  mPiZeroNtuple->Write();
  mPiZeroMixNtuple->Write();
  //mPointsNtuple->Write();
 
  for (Int_t i=1; i<=ntower; i++)
  {
    mInvMass[i]->Write();
  }

  hfile->Write();
  hfile->Close();
  cout <<"++++++++++ StPi0InvMassMaker::saveHistograms(): TFile "<< hfile->GetName() <<" is written on disk"<<endl;

  return;
}


//________________________________________________________________________________________________________
StThreeVectorF StPi0InvMassMaker::getPoint(StEmcPoint *p, Int_t &id, Float_t &e, Float_t &pt, Int_t &n, Int_t &t)
{
  StEvent *event = (StEvent*)this->GetInputDS("StEvent");
  if(!event)
  {
    cout << "++++++++++++ StPi0InvMassMaker::getPoint: Can't get Event pointer" << endl;
    return  StThreeVectorF(kStOk,0,0);
  }

  StPrimaryVertex* primaryVertex = event->primaryVertex(); 
  if(!primaryVertex)
  {
    cout << "++++++++++++ StPi0InvMassMaker::getPoint: Event w/o primary vertex" << endl;
    return StThreeVectorF(kStOk,0,0);
  }
  // get primary vertex
  StThreeVectorF MainVertexPosition;
  MainVertexPosition = primaryVertex->position();
  
    //cout <<" getpt 1 , main-vertex "<<MainVertexPosition<<"        WRONG for evmixing !!!"<<endl;
    //cout <<"p->position() "<<p->position()<<endl;
    
  //const StThreeVectorF& pointVector = p->position() - MainVertexPosition; 
  const StThreeVectorF& pointVector = p->position();
  
  // get tower id
  StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
  //emcGeom = StEmcGeom::instance("bemc");
  Int_t mod, eta, sub;
  emcGeom->getBin(pointVector.phi(), pointVector.pseudoRapidity(), mod, eta, sub);
  emcGeom->getId(mod, eta, sub, id);

  
  // get point energy
  e = p->energy();

  // get photon pT 
  Float_t theta = 0.;
  emcGeom->getTheta(mod, eta, theta);
  pt = e * sin(theta);
  
  // get point-track association
  n = p->nTracks();     

  // get point type 
  t=0;
  if (p->energyInDetector(StDetectorId(kBarrelEmcTowerId+2)))
    t++;
  if (p->energyInDetector(StDetectorId(kBarrelEmcTowerId+3)))
    t+=2;
  
  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::getPoint: tower, t, n, e, pt ---> "<<id<<" "<<t<<" "<<n<<" "<<e<<" "<<pt<< endl;
  
  return pointVector;
}


//________________________________________________________________________________________________________

Int_t StPi0InvMassMaker::doTrackPtHist(float eT, float threshold, TObjArray *photonlist)
{
  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::doTrackPtHist: entered"<<endl;

  StEvent *event = (StEvent*)this->GetInputDS("StEvent");
  if (!event)
  {
    cout << "StPi0InvMassMaker::doTrackPtHist: Can't get Event pointer" << endl;
    return kStOk;
  }

  // tracks
  StSPtrVecTrackNode& trackNodes = event->trackNodes();
  StTrack* track;
  StThreeVectorF trackMomentum;

  for (size_t nodeIndex=0; nodeIndex < trackNodes.size(); nodeIndex++)
  {
    size_t numberOfTracksInNode = trackNodes[nodeIndex]->entries(global);

    for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)
    {
      track = trackNodes[nodeIndex]->track(global,trackIndex);

      if (track && track->flag()>=0)
      {
	trackMomentum = track->geometry()->momentum();

	if (eT < threshold)
	  mTrackPtHistMinBias->Fill(trackMomentum.perp());
	else
	  mTrackPtHistHiTower->Fill(trackMomentum.perp());
      }
    }
  }

  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::doTrackPtHist: got tracks, try to get points"<<endl;

  // EMC points
  for (Int_t i=0; i<photonlist->GetEntries(); i++)
  {
    StEmcPoint *p = (StEmcPoint *) photonlist->At(i);
    const StThreeVectorF& pointVector = p->position();
  
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    //emcGeom = StEmcGeom::instance("bemc");
    Int_t mod, eta, sub;
    emcGeom->getBin(pointVector.phi(), pointVector.pseudoRapidity(), mod, eta, sub);
    //emcGeom->getId(mod, eta, sub, id);

    Float_t e = p->energy();

    // get photon pT
    Float_t theta = 0.;
    emcGeom->getTheta(mod, eta, theta);
    Float_t pt = e * sin(theta);
 
    if (eT < threshold)
      mPointPtHistMinBias->Fill(pt);
    else
      mPointPtHistHiTower->Fill(pt);
    }
    
  return kStOk;
}


//________________________________________________________________________________________________________
Float_t StPi0InvMassMaker::getHiTowerEt(StEmcCollection* emccol)
{
  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::getHiTowerEt: entered"<<endl;

  // Energy is converted to Et assuming z(vertex)=0
  // because this is how it's done in the trigger

  StEmcDetector* bemc = emccol->detector(kBarrelEmcTowerId);
  if (!bemc)
  {
    cout <<"++++++++++ StPi0InvMassMaker::getHiTowerEt(): No BEMC detector"<<endl;
    return 0;
  }

  StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
  Float_t hiTowerEt = 0;
  Float_t e, eT, theta;

  for (UInt_t i=1; i<=bemc->numberOfModules(); i++)
  {
    StEmcModule* module = bemc->module(i);
    StSPtrVecEmcRawHit& hits = module->hits();
    StSPtrVecEmcRawHitIterator hIter;
    
    for (hIter=hits.begin(); hIter!=hits.end(); hIter++)
    {
      //YYY
      e = (*hIter)->energy();
      emcGeom->getTheta((*hIter)->module(),(*hIter)->eta(), theta);
      eT = e * sin(theta);

      if (eT > hiTowerEt) hiTowerEt = eT;
    }
  } 
  return hiTowerEt;
}

//________________________________________________________________________________________________________
Int_t StPi0InvMassMaker::associateTracksWithEmcPoints(StMaker* anyMaker)
{
  // What it does:
  //
  // 1. Runs through all "good" global tracks. Each track is projected on BEMC using
  //    StEmcPosition::trackOnEmc(&pos, &mom, track, BFIELD);
  //
  // 2. Finds corresponding emc point
  //
  // 3. If such point exists, adds pointer to this track to the
  //    vector of tracks StPtrVecTrack (data member of StEmcPoint)

  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::associateTracksWithEmcPoints: ENTERED"<<endl;
	
  StEvent *event = (StEvent*)anyMaker->GetInputDS("StEvent");
  if (!event)
  {
    cout << "++++++++++++ StPi0InvMassMaker::associateTracksWithEmcPoints: Can't get Event pointer" << endl;
    return kStOk;
  }
  StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
  if (!emccol)
  {
    cout << "++++++++++++ StPi0InvMassMaker::associateTracksWithEmcPoints: No EMC Collection" << endl;
    return kStOk;
  }
  StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
  if (bEmcPoints.size()==0)
  {
    //cout << "++++++++++++ StPi0InvMassMaker::associateTracksWithEmcPoints: No BEMC points found" << endl;
    return kStOk;
  }

  // reset all existing track-to-point associations
  for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
    (*it)->track().clear(); 
  // deletes all elements
  // this is NOT a structural container, so the tracks don't get deleted


  Double_t bFld = 0.;
  StEventSummary* summary = event->summary();
  if (summary)
  {
    bFld = summary->magneticField()/10.; // bFld in Tesla
    if (debug) cout <<"***** StPi0InvMassMaker::associateTracksWithEmcPoints(): StEvent::summary()->magneticField() = "<<bFld<<" [Tesla]"<< endl;
  }
  if (fabs(bFld)<0.01)
  {
    cout << "***** StPi0InvMassMaker::associateTracksWithEmcPoints() finished : wrong mBField !" << endl;
    return kStWarn;
  }

  Int_t mod, eta, sub;
  StEmcPosition* pos = new StEmcPosition();
  StThreeVectorD position, momentum;
  StSPtrVecTrackNode& trackNodes = event->trackNodes();
  StTrack* track;
  StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
  
  for (size_t nodeIndex=0; nodeIndex<trackNodes.size(); nodeIndex++)
  {
    size_t numberOfTracksInNode = trackNodes[nodeIndex]->entries(global);
    
    for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)
    {
      track = trackNodes[nodeIndex]->track(global,trackIndex);

      if (track && track->flag()>=0)
      {
        Bool_t ok = pos->trackOnEmc(&position, &momentum, track, bFld); // bFld in Tesla
	if (!ok) continue;

	// get (mod, eta, sub) from track
	emcGeom->getBin(position.phi(), position.pseudoRapidity(), mod, eta, sub);

	for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
	{	    
	  StPtrVecEmcCluster bEmcClusters = (*it)->cluster(kBarrelEmcTowerId);

	  for (StPtrVecEmcClusterIterator cIter=bEmcClusters.begin(); cIter!=bEmcClusters.end(); cIter++)
	  {
	    StPtrVecEmcRawHit& bEmcHits = (*cIter)->hit();

	    for (StPtrVecEmcRawHitIterator hIter=bEmcHits.begin(); hIter!=bEmcHits.end(); hIter++)
            {
	      // compare (mod, eta, sub) with emc hit	    
	      if (mod == (Int_t)(*hIter)->module() && 
	          eta == (Int_t)(*hIter)->eta() && 
		  sub == (Int_t)(*hIter)->sub())
              {
	        (*it)->addTrack(track);
   	        break;
   	      }
            }
  	  }
        }

      }
    }
  }
  delete pos;
  return kStOk;
}


//________________________________________________________________________________________________________
void StPi0InvMassMaker::getPhotonSpectra(TObjArray *photonlist, float trigger, int runnm, float vertex_z, int bemc_hits)
{
  if (debug) cout <<"++++++++++ StPi0InvMassMaker::GetPhotonSpectra: entered"<<endl;
  
  Int_t cntb=0;
  Int_t id1, n1, t1;
  Float_t e1, pt1;
  
  Float_t pt1_trg=0;
  Int_t id1_trg=0;	  
  
  for (Int_t i=0; i<photonlist->GetEntries(); i++)
  {
    StEmcPoint *p1 = (StEmcPoint *) photonlist->At(i);
   
    // get EMC point information 
    StThreeVectorF v1 = getPoint(p1, id1,e1,pt1,n1,t1);
    //cout <<"--------------------> pt1 = " <<pt1<<endl;
    
    mTowerIdhit->Fill(id1);
    mTowerIdhit2->Fill(runnm,id1);

    /*
    if (n1==0) mPointsNtuple->Fill(trigger,
		                   e1,
		    	           t1,
			           id1);
    mPointsNtuple->Fill(vertex_z,
			trigger,
			runnm,
			e1,
			n1,
			t1,
			id1,
			v1.pseudoRapidity(),
			v1.phi());
    */
    
    if (n1==0)
    {
      if (pt1>pt1_trg)
      { 
	pt1_trg = pt1;
	id1_trg = id1;
      }
      
      //if (trigger==3 || trigger==0 || trigger==0.5 || trigger==2.5) mPhotonSpectra_MB->Fill(pt1);
      //if (trigger==3 || trigger==1 || trigger==0.5 || trigger==1.5) mPhotonSpectra_HT1->Fill(pt1);
      //if (trigger==3 || trigger==2 || trigger==1.5 || trigger==2.5) mPhotonSpectra_HT2->Fill(pt1);

      /*
      //XXX
      // selected tower CUTS
      if (gain[id1]>0.)
      {
	if (trigger==3 || trigger==0 || trigger==0.5 || trigger==2.5) mPhotonSpectraSel_MB->Fill(pt1);
	if (trigger==3 || trigger==1 || trigger==0.5 || trigger==1.5) mPhotonSpectraSel_HT1->Fill(pt1);
	if (trigger==3 || trigger==2 || trigger==1.5 || trigger==2.5) mPhotonSpectraSel_HT2->Fill(pt1);
      }
      */
      
      // count number of photons
      cntb++;
    }

    // get proton tower energy
    //if (n1==1) // >0 ???
      //mProtonEmcEnergy->Fill(e1); 
  }

  //
  if (pt1_trg>0)
  {
    if (trigger==3 || trigger==0 || trigger==0.5 || trigger==2.5) mPhotonSpectra_MB->Fill(pt1_trg);
    if (trigger==3 || trigger==1 || trigger==0.5 || trigger==1.5) mPhotonSpectra_HT1->Fill(pt1_trg);
    if (trigger==3 || trigger==2 || trigger==1.5 || trigger==2.5) mPhotonSpectra_HT2->Fill(pt1_trg);

    if (gain[id1_trg]>0.)
    {
      if (trigger==3 || trigger==0 || trigger==0.5 || trigger==2.5) mPhotonSpectraSel_MB->Fill(pt1_trg);
      if (trigger==3 || trigger==1 || trigger==0.5 || trigger==1.5) mPhotonSpectraSel_HT1->Fill(pt1_trg);
      if (trigger==3 || trigger==2 || trigger==1.5 || trigger==2.5) mPhotonSpectraSel_HT2->Fill(pt1_trg);
    }
  }
  
  if (cntb>=2) mBemcPointsAfterCuts2Photons->Fill(bemc_hits);
  return;
}

//________________________________________________________________________________________________________
void StPi0InvMassMaker::getInvMass(int mode, TObjArray *photonlist1, TObjArray *photonlist2, float trigger, float vertex_z)
{
  if (debug) cout <<"++++++++++ StPi0InvMassMaker::getInvMass: list1 "<<photonlist1->GetEntries()<<endl;
  if (debug) cout <<"++++++++++ StPi0InvMassMaker::getInvMass: list2 "<<photonlist2->GetEntries()<<endl;
   
  // 1st particle
  for (Int_t i=0; i<photonlist1->GetEntries(); i++)
  {
    if (debug) cout <<"++++++++++ StPi0InvMassMaker::getInvMass: first loop (mode "<<mode<<")"<<endl;
    StEmcPoint *p1 = (StEmcPoint *) photonlist1->At(i);

    Int_t id1, n1, t1;
    Float_t e1, pt1;
    StThreeVectorF v1 = getPoint(p1, id1,e1,pt1,n1,t1);
   

    // 2nd particle
    if (debug) cout <<"++++++++++ StPi0InvMassMaker::getInvMass: second loop (mode "<<mode<<")"<<endl; 

    Int_t j = 999;
    if (mode==0) j=i+1;
    if (mode==1) j=0;

    for (Int_t k=j; k<photonlist2->GetEntries(); k++)
    {
      StEmcPoint *p2 = (StEmcPoint *) photonlist2->At(k);
      
      Int_t id2, n2, t2;
      Float_t e2, pt2;
      StThreeVectorF v2 = getPoint(p2, id2,e2,pt2,n2,t2);
    

      // Pi0 CUTS 
      if (n1==0 && n2==0 && t1>-99 && t2>-99)
      {
	if (debug) cout <<"++++++++++ StPi0InvMassMaker::getInvMass: get invariant mass for photon pair"<<endl;

	double mInvPi0; 
        StThreeVectorF pPi0;
        Float_t asym;
        Float_t cosAng;
	Float_t phi1;
	Float_t phi2;
        getMass(v1,v2,e1,e2, mInvPi0,pPi0,asym,cosAng, phi1,phi2);
    
	
	// get Pi0 minv distribution for a certain set of cuts
	if (mode==0 && t1>0 && t2>0 && n1==0 && n2==0 && pPi0.perp()>1.5 && asym<0.5)
	  getInvMassTowerwise(id1, id2, e1, e2, mInvPi0);


        // selected tower CUTS 
	//cout <<"++++++ gain:  "<<gain[id1]<<"  "<<gain[id2]<<endl;
	if (gain[id1]>0. && gain[id2]>0.)
	if (asym<0.5)
        {	
          Double_t corr_factor = TMath::Sqrt(gain[id1] * gain[id2]);
	  //cout <<"++++++ gain of accepted towers:  "<<gain[id1]<<"  "<<gain[id2]<<endl;

	  if (mode==0) mPiZeroNtuple->Fill(pPi0.perp(),
				           corr_factor*mInvPi0);
	
	  if (mode==1) mPiZeroMixNtuple->Fill(pPi0.perp(),
				              corr_factor*mInvPi0);
	}		
      } // quality cuts 

    } // 2nd particle
  } // 1st particle

  return;
}


//________________________________________________________________________________________________________
Bool_t StPi0InvMassMaker::getMass(StThreeVectorF P1, StThreeVectorF P2, Float_t e1, Float_t e2, double &mInv, StThreeVectorF &pPi0, Float_t &asym, Float_t &cosAng, Float_t &phi1, Float_t &phi2)
{
  if (debug) cout << "++++++++++++ StPi0InvMassMaker::getMass(): enter pi0 minv calcualtion..." << endl;
  
  Double_t VecProd = P1.x()*P2.x() + P1.y()*P2.y() + P1.z()*P2.z();
  cosAng = VecProd/(P1.magnitude()*P2.magnitude());

  double mInv2 = 2*e1*e2*(1-cosAng);      
  if (mInv2>0) 
  {
    mInv = TMath::Sqrt(mInv2);
  }
  else 
    mInv = -999.0;
  
  asym = fabs(e1-e2)/(e1+e2);

  // for correlation studies
  phi1 = P1.phi();
  phi2 = P2.phi();

  pPi0 = e1*P1/P1.mag() + e2*P2/P2.mag();
  
  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::getMass(): mInv, asym, pPi0: "<<mInv<<" "<<asym<<" "<<pPi0<<endl;
  
  return kTRUE;
}      


//________________________________________________________________________________________________________
void StPi0InvMassMaker::getInvMassTowerwise(Int_t id1, Int_t id2, Float_t e1, Float_t e2, double mInv)
{ 
  if (debug) cout << "++++++++++++ StPi0InvMassMaker::getInvMassTowerwise(): get minv for each tower" << endl;

  Int_t id=0;
  Float_t e=0.;

  // to avoid fake towers
  if (id1>0 && id1<=ntower && id2>0 && id2<=ntower)
  {
    if (e1>e2) 
    {
      id=id1;
      e=e1;
    }
    else if (e2>=e1) 
    {
      id=id2;
      e=e2;
    }
    else 
      cout <<"++++++++++++ StPi0InvMassMaker::getInvMassTowerwise(): mismatch photon energy ???"<<endl;
    
    if (debug) 
      cout <<"++++++++++++ StPi0InvMassMaker::getInvMassTowerwise(): after selection...id/e = "<<id<<" "<<e<<endl;
    
    mTowerIdTw->Fill(id);
    mInvMass[id]->Fill(mInv);
  }
  else
    cout <<"++++++++++++ StPi0InvMassMaker::getInvMassTowerwise(): id<=0 or id>2400 ?!"<<endl;
	  
  return;
}


//________________________________________________________________________________________________________
void StPi0InvMassMaker::getTowerHitInfo()
{
  StEvent *event = (StEvent*)this->GetInputDS("StEvent");
  if (!event)
  {
    cout << "++++++++++++ StPi0InvMassMaker::getTowerHitInfo(): Can't get Event pointer" << endl;
    return;
  }
  StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
  if (!emccol)
  {
    cout << "++++++++++++ StPi0InvMassMaker::getTowerHitInfo():  No EMC Collection" << endl;
    return;
  }
  StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
  if (bEmcPoints.size()==0)
  {
    //cout << "++++++++++++ StPi0InvMassMaker::getTowerHitInfo():  No BEMC points found" << endl;
    return;
  }

  StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");

  // for(Int_t det=0; det<4; det++) det=0
  StDetectorId id = static_cast<StDetectorId>(0+kBarrelEmcTowerId);
  StEmcDetector* detector = emccol->detector(id);
  if (detector)
  {
    for (UInt_t j=1; j<=120; j++)
    {
      StEmcModule* module = detector->module(j);
      if (module)
      {
	StSPtrVecEmcRawHit& rawHit = module->hits();
	for (Int_t k=0; k<(Int_t)rawHit.size(); k++)
	{
	  int tid;
	  emcGeom->getId(rawHit[k]->module(),rawHit[k]->eta(),rawHit[k]->sub(),tid);
	}
      }
    }
  } 
  else 
    cout << "++++++++++ StPi0InvMassMaker::getTowerHitInfo(): no detector" << endl;
  
  return;
}

//________________________________________________________________________________________________________
Bool_t StPi0InvMassMaker::readPointList()
{
  StEvent *event = (StEvent*)this->GetInputDS("StEvent");
  if (!event)
  {
    cout << "++++++++++++ StPi0InvMassMaker::readPointList(): Can't get Event pointer" << endl;
    return kStWarn;
  }
  StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
  if (!emccol)
  {
    cout << "++++++++++++ StPi0InvMassMaker::readPointList():  No EMC Collection" << endl;
    return kStOk;
  }
  StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
  if (bEmcPoints.size()==0)
  {
    //cout << "++++++++++++ StPi0InvMassMaker::readPointList():  No BEMC points found" << endl;
    return kStOk;
  }
  if (debug) cout << "++++++++++++ StPi0InvMassMaker::readPointList(): Number of BEMC points: " << bEmcPoints.size() << endl;

  StEmcPoint* point=0;
  for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
  {
    point = *it;
    photonlist->Add(point);
  }

  if (debug) cout <<"++++++++++++ StPi0InvMassMaker::readPointList(): StEmcPoint point list: "<< photonlist->GetEntries() << endl;
  //cout <<"++++++++++++ StPi0InvMassMaker::readPointList(): StEmcPoint dummy list: "<< dummylist->GetEntries() << endl;

  if (photonlist->GetEntries()<=0)
  {
    photonlist->Clear(); 
    return kStOk;    
  }
  return true;
}


//________________________________________________________________________________________________________
void StPi0InvMassMaker::readBadRunList()
{
  // bad run list, see remarks below
  /******************Y4MBFF**************/
  badrunid[0] = 4033025;
  badrunid[1] = 4034002;
  badrunid[2] = 4034004;
  badrunid[3] = 4034005;//these runs are surely bad
  badrunid[4] = 4034010;
  badrunid[5] = 4034012;
  badrunid[6] = 4034013;
  badrunid[7] = 4034015;
  /************************************/
 
  /******************Y4MBRFF*************/
  badrunid[8] = 4020015;
  badrunid[9] = 4054041;
  badrunid[10] = 4054042;
  badrunid[11] = 4055014;
  badrunid[12] = 4056011;
  badrunid[13] = 4056012;
  badrunid[14] = 4056013;//these runs are surely bad
  badrunid[15] = 4059013;
  badrunid[16] = 4059014;
  badrunid[17] = 4059024;
  badrunid[18] = 4059025;
  badrunid[19] = 4059026;
  /************************************/
  
  /****************Y4CBRFF**************/
  badrunid[20] = 4054002;
  badrunid[21] = 4054003;
  badrunid[22] = 4054010;
  badrunid[23] = 4054012;
  badrunid[24] = 4054013;
  badrunid[25] = 4054014;
  badrunid[26] = 4054015;
  badrunid[27] = 4054016;
  badrunid[28] = 4054021;
  badrunid[29] = 4054022;
  badrunid[30] = 4054029;
  badrunid[31] = 4054030;
  badrunid[32] = 4054031;
  badrunid[33] = 4054032;
  badrunid[34] = 4054033;
  badrunid[35] = 4054036;
  badrunid[36] = 4054037;
  badrunid[37] = 4054038;
  badrunid[38] = 4054039;
  badrunid[39] = 4054056;
  badrunid[40] = 4054057;
  badrunid[41] = 4054058;
  badrunid[42] = 4054059;
  badrunid[43] = 4054060;
  badrunid[44] = 4054073;
  badrunid[45] = 4055002;
  badrunid[46] = 4055005;
  badrunid[47] = 4055006;
  badrunid[48] = 4055007;
  badrunid[49] = 4055008;
  badrunid[50] = 4055009;
  badrunid[51] = 4055010;
  badrunid[52] = 4055012;
  badrunid[53] = 4055013;
  badrunid[54] = 4055015;
  badrunid[55] = 4055016;
  badrunid[56] = 4055017;
  badrunid[57] = 4055018;
  badrunid[58] = 4055019;
  badrunid[59] = 4055020;
  badrunid[60] = 4055021;
  badrunid[61] = 4055022;
  badrunid[62] = 4055023;
  badrunid[63] = 4056002;
  badrunid[64] = 4056003;
  badrunid[65] = 4056004;
  badrunid[66] = 4056008;
  badrunid[67] = 4056009;
  badrunid[68] = 4056010;
  badrunid[69] = 4056014;
  badrunid[70] = 4056015;
  badrunid[71] = 4056016;
  badrunid[72] = 4056017;
  badrunid[73] = 4056018;
  badrunid[74] = 4056019;
  badrunid[75] = 4056020;
  badrunid[76] = 4056021;
  badrunid[77] = 4056027;
  badrunid[78] = 4056033;
  badrunid[79] = 4059012;
 
  badrunid[80] = 4060057;
  badrunid[81] = 4060091;
  badrunid[82] = 4062056;
  badrunid[83] = 4063040;
  
  /**********************************/
  badrunid[84] = 4044026;//these runs have some problem
  badrunid[85] = 4044027;
  badrunid[86] = 4044028;
  badrunid[87] = 4044029;
  badrunid[88] = 4044031;
  badrunid[89] = 4044032;
  badrunid[90] = 4044033;
  badrunid[91] = 4044034;
  badrunid[92] = 4044035;
  badrunid[93] = 4044036;
  badrunid[94] = 4045007;
  badrunid[95] = 4045008;
  badrunid[96] = 4045009;
  badrunid[97] = 4045010;
  badrunid[98] = 4045011;
  badrunid[99] = 4045012;
  badrunid[100] = 4045014;
  badrunid[101] = 4045015;
  badrunid[102] = 4045017;
  badrunid[103] = 4054003;
  badrunid[104] = 4054010;
  badrunid[105] = 4054012;
  badrunid[106] = 4054014;
  badrunid[107] = 4054015;
  badrunid[108] = 4054030;
  badrunid[109] = 4054033;
  badrunid[110] = 4054037;
  badrunid[111] = 4054038;
  badrunid[112] = 4054056;
  badrunid[113] = 4054057;
  badrunid[114] = 4054060;
  badrunid[115] = 4055002;
  badrunid[116] = 4055005;
  badrunid[117] = 4055007;
  badrunid[118] = 4055008;
  badrunid[119] = 4055009;
  badrunid[120] = 4055010;
  badrunid[121] = 4055012;
  badrunid[122] = 4055015;
  badrunid[123] = 4055016;
  badrunid[124] = 4055018;
  badrunid[125] = 4060057;
  badrunid[126] = 4062054;
  badrunid[127] = 4056008;
  
  /**************Y4MBRFF***************/
  badrunid[128] = 4020036;
  badrunid[129] = 4022061;
  badrunid[130] = 4022100;
  badrunid[131] = 4021016;
  badrunid[132] = 4022055;//these runs have problems
  badrunid[133] = 4020043;
  badrunid[134] = 4020044;
  badrunid[135] = 4019023;
  badrunid[136] = 4020016;
  /***********************************/
  //my List
  badrunid[137] = 4066058;
  badrunid[138] = 4049002;   //bad due to const. number of BEMC hits
  badrunid[139] = 4049003;
  badrunid[140] = 4049004;
  //badrunid[141] = 4065003;
  
  return;
}

