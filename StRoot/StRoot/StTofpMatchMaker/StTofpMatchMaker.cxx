/***************************************************************************
 *
 * $Id: StTofpMatchMaker.cxx,v 1.15 2018/02/26 23:26:52 smirnovd Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: Match Maker for TOFp detector
 *             
 **************************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "StTofUtil/StTofGeometry.h"
#include "StTofUtil/StTofSlatCollection.h"
#include "tables/St_pvpdStrobeDef_Table.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StTofUtil/tofPathLength.hh"
#include "StTofpMatchMaker.h"



//---------------------------------------------------------------------------
/// default constructor, set default values
StTofpMatchMaker::StTofpMatchMaker(const Char_t *name): StMaker(name){
  // zero counters & pointers
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mTofStrobeEventCounter = 0;
  mAcceptAndStrobe = 0;
  mAcceptAndBeam = 0;
  mTofGeom = 0;

  // set default values
  setValidAdcRange(0,1024);
  setValidTdcRange(30,1200);
  setOuterTrackGeometry();
  setMinHitsPerTrack(0);
  setMinFitPointsPerTrack(0);
  setMaxDCA(9999.);

  createHistograms(kTRUE);
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

StTofpMatchMaker::~StTofpMatchMaker(){ /* nope */}

void StTofpMatchMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
/// Init:  inform user of parameter settings and book histograms
Int_t StTofpMatchMaker::Init(){
  gMessMgr->Info("StTofpMatchMaker -- initializing ...","OS");
  gMessMgr->Info("","OST") << "Valid TDC range: " << mMinValidTdc << " " << mMaxValidTdc << endm;
  gMessMgr->Info("","OST") << "Valid ADC range: " << mMinValidAdc << " " << mMaxValidAdc << endm;
  gMessMgr->Info("","OST") << "Minimum hits per track: " << mMinHitsPerTrack << endm;
  gMessMgr->Info("","OST") << "Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
  gMessMgr->Info("","OST") << "Maximum DCA: " << mMaxDCA << endm;
  if (!mOuterTrackGeometry)
    gMessMgr->Warning("Warning: using standard trackgeometry()","OST");

  if(m_Mode) {
    setHistoFileName("tofana.root");
  } else {
    setHistoFileName("");
  }

  if (mHisto){
    bookHistograms();
    gMessMgr->Info("Histograms are booked","OST");
    if (mHistoFileName!="")
        gMessMgr->Info("","OST") << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
  }

  // reset event counters
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mTofStrobeEventCounter = 0;
  mAcceptAndStrobe = 0;
  mAcceptAndBeam = 0;

  return kStOK;
}


//---------------------------------------------------------------------------
/// InitRun: (re-)initialize the tofp geometry
Int_t StTofpMatchMaker::InitRun(int runnumber){

  // determine TOF configuration from run#
  mYear2 = (runnumber<4000000);
  mYear3 = (runnumber>4000000&&runnumber<5000000);
  mYear4 = (runnumber>5000000&&runnumber<6000000);
  mYear5 = (runnumber>6000000);

  gMessMgr->Info("StTofpMatchMaker -- reinitializing TofGeometry (InitRun)","OS" );
  mTofGeom = new StTofGeometry();
  mTofGeom->init(this);

  gMessMgr->Info("                 -- retrieving run parameters","OS");
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/pvpdStrobeDef");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    //    assert(mDbDataSet);
    return kStErr;
  }
  St_pvpdStrobeDef* pvpdStrobeDef = static_cast<St_pvpdStrobeDef*>(mDbDataSet->Find("pvpdStrobeDef"));
  if (!pvpdStrobeDef){
    gMessMgr->Error("unable to find TOF run param table","OS");
    //    assert(pvpdStrobeDef);
    return kStErr;
  }
  pvpdStrobeDef_st *strobeDef = static_cast<pvpdStrobeDef_st*>(pvpdStrobeDef->GetArray());
  int numRows = pvpdStrobeDef->GetNRows();
  if (NPVPD != numRows) gMessMgr->Warning("#tubes inconsistency in dbase");
  for (int i=0;i<NPVPD;i++){
    int ii = strobeDef[i].id - 1;
    mStrobeTdcMin[ii] = strobeDef[i].strobeTdcMin;
    mStrobeTdcMax[ii] = strobeDef[i].strobeTdcMax;
    if (Debug())
      LOG_INFO << "tube " << strobeDef[i].id << "  min:"<< strobeDef[i].strobeTdcMin
	   <<" max:"<< strobeDef[i].strobeTdcMax<< endm;
  }



  return kStOK;
}

/// FinishRun: clean up tofp geometry
Int_t StTofpMatchMaker::FinishRun(int runnumber){
  gMessMgr->Info("StTofpMatchMaker -- cleaning up geometry (FinishRun)","OS" );
  if (mTofGeom) delete mTofGeom;
  mTofGeom=0;
  return kStOK;
}


//---------------------------------------------------------------------------
/// Finish: dump usage statistics and write histograms to file
Int_t StTofpMatchMaker::Finish(){
   gMessMgr->Info("","OS") << "StTofpMatchMaker -----  RUN SUMMARY ----- (Finish)\n"
       << "\tProcessed "  << mEventCounter << " events."
       << " Accepted  "   << mAcceptedEventCounter << " events."
       << " Rejected  "   << mEventCounter - mAcceptedEventCounter << " events\n"
       << "\tTOF events " << mTofEventCounter
       << ". Beam "       << mTofEventCounter - mTofStrobeEventCounter
       << "  Strobe "     << mTofStrobeEventCounter
       << "\n\t Accept & Strobe " << mAcceptAndStrobe << " events\n"
       << "\t Accept & Beam   "   << mAcceptAndBeam   << " events" << endm;
  
  if (mHistoFileName!="") writeHistogramsToFile();
  return kStOK;
}


//---------------------------------------------------------------------------
/// Make: match extrapolated TPC tracks to TOFp slats
Int_t StTofpMatchMaker::Make(){
  gMessMgr->Info("StTofpMatchMaker -- welcome","OS");

  if(mYear5) {
    gMessMgr->Info("StTofpMatchMaker -- no TOFp in and after Run 5","OS");
    return kStOK;
  }

  // event selection ...
  StEvent *event = (StEvent *) GetInputDS("StEvent");
  if (!validEvent(event)){
    gMessMgr->Info("StTofpMatchMaker -- nothing to do ... bye-bye","OST");
    return kStOK;
  }

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StTofCollection *theTof = event->tofCollection();
  getTofData(theTof);

  //.........................................................................
  // update TOFp slat counters
  float sumAdcTofp(0.); int nAdcTofp(0), nTdcTofp(0), nAdcTdcTofp(0);
  for (int i=0;i<NTOFP;i++){
    sumAdcTofp += mTofpAdc[i];
    bool tdcValid = validTdc(mTofpTdc[i]);
    bool adcValid = validAdc(mTofpAdc[i]);
    if (tdcValid) nTdcTofp++;
    if (adcValid) nAdcTofp++;
    if (adcValid && tdcValid) nAdcTdcTofp++;
  }
  if (Debug())
    LOG_INFO << " TOFp #Adc:" << nAdcTofp << "   #Tdc:" << nTdcTofp << endm;

  // update pVPD tubes counters
  float sumAdcPvpd=0; int nAdcPvpd=0, nTdcPvpd=0;
  for (int i=0;i<NPVPD;i++){
    sumAdcPvpd += mPvpdAdc[i];
    bool tdcValid = validTdc(mPvpdTdc[i]);
    bool adcValid = validAdc(mPvpdAdc[i]);
    if (tdcValid) nTdcPvpd++;
    if (adcValid) nAdcPvpd++;
  }
  if (Debug())
    LOG_INFO << " pVPD #Adc:" << nAdcPvpd << "   #Tdc:" << nTdcPvpd << endm;


    
  if (Debug()){
    // number of primary tracks
    //  (note: different meaning in event.root and richtof.root)
    int refmult(0);
    bool richTofMuDST = (event->summary()->numberOfExoticTracks() == -999);
    if (richTofMuDST)
      refmult = event->summary()->numberOfGoodTracks();
    else
      refmult = uncorrectedNumberOfPrimaries(*event);

    LOG_INFO << " #Tracks           :"      << event->summary()->numberOfTracks()
	 << "\n #goodPrimaryTracks:"    << event->summary()->numberOfGoodPrimaryTracks()
	 << "\n #uncorr.prim.tracks  :" << refmult << endm;
    if (!richTofMuDST)
      LOG_INFO << " #goodTracks (global):"  << event->summary()->numberOfGoodTracks() << endm;
  }



  //.........................................................................
  // A. build vector of candidate slats with valid ADC signals 
  idVector validSlatIdVec;
  for (int i=0;i<NTOFP;i++){
    unsigned short slatId = mTofGeom->daqToSlatId(i);
    float rawAdc = mTofpAdc[i];
    float rawTdc = mTofpTdc[i];
    if (mHisto) hTofpSlatIdA0->Fill(i+1);

    if (validAdc(rawAdc) && validTdc(rawTdc)){
      validSlatIdVec.push_back(slatId);
      if (mHisto) hTofpSlatIdA1->Fill(i+1);
    }
  }
  gMessMgr->Info("","OST") << "A: #valid slats: " << validSlatIdVec.size() << endm;
  // end of Sect.A
  if(!validSlatIdVec.size()) return kStOK;

  //.........................................................................
  // B. loop over global tracks and determine all slat-track matches
  //
  tofSlatHitVector allSlatsHitVec;
  allSlatsHitVec.clear();
  StSPtrVecTrackNode& nodes = event->trackNodes();
  int nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofSlatHitVector slatHitVec;
    slatHitVec.clear();
    StTrack *theTrack = nodes[iNode]->track(global);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();

      idVector projTrayVec;
      if(!mTofGeom->projTrayVector(theHelix, projTrayVec)) continue;

      Bool_t hitTofp = kFALSE;
      for(size_t ii = 0; ii<projTrayVec.size(); ii++) {
        if(projTrayVec[ii]==mTofpTrayId) hitTofp = kTRUE;
      }
      if(!hitTofp) continue;

      slatHitVec = mTofGeom->tofHelixToArray(theHelix, validSlatIdVec);
      if (slatHitVec.size()>0 && mHisto) hTofpSlatHitVecSize->Fill(slatHitVec.size());

      for (size_t ii = 0; ii < slatHitVec.size(); ii++) {
	slatHitVec[ii].trackIdVec.push_back(iNode);
	allSlatsHitVec.push_back(slatHitVec[ii]);
	if (mHisto){
	  int id = mTofGeom->slatIdToDaq(slatHitVec[ii].slatIndex);
	  float xhit = slatHitVec[ii].hitPosition.x();
	  float yhit = slatHitVec[ii].hitPosition.y();
	  float zhit = slatHitVec[ii].hitPosition.z();
	  float phihit = atan2(yhit,xhit);
	  hTofpSlatIdB1->Fill(id);
	  hTofpHitMap1->Fill(zhit,phihit);
	}

	if (Debug()){ 
	  LOG_INFO << "B: trackid=";
	  idVectorIter ij = slatHitVec[ii].trackIdVec.begin();
	  while (ij != slatHitVec[ii].trackIdVec.end()) {LOG_INFO << " " << *ij; ij++;}
	  LOG_INFO << "\tind=" << mTofGeom->slatIdToDaq(slatHitVec[ii].slatIndex)
	       << "\thitprof="<< slatHitVec[ii].hitProfile 
	       << "\ts="<<slatHitVec[ii].s << "\tthxy="<<slatHitVec[ii].theta_xy 
	       << "\tthzr="<<slatHitVec[ii].theta_zr;
	  if (slatHitVec.size()>1) LOG_INFO << " M" << endm;
	  else LOG_INFO << endm;
	}
      }
    } // existing global track
  } // loop over nodes

  gMessMgr->Info("","OST") << "B: #matched/#avail/#total tracknodes: " 
			   <<allSlatsHitVec.size() << "/" <<nAllTracks 
			   << "/" << nodes.size() << endm;
  // end of Sect.B
  if(!allSlatsHitVec.size()) return kStOK;

  //.........................................................................
  // C Neighbours -- identify crosstalk, geometry shifts (only fill histograms)
  //
  if (mHisto){
    for (tofSlatHitVectorIter ij = allSlatsHitVec.begin();ij!=allSlatsHitVec.end(); ij++){
      int slatId=ij->slatIndex;
      int daqId= mTofGeom->slatIdToDaq(slatId);
      
      idVector slatNeighbours = mTofGeom->slatNeighboursWide(slatId);
      idVector slatCloseNeighbours = mTofGeom->slatNeighbours(slatId);

      // loop over neighbour candidates
      bool matchedNeighbours(false);
      for (idVectorIter kk=slatNeighbours.begin();kk!=slatNeighbours.end();kk++){
	// is the neighbour matched to a slat ...
	for (tofSlatHitVectorIter jj = allSlatsHitVec.begin();jj!=allSlatsHitVec.end();jj++){
	  if (jj->slatIndex == *kk) matchedNeighbours=true;
	}
      }
      
      // scan for valid TDCs in case no matched neighbours were found
      if (!matchedNeighbours){
	bool tdcOK = validTdc(mTofpTdc[daqId-1]);
	int nHitNeighbours(0),nNoHitNeighbours(0);
	
	for (idVectorIter kk=slatCloseNeighbours.begin();kk!=slatCloseNeighbours.end();kk++){
	  int neighborDaqId=mTofGeom->slatIdToDaq(*kk);
	  int neighborSlatId=*kk;
	  if (validTdc(mTofpTdc[neighborDaqId-1])){
	    int iEta = mTofGeom->tofSlat(neighborSlatId).ieta - mTofGeom->tofSlat(slatId).ieta;
	    int iPhi = mTofGeom->tofSlat(neighborSlatId).iphi - mTofGeom->tofSlat(slatId).iphi;


	    if (tdcOK){
	      hTofpMatchHit[daqId-1]->Fill(iEta,iPhi);
	      nHitNeighbours++;	    
	    }
	    else {
	      hTofpMatchNoHit[daqId-1]->Fill(iEta,iPhi);
	      nNoHitNeighbours++;
	    }
	  }
	} // loop over close slat neighbours

	if (tdcOK){
	  hTofpMatchHit[daqId-1]->Fill(0.,0.,nHitNeighbours);
	}
	else{
	  hTofpMatchNoHit[daqId-1]->Fill(0.,0.,nNoHitNeighbours);
	}
	
      } // no matched neighbrs
    }
  }
  // end of Sect.C



  //.........................................................................
  // D. sort hit vectors  and deal with (discard) slats matched by multiple tracks
  //
  int nSingleHitSlats(0);
  tofSlatHitVector singleHitSlatsVec;
  StructSlatHit slatHit;

  tofSlatHitVector tempVec = allSlatsHitVec;
  tofSlatHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    int nTracks = 0;
    vector<StThreeVectorD> vPosition;
    vector<vector<StThreeVectorD> > vLayerHitPositions;
    vector<Int_t> vHitProfile;
    vector<Float_t> vS, vTheta_xy, vTheta_zr;
    idVector trackIdVec;

    tofSlatHitVectorIter tempIter=tempVec.begin();
    tofSlatHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->slatIndex == erasedIter->slatIndex) {
	nTracks++;
	// save all hit data in temporary vectors
	trackIdVec.push_back(erasedIter->trackIdVec.back());
	vPosition.push_back(erasedIter->hitPosition);
	vLayerHitPositions.push_back(erasedIter->layerHitPositions);
	vHitProfile.push_back(erasedIter->hitProfile);
	vS.push_back(erasedIter->s);
	vTheta_xy.push_back(erasedIter->theta_xy);
	vTheta_zr.push_back(erasedIter->theta_zr);

	if (mHisto){
	 float xhit = erasedIter->hitPosition.x();
	 float yhit = erasedIter->hitPosition.y();
	 float zhit = erasedIter->hitPosition.z();
	 float phihit = atan2(yhit,xhit);
	 hTofpHitMap2->Fill(zhit,phihit);
	}

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    if (mHisto){
      int daqId = mTofGeom->slatIdToDaq(tempIter->slatIndex);
      hTofpSlatIdD1->Fill(daqId);
    }

    if (nTracks==1){
      nSingleHitSlats++;
      // for singly hit slat, copy data in singleHitSlatsVec
      slatHit.slatIndex = tempIter->slatIndex;
      slatHit.hitPosition = vPosition[0];
      slatHit.layerHitPositions = vLayerHitPositions[0];
      slatHit.trackIdVec = trackIdVec;
      slatHit.hitProfile =  vHitProfile[0];
      slatHit.s    =  vS[0];
      slatHit.theta_xy =  vTheta_xy[0];
      slatHit.theta_zr =  vTheta_zr[0];

      singleHitSlatsVec.push_back(slatHit);

      if (mHisto){
	int daqId = mTofGeom->slatIdToDaq(tempIter->slatIndex);
	float xhit = slatHit.hitPosition.x();
	float yhit = slatHit.hitPosition.y();
	float zhit = slatHit.hitPosition.z();
	float phihit = atan2(yhit,xhit);
	hTofpSlatIdD2->Fill(daqId);
	hTofpHitMap3->Fill(zhit,phihit);
      }

      // debugging output
      if (Debug()) {
	LOG_INFO << "D: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	     << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	     << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:";
	idVectorIter ij=trackIdVec.begin();
	while (ij != trackIdVec.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO <<endm;
      }
    }
    else if (nTracks>1){
      // for multiple hit slats either discard (yes) or
      // find the most likely candidate.
    } else
       gMessMgr->Warning("","OST")  << "D: no tracks extrapolate to matched slat ... should not happen!" << endm;

    tempVec = erasedVec;
  }
   gMessMgr->Info("","OST") << "D: #before/#after: " << allSlatsHitVec.size() 
			    << "/" << singleHitSlatsVec.size() << endm;
  //end of Sect.D



  //.........................................................................
  // E. sort and deal singleHitSlatsVector for multiple slats associated to single tracks
  //
  tofSlatHitVector allMatchedSlatsVec;
  tempVec = singleHitSlatsVec;
  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructSlatHit slatHit;
    int nSlats = 0;
    vector<StThreeVectorD> vPosition;
    vector< vector<StThreeVectorD> > vLayerHitPositions;
    vector<Int_t> vHitProfile;
    vector<Float_t> vS, vTheta_xy, vTheta_zr;
    idVector vTrackId;
    vector<Int_t> slatIndex;

    tofSlatHitVectorIter tempIter=tempVec.begin();
    tofSlatHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nSlats++;
	// save all hit data in temporary vectors
	slatIndex.push_back(erasedIter->slatIndex);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vPosition.push_back(erasedIter->hitPosition);
	vLayerHitPositions.push_back(erasedIter->layerHitPositions);
	vHitProfile.push_back(erasedIter->hitProfile);
	vS.push_back(erasedIter->s);
	vTheta_xy.push_back(erasedIter->theta_xy);
	vTheta_zr.push_back(erasedIter->theta_zr);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    
    if (nSlats==1){
      // for singly hit slat, copy data in singleHitSlatsVec
      slatHit.slatIndex = slatIndex[0];
      slatHit.hitPosition = vPosition[0];
      slatHit.layerHitPositions = vLayerHitPositions[0];
      slatHit.trackIdVec.push_back(vTrackId[0]);
      slatHit.hitProfile =  vHitProfile[0];
      slatHit.s    =  vS[0];
      slatHit.theta_xy =  vTheta_xy[0];
      slatHit.theta_zr =  vTheta_zr[0];
      slatHit.matchFlag = 0; 

      allMatchedSlatsVec.push_back(slatHit);

      if (mHisto){
	int daqId = mTofGeom->slatIdToDaq(slatIndex[0]);
	hTofpSlatIdE1->Fill(daqId);
      }

      // debugging output
      if (Debug()) {
	LOG_INFO << "E: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	     << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	     << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO <<endm;
      }
    }
    else if (nSlats>1){   // for multiple hit slats  find the most likely candidate.
      int thiscandidate(-99);
      int thisMatchFlag(0);

      // 1. sort on hitprofile weight
      int weight(0);
      vector<int> weightCandidates;
      thisMatchFlag = 1;
      if (Debug()) LOG_INFO << "E: find ... weight ";
      for (int i=0;i<nSlats;i++){
	int hitWeight = vLayerHitPositions[i].size();
	if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[i]) << "("<<hitWeight<<")"<<" ";
	if (hitWeight>weight) {
	  weight=hitWeight;
	  weightCandidates.clear();
	  weightCandidates.push_back(i);
	} else if (hitWeight == weight)
	  weightCandidates.push_back(i);
      }
      if (weightCandidates.size()==1){
	thiscandidate = weightCandidates[0];
	int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	if (mHisto) hTofpSlatIdE2->Fill(daqId);
	if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
      }

      // 2. if still undecided check on ss
      if (weightCandidates.size()>1){
	Float_t ss(0);
	vector<int> ssCandidates;
	thisMatchFlag = 2;
	if (Debug()) LOG_INFO << " ss ";
	for (unsigned int i=0;i<weightCandidates.size();i++){
	  int ii=weightCandidates[i];	  
	  if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	  if (vS[ii]>ss){
	    ss = vS[ii];
	    ssCandidates.clear();
	    ssCandidates.push_back(ii);
	  }else if  (vS[ii]==ss)
	    ssCandidates.push_back(ii);	  
	}
	if (ssCandidates.size()==1){
	  thiscandidate = ssCandidates[0];
	  int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	  if (mHisto) hTofpSlatIdE3->Fill(daqId);
    	  if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
	}

	// 3. if still undecided go for closest/first hit
	if (ssCandidates.size()>1){
	  Int_t hitprof(0);
	  vector<int> profileCandidates;
	  thisMatchFlag = 3;
	  if (Debug()) LOG_INFO << " hprof ";
	  for (unsigned int i=0;i<ssCandidates.size();i++){
	    int ii=ssCandidates[i];
	    if (Debug()) LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	    if (vHitProfile[ii]>hitprof){
	      hitprof = vHitProfile[ii];
	      profileCandidates.clear();
	      profileCandidates.push_back(ii);
	  }else if  (vHitProfile[ii]==hitprof)
	    profileCandidates.push_back(ii);	  
	  } 
	  if (profileCandidates.size()==1){
	    thiscandidate = profileCandidates[0];
	    int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	    if (mHisto) hTofpSlatIdE4->Fill(daqId);
	    if (Debug()) LOG_INFO << "candidate =" << daqId << endm;
	  }
	  else
	    if (Debug()) LOG_INFO << "none" << endm;
	}


	// forget it, and let user know of the non-decision
	if (thiscandidate == -99 && Debug()){
	  LOG_INFO << "E: ind=";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << mTofGeom->slatIdToDaq(slatIndex[ii]) << " ";
	  LOG_INFO << "\ttrkid:" << vTrackId[0] << " Unable to decide. "; 
	  LOG_INFO << "(hitprofs:";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << vHitProfile[ii] << " ";
	  LOG_INFO << " ss:";
	  for (unsigned int ii=0;ii<slatIndex.size();ii++) 
	    LOG_INFO << vS[ii] << " ";
	  LOG_INFO << ")" << endm;
	}

      }


      if (thiscandidate>=0){
	slatHit.slatIndex = slatIndex[thiscandidate];
	slatHit.hitPosition = vPosition[thiscandidate];
	slatHit.layerHitPositions = vLayerHitPositions[thiscandidate];
	//slatHit.trackIdVec.clear();
	slatHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	slatHit.hitProfile =  vHitProfile[thiscandidate];
	slatHit.s    =  vS[thiscandidate];
	slatHit.theta_xy =  vTheta_xy[thiscandidate];
	slatHit.theta_zr =  vTheta_zr[thiscandidate];
	slatHit.matchFlag = thisMatchFlag;
	
	allMatchedSlatsVec.push_back(slatHit);

	if (mHisto){
	  int daqId = mTofGeom->slatIdToDaq(slatIndex[thiscandidate]);
	  hTofpSlatIdE5->Fill(daqId);
	}
	
	// debugging output
	if (Debug()) {
	  LOG_INFO << "E: ind=" << mTofGeom->slatIdToDaq(slatHit.slatIndex)
	       << "\thitprof="<< slatHit.hitProfile << "\ts="<<slatHit.s
	       << "\tthxy="<<slatHit.theta_xy << "\tthzr="<<slatHit.theta_zr << "\ttrackid:"
	       << vTrackId[thiscandidate] << endm;
	}
      }
    } else
       gMessMgr->Warning("","OS")  << "E: no slats belong to this track ... should not happen!" << endm;

    tempVec = erasedVec;
  }
   gMessMgr->Info("","OST") << "E: #before/#after: " << singleHitSlatsVec.size()
			    << "/" << allMatchedSlatsVec.size() << endm;
  // end of Sect.E
  


  //.........................................................................
  // F. perform further selection and
  //    fill valid track histograms and SlatCollection
  //
  StTofSlatCollection *mSlatCollection =  new StTofSlatCollection;
  int nValidSingleHitSlats(0), nValidSinglePrimHitSlats(0);

  for (size_t ii=0; ii < allMatchedSlatsVec.size(); ii++){
    int daqId = mTofGeom->slatIdToDaq(allMatchedSlatsVec[ii].slatIndex);
    int jj = daqId-1;

    if (allMatchedSlatsVec[ii].trackIdVec.size()!=1)
       gMessMgr->Warning("","OST") << "F: WHAT!?!  mult.matched slat in single slat list " << daqId << endm;

    // 1. fill valid single track AND valid tdc histograms
    if (validTdc(mTofpTdc[jj])) nValidSingleHitSlats++;

    // get track-id from slat hit vector
    unsigned int trackNode = allMatchedSlatsVec[ii].trackIdVec[0];
    StTrack *theTrack = nodes[trackNode]->track(primary);

    // 2. continue only if the (primary) track exists
    if (validTofTrack(theTrack)){
      nValidSinglePrimHitSlats++;
      if (mHisto){
	float xhit = allMatchedSlatsVec[ii].hitPosition.x();
	float yhit = allMatchedSlatsVec[ii].hitPosition.y();
	float zhit = allMatchedSlatsVec[ii].hitPosition.z();
	float phihit = atan2(yhit,xhit);
	hTofpHitMap4->Fill(zhit,phihit);
	hTofpSlatIdF1->Fill(daqId);
      }
	  
      //--- store number of hits per track
      int nHitsPerTrack = theTrack->topologyMap().numberOfHits(kTpcId);
      if(mHisto) hTofpNumberOfTrackHits->Fill(nHitsPerTrack);
	  
      // select the apropriate track geometry
      StTrackGeometry *theTrackGeometry = trackGeometry(theTrack);

      //--- get momentum from track
      const StThreeVectorF momentum = theTrackGeometry->momentum();
      if (mHisto) hTofpPtTrack->Fill(momentum.perp());
	    
      //--- calculate flight path
      double pathLength = tofPathLength(&event->primaryVertex()->position(), 
					&allMatchedSlatsVec[ii].hitPosition,
					theTrackGeometry->helix().curvature());
	    

      //--- calculate local hit position on slat based first, last and middle plane
      //    (middle plane is the average of first and last plane, which is mathematically
      //     the same as SlatHitVec.hitPosition ... )
      StThreeVectorD *pInnerLayer, *pOuterLayer;
      pInnerLayer =  &(*(allMatchedSlatsVec[ii].layerHitPositions.begin()));
      pOuterLayer =  &(*(allMatchedSlatsVec[ii].layerHitPositions.end() - 1));

      //--- dig out from the dedx and rich pid traits (only for Debug mode)
      float dedx(0.), cherang(0);
      int dedx_np(0), cherang_nph(0);
      if (Debug()){
	StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
	for (unsigned int it=0;it<traits.size();it++){
	  if (traits[it]->detector() == kTpcId){
	    StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[it]);
	    if (pid && pid->method() ==kTruncatedMeanId){
	      dedx    = pid->mean();
	      dedx_np =  pid->numberOfPoints();
	    }
	  } else if  (traits[it]->detector() == kRichId){
	    StRichPidTraits *pid = dynamic_cast<StRichPidTraits*>(traits[it]);
	    if (pid){ 
	      StRichSpectra* pidinfo = pid->getRichSpectra();
	      if (pidinfo && pidinfo->getCherenkovPhotons()>2){
		cherang     = pidinfo->getCherenkovAngle();
		cherang_nph = pidinfo->getCherenkovPhotons();
	      }
	    }
	  }
	}
      }

      //--- calculate local hit position on slat based on average hitposition
      float localHitPos = mTofGeom->slatHitPosition(&allMatchedSlatsVec[ii].hitPosition);

      // Fill TOF Slat Collection
      //StTofSlat *tofSlat = new StTofSlat(jj+1,(int)mTofpAdc[jj],(int)mTofpTdc[jj],theTrack);
      StTofSlat *tofSlat = new StTofSlat(jj+1,(int)mTofpAdc[jj],(int)mTofpTdc[jj],theTrack,
      				   localHitPos, allMatchedSlatsVec[ii].hitProfile,
      				   allMatchedSlatsVec[ii].matchFlag);
      tofSlat->setPosition(allMatchedSlatsVec[ii].hitPosition);
      mSlatCollection->push_back(tofSlat);

      // dump debug data
      if (Debug()){
	LOG_INFO << "F: ind=" << mTofGeom->slatIdToDaq(allMatchedSlatsVec[ii].slatIndex) 
	     << "\ttrackid:";
	idVectorIter ij=allMatchedSlatsVec[ii].trackIdVec.begin();
	while (ij != allMatchedSlatsVec[ii].trackIdVec.end()) { LOG_INFO << " " << *ij; ij++; }
	LOG_INFO << "\tR=" << 1/(theTrackGeometry->helix().curvature())
	     << "\tpT=" << momentum.perp() << "\tp=" << momentum.mag()
	     << "\thits="<< nHitsPerTrack << "\ts="<< pathLength
	     << "\t#fitp=" <<theTrack->fitTraits().numberOfFitPoints(kTpcId)
	     << "\t#trkp=" <<theTrack->detectorInfo()->numberOfPoints(kTpcId)
	     << " \tdedx=" << dedx 
	     << " \tdca="<< theTrack->geometry()->helix().distance(event->primaryVertex()->position());
	if (cherang!=0) LOG_INFO  << " \trich="<< cherang << " (" << cherang_nph << ")";
	LOG_INFO << endm;
      }

    } // track exists 
  }
  
  storeMatchData(mSlatCollection,theTof);
  delete mSlatCollection;

  gMessMgr->Info("","OST") << "F: #before/#after: " << allMatchedSlatsVec.size()
			   << "/" <<nValidSinglePrimHitSlats << endm;

  //check StEvent collections --
  if (theTof->dataPresent())
     gMessMgr->Info("- TofCollection: raw data container present","OST");
  if (theTof->slatsPresent()){
     gMessMgr->Info("- TofCollection: slat container present","OST");
    if (Debug()){
      StSPtrVecTofSlat& tmpSlatTofVec = theTof->tofSlats();
      for (size_t i = 0; i < tmpSlatTofVec.size(); i++) {
	StTofSlat* p = tmpSlatTofVec[i];
	LOG_INFO << p->slatIndex() << " " << p->adc() << " " << p->tdc()
	     << " " << p->associatedTrack() << endm;
      }
    }
  }
  //-- end check

 // end of Sect.F


  // fill occupancy plots
  if (mHisto){
    hTofpNumberOfValidAdc->Fill(nAdcTofp);
    hTofpNumberOfValidTdc->Fill(nTdcTofp);
    hTofpNumberOfValidSlats->Fill(nAdcTdcTofp);
    hTofpNumberOfGlobalTracks->Fill(allSlatsHitVec.size());
    hTofpNumberOfHitSlats->Fill(allMatchedSlatsVec.size());
    hTofpNumberOfSingleHitTracks->Fill(nSingleHitSlats);
    hTofpNumberOfSingleValidHitTracks->Fill(nValidSingleHitSlats);
  }
  gMessMgr->Info("","OST") << "#(slat tracks): " << allSlatsHitVec.size()
			   << " #(hit slats): " << allMatchedSlatsVec.size()
			   << " #slats (valid tdc): " << nTdcTofp
			   << " #(single hits): " << nSingleHitSlats 
			   << " #(single valid hits): " << nValidSingleHitSlats
			   << " #(single prim valid hits): " << nValidSinglePrimHitSlats
			   << endm;



  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
     gMessMgr->Info("","OST") << "CPU time for StTofpMatchMaker::Make(): "
			      << timer.elapsedTime() << " sec\n" << endm;
  }

  gMessMgr->Info("StTofpMatchMaker -- bye-bye","OS");
  return kStOK;
}


//---------------------------------------------------------------------------
/// store local slat collection in StEvent's tofCollection
Int_t StTofpMatchMaker::storeMatchData(StTofSlatCollection *slatCollection,
					 StTofCollection* tofCollection){
  if(!tofCollection){
    gMessMgr->Error("No TofCollection -- returning","OS");
    return kStErr;
  }

  for (size_t j=0;j<slatCollection->size();j++){
    tofCollection->addSlat(slatCollection->getSlat(j)); 
    if (Debug())
      LOG_INFO << "storing " << j << "  " 
	   << slatCollection->getSlat(j)->slatIndex() << endm;
  }
  return kStOK;
}


//---------------------------------------------------------------------------
/// create a local copy of the raw tofp data tofData in StEvent's tofCollection
Int_t StTofpMatchMaker::getTofData(StTofCollection* tofCollection){
  if (!tofCollection) return kStERR;
  StSPtrVecTofData &tofData = tofCollection->tofData();

  // perform consistency check
  bool dataOK(true);
  gMessMgr->Info("TOF raw data consistency test","OS");
  for (int i=0;i<48;i++){
    if (tofData[i]->dataIndex()  != mTofGeom->daqToSlatId(i)) {
      dataOK = false;
      gMessMgr->Warning("","OST") << "===>WARNING: " << tofData[i]->dataIndex() << " " << mTofGeom->daqToSlatId(i) << endm;
    }
    //if (Debug()) LOG_INFO << *tofData[i];
  }
  //LOG_INFO << " done" << endm;
  
  for (int i=0;i<NTOFP;i++){
    mTofpAdc[i] = tofData[i]->adc();
    mTofpTdc[i] = tofData[i]->tdc();
  }

  if (mYear2){
    // swap ADC channels 3 and 4 ... this should move to the DAQreader!
    float tmp   = mTofpAdc[3];
    mTofpAdc[3] = mTofpAdc[2];
    mTofpAdc[2] = tmp;
  }

  for (int i=0;i<NPVPD;i++){
    mPvpdAdc[i] = tofData[42+i]->adc(); 
    mPvpdTdc[i] = tofData[42+i]->tdc(); 
    if (mYear3||mYear4)
      mPvpdAdcLoRes[i] = tofData[54+i]->adc();
  }

  if (!dataOK) return kStWarn;

  return kStOK;
}


//---------------------------------------------------------------------------
/// Book histograms and create ordered collection for easy manipulation
void StTofpMatchMaker::bookHistograms(void){


  //.........................................................................  
  // Book Hit Position Correction histograms
  mHitPosHistNames = new TOrdCollection;
  hTofpHitMap1 = new TH2D("tofpHitMap1","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  mHitPosHistNames->AddLast(hTofpHitMap1);
  hTofpHitMap2 = new TH2D("tofpHitMap2","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  mHitPosHistNames->AddLast(hTofpHitMap2);
  hTofpHitMap3 = new TH2D("tofpHitMap3","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  mHitPosHistNames->AddLast(hTofpHitMap3);
  hTofpHitMap4 = new TH2D("tofpHitMap4","valid hit positions", 500,-250,0, 120,-1.23, -1.08);
  mHitPosHistNames->AddLast(hTofpHitMap4);
  hTofpSlatHitVecSize = new TH1D("SlatMult","Slat Mult per Track",10,0,10);
  mHitPosHistNames->AddLast(hTofpSlatHitVecSize);
  hTofpSlatIdA0 = new TH1D("tofpSlatIdA0","events per slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdA0);
  hTofpSlatIdA1 = new TH1D("tofpSlatIdA1","valid slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdA1);
  hTofpSlatIdB1 = new TH1D("tofpSlatIdB1","#tracks match  valid slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdB1);
  hTofpSlatIdD1 = new TH1D("tofpSlatIdD1","track match per valid slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdD1);
  hTofpSlatIdD2 = new TH1D("tofpSlatIdD2","single track match per slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdD2);
  hTofpSlatIdE1 = new TH1D("tofpSlatIdE1","one slat for one track match",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdE1);
  hTofpSlatIdE2 = new TH1D("tofpSlatIdE2","recovered from hitprof-weight",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdE2);
  hTofpSlatIdE3 = new TH1D("tofpSlatIdE3","recovered from ss",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdE3);
  hTofpSlatIdE4 = new TH1D("tofpSlatIdE4","recovered from closest hitplane",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdE4);
  hTofpSlatIdE5 = new TH1D("tofpSlatIdE5","total recovered slat per track match",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdE5);
  hTofpSlatIdF1 = new TH1D("tofpSlatIdF1","primary track match per slat",41,0.5,41.5);
  mHitPosHistNames->AddLast(hTofpSlatIdF1);


  //.........................................................................  
  // Book track related histograms
  mTrackHistNames = new TOrdCollection;
  hTofpNumberOfTrackHits = new TH1D("tofpNumberOfTrackHits","numberOfTrackHits",80,0,80);
  mTrackHistNames->AddLast(hTofpNumberOfTrackHits);
  hTofpPtTrack = new TH1D("tofpPtTrack","ptTrack",250,0.,10);
  mTrackHistNames->AddLast(hTofpPtTrack);
  hTofpDCATrackprimVertex = new TH1D("tofpDCATrackprimVertex","DCA distribution",6000,-30.,30.);
  mTrackHistNames->AddLast(hTofpDCATrackprimVertex);

  mOccupancyHistNames = new TOrdCollection;
  hTofpNumberOfValidAdc = new TH1D("tofpNumberOfValidTdc","numberOfValidTdc",41,0,41);
  mOccupancyHistNames->AddLast(hTofpNumberOfValidAdc);
  hTofpNumberOfValidTdc = new TH1D("tofpNumberOfValidAdc","numberOfValidAdc",41,0,41);
  mOccupancyHistNames->AddLast(hTofpNumberOfValidTdc);
  hTofpNumberOfValidSlats = new TH1D("tofpNumberOfValidSlats","numberOfValidSlats",41,0,41);
  mOccupancyHistNames->AddLast(hTofpNumberOfValidSlats);
  hTofpNumberOfGlobalTracks = new TH1D("tofpNumberOfGlobalTracks","numberOfGlobalTracks",50,0,50);
  mOccupancyHistNames->AddLast(hTofpNumberOfGlobalTracks);
  hTofpNumberOfHitSlats = new TH1D("tofpNumberOfHitSlats","numberOfHitSlats",50,0,50);
  mOccupancyHistNames->AddLast(hTofpNumberOfHitSlats);
  hTofpNumberOfSingleHitTracks = new TH1D("tofpNumberOfSingleHitTracks","numberOfSingleHitTracks",50,0,50);
  mOccupancyHistNames->AddLast(hTofpNumberOfSingleHitTracks);
  hTofpNumberOfSingleValidHitTracks = new TH1D("tofpNumberOfSingleValidTracks","numberOfSingleValidHitTracks",50,0,50);
  mOccupancyHistNames->AddLast(hTofpNumberOfSingleValidHitTracks);

  mMatchHistNames = new TOrdCollection;
  char buf[20];
  for (int i=0;i<NTOFP;i++){
    sprintf(buf,"tofpSlathit_%d",i+1);
    hTofpMatchHit[i] = new TH2D(buf,buf,5,-2.5,2.5,5,-2.5,2.5);
    hTofpMatchHit[i]->SetXTitle("iEta");
    hTofpMatchHit[i]->SetYTitle("iPhi");
    mMatchHistNames->AddLast(hTofpMatchHit[i]);
    sprintf(buf,"tofpSlatnohit_%d",i+1);
    hTofpMatchNoHit[i] = new TH2D(buf,buf,5,-2.5,2.5,5,-2.5,2.5);
    hTofpMatchNoHit[i]->SetXTitle("iEta");
    hTofpMatchNoHit[i]->SetYTitle("iPhi");
    mMatchHistNames->AddLast(hTofpMatchNoHit[i]);
  }

  return;
}


//---------------------------------------------------------------------------
/// store histograms in a seperate root file
void StTofpMatchMaker::writeHistogramsToFile(){
 // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  gMessMgr->Info("","OST") << "StTofpMatchMaker::writeHistogramsToFile()"
			   << " histogram file " <<  mHistoFileName << endm;

  theHistoFile->mkdir("hitpos","hit position histograms");
  theHistoFile->cd("hitpos");
  mHitPosHistNames->Write();
  delete mHitPosHistNames; mHitPosHistNames=0;

  theHistoFile->mkdir("track","Tracking Plots");
  theHistoFile->cd("track");
  mTrackHistNames->Write();
  delete mTrackHistNames; mTrackHistNames=0;

  theHistoFile->mkdir("occup","Occupancy Plots");
  theHistoFile->cd("occup");
  mOccupancyHistNames->Write();
  delete mOccupancyHistNames; mOccupancyHistNames=0;

  theHistoFile->mkdir("match","Matching Plots");
  theHistoFile->cd("match");
  mMatchHistNames->Write();
  delete mMatchHistNames; mMatchHistNames=0;

  theHistoFile->Write();  
  theHistoFile->Close();    

  return;
}


//---------------------------------------------------------------------------
/// determine pVPD event type (strobe or beam)
bool StTofpMatchMaker::strobeEvent(StSPtrVecTofData& tofData){
  // determine strobe event from pVPD TDC data
  
  int nStrobedPvpdTdcs=0;
  for(int i=0;i<NPVPD;i++)
    if((tofData[42+i]->tdc()>mStrobeTdcMin[i]) &&
       (tofData[42+i]->tdc()<mStrobeTdcMax[i]))
      nStrobedPvpdTdcs++;
  
  if (nStrobedPvpdTdcs==NPVPD) return true;

  return false;
}


//---------------------------------------------------------------------------
/// determine whether this is a valid TOF beam event
bool StTofpMatchMaker::validEvent(StEvent *event){
  mEventCounter++;
  // 1. must have non-zero pointer
  if (!event) return false;

  // 2. must have a valid primary vertex 
  if (!event->primaryVertex()) return false;
  mAcceptedEventCounter++;

  // 3a. must have TOF collection
  if (!event->tofCollection()){
    gMessMgr->Info("TOF is not present","OST");
    return false;
  }

  // 3b. must have TOF raw data available
  if (!(event->tofCollection()->dataPresent())){
    gMessMgr->Info("TOF is present but no Raw Data","OST");
    if  (!(event->tofCollection()->slatsPresent())){
      gMessMgr->Info("              and no Slat Data","OST");
      return false;
    }
  }
  mTofEventCounter++;


  // 4. must be a TOF beam event, i.e. a non-strobe event
  StSPtrVecTofData  &tofData = event->tofCollection()->tofData();
  if (strobeEvent(tofData)){
    mTofStrobeEventCounter++;
    if (event->primaryVertex()) mAcceptAndStrobe++; // keep track of #valid strobed evts
    gMessMgr->Info("strobe event","OTS");
    return false;
  }
  mAcceptAndBeam++;

  // and we have a winner!
  gMessMgr->Info("TOF present ... and valid beam event","OTS");

  return true;
}


//---------------------------------------------------------------------------
/// determine whether this is a valid TPC track
bool StTofpMatchMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track
  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;

  return true;
}


//---------------------------------------------------------------------------
/// determine whether this is a valid TOF track
bool StTofpMatchMaker::validTofTrack(StTrack *track){
  // select valid tracks for time-of-flight calculations

  // 1. track must exist
  if (!track) return false;

  // 2. track must be a primary track
  if (!dynamic_cast<StPrimaryTrack*>(track)) return false;

  // 3. DCA cut (obsolete?)
  double DCA= track->impactParameter();
  int charge = track->geometry()->charge();
  if (mHisto) hTofpDCATrackprimVertex->Fill(DCA*charge);
  if (DCA > mMaxDCA) {
    gMessMgr->Info("","OST") << "dca>max:" << DCA<< endm;
    return false;
  }

  return true;
}  


//---------------------------------------------------------------------------
/// returns the proper track geometry, based on a global user setting
StTrackGeometry* StTofpMatchMaker::trackGeometry(StTrack* track){
  // returns apropriate StTrackGeometry (standard or outerGeometry)
  if (!track) return 0;
  StTrackGeometry *thisTrackGeometry;
  if (mOuterTrackGeometry)
    thisTrackGeometry = track->outerGeometry();
  else
    thisTrackGeometry = track->geometry();
  return thisTrackGeometry;
}


//---------------------------------------------------------------------------

/***************************************************************************
 *
 * $Log: StTofpMatchMaker.cxx,v $
 * Revision 1.15  2018/02/26 23:26:52  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.14  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.13  2012/12/14 06:36:02  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.12  2007/04/17 23:01:03  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.11  2005/04/12 17:32:45  dongx
 * update for year 5 data. Tofp removed, so do nothing in this maker from now on.
 *
 * Revision 1.10  2004/06/10 15:54:31  dongx
 * rename the defition of int vector
 *
 * Revision 1.9  2004/06/09 21:28:05  dongx
 * update matching : checking before projecting track, improve the speed by around an order of magnitude
 *
 * Revision 1.8  2004/04/10 04:32:39  dongx
 * fix a potential crashing of filling histo w/o mHisto
 *
 * Revision 1.7  2004/03/11 22:29:32  dongx
 * -remove assert()
 * -add member mYear4
 * -use m_Mode to control the output root file
 *
 * Revision 1.6  2003/12/05 08:17:27  geurts
 * changed default TDC and ADC ranges
 *
 * Revision 1.5  2003/09/17 19:40:12  geurts
 * zeroed event counters and one pointer
 *
 * Revision 1.4  2003/09/15 22:38:10  geurts
 * dBase updates:
 *  - removed initLocalDb option
 *  - introduced dBase parameters for strobe event definitions
 *
 * Revision 1.3  2003/09/13 19:15:52  geurts
 * Changed passing of StSPtrVecTofData for strobeEvent (see bugtracker ticket #172)
 *
 * Revision 1.2  2003/09/02 17:59:11  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2003/08/08 18:31:26  geurts
 * first release
 *
 */
