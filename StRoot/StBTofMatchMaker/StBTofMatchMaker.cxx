/*******************************************************************
 *
 * $Id: StBTofMatchMaker.cxx,v 1.2 2009/07/24 18:52:53 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: BTof Match Maker to do the matching between the 
 *              fired celles and TPC tracks
 *
 *****************************************************************
 *
 * $Log: StBTofMatchMaker.cxx,v $
 * Revision 1.2  2009/07/24 18:52:53  dongx
 * - Local Z window restricted in the projection
 * - ToT selection is used firstly when more than one hits associated with a track
 * - matchFlag updated
 *    0:   no matching
 *    1:   1-1 matching
 *    2:   1-2 matching, pick up the one with higher ToT value (<25ns)
 *    3:   1-2 matching, pick up the one with closest projection posision along y
 *
 * Revision 1.1  2009/06/23 13:15:03  geurts
 * *** empty log message ***
 *
 *
 *******************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StDcaGeometry.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StBTofPidTraits.h"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "StHelix.hh"
#include "StBTofCollection.h"
#include "StBTofUtil/tofPathLength.hh"
#include "StBTofUtil/StBTofGeometry.h"
#include "StBTofUtil/StBTofHitCollection.h"
#include "tables/St_tofConfig_Table.h"
#include "tables/St_tofTrayConfig_Table.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TTree.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StBTofMatchMaker.h"
//#include "TMemStat.h"

ClassImp(StBTofMatchMaker)

//---------------------------------------------------------------------------
StBTofMatchMaker::StBTofMatchMaker(const Char_t *name): StMaker(name){
  // set default values
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mAcceptAndBeam = 0;

  mBTofGeom = 0;

  mWidthPad = 3.45;

  setOuterTrackGeometry();
  setMinHitsPerTrack(15);
  setMinFitPointsPerTrack(15);
  setMinFitPointsOverMax(0.52);
  setMaxDCA(9999.);

  setCreateHistoFlag(kFALSE);
  setHistoFileName("tofana.root");
  setCreateTreeFlag(kFALSE);
  setSaveGeometry(kFALSE);
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

StBTofMatchMaker::~StBTofMatchMaker(){ /* nope */}

//void StBTofMatchMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::Init(){
  LOG_INFO << "StBTofMatchMaker -- initializing ..." << endm;
  if(Debug()) {
    LOG_INFO << "Minimum hits per track: " << mMinHitsPerTrack << endm;
    LOG_INFO << "Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
    LOG_INFO << "Maximum DCA: " << mMaxDCA << endm;
  }
  if (!mOuterTrackGeometry) {
    LOG_WARN << "using standard trackgeometry()" << endm;
  }
  
  // m_Mode can be set by SetMode() method
  if(m_Mode) {
//    setHistoFileName("tofana.root");
  } else {
    setHistoFileName("");
  }

  if (mHisto){
    bookHistograms();
    LOG_INFO << "Histograms are booked" << endm;
    if (mHistoFileName!="") {
      LOG_INFO << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
    }
  }

  // reset event counters
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mAcceptAndBeam = 0;
  
  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::InitRun(Int_t runnumber){

  // determine TOF configuration from run#

  LOG_INFO << "StBTofMatchMaker -- Initializing TofGeometry (InitRun)" << endm;
  /////////////////////////////////////////////////////////////////////
  // TOF geometry initializtion -- from GEANT geometry directly
  //                               need St_geant_Maker be loaded before
  /////////////////////////////////////////////////////////////////////
  StTimer geomTimer;
  geomTimer.start();
  mBTofGeom = new StBTofGeometry("btofGeom","btofGeom in MatchMaker");
  if(!mBTofGeom->IsInitDone()) {
    gMessMgr->Info("BTofGemetry initialization..." ,"OS");
    TVolume *starHall = (TVolume *)GetDataSet("HALL");
    mBTofGeom->Init(this, starHall);
  }
  // other makers can get this geometry
  if(mGeometrySave) {
    if(TDataSet *geom = GetDataSet("btofGeometry")) delete geom;
    AddConst(new TObjectSet("btofGeometry",mBTofGeom)); 
  }

  geomTimer.stop();
  LOG_INFO << "CPU time for StBTofGeometry initialization : " << geomTimer.elapsedTime() << " sec" << endm;

  return kStOK;
}

//----------------------------------------------------------------------------
Int_t StBTofMatchMaker::FinishRun(Int_t runnumber){

  LOG_INFO << "StBTofMatchMaker -- cleaning up geometry (FinishRun)" << endm;
  if (mBTofGeom) delete mBTofGeom;
  mBTofGeom=0;

  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::Finish(){

  LOG_INFO << "StBTofMatchMaker -----  RUN SUMMARY ----- (Finish)\n"
       << "\tProcessed "  << mEventCounter << " events."
       << " Accepted  "   << mAcceptedEventCounter << " events."
       << " Rejected  "   << mEventCounter - mAcceptedEventCounter << " events\n"
       << "\tTOF events " << mTofEventCounter
       << "\t Accept & Beam   "   << mAcceptAndBeam   << " events" << endm;
  
  //if (mHisto) writeHistogramsToFile();
  if (mHistoFileName!="") writeHistogramsToFile();
  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::Make(){
  LOG_INFO << "StBTofMatchMaker -- welcome" << endm;
  LOG_DEBUG << " processing event ... " << endm;

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
//  if (!validEvent(mEvent)){
  if(!mEvent || !(mEvent->btofCollection()) || !(mEvent->btofCollection()->hitsPresent()) ) {
    if (!mEvent) cout << "no mevent" << endl;
    if (!(mEvent->btofCollection())) cout << "no btofcollection" << endl;
    if (!(mEvent->btofCollection()->hitsPresent()) ) cout << "no hitspresent" << endl;
    LOG_INFO << "StBTofMatchMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }
  if(mHisto) mEventCounterHisto->Fill(1);

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StBTofCollection *theTof = mEvent->btofCollection();

  //.........................................................................
  // read data from StBTofHit
  /// A. build vector of candidate cells
  //
  tofCellHitVector daqCellsHitVec;
  idVector validModuleVec;

  // multi-tray system
  StSPtrVecBTofHit& tofHits = theTof->tofHits();
  for(size_t i=0;i<tofHits.size();i++) {
    //fg StBTofHit* aHit = dynamic_cast<StBTofHit *>(tofHits[i]);
    StBTofHit* aHit = tofHits[i];
     if(!aHit) continue;
     if(aHit->tray()<=0||aHit->tray()>mNTray) continue;  // barrel tray hits

     int trayId = aHit->tray();
     int moduleId = aHit->module();
     int cellId = aHit->cell();

     LOG_DEBUG <<"A: fired hit in " << " tray="<< trayId << " module="<<moduleId<<" cell="<<cellId<<endm;

     StructCellHit aDaqCellHit;
     aDaqCellHit.tray = trayId;
     aDaqCellHit.module = moduleId;
     aDaqCellHit.cell = cellId;
     aDaqCellHit.tot = aHit->tot();
     aDaqCellHit.index2BTofHit = i;
     daqCellsHitVec.push_back(aDaqCellHit);

     // additional valid number configuration
     int id = trayId*100+moduleId;
     //fg bool ifind = kFALSE;
     //fg for(size_t im=0;im<validModuleVec.size();im++) {
     //fg   if(id==validModuleVec[im]) {
     //fg     ifind = kTRUE;
     //fg     break;
     //fg   }
     //fg }
     //fg if(!ifind) validModuleVec.push_back(id);
     if (find(validModuleVec.begin(), validModuleVec.end(), id) == validModuleVec.end())
       validModuleVec.push_back(id);

     if(mHisto) {
       mDaqOccupancy[trayId-1]->Fill((moduleId-1)*mNCell+(cellId-1));
     }      
  }
      
  // end of Sect.A
  LOG_DEBUG << "    total # of cells = " << daqCellsHitVec.size() << endm;
  if(Debug()) {
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_DEBUG << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
//  if(!daqCellsHitVec.size()) return kStOK;


  //.........................................................................
  /// B. loop over global tracks and determine all cell-track matches
  //
  tofCellHitVector allCellsHitVec;
  StructCellHit cellHit;

  StTimer projTimer;
  if (doPrintCpuInfo) projTimer.start();

  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  Int_t nAllTracks=0;
  Int_t nPrimaryHits = 0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    tofCellHitVector cellHitVec;
    //    cellHitVec.clear();
    StGlobalTrack *theTrack = dynamic_cast<StGlobalTrack*>(nodes[iNode]->track(global));
    if(!theTrack) continue;

    bool isPrimary = kFALSE;
    StPrimaryTrack *pTrack = dynamic_cast<StPrimaryTrack*>(theTrack->node()->track(primary));
    if(pTrack) isPrimary = kTRUE;

    StThreeVectorF mom = theTrack->geometry()->momentum();
    float pt = mom.perp();
    float eta = mom.pseudoRapidity();
    float phi = mom.phi();
    //fg if (phi<0.) phi += 2.*3.14159;
    if (phi<0.) phi += 2.*M_PI;

    float dEdx = -999.;
    int ndEdxpts = 0;
    float nSigmaPion = -999.;
    static StTpcDedxPidAlgorithm PidAlgorithm;
    static StPionPlus* Pion = StPionPlus::instance();

    const StParticleDefinition* pd = theTrack->pidTraits(PidAlgorithm);
    if (pd) {
      nSigmaPion = PidAlgorithm.numberOfSigma(Pion);
    }
    if ( PidAlgorithm.traits() ) {
      dEdx = PidAlgorithm.traits()->mean();
      ndEdxpts = PidAlgorithm.traits()->numberOfPoints();
    }
    int nfitpts = theTrack->fitTraits().numberOfFitPoints(kTpcId);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      if(mHisto) {
        mTrackPtEta->Fill(pt, eta);
        mTrackPtPhi->Fill(pt, phi);
        mTrackNFitPts->Fill(nfitpts);
        if(dEdx>0.) mTrackdEdxvsp->Fill(mom.mag(), dEdx*1.e6);
        if(fabs(nSigmaPion)<5.) mNSigmaPivsPt->Fill(pt, nSigmaPion+5.*theTrack->geometry()->charge());
      }

      if(mSaveTree) {
        trackTree.pt = pt;
        trackTree.eta = eta;
        trackTree.phi = phi;
        trackTree.nfitpts = nfitpts;
        trackTree.dEdx = dEdx*1.e6;
        trackTree.ndEdxpts = ndEdxpts;
        trackTree.charge = theTrack->geometry()->charge();      
        trackTree.projTrayId = 0;
        trackTree.projCellChan = -1;
        trackTree.projY = -999.;
        trackTree.projZ = -999.;
      }

      nAllTracks++;
      StPhysicalHelixD theHelix = trackGeometry(theTrack)->helix();

//      IntVec projTrayVec;
//      if(!mBTofGeom->projTrayVector(theHelix, projTrayVec)) continue;

      IntVec idVec;
      DoubleVec pathVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      if(mBTofGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec) ) {
//      if(mBTofGeom->HelixCrossCellIds(theHelix, validModuleVec, projTrayVec, idVec, pathVec, crossVec)) {
	Int_t cells = idVec.size();
	for (Int_t i=0; i<cells; i++) {
            Int_t icell,imodule,itray;
            Double_t local[3],global[3];
            for(Int_t i2=0;i2<3;i2++){
                 local[i2]=0;
            }
            global[0]=crossVec[i].x();
            global[1]=crossVec[i].y();
            global[2]=crossVec[i].z();
            mBTofGeom->DecodeCellId(idVec[i], icell, imodule, itray);
//	    LOG_INFO << " decode " << idVec[i] << "  to tray#" << itray << " module#" << imodule << " cell#" << icell << endm;
	    StBTofGeomSensor* sensor = mBTofGeom->GetGeomSensor(imodule,itray);
	    if(!sensor) {
	      LOG_WARN << " No sensitive module in the projection??? -- Something weird!!! " << endm;
	      continue;
	    }
            sensor->Master2Local(&global[0],&local[0]);
            icell = sensor->FindCellIndex(local);
	    //	    StThreeVectorD glo=sensor->GetCenterPosition();
	    StThreeVectorD glo(global[0], global[1], global[2]);
	    StThreeVectorD hitPos(local[0], local[1], local[2]);
//	    delete sensor;   /// function in StBTofGeometry modified. Don't delete here.
	    if (local[2]<=2.7&&local[2]>=-3.4) {
//            if (fabs(local[2])<4.) {   // loose local z cut at first step
	      ncells++;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back(iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHitVec.push_back(cellHit);
	      allCellsHitVec.push_back(cellHit);

              if(isPrimary) nPrimaryHits++;

	      if(mHisto) {
                mDaqOccupancyProj[itray-1]->Fill((imodule-1)*mNCell+(icell-1));
		mHitsPosition->Fill(hitPos.y(), hitPos.z());
	      }
	      
              if(mSaveTree) {
                trackTree.projTrayId = itray;
                trackTree.projCellChan = (imodule-1)*mNCell+(icell-1);
                trackTree.projY = local[1];
                trackTree.projZ = local[2];
              }

	      LOG_DEBUG <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
	      LOG_DEBUG <<"   hit position " << hitPos << endm;
	      // LOG_DEBUG <<"   momemtum= " << pt << " " << eta << " " << phi << endm;
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

      if(mHisto && mSaveTree) mTrackTree->Fill();

    } // if(ValidTrack).. 
  } // loop over nodes
  LOG_DEBUG << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm;
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    mHitsPrimaryInEvent->Fill(nPrimaryHits);
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B
  if (doPrintCpuInfo) {
    projTimer.stop();
    LOG_INFO << "CPU time for Step B - projection : "
	 << projTimer.elapsedTime() << " sec" << endm;
  }

  //.........................................................................
  /// C. Match find Neighbours -- identify crosstalk
  //
  tofCellHitVector matchHitCellsVec;

  tofCellHitVectorIter daqIter = daqCellsHitVec.begin();
  for(unsigned int idaq=0;idaq<daqCellsHitVec.size();idaq++, daqIter++) {
    tofCellHitVectorIter proIter = allCellsHitVec.begin();
    for(unsigned int ipro=0;ipro<allCellsHitVec.size();ipro++, proIter++) {

      int daqIndex = (daqIter->module-1)*6 + (daqIter->cell-1);
      int proIndex = (proIter->module-1)*6 + (proIter->cell-1);
      int hisIndex = daqIter->tray - 1;
//      int daqAllIndex = (daqIter->tray - 1)*mNTOF + daqIndex;
//      int proAllIndex = (proIter->tray - 1)*mNTOF + proIndex;
      if(daqIter->tray==proIter->tray) {
	if (mHisto) {
	  if(hisIndex>=0&&hisIndex<mNTray) {
	    mHitCorr[hisIndex]->Fill(proIndex,daqIndex);
	    mHitCorrModule[hisIndex]->Fill(proIter->module-1,daqIter->module-1);
	  } else {
	    LOG_WARN << " weird tray # " << daqIter->tray << endm;
	  }
	}
      }
      if( (daqIter->tray==proIter->tray)&& 
	  (daqIter->module==proIter->module) &&
	  ( ( (proIter->cell==6)&&((proIter->cell==daqIter->cell) ||
				   (proIter->cell==daqIter->cell+1)) )
	    || ( (proIter->cell==1)&&((proIter->cell==daqIter->cell) ||
				      (proIter->cell==daqIter->cell-1)) )
	    || ( (proIter->cell>=2&&proIter->cell<=6) &&
		 ( (proIter->cell==daqIter->cell) ||
		   (proIter->cell==daqIter->cell-1) ||
		   (proIter->cell==daqIter->cell+1) ) ) ) ) {
	cellHit.tray = daqIter->tray;
	cellHit.module = daqIter->module;
	cellHit.cell = daqIter->cell;
	cellHit.hitPosition = proIter->hitPosition;
	cellHit.trackIdVec = proIter->trackIdVec;
	cellHit.zhit = proIter->zhit;
	cellHit.yhit = proIter->yhit;
        cellHit.tot = daqIter->tot;
        cellHit.index2BTofHit = daqIter->index2BTofHit;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  LOG_DEBUG << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm;
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);

  //.........................................................................
  /// D. sort hit vectors  and deal with (discard) cells matched by multiple tracks
  //
  Int_t nSingleHitCells(0);
  Int_t nMultiHitsCells(0);

  tofCellHitVector singleHitCellsVec;
  tofCellHitVector multiHitsCellsVec;

  tofCellHitVector tempVec = matchHitCellsVec;
  tofCellHitVector erasedVec = tempVec;
  while (tempVec.size() != 0) {
    Int_t nTracks = 0;
    idVector trackIdVec;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->tray == erasedIter->tray &&
	 tempIter->module == erasedIter->module &&
	 tempIter->cell == erasedIter->cell) {
	nTracks++;
	trackIdVec.push_back(erasedIter->trackIdVec.back());  // merge
	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    cellHit.cell = tempIter->cell;
    cellHit.module = tempIter->module;
    cellHit.tray = tempIter->tray;
    cellHit.hitPosition = tempIter->hitPosition;
    cellHit.trackIdVec = trackIdVec;
    cellHit.zhit = tempIter->zhit;
    cellHit.yhit = tempIter->yhit;
    cellHit.tot = tempIter->tot;
    cellHit.index2BTofHit = tempIter->index2BTofHit;

    Float_t ycenter = (tempIter->cell-1-2.5)*mWidthPad;
    Float_t dy = tempIter->yhit - ycenter;
    Float_t dz = tempIter->zhit;

    if(mHisto) {
      mTracksPerCellMatch1->Fill(trackIdVec.size());
//      mDaqOccupancyMatch1->Fill((tempIter->module-1)*mNCell+(tempIter->cell-1));
      mDeltaHitMatch1->Fill(dy, dz);
    }

    if (nTracks==1){
      nSingleHitCells++;      
      singleHitCellsVec.push_back(cellHit);
    } else if (nTracks>1){
      nMultiHitsCells++;
      multiHitsCellsVec.push_back(cellHit);
      // for multiple hit cells either discard (yes) or
      // find the most likely candidate.
    } else {
      LOG_WARN << "D: no tracks extrapolate to matched cell ... should not happen!" << endm;
    }
    
    LOG_DEBUG << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
    if (Debug()) {
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_DEBUG << " " << *ij; ij++; }
    }
    LOG_DEBUG <<endm;

    tempVec = erasedVec;
  }
  LOG_DEBUG << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm;
  //end of Sect.D

  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 

  //.........................................................................
  /// E. sort and deal singleHitCellsVector for multiple cells associated to single tracks
  //
  tofCellHitVector FinalMatchedCellsVec;
  //  FinalMatchedCellsVec.clear();
  tempVec = singleHitCellsVec;
  if(mHisto) {
    mCellsPerEventMatch2->Fill(tempVec.size());
    for(unsigned int ii=0;ii<tempVec.size();ii++) {
      mTracksPerCellMatch2->Fill(tempVec[ii].trackIdVec.size());
//      mDaqOccupancyMatch2->Fill((tempVec[ii].module-1)*mNCell+(tempVec[ii].cell-1));
      Float_t ycenter = (tempVec[ii].cell-1-2.5)*mWidthPad;
      Float_t dy = tempVec[ii].yhit-ycenter;
      Float_t dz = tempVec[ii].zhit;
      mDeltaHitMatch2->Fill(dy, dz);
    }
  }

  erasedVec = tempVec;
  while (tempVec.size() != 0) {
    StructCellHit cellHit;
    Int_t nCells = 0;
    idVector vTrackId;
    vector<StThreeVectorD> vPosition;
    vector<Int_t> vchannel, vtray, vmodule, vcell;
    vector<Float_t> vzhit, vyhit;
    vector<Double_t> vtot;
    vector<Int_t> vindex2BTofHit;

    tofCellHitVectorIter tempIter=tempVec.begin();
    tofCellHitVectorIter erasedIter=erasedVec.begin();
    while(erasedIter!= erasedVec.end()) {
      if(tempIter->trackIdVec.back() == erasedIter->trackIdVec.back()) {
	nCells++;
	vtray.push_back(erasedIter->tray);
	vmodule.push_back(erasedIter->module);
	vcell.push_back(erasedIter->cell);
	vPosition.push_back(erasedIter->hitPosition);
	vTrackId.push_back(erasedIter->trackIdVec.back());
	vzhit.push_back(erasedIter->zhit);
	vyhit.push_back(erasedIter->yhit);
        vtot.push_back(erasedIter->tot);
        vindex2BTofHit.push_back(erasedIter->index2BTofHit);

	erasedVec.erase(erasedIter);
	erasedIter--;
      }
      erasedIter++;
    }

    if (nCells==1){
      // for singly hit cell, copy data in singleHitCellsVec
      cellHit.tray = vtray[0];
      cellHit.module = vmodule[0];
      cellHit.cell = vcell[0];
      cellHit.trackIdVec.push_back(vTrackId[0]);
      cellHit.hitPosition = vPosition[0];
      cellHit.matchFlag = 1; 
      cellHit.zhit = vzhit[0];
      cellHit.yhit = vyhit[0];
      cellHit.tot = vtot[0];
      cellHit.index2BTofHit = vindex2BTofHit[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      if (Debug()) {
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_DEBUG << " " << *ij; ij++; }
      }
      LOG_DEBUG <<endm;
      
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on tot
      Float_t tot(0.);
      vector<Int_t> ttCandidates;
      for (Int_t i=0;i<nCells;i++) {
        Double_t tt = vtot[i];
        if(tt<25.&&tt>tot) {
          tot = tt;
          ttCandidates.clear();
          ttCandidates.push_back(i);
        } else if (tt==tot) {
          ttCandidates.push_back(i);
        }
      }
      if (ttCandidates.size()==1) {
        thiscandidate = ttCandidates[0];
        thisMatchFlag = 2;
      } else if (ttCandidates.size()>1) {  // sort on hitposition
        Float_t ss(99.);
        vector<Int_t> ssCandidates;
        for(size_t j=0;j<ttCandidates.size();j++) {
          Float_t yy = vyhit[ttCandidates[j]];
          Float_t ycell = (vcell[ttCandidates[j]]-1-2.5)*mWidthPad;
          Float_t ll = fabs(yy-ycell);
          if(ll<ss) {
            ss = ll; 
            ssCandidates.clear();
            ssCandidates.push_back(ttCandidates[j]);
          }else if  (ll==ss)
            ssCandidates.push_back(ttCandidates[j]);
        }
        if (ssCandidates.size()==1){
          thiscandidate = ssCandidates[0];
          thisMatchFlag = 3;
        }
      }

      if (thiscandidate>=0) {
	cellHit.tray = vtray[thiscandidate];
	cellHit.module = vmodule[thiscandidate];
	cellHit.cell = vcell[thiscandidate];
	cellHit.trackIdVec.push_back(vTrackId[thiscandidate]);
	cellHit.hitPosition = vPosition[thiscandidate];
	cellHit.matchFlag = thisMatchFlag;
	cellHit.zhit = vzhit[thiscandidate];
	cellHit.yhit = vyhit[thiscandidate];
        cellHit.tot = vtot[thiscandidate];
        cellHit.index2BTofHit = vindex2BTofHit[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm;
      }

    } else {
      LOG_WARN << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  LOG_DEBUG << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm;
  // end of Sect.E

  //.........................................................................
  //// F. perform further selection and fill valid track histograms, ntuples and BTofPidTraits
  //
  if(mHisto) {
    if(FinalMatchedCellsVec.size()) mEventCounterHisto->Fill(10);
    mCellsPerEventMatch3->Fill(FinalMatchedCellsVec.size());
  }

//  StSPtrVecBTofHit& tofHits = theTof->tofHits();
  Int_t nValidSingleHitCells(0), nValidSinglePrimHitCells(0);

  for (size_t ii=0; ii < FinalMatchedCellsVec.size(); ii++){
    Int_t tray = FinalMatchedCellsVec[ii].tray;
    Int_t module = FinalMatchedCellsVec[ii].module;
    Int_t cell = FinalMatchedCellsVec[ii].cell;

    Float_t ycenter = (cell-1-2.5)*mWidthPad;
    Float_t dy = FinalMatchedCellsVec[ii].yhit - ycenter;
    Float_t dz = FinalMatchedCellsVec[ii].zhit;
    if (FinalMatchedCellsVec[ii].trackIdVec.size()!=1)
      LOG_WARN << "F: WHAT!?!  mult.matched cell in single cell list " << tray << " " << module << " " << cell << endm;

    if(mHisto) {
      mTracksPerCellMatch3->Fill(FinalMatchedCellsVec[ii].trackIdVec.size());
//      mDaqOccupancyMatch3->Fill((module-1)*mNCell+(cell-1));
      mDeltaHitMatch3->Fill(dy, dz);
      mDeltaHitFinal[tray-1]->Fill(dy,dz);      
    }

    // get track-id from cell hit vector
    unsigned int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
    StTrack *globalTrack = nodes[trackNode]->track(global);
    if(!globalTrack) {
      LOG_WARN << "Wrong global track!" << endm;
      continue;
    }

    // Fill association in TOF Hit Collection
    //fg StBTofHit *tofHit = dynamic_cast<StBTofHit *>(tofHits[FinalMatchedCellsVec[ii].index2BTofHit]);
    StBTofHit *tofHit = tofHits[FinalMatchedCellsVec[ii].index2BTofHit];
    if(tofHit->tray()!=tray || tofHit->module()!=module || tofHit->cell()!=cell) {
      LOG_WARN << "Wrong hit in the BTofHitCollection!" << endm;
      continue;
    }
    nValidSingleHitCells++;

    StTrack *theTrack = nodes[trackNode]->track(primary);
    if(theTrack) nValidSinglePrimHitCells++;

    ///
    tofHit->setAssociatedTrack(globalTrack);

    // Fill the matched data in StBTofPidTraits
    StBTofPidTraits *pidTof = new StBTofPidTraits();
    pidTof->setTofHit(tofHit);
    pidTof->setMatchFlag(FinalMatchedCellsVec[ii].matchFlag);
    pidTof->setYLocal(dy);
    pidTof->setZLocal(FinalMatchedCellsVec[ii].zhit);
    pidTof->setPosition(FinalMatchedCellsVec[ii].hitPosition);

    globalTrack->addPidTraits(pidTof);

  } // end final matched cells
  
  if(mHisto) {
    mCellsPrimaryPerEventMatch3->Fill(nValidSinglePrimHitCells);
  }
  
  LOG_DEBUG << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm;
 // end of Sect.F

  LOG_INFO << " #(daq hits): " << daqCellsHitVec.size()
       << "\t#(proj hits): " << allCellsHitVec.size()
       << "\t#(prim proj hits): " << nPrimaryHits
       << "\n#(matched hits): " << FinalMatchedCellsVec.size() 
       << "\n#(single valid hits): " << nValidSingleHitCells
       << "\t#(single prim valid hits): " << nValidSinglePrimHitCells
       << endm;

  //check StEvent collections --
  if (theTof->hitsPresent()){
    gMessMgr->Info("","OS") << " BTofCollection: hit container present."<<endm;
    if (Debug()){
      StSPtrVecBTofHit& tmpCellTofVec = theTof->tofHits();
      gMessMgr->Info("","OS") << " # of hits in this event:" << tmpCellTofVec.size() << endm;
      for (size_t i = 0; i < tmpCellTofVec.size(); i++) {
	StBTofHit* p = tmpCellTofVec[i];
        LOG_INFO << (*p) << endm;
      }
    }
  }
  //-- end check


  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time for StBTofMatchMaker::Make(): "
	 << timer.elapsedTime() << " sec" << endm;
  }

  LOG_INFO << "StBTofMatchMaker -- bye-bye" << endm;

  return kStOK;
}

//---------------------------------------------------------------------------
// Book histograms and create ordered collection for easy manipulation
void StBTofMatchMaker::bookHistograms(void){

  mEventCounterHisto = new TH1D("eventCounter","eventCounter",20,0,20);

  mCellsMultInEvent = new TH1D("cellsPerEvent","cellsPerEvent",1000,0,1000);
  mHitsMultInEvent  = new TH1D("hitsPerEvent","hitsPerEvent",1000,0,1000);
  mHitsPrimaryInEvent  = new TH1D("hitsPrimaryPerEvent","hitsPrimaryPerEvent",1000,0,1000);
  mHitsMultPerTrack = new TH1D("hitsPerTrack","hitsPerTrack",10,0,10);
  mHitsPosition     = new TH2D("hitsPosition","hitsPositions",300,-15.,15.,200,-5.,5.);

  // occupancy
  for(int i=0;i<mNTray;i++) {
    char hisname[100];
    sprintf(hisname,"Occupancy_Tray_%d",i+1);
    mDaqOccupancy[i]     = new TH1D(hisname,"",192,0,192);
    sprintf(hisname,"OccupancyProj_Tray_%d",i+1);
    mDaqOccupancyProj[i] = new TH1D(hisname,"",192,0,192);
  }

  // correlation
  for(int i=0;i<mNTray;i++) {
    char hisname[100];
    sprintf(hisname,"Corr_Tray_%d",i+1);
    mHitCorr[i] = new TH2D(hisname,"",192,0,192,192,0,192);
    sprintf(hisname,"Corr_Tray_%d_module",i+1);
    mHitCorrModule[i] = new TH2D(hisname,"",32,0,32,32,0,32);
  }

  // project hit position
  for(int i=0;i<mNTray;i++) {
    char hisname[100];
    sprintf(hisname,"LocalYZ_Tray_%d",i+1);
    mDeltaHitFinal[i] = new TH2D(hisname,"",300,-15.,15.,200,-5.,5.);
  }

  // TPC tracks
  if(mSaveTree) {
    mTrackTree = new TTree("track","track");
    mTrackTree->Branch("pt",&trackTree.pt,"pt/F");
    mTrackTree->Branch("eta",&trackTree.eta,"eta/F");
    mTrackTree->Branch("phi",&trackTree.phi,"phi/F");
    mTrackTree->Branch("nfitpts",&trackTree.nfitpts,"nfitpts/I");
    mTrackTree->Branch("dEdx",&trackTree.dEdx,"dEdx/F");
    mTrackTree->Branch("ndEdxpts",&trackTree.ndEdxpts,"ndEdxpts/I");
    mTrackTree->Branch("charge",&trackTree.charge,"charge/I");
    mTrackTree->Branch("projTrayId",&trackTree.projTrayId,"projTrayId/I");
    mTrackTree->Branch("projCellChan",&trackTree.projCellChan,"projCellChan/I");
    mTrackTree->Branch("projY",&trackTree.projY,"projY/F");
    mTrackTree->Branch("projZ",&trackTree.projZ,"projZ/F");
  }

  mTrackPtEta = new TH2D("trackPtEta","",100,0.,5.,60,-1.5,1.5);
  //mTrackPtPhi = new TH2D("trackPtPhi","",100,0.,5.,120,0.,2*3.14159);
  mTrackPtPhi = new TH2D("trackPtPhi","",100,0.,5.,120,0.,2.*M_PI);
  mTrackNFitPts = new TH1D("trackNFitPts","",50,0.,50.);
  mTrackdEdxvsp = new TH2D("trackdEdxvsp","",500,0.,5.,1000,0.,10.);
  mNSigmaPivsPt = new TH2D("nSigmaPivsPt","",500,0.,5.,1000,-10.,10.);

  // association  
  mCellsPerEventMatch1 = new TH1D("cellsPerEventMatch1","cellPerEventMatch1",100,0,100);
  mHitsPerEventMatch1 = new TH1D("hitsPerEventMatch1","hitsPerEventMatch1",100,0,100);
  mCellsPerTrackMatch1 = new TH1D("cellsPerTrackMatch1","cellsPerTrackMatch1",100,0,100);
  mTracksPerCellMatch1 = new TH1D("tracksPerCellMatch1","tracksPerCellMatch1",100,0,100);
  mDeltaHitMatch1 = new TH2D("deltaHitMatch1","deltaHitMatch1",300,-15,15,200,-5.,5.);

  // kick out multi-hit
  mCellsPerEventMatch2 = new TH1D("cellsPerEventMatch2","cellPerEventMatch2",100,0,100);
  mHitsPerEventMatch2 = new TH1D("hitsPerEventMatch2","hitsPerEventMatch2",100,0,100);
  mCellsPerTrackMatch2 = new TH1D("cellsPerTrackMatch2","cellsPerTrackMatch2",100,0,100);
  mTracksPerCellMatch2 = new TH1D("tracksPerCellMatch2","tracksPerCellMatch2",100,0,100);
  mDeltaHitMatch2 = new TH2D("deltaHitMatch2","deltaHitMatch2",300,-15,15,200,-5.,5.);

  // sort out multi matched cells
  mCellsPerEventMatch3 = new TH1D("cellsPerEventMatch3","cellsPerEventMatch3",100,0,100);
  mHitsPerEventMatch3 = new TH1D("hitsPerEventMatch3","hitsPerEventMatch3",100,0,100);
  mCellsPerTrackMatch3 = new TH1D("cellsPerTrackMatch3","cellsPerTrackMatch3",100,0,100);
  mTracksPerCellMatch3 = new TH1D("tracksPerCellMatch3","tracksPerCellMatch3",100,0,100);
  mDeltaHitMatch3 = new TH2D("deltaHitMatch3","deltaHitMatch3",300,-15,15,200,-5.,5.);

  mCellsPrimaryPerEventMatch3 = new TH1D("cellsPrimaryPerEventMatch3","cellsPrimaryPerEventMatch3",100,0,100);

  return;
}


//---------------------------------------------------------------------------
// store histograms in a seperate root file
void StBTofMatchMaker::writeHistogramsToFile(){
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  LOG_INFO << "StBTofMatchMaker::writeHistogramsToFile()"
       << " histogram file " <<  mHistoFileName << endm;

  theHistoFile->cd();

  if(mHisto) {


    for(int i=0;i<mNTray;i++) {
      mDaqOccupancy[i]->Write();
      mDaqOccupancyProj[i]->Write();
      mHitCorr[i]->Write();
      mHitCorrModule[i]->Write();
      mDeltaHitFinal[i]->Write();
    }

    mEventCounterHisto->Write();
    mCellsMultInEvent->Write();
    mHitsMultInEvent->Write();
    mHitsPrimaryInEvent->Write();
    mHitsMultPerTrack->Write();
    mHitsPosition->Write();

    mTrackPtEta->Write();
    mTrackPtPhi->Write();
    mTrackNFitPts->Write();
    mTrackdEdxvsp->Write();
    mNSigmaPivsPt->Write();

    mCellsPerEventMatch1->Write();
    mHitsPerEventMatch1->Write();
    mCellsPerTrackMatch1->Write();
    mTracksPerCellMatch1->Write();
    mDeltaHitMatch1->Write();
    
    mCellsPerEventMatch2->Write();
    mHitsPerEventMatch2->Write();
    mCellsPerTrackMatch2->Write();
    mTracksPerCellMatch2->Write();
    mDeltaHitMatch2->Write();

    mCellsPerEventMatch3->Write();
    mHitsPerEventMatch3->Write();
    mCellsPerTrackMatch3->Write();
    mTracksPerCellMatch3->Write();
    mDeltaHitMatch3->Write();

    mCellsPrimaryPerEventMatch3->Write();
    
    theHistoFile->Write();  
    theHistoFile->Close(); 
   
  }

  if(mSaveTree) {
    TFile *theTreeFile =  new TFile((mHistoFileName+".tree.root").c_str(), "RECREATE");
    theTreeFile->cd();
    mTrackTree->Write();
    theTreeFile->Write();
    theTreeFile->Close();
  }

  return;
}

//---------------------------------------------------------------------------
// determine whether this is a valid TOF beam event
bool StBTofMatchMaker::validEvent(StEvent *event){
  mEventCounter++;
  // 1. must have non-zero pointer
  if (!event) return false;
  if(mHisto) mEventCounterHisto->Fill(1);

  // 2. must have a valid primary vertex 
//  if (!event->primaryVertex()) return false;
  mAcceptedEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(2);

  // 3a. must have TOF collection
  if (!event->btofCollection()){
    LOG_WARN << "TOF is not present" << endm;
    return false;
  }
  if(mHisto) mEventCounterHisto->Fill(3);

  // 3b. must have TOF raw data available
  if (!(event->btofCollection()->rawHitsPresent()) ) {
    LOG_WARN << "TOF is present but no Raw Hits" << endm;
    if  (!(event->btofCollection()->hitsPresent())){
      LOG_WARN << "              and no Cell Data" << endm;
      return false;
    }
    return false;
  }
  mTofEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(4);
  
  if(mHisto) mEventCounterHisto->Fill(5);
  
  mAcceptAndBeam++;

  // and we have a winner!
  LOG_INFO << "TOF present ... and valid beam event" << endm;

  return true;
}


//---------------------------------------------------------------------------
// determine whether this is a valid TPC track
bool StBTofMatchMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track
  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. minimum #fit points over #maximum points
  //fg float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
  float ratio = (float)track->fitTraits().numberOfFitPoints(kTpcId) / (1.0*track->numberOfPossiblePoints(kTpcId));
  if (ratio < mMinFitPointsOverMax) return false;

  return true;
}

//---------------------------------------------------------------------------
// returns the proper track geometry, based on a global user setting
StTrackGeometry* StBTofMatchMaker::trackGeometry(StTrack* track){
  // returns apropriate StTrackGeometry (standard or outerGeometry)
  if (!track) return 0;
  StTrackGeometry *thisTrackGeometry;
  if (mOuterTrackGeometry)
    thisTrackGeometry = track->outerGeometry();
  else
    thisTrackGeometry = track->geometry();
  return thisTrackGeometry;
}
