/*******************************************************************
 *
 * $Id: StBTofMatchMaker.cxx,v 1.21 2018/03/16 18:38:49 genevb Exp $
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
 * Revision 1.21  2018/03/16 18:38:49  genevb
 * Use TGeo initializer for BTof geometry
 *
 * Revision 1.20  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.19  2012/05/07 14:11:16  fisyak
 * Keep btofGeometry in const area for future use
 *
 * Revision 1.18  2011/08/04 19:14:02  geurts
 * Bug fix: allow ideal geometry setting in the case an alignment file is used [Patrick]
 *
 * Revision 1.17  2011/07/27 16:13:58  geurts
 * Alignment calibration modifications [Patrick Huck]:
 *  -  modified to open the local Z window cut to determine the z offset
 *  -  variables mZLocalCut, mCalculateAlign and mAlignFileName added
 *  -  functions setCalculateAlign and setAlignFileName added
 *
 * Revision 1.16  2010/08/09 19:18:45  geurts
 * Include local theta calculation in CellHit structure. Pass LocalTheta info on to TOF PID traits. [Masa]
 *
 * Revision 1.15  2010/07/16 04:25:16  geurts
 * initialize mUseIdealGeometry to be kFALSE in ctor
 *
 * Revision 1.14  2010/07/14 20:35:21  geurts
 * introduce switch to enable ideal MC geometry, without alignment updates. Default: disabled
 *
 * Revision 1.13  2010/05/25 22:09:38  geurts
 * improved database handling and reduced log output
 *
 * Revision 1.12  2010/05/20 22:58:47  geurts
 * Keep BTofMatchMaker from crashing ungracefully when no mEvent or BTOF Collection is found
 *
 * Revision 1.11  2010/03/22 19:40:28  dongx
 * Fixed a bug in setting index2Primary in processMuDst
 *
 * Revision 1.10  2010/03/19 22:25:39  dongx
 * - Added getBTofGeom() function for outside use
 * - Remove AddConst(btofGeometry) to avoid crash due to duplication
 * - TOT selection window opened to 40 ns
 * - Added CPU timer printouts for processStEvent() funciton
 *
 * Revision 1.9  2010/03/04 21:59:27  dongx
 * Further addition in the initial clean up for primary tracks too
 *
 * Revision 1.8  2010/03/04 21:40:54  dongx
 * Added clean up in processMuDst to remove associations done before
 *
 * Revision 1.7  2010/03/04 03:54:28  dongx
 * Further improvement on the processMuDst speed in Step F.
 *
 * Revision 1.6  2010/03/04 00:08:47  dongx
 * Removed primary check for globals at projection in accessing MuDst function as it takes significant CPU time. Waiting for MuDst update
 * Added some more debugging output for CPU time usage
 *
 * Revision 1.5  2010/02/25 05:17:10  dongx
 * Geometry initalization moved from Init() to InitRun()
 *
 * Revision 1.4  2009/09/15 00:30:45  dongx
 * 1) Added the functionality to perform the matching with MuDst directly.
 * 2) Several updates on the track cuts used for matching
 *    - flag<1000 was added
 *    - nHits>15 cut was removed
 * 3) Created a new StBTofPidTraits for any primary track
 * 4) Local Z window cut set to symmetric (fabs(localz)<3.05)
 * 5) Some small changes in the LOGGER output.
 *
 * Revision 1.3  2009/08/26 20:33:56  dongx
 * Geometry init moved to Init() function, also allow reading in from others
 *
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
#include "TGeoManager.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"

#include "StBTofMatchMaker.h"
//#include "TMemStat.h"


//---------------------------------------------------------------------------
StBTofMatchMaker::StBTofMatchMaker(const Char_t *name): StMaker(name){
  // set default values
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mAcceptAndBeam = 0;

  mBTofGeom = 0;

  mWidthPad = 3.45;
  mZLocalCut = 3.05;

  setOuterTrackGeometry();
  setMinHitsPerTrack(15);
  setMinFitPointsPerTrack(15);
  setMinFitPointsOverMax(0.52);
  setMaxDCA(9999.);

  setCreateHistoFlag(kFALSE);
  setHistoFileName("tofana.root");
  setCreateTreeFlag(kFALSE);
  setSaveGeometry(kFALSE);
  mInitFromOther = kFALSE;
  mUseIdealGeometry = kFALSE;
  mCalculateAlign   = kFALSE;
  setAlignFileName("");
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;

  mEvent = 0;
  mMuDst = 0;
  mMuDstIn = kFALSE;
}

StBTofMatchMaker::~StBTofMatchMaker(){ /* nope */}

//void StBTofMatchMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::Init(){
  LOG_INFO << "Initializing match settings:" << endm;
  LOG_INFO << "  Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
  LOG_INFO << "  Maximum DCA: " << mMaxDCA << endm;
  
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

  // for alignment calculate, we start from ideal geometry
  if(mCalculateAlign) {
    if (mAlignFileName=="") mUseIdealGeometry = kTRUE;
    mZLocalCut = 5.0;
  }

  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::InitRun(Int_t runnumber){

  // determine TOF configuration from run#
  LOG_INFO << "Initializing BTOF Geometry:" << endm;
  /////////////////////////////////////////////////////////////////////
  // TOF geometry initializtion -- from GEANT geometry directly
  //                               need St_geant_Maker be loaded before
  /////////////////////////////////////////////////////////////////////
  StTimer geomTimer;
  geomTimer.start();
  mBTofGeom = 0;
  if(TDataSet *geom = GetDataSet("btofGeometry")) {
    mBTofGeom = (StBTofGeometry *)geom->GetObject();
    LOG_INFO << " Found btofGeometry ... " << endm; 
    mInitFromOther = kTRUE;
  } else {
    mBTofGeom = new StBTofGeometry("btofGeometry","btofGeometry in MatchMaker");
    LOG_INFO << " Create a new btofGeometry ... " << endm;
    AddConst(new TObjectSet("btofGeometry",mBTofGeom));
  } 
  if(mBTofGeom && !mBTofGeom->IsInitDone()) {
    LOG_INFO << " BTofGeometry initialization ... " << endm;
    //fg if(runnumber<1000000) mBTofGeom->SetMCOn();
    if (mUseIdealGeometry) mBTofGeom->SetMCOn();
    else                   mBTofGeom->SetMCOff();
    LOG_INFO << " Alignment file: " << mAlignFileName.c_str() << endm;
    mBTofGeom->SetAlignFile(mAlignFileName.c_str());
      TVolume *starHall = gGeoManager ? nullptr : (TVolume *) GetDataSet("HALL");
      mBTofGeom->Init(this, starHall, gGeoManager);
  }

  geomTimer.stop();
  LOG_INFO << "CPU time for StBTofGeometry initialization : " << geomTimer.elapsedTime() << " sec" << endm;

  return kStOK;
}

//----------------------------------------------------------------------------
Int_t StBTofMatchMaker::FinishRun(Int_t runnumber){

  LOG_INFO << "StBTofMatchMaker -- cleaning up geometry (Finish)" << endm;
  if (!mInitFromOther && mBTofGeom) delete mBTofGeom;
  mBTofGeom=0;

  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchMaker::Finish(){

  LOG_DEBUG << "StBTofMatchMaker -----  RUN SUMMARY ----- (Finish)\n"
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

  if(mMuDstIn) processMuDst();
  else         processStEvent();

  return kStOK;
}

//---------------------------------------------------------------------------
void StBTofMatchMaker::processStEvent(){

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
  if(!mEvent || !(mEvent->btofCollection()) || !(mEvent->btofCollection()->hitsPresent()) ) {
    if (!mEvent) {LOG_INFO << "no StEvent" << endm;}
    else
      if (!(mEvent->btofCollection())) {LOG_INFO << "no BTof Collection" << endm;}
    else
      if (!(mEvent->btofCollection()->hitsPresent()) ) LOG_INFO << "no BTOF hits present" << endm;
    LOG_INFO << "StBTofMatchMaker -- nothing to do ... bye-bye" << endm;
    return;
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
  LOG_INFO << " Number of BTOF Hits = " << tofHits.size() << endm;
  for(size_t i=0;i<tofHits.size();i++) {
    //fg StBTofHit* aHit = dynamic_cast<StBTofHit *>(tofHits[i]);
    StBTofHit* aHit = tofHits[i];
     if(!aHit) continue;
     if(aHit->tray()<=0||aHit->tray()>mNTray) continue;  // barrel tray hits

     int trayId = aHit->tray();
     int moduleId = aHit->module();
     int cellId = aHit->cell();

     if(Debug()) { LOG_INFO <<"A: fired hit in " << " tray="<< trayId << " module="<<moduleId<<" cell="<<cellId<<endm; }

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
  if(Debug()) { 
    LOG_INFO << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_DEBUG << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
//  if(!daqCellsHitVec.size()) return;
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step A - loading hits : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
  }

  //.........................................................................
  /// B. loop over global tracks and determine all cell-track matches
  //
  tofCellHitVector allCellsHitVec;
  StructCellHit cellHit;

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
      DoubleVec pathVec, thetaVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      if(mBTofGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec,thetaVec) ) {
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
            if (fabs(local[2])<mZLocalCut) {   // to be consistent with GEANT geometry
                                         // and the alignment calibration
//	    if (local[2]<=2.7&&local[2]>=-3.4) {
//            if (fabs(local[2])<4.) {   // loose local z cut at first step
	      ncells++;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back((Int_t)iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHit.theta = (Double_t)thetaVec[i];
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

	      if(Debug()) {
                LOG_DEBUG <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
	        LOG_DEBUG <<"   hit position " << hitPos << endm;
	        // LOG_DEBUG <<"   momemtum= " << pt << " " << eta << " " << phi << endm;
              }
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

      if(mHisto && mSaveTree) mTrackTree->Fill();

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug()) { LOG_INFO << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nodes.size() << endm; }
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    mHitsPrimaryInEvent->Fill(nPrimaryHits);
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step B - projection : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
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
	cellHit.theta = proIter->theta;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  if(Debug()) { LOG_INFO << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm; }
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step C - matching : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
  }

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
    cellHit.theta = tempIter->theta;

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
    
    if(Debug()) { 
      LOG_DEBUG << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_DEBUG << " " << *ij; ij++; }
      LOG_DEBUG << endm;
    }

    tempVec = erasedVec;
  }
  if(Debug()) { LOG_INFO << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm; }
  //end of Sect.D

  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step D - erasing : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
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
    vector<Double_t> vtot, vtheta;
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
	vtheta.push_back(erasedIter->theta);

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
      cellHit.theta = vtheta[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      if(Debug()) {
        LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_DEBUG << " " << *ij; ij++; }
        LOG_DEBUG << endm;
      }
      
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on tot
      Float_t tot(0.);
      vector<Int_t> ttCandidates;
      for (Int_t i=0;i<nCells;i++) {
        Double_t tt = vtot[i];
        if(tt<40.&&tt>tot) {    // open the ToT cut to 40 ns
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
	cellHit.theta = vtheta[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	if(Debug()) { LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm; }
      }

    } else {
      LOG_WARN << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  if(Debug()) { LOG_INFO << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm; }
  // end of Sect.E
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step E - sorting : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
  }

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
    int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
    StGlobalTrack *globalTrack = dynamic_cast<StGlobalTrack*>(nodes[trackNode]->track(global));
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

    ///
    tofHit->setAssociatedTrack(globalTrack);

    // Fill the matched data in StBTofPidTraits
    StBTofPidTraits *pidTof = new StBTofPidTraits();
    pidTof->setTofHit(tofHit);
    pidTof->setMatchFlag(FinalMatchedCellsVec[ii].matchFlag);
    pidTof->setYLocal(dy);
    pidTof->setZLocal(FinalMatchedCellsVec[ii].zhit);
    pidTof->setPosition(FinalMatchedCellsVec[ii].hitPosition);
    pidTof->setThetaLocal(FinalMatchedCellsVec[ii].theta);
    globalTrack->addPidTraits(pidTof);

    StPrimaryTrack *pTrack = dynamic_cast<StPrimaryTrack*>(nodes[trackNode]->track(primary));
    if(pTrack) {
      nValidSinglePrimHitCells++;
      StBTofPidTraits *ppidTof = new StBTofPidTraits();
      ppidTof->setTofHit(tofHit);
      ppidTof->setMatchFlag(FinalMatchedCellsVec[ii].matchFlag);
      ppidTof->setYLocal(dy);
      ppidTof->setZLocal(FinalMatchedCellsVec[ii].zhit);
      ppidTof->setPosition(FinalMatchedCellsVec[ii].hitPosition);
      ppidTof->setThetaLocal(FinalMatchedCellsVec[ii].theta);
      pTrack->addPidTraits(ppidTof);
    }

  } // end final matched cells
  
  if(mHisto) {
    mCellsPrimaryPerEventMatch3->Fill(nValidSinglePrimHitCells);
  }
  
  if(Debug()) { LOG_INFO << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm; }
 // end of Sect.F
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step F - final : "
         << timer.elapsedTime() << " sec" << endm;
    timer.start();
  }

  LOG_INFO << "#(daq): " << daqCellsHitVec.size()
       << " #(proj): " << allCellsHitVec.size()
       << " #(prim proj): " << nPrimaryHits
       << " #(matched): " << FinalMatchedCellsVec.size() 
       << " #(single valid): " << nValidSingleHitCells
       << " #(single prim valid): " << nValidSinglePrimHitCells
       << endm;

  //check StEvent collections --
  if (theTof->hitsPresent()){
    LOG_DEBUG << " BTofCollection: hit container present."<<endm;
    if (Debug()){
      StSPtrVecBTofHit& tmpCellTofVec = theTof->tofHits();
      LOG_INFO << " # of hits in this event:" << tmpCellTofVec.size() << endm;
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

  LOG_DEBUG << "StBTofMatchMaker -- bye-bye" << endm;

  return;
}

//---------------------------------------------------------------------------
void StBTofMatchMaker::processMuDst(){

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
  if(!mMuDstMaker) {
    LOG_WARN << " No MuDstMaker ...  bye-bye ... " << endm;
    return;
  }
  mMuDst = mMuDstMaker->muDst();
  if(!mMuDst) {
    LOG_WARN << " No MuDst ... bye-bye" << endm;
    return;
  }

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // read data from StBTofHit
  /// A. build vector of candidate cells
  //
  tofCellHitVector daqCellsHitVec;
  idVector validModuleVec;

  // multi-tray system
  Int_t nhits = mMuDst->numberOfBTofHit();
  LOG_INFO << " Number of BTOF Hits = " << nhits << endm;
  if(mHisto&&nhits>0) mEventCounterHisto->Fill(1);
  for(int i=0;i<nhits;i++) {
     StMuBTofHit *aHit = (StMuBTofHit*)mMuDst->btofHit(i);
     if(!aHit) continue;
     if(aHit->tray()<=0||aHit->tray()>mNTray) continue;  // barrel tray hits

     // clean up any association done before
     aHit->setIndex2Primary(-1);
     aHit->setIndex2Global(-1);
     aHit->setAssociatedTrackId(-1);

     int trayId = aHit->tray();
     int moduleId = aHit->module();
     int cellId = aHit->cell();

     if(Debug()) { LOG_INFO <<"A: fired hit in " << " tray="<< trayId << " module="<<moduleId<<" cell="<<cellId<<endm; }

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
  if(Debug()) {
    LOG_DEBUG << "    total # of cells = " << daqCellsHitVec.size() << endm;
    for(size_t iv = 0;iv<validModuleVec.size();iv++) {
      LOG_DEBUG << " module # " << validModuleVec[iv] << " Valid! " << endm;
    }
  }
  if(mHisto) {
    mCellsMultInEvent->Fill(daqCellsHitVec.size());
    if(daqCellsHitVec.size()) mEventCounterHisto->Fill(6);
  }
//  if(!daqCellsHitVec.size()) return;
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step A - loading hits : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
  }


  //.........................................................................
  /// B. loop over global tracks and determine all cell-track matches
  //
  Int_t index2Primary[50000]; // map
  memset(index2Primary, -1, sizeof(index2Primary));
  for(int ip=0;ip<(int)mMuDst->array(muPrimary)->GetEntries();ip++) {
    StMuTrack *pTrack = (StMuTrack *)mMuDst->array(muPrimary)->UncheckedAt(ip);
    if(!pTrack) continue;
    int gIndex = pTrack->index2Global();
    if(gIndex<0) continue;
    index2Primary[gIndex] = ip;
  }

  tofCellHitVector allCellsHitVec;
  StructCellHit cellHit;

  Int_t nGlobals = mMuDst->numberOfGlobalTracks();
  Int_t nAllTracks=0;
  Int_t nPrimaryHits = 0;
  for (int iNode=0; iNode<nGlobals; iNode++){
    tofCellHitVector cellHitVec;
    //    cellHitVec.clear();
    StMuTrack *theTrack = mMuDst->globalTracks(iNode);
    if(!theTrack) continue;

    // clean up any association done before
    StMuBTofPidTraits pidTof;
    theTrack->setBTofPidTraits(pidTof);
    theTrack->setIndex2BTofHit(-1);

    bool isPrimary = kFALSE;
    int pIndex = index2Primary[iNode];
    if(pIndex>=0) { 
      isPrimary = kTRUE;
      StMuTrack *pTrack = (StMuTrack *)mMuDst->array(muPrimary)->UncheckedAt(pIndex);
      if(pTrack) {
        pTrack->setBTofPidTraits(pidTof);
        pTrack->setIndex2BTofHit(-1);
      }
    }

    StThreeVectorF mom = theTrack->momentum();
    float pt = mom.perp();
    float eta = mom.pseudoRapidity();
    float phi = mom.phi();
    //fg if (phi<0.) phi += 2.*3.14159;
    if (phi<0.) phi += 2.*M_PI;

    float nSigmaPion = theTrack->nSigmaPion();
    float dEdx = theTrack->dEdx();
    int ndEdxpts = theTrack->nHitsDedx();
    int nfitpts = theTrack->nHitsFit(kTpcId);

    // make sure we have a track, a miniDST might have removed it...
    if (validTrack(theTrack)){
      if(mHisto) {
        mTrackPtEta->Fill(pt, eta);
        mTrackPtPhi->Fill(pt, phi);
        mTrackNFitPts->Fill(nfitpts);
        if(dEdx>0.) mTrackdEdxvsp->Fill(mom.mag(), dEdx*1.e6);
        if(fabs(nSigmaPion)<5.) mNSigmaPivsPt->Fill(pt, nSigmaPion+5.*theTrack->charge());
      }

      if(mSaveTree) {
        trackTree.pt = pt;
        trackTree.eta = eta;
        trackTree.phi = phi;
        trackTree.nfitpts = nfitpts;
        trackTree.dEdx = dEdx*1.e6;
        trackTree.ndEdxpts = ndEdxpts;
        trackTree.charge = theTrack->charge();      
        trackTree.projTrayId = 0;
        trackTree.projCellChan = -1;
        trackTree.projY = -999.;
        trackTree.projZ = -999.;
      }

      nAllTracks++;
      StPhysicalHelixD theHelix = mOuterTrackGeometry ? theTrack->outerHelix() : theTrack->helix();

//      IntVec projTrayVec;
//      if(!mBTofGeom->projTrayVector(theHelix, projTrayVec)) continue;

      IntVec idVec;
      DoubleVec pathVec, thetaVec;
      PointVec  crossVec;

//       idVec.clear();
//       pathVec.clear();
//       crossVec.clear();

      Int_t ncells = 0;
      if(mBTofGeom->HelixCrossCellIds(theHelix,idVec,pathVec,crossVec,thetaVec) ) {
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
            if (fabs(local[2])<mZLocalCut) {   // to be consistent with GEANT geometry
                                         // and the alignment calibration
//	    if (local[2]<=2.7&&local[2]>=-3.4) {
//            if (fabs(local[2])<4.) {   // loose local z cut at first step
	      ncells++;
	      cellHit.tray = itray;
	      cellHit.module = imodule;
	      cellHit.cell = icell;
	      cellHit.trackIdVec.push_back(iNode);
	      cellHit.hitPosition = glo;        // global position
	      cellHit.zhit = (Float_t)hitPos.z();
	      cellHit.yhit = (Float_t)hitPos.y();
	      cellHit.theta = (Double_t)thetaVec[i];
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

              if(Debug()) {
     	        LOG_DEBUG <<"B: nodeid=" << iNode << "  projected in " << " tray="<< itray << " module="<<imodule<<" cell="<<icell<<endm;
  	        LOG_DEBUG <<"   hit position " << hitPos << endm;
	        // LOG_DEBUG <<"   momemtum= " << pt << " " << eta << " " << phi << endm;
              }
	    }
	} // for (Int_t i=0...)
      } // endif(helixcross...)
      if(ncells>0&&mHisto) mHitsMultPerTrack->Fill(ncells);

      if(mHisto && mSaveTree) mTrackTree->Fill();

    } // if(ValidTrack).. 
  } // loop over nodes
  if(Debug()) { LOG_INFO << "B:  matched/available/total #tracknodes: " <<allCellsHitVec.size() << "/" <<nAllTracks << "/" << nGlobals << endm; }
  if(mHisto) {
    mHitsMultInEvent->Fill(allCellsHitVec.size());
    mHitsPrimaryInEvent->Fill(nPrimaryHits);
    if(allCellsHitVec.size()) mEventCounterHisto->Fill(7);
  }
  // end of Sect.B
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step B - projection : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
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
    cellHit.theta = proIter->theta;
	matchHitCellsVec.push_back(cellHit);
      }
    }
  } //end {sec. C}
  if(Debug()) { LOG_INFO << "C: before/after: " << allCellsHitVec.size() << "/" << matchHitCellsVec.size() << endm; }
  if(mHisto&&matchHitCellsVec.size()) mEventCounterHisto->Fill(8);
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step C - matching : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
  }

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
    cellHit.theta = tempIter->theta;

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
    
    if(Debug()) {
      LOG_DEBUG << "D: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
      idVectorIter ij=trackIdVec.begin();
      while (ij != trackIdVec.end()) { LOG_DEBUG << " " << *ij; ij++; }
      LOG_DEBUG << endm;
    }

    tempVec = erasedVec;
  }
  if(Debug()) { LOG_INFO << "D: before/after: " << matchHitCellsVec.size() << "/" << singleHitCellsVec.size() << endm; }
  //end of Sect.D

  if(mHisto) {
    mCellsPerEventMatch1->Fill(singleHitCellsVec.size()+multiHitsCellsVec.size());
    if(singleHitCellsVec.size()) mEventCounterHisto->Fill(9);
  } 
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step D - erasing : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
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
    vector<Double_t> vtot, vtheta;
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
    vtheta.push_back(erasedIter->theta);

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
      cellHit.theta = vtheta[0];

      FinalMatchedCellsVec.push_back(cellHit);

      // debugging output
      if(Debug()) {
        LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:";
	idVectorIter ij=vTrackId.begin();
	while (ij != vTrackId.end()) { LOG_DEBUG << " " << *ij; ij++; }
        LOG_DEBUG << endm;
      }
      
    }
    else if (nCells>1){   // for multiple hit cells  find the most likely candidate.
      Int_t thiscandidate(-99);
      Int_t thisMatchFlag(0);

      // sort on tot
      Float_t tot(0.);
      vector<Int_t> ttCandidates;
      for (Int_t i=0;i<nCells;i++) {
        Double_t tt = vtot[i];
        if(tt<40.&&tt>tot) {    // open the ToT to 40 ns
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
    cellHit.theta = vtheta[thiscandidate];

	FinalMatchedCellsVec.push_back(cellHit);
	
	// debugging output
	if(Debug()) { LOG_DEBUG << "E: itray=" << cellHit.tray << " imodule=" << cellHit.module << " icell=" << cellHit.cell << "\ttrackid:" << vTrackId[thiscandidate] << endm; }
      }

    } else {
      LOG_WARN << "E: no cells belong to this track ... should not happen!" << endm;
    }

    tempVec = erasedVec;
  }

  if(Debug()) { LOG_INFO << "E: before/after: " << singleHitCellsVec.size() << "/" << FinalMatchedCellsVec.size() << endm; }
  // end of Sect.E
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step E - sorting : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
  }

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
    int trackNode = FinalMatchedCellsVec[ii].trackIdVec[0];
    StMuTrack *globalTrack = mMuDst->globalTracks(trackNode);
    if(!globalTrack) {
      LOG_WARN << "Wrong global track!" << endm;
      continue;
    }

    // Fill association in TOF Hit Collection
    StMuBTofHit *tofHit = mMuDst->btofHit(FinalMatchedCellsVec[ii].index2BTofHit);
    if(tofHit->tray()!=tray || tofHit->module()!=module || tofHit->cell()!=cell) {
      LOG_WARN << "Wrong hit in the MuBTofHit!" << endm;
      continue;
    }
    nValidSingleHitCells++;

    /// set cross-indices
    tofHit->setAssociatedTrackId(globalTrack->id());
    tofHit->setIndex2Global(trackNode);
    globalTrack->setIndex2BTofHit(FinalMatchedCellsVec[ii].index2BTofHit);

    int ip = index2Primary[trackNode];
    StMuTrack *primaryTrack = 0;
    if(ip>=0) {
      nValidSinglePrimHitCells++;
      tofHit->setIndex2Primary(ip);
      primaryTrack = (StMuTrack *)mMuDst->array(muPrimary)->UncheckedAt(ip);
      if(primaryTrack) {
        primaryTrack->setIndex2BTofHit(FinalMatchedCellsVec[ii].index2BTofHit);
      }
    }

    // Fill the matched data in StBTofPidTraits
    StMuBTofPidTraits pidTof = globalTrack->btofPidTraits();
    pidTof.setMatchFlag(FinalMatchedCellsVec[ii].matchFlag);
    pidTof.setYLocal(dy);
    pidTof.setZLocal(FinalMatchedCellsVec[ii].zhit);
    pidTof.setPosition(FinalMatchedCellsVec[ii].hitPosition);
    pidTof.setThetaLocal(FinalMatchedCellsVec[ii].theta);
    globalTrack->setBTofPidTraits(pidTof);

    if(primaryTrack) {
      StMuBTofPidTraits ppidTof = primaryTrack->btofPidTraits();
      ppidTof.setMatchFlag(FinalMatchedCellsVec[ii].matchFlag);
      ppidTof.setYLocal(dy);
      ppidTof.setZLocal(FinalMatchedCellsVec[ii].zhit);
      ppidTof.setPosition(FinalMatchedCellsVec[ii].hitPosition);
      ppidTof.setThetaLocal(FinalMatchedCellsVec[ii].theta);
      primaryTrack->setBTofPidTraits(ppidTof);
    }

  } // end final matched cells
  
  if(mHisto) {
    mCellsPrimaryPerEventMatch3->Fill(nValidSinglePrimHitCells);
  }
  
  if(Debug()) { LOG_INFO << "F: before/after" << FinalMatchedCellsVec.size() << "/" <<nValidSinglePrimHitCells << endm; }
 // end of Sect.F
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time after Step F - final : "
	 << timer.elapsedTime() << " sec" << endm;
    timer.start();    
  }

  LOG_INFO << " #(daq hits): " << daqCellsHitVec.size()
       << "\t#(proj hits): " << allCellsHitVec.size()
       << "\t#(prim proj hits): " << nPrimaryHits
       << "\n#(matched hits): " << FinalMatchedCellsVec.size() 
       << "\n#(single valid hits): " << nValidSingleHitCells
       << "\t#(single prim valid hits): " << nValidSinglePrimHitCells
       << endm;

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

  return;
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
// determine whether this is a valid TPC track
bool StBTofMatchMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0 || track->flag()>=1000) return false;

  // 3. minimum #hits per track - obsolete
  //  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. minimum #fit points over #maximum points
  //fg float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
  float ratio = (float)track->fitTraits().numberOfFitPoints(kTpcId) / (1.0*track->numberOfPossiblePoints(kTpcId));
  if (ratio < mMinFitPointsOverMax) return false;

  return true;
}

//---------------------------------------------------------------------------
// determine whether this is a valid TPC track
bool StBTofMatchMaker::validTrack(StMuTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0 || track->flag()>=1000) return false;

  // 3. minimum #hits per track - obsolete
  //  if (track->nHits() < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->nHitsFit(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. minimum #fit points over #maximum points
  //fg float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
  float ratio = (float)track->nHitsFit(kTpcId) / (1.0*track->nHitsPoss(kTpcId));
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
