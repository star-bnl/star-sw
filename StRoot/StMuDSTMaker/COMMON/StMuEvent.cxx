
/***************************************************************************
 *
 * $Id: StMuEvent.cxx,v 1.30 2019/02/21 13:32:54 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include <string.h> 
#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h" 
#include "StEvent/StEventSummary.h" 
#include "StEvent/StEventInfo.h" 
#include "StEvent/StDetectorState.h" 
#include "StEvent/StIstHitCollection.h"
#include "StEvent/StSsdHitCollection.h"
#include "StEvent/StPxlHitCollection.h"
#include "StEvent/StPxlSectorHitCollection.h"
#include "StEvent/StPxlLadderHitCollection.h"

#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuFtpcRefMult.hh"

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StTimer.hh"

#include "StMuException.hh"
#include "StMuEvent.h"
#include "StMuTrack.h"
#include "StMuL3EventSummary.h"
#include "StMuCut.h"
#include "StMuDebug.h"
#include "StMuDst.h"
#include "StMuPrimaryVertex.h"
#include "StMuBTofHit.h"
#include "StMuETofHit.h"
#include "StMuETofDigi.h"

#include "TClonesArray.h"
#include "TObject.h"
#include "TClass.h"

ClassImp(StMuEvent)

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
  StMuEvent::StMuEvent() : mTriggerData(0), mPrimaryVertexError(-999.,-999.,-999) { 
  DEBUGMESSAGE("");
  int n = (char*)mReactionPlanePtWgt - (char*)&mRefMultPos+sizeof(mReactionPlanePtWgt);
  memset(&mRefMultPos,0,n);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::StMuEvent(const StEvent* event) : mPrimaryVertexError(-999.,-999.,-999.) { 
  try {
    fill(event);
  }
  catch (StMuException e) {
    throw e;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::~StMuEvent(){
  DEBUGMESSAGE("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuEvent::clear(){
  DEBUGMESSAGE1("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuEvent::fill(const StEvent* event){
  DEBUGMESSAGE("");
  if ( !event ) throw StMuExceptionNullPointer("no StEvent",__PRETTYF__);
  if ( !event->info() ) throw StMuExceptionNullPointer("no event info",__PRETTYF__);
  if ( !event->runInfo() ) throw StMuExceptionNullPointer("no run info",__PRETTYF__);
  if ( !event->summary() ) throw StMuExceptionNullPointer("no event summary",__PRETTYF__);
//  if (event->numberOfPrimaryVertices() != 1 ) throw StMuExceptionBadValue("!= 1 primary vertex");


  /// classes that we just copy from StEvent
  mRunInfo = *event->runInfo();
  mEventInfo = *event->info();
  mEventSummary = *event->summary();
  const StPrimaryVertex *p_vtx=event->primaryVertex();
  if (p_vtx) {
    mPrimaryVertexError=p_vtx->positionError();
  }

  if ( !event->triggerDetectorCollection() ) {
    DEBUGVALUE2(event->type());
    DEBUGVALUE2(event->info()->time());
    DEBUGMESSAGE2("no trigger detector collection, creating dummy");
  }
  else {
    mVpdTriggerDetector = event->triggerDetectorCollection()->vpd();
    mMtdTriggerDetector = event->triggerDetectorCollection()->mtd();
    mCtbTriggerDetector = event->triggerDetectorCollection()->ctb();
    mZdcTriggerDetector = event->triggerDetectorCollection()->zdc();
    mBbcTriggerDetector = event->triggerDetectorCollection()->bbc();
    mEmcTriggerDetector = event->triggerDetectorCollection()->emc();
    mFpdTriggerDetector = event->triggerDetectorCollection()->fpd();
    mFmsTriggerDetector = event->triggerDetectorCollection()->fms();
  }

  if (event->fpdCollection())
    mFpdCollection = *event->fpdCollection();
  if (event->l0Trigger())
    mL0Trigger = *event->l0Trigger();
  mL3EventSummary.fill(event);

  mTriggerIdCollection.fill( event->triggerIdCollection() ); // pointer check done inside


  mRefMultPos = uncorrectedNumberOfPositivePrimaries(*event);
  mRefMultNeg = uncorrectedNumberOfNegativePrimaries(*event); 
  mRefMultFtpcEast = uncorrectedNumberOfFtpcEastPrimaries(*event);
  mRefMultFtpcWest = uncorrectedNumberOfFtpcWestPrimaries(*event); 

  if (event->triggerData()) // MC has no triggerData
    mL2Result.Set(event->triggerData()->l2ResultLength(),(const Int_t*) event->triggerData()->l2Result());

  // calibrated vpd for TOF - X.Dong
  mVpdEast = mVpdWest = 0;
  mVpdTstart = mVpdTdiff = 0.;
  if (event->tofCollection()) {
    mVpdEast = event->tofCollection()->vpdEast();
    mVpdWest = event->tofCollection()->vpdWest();
    mVpdTstart = event->tofCollection()->tstart();
    mVpdTdiff = event->tofCollection()->tdiff();
    mVpdVz = event->tofCollection()->vzVpd();
  }
  // trigger data
  mTriggerData = const_cast<StTriggerData*>(event->triggerData());  
  if(mTriggerData!=0) mTriggerData->setDebug(0);

  // HFT hits per layer - dongx
  for(int i=0;i<4;i++) mNHitsHFT[i] = 0;
  const StPxlHitCollection* PxlHitCollection = event->pxlHitCollection();
  if(PxlHitCollection) {
    UInt_t numberOfSectors=PxlHitCollection->numberOfSectors();
    for(UInt_t i=0;i<numberOfSectors;i++){
      const StPxlSectorHitCollection* PxlSectorHitCollection=PxlHitCollection->sector(i);
      if(!PxlSectorHitCollection) continue;

      UInt_t numberOfLadders=PxlSectorHitCollection->numberOfLadders();
      for(UInt_t j=0;j<numberOfLadders;j++){
        const StPxlLadderHitCollection* PxlLadderHitCollection=PxlSectorHitCollection->ladder(j);
        if(!PxlLadderHitCollection) continue;

        if(j==0) mNHitsHFT[0] += PxlLadderHitCollection->numberOfHits();
        else     mNHitsHFT[1] += PxlLadderHitCollection->numberOfHits();
      }
    }
  } // PXL hits

  const StIstHitCollection *IstHitCollection = event->istHitCollection();
  if(IstHitCollection) mNHitsHFT[2] += IstHitCollection->numberOfHits();

  const StSsdHitCollection *SsdHitCollection = event->ssdHitCollection();
  if(SsdHitCollection) mNHitsHFT[3] += SsdHitCollection->numberOfHits();
  //  

} 

unsigned short StMuEvent::refMultPos(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultPos;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultPos();
  return 0;
}

unsigned short StMuEvent::refMultNeg(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultNeg;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultNeg();
  return 0;
}

unsigned short StMuEvent::refMult(int vtx_id) {return refMultPos(vtx_id)+refMultNeg(vtx_id);}

unsigned short StMuEvent::refMultFtpcEast(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultFtpcEast;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultFtpcEast();
  return 0;
}

unsigned short StMuEvent::refMultFtpcWest(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultFtpcWest;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultFtpcWest();
  return 0;
}

unsigned short StMuEvent::refMultFtpc(int vtx_id) {return refMultFtpcEast(vtx_id)+refMultFtpcWest(vtx_id);}

StThreeVectorF StMuEvent::primaryVertexPosition(int vtx_id) const {
	StThreeVectorF vz(-999,-999,-999);
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1)){
	 if(fabs(mEventSummary.primaryVertexPosition().x()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().y()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().z()) < 1.e-5) return vz;   
	 else return mEventSummary.primaryVertexPosition();
	}
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->position();
  return vz;
}
StThreeVectorF StMuEvent::primaryVertexErrors(int vtx_id) const {
	StThreeVectorF vz(-999,-999,-999);
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1)){
	 if(fabs(mEventSummary.primaryVertexPosition().x()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().y()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().z()) < 1.e-5) return vz;   
	 else return mPrimaryVertexError;
	}
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->posError();
  return vz;
}

unsigned short StMuEvent::grefmult(int vtx_id){
    unsigned short grefmult = 0;
	StMuTrack *glob;
	//For old MuDsts where there was one vertex per event
	if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1)){
		if(!(fabs(mEventSummary.primaryVertexPosition().x()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().y()) < 1.e-5 && fabs(mEventSummary.primaryVertexPosition().z()) < 1.e-5)){	
			for (int i=0;i<StMuDst::globalTracks()->GetEntriesFast();i++){
				glob = StMuDst::globalTracks(i);
				if (fabs(glob->eta()) <  0.5 && fabs(glob->dcaGlobal().mag()) < 3 && glob->nHitsFit(kTpcId) >= 10) grefmult++;            
			}
			return grefmult;
		}	
		else return 0;
	}	

	if (vtx_id == -1)
		vtx_id = StMuDst::currentVertexIndex();

	if (StMuDst::primaryVertex(vtx_id)){
        for (int i=0;i<StMuDst::globalTracks()->GetEntriesFast();i++){
			glob = StMuDst::globalTracks(i);
			if (fabs(glob->eta()) <  0.5 && fabs(glob->dcaGlobal(vtx_id).mag()) < 3 && glob->nHitsFit(kTpcId) >= 10) grefmult++;            
        }
        return grefmult;
    }
	else return 0;
}

unsigned short StMuEvent::btofTrayMultiplicity(){

	unsigned short btofmult = (unsigned short)StMuDst::numberOfBTofHit();
	for(unsigned int i=0;i< StMuDst::numberOfBTofHit();i++) if(StMuDst::btofHit(i)->tray() > 120) btofmult--;
	return btofmult;

}

unsigned short StMuEvent::etofHitMultiplicity(){
  return (unsigned short) StMuDst::numberOfETofHit();
}
unsigned short StMuEvent::etofDigiMultiplicity(){
  return (unsigned short) StMuDst::numberOfETofDigi();
}

float StMuEvent::nearestVertexZ(int vtx_id){

	float dz = 999.0;
	//For old MuDsts where there was one vertex per event
	if (StMuDst::numberOfPrimaryVertices()==0) return dz;
	const int Nvert = StMuDst::primaryVertices()->GetEntriesFast();
	if(Nvert < 2) return dz;
	
	if (vtx_id == -1) vtx_id = StMuDst::currentVertexIndex();	
	float z =  primaryVertexPosition(vtx_id).z();
	for(int i=0;i<Nvert;i++){
		if(vtx_id!=i) {
			if(fabs(z-StMuDst::primaryVertex(i)->position().z()) < dz) dz = fabs(z-StMuDst::primaryVertex(i)->position().z());
		}
	}
	return dz;
}

/***************************************************************************
 *
 * $Log: StMuEvent.cxx,v $
 * Revision 1.30  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.29  2017/04/17 20:40:56  smirnovd
 * StMuEvent: Declare getters const. They don't modify anything
 *
 * Revision 1.28  2015/03/06 20:02:01  jdb
 * Added 4 unsigned shorts to StMuEvent at request of Xin Dong. Change StMuEvent.{h, cxx}
 *
 * Revision 1.27  2012/11/26 23:14:33  fisyak
 * Replace GetEntries() by GetEntriesFast(), fix print outs
 *
 * Revision 1.26  2011/08/30 14:41:26  fisyak
 * Keep DAQ time for the event untouched
 *
 * Revision 1.25  2010/05/29 16:36:52  tone421
 * Added pointer protection to StTriggerData member
 *
 * Revision 1.24  2010/05/28 19:47:51  tone421
 * Removed a cout needed for test purposes in StMuDstMaker. Made sure StTriggerData objects copied into the MuDst have a debug value of 0..
 *
 * Revision 1.23  2010/05/26 04:25:50  tone421
 * Added StTriggerData arrays in muevent and fixed an issue with PMD arrays being read....
 *
 * Revision 1.22  2010/02/03 17:16:22  tone421
 * Added function StMuEvent::nearestVertexZ(int vtx_id) which returns the z distance of the nearest vertex in relation to vertex vtx_id
 *
 * Revision 1.21  2010/02/03 04:54:45  tone421
 * Added StMuEvent::btofTrayMultiplicity() to return only TOF hits from trays. Should be looked at instead of ctbSum for run 9 and beyond.
 *
 * Revision 1.20  2009/01/09 19:43:47  tone421
 * OAdded gremult in StMuEvent (globals tracks with DCA < 3cm, >= 10 TPC fit hits and |eta| < 0.5)
 *
 * Revision 1.19  2008/11/18 15:34:32  tone421
 * 2 changes. The first ensures StMuEvent::primaryVertexPosition() returns the position of current vertex (set by StMuDst::setVertexIndex(Int_t vtx_id)): previously it returned the position of best ranked vertex. The second insures events with no vertex have a PVx=PYy=PYz=-999 rather than 0.
 *
 * Revision 1.18  2008/06/26 15:46:13  tone421
 *
 * Add getter and setter for the vpd z vertex position
 *
 * Revision 1.17  2008/02/20 09:00:48  mvl
 * Included FMS data (StFMSTriggerDetector) (code by Akio)
 *
 * Revision 1.16  2007/09/21 02:27:12  mvl
 * Added calibrated VPD info from StTofCollection (run-8 prep)
 *
 * Revision 1.15  2007/09/05 23:21:21  mvl
 * Added StMtdTriggerDetector
 *
 * Revision 1.14  2007/04/20 06:25:21  mvl
 * Removed Q-vectors (will implement utility class).
 * Added Vpd info.
 *
 * Revision 1.12  2006/09/20 17:23:39  mvl
 * Added protected for events with StTriggerData (e.g. simulation)
 *
 * Revision 1.11  2006/09/20 01:50:35  mvl
 * Added data member and code for L2Result array (TArrayI).
 *
 * Revision 1.10  2005/08/19 19:46:05  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function taht works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.9  2004/12/02 00:19:52  mvl
 * Added error on primary vertex
 *
 * Revision 1.8  2004/08/04 17:57:13  mvl
 * Added EMC trigger information and fpd trigger (tower) information
 *
 * Revision 1.7  2004/02/17 04:56:36  jeromel
 * Extended help, added crs support, restored __GNUC__ for PRETTY_FUNCTION(checked once
 * more and yes, it is ONLY defined in GCC and so is __FUCTION__),  use of a consistent
 * internal __PRETTYF__, return NULL if no case selected (+message) and protected against
 * NULL mChain.
 *
 * Revision 1.6  2003/10/20 19:50:13  perev
 * workaround added for TClonesArray::Delete + some cleanup of MuEmc
 *
 * Revision 1.5  2003/07/22 19:14:41  laue
 * multiplicities for FTPC added
 *
 * Revision 1.4  2003/02/20 15:29:42  laue
 * StMuTriggerIdCollection added
 *
 * Revision 1.3  2003/02/19 13:51:58  laue
 * added the StTriggerIdCollection
 *
 * Revision 1.2  2002/08/27 19:05:57  laue
 * Minor updates to make the muDst from simulation work
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
