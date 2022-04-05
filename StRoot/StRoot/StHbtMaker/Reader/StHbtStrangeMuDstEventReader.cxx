/***************************************************************************
 *
 * $Id: StHbtStrangeMuDstEventReader.cxx,v 1.2 2001/06/21 19:18:42 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * This is the HbtEventReader class to be used when reading the
 * StStrangeMuDsts produced by the STAR Strangeness group
 *
 ***************************************************************************
 *
 * $Log: StHbtStrangeMuDstEventReader.cxx,v $
 * Revision 1.2  2001/06/21 19:18:42  laue
 * Modified Files: (to match the changed base classes)
 * 	StHbtAsciiReader.cxx StHbtAsciiReader.h
 * 	StHbtAssociationReader.cxx StHbtAssociationReader.h
 *  	StHbtBinaryReader.cxx StHbtBinaryReader.h
 *  	StHbtGstarTxtReader.cxx StHbtGstarTxtReader.h
 *  	StHbtStrangeMuDstEventReader.cxx
 *  	StHbtStrangeMuDstEventReader.h StStandardHbtEventReader.cxx
 * Added Files: new reader
 *  	StHbtTTreeReader.cxx StHbtTTreeReader.h
 *
 * Revision 1.1  2000/12/13 20:45:00  laue
 * New reader to read directly from the StStrangeMuDstMaker's V0 files
 *
 *
 **************************************************************************/
#include "StHbtMaker/Reader/StHbtStrangeMuDstEventReader.h"

#include "StChain.h"

#include <math.h>


#include "StHbtMaker/Reader/StHbtGstarTxtReader.h"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"

#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"  
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
 
#ifdef __ROOT__
ClassImp(StHbtStrangeMuDstEventReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//__________________
StHbtStrangeMuDstEventReader::StHbtStrangeMuDstEventReader(){
  mReaderStatus = 0;  // "good"
  mStrangeMuDstMaker =0;
}
//__________________
StHbtStrangeMuDstEventReader::StHbtStrangeMuDstEventReader(StStrangeMuDstMaker* maker) : mStrangeMuDstMaker(maker) {
  mReaderStatus = 0;  // "good"
}
//__________________
StHbtStrangeMuDstEventReader::~StHbtStrangeMuDstEventReader(){
  if (mEventCut) delete mEventCut;
  if (mV0Cut) delete mV0Cut;
}
//__________________
StHbtString StHbtStrangeMuDstEventReader::Report(){
  StHbtString temp = "\n This is the StHbtStrangeMuDstEventReader\n";
  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
//__________________
StHbtEvent* StHbtStrangeMuDstEventReader::ReturnHbtEvent(){
#ifdef STHBTDEBUG
    cout << " StHbtStrangeMuDstEventReader::ReturnHbtEvent()" << endl;
#endif
  StStrangeEvMuDst* strangeEvMuDst = mStrangeMuDstMaker->GetEvent();
  if (!strangeEvMuDst){
    cout << " StHbtStrangeMuDstEventReader::ReturnHbtEvent() - No StrangeEvMuDst !!! " << endl;
    return 0;
  }

  StHbtEvent* hbtEvent = new StHbtEvent;
  StHbtThreeVector vp = StHbtThreeVector(strangeEvMuDst->primaryVertexX(),strangeEvMuDst->primaryVertexY(),strangeEvMuDst->primaryVertexZ());
  cout << " StHbtStrangeMuDstEventReader::ReturnHbtEvent() - primary vertex : " << vp << endl;
  hbtEvent->SetPrimVertPos(vp);
  hbtEvent->SetNumberOfTracks(strangeEvMuDst->primaryTracks());
  hbtEvent->SetEventNumber(strangeEvMuDst->event());

  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }

  for( int i= 0; i < mStrangeMuDstMaker->GetNV0(); i++){
    StV0MuDst* v0FromMuDst = mStrangeMuDstMaker->GetV0(i);
    //v0FromMuDst->UpdateV0();
    StHbtV0* hbtV0 = new StHbtV0;
    hbtV0->SetdecayLengthV0(v0FromMuDst->decayLengthV0());
    hbtV0->SetdecayVertexV0X(v0FromMuDst->decayVertexV0X());
    hbtV0->SetdecayVertexV0Y(v0FromMuDst->decayVertexV0Y());
    hbtV0->SetdecayVertexV0Z(v0FromMuDst->decayVertexV0Z());
    hbtV0->SetdcaV0Daughters(v0FromMuDst->dcaV0Daughters());
    hbtV0->SetdcaV0ToPrimVertex(v0FromMuDst->dcaV0ToPrimVertex());
    hbtV0->SetdcaPosToPrimVertex(v0FromMuDst->dcaPosToPrimVertex());
    hbtV0->SetdcaNegToPrimVertex(v0FromMuDst->dcaNegToPrimVertex());
    hbtV0->SetmomPosX(v0FromMuDst->momPosX());
    hbtV0->SetmomPosY(v0FromMuDst->momPosY());
    hbtV0->SetmomPosZ(v0FromMuDst->momPosZ());
    hbtV0->SetmomNegX(v0FromMuDst->momNegX());
    hbtV0->SetmomNegY(v0FromMuDst->momNegY());
    hbtV0->SetmomNegZ(v0FromMuDst->momNegZ());
#ifdef STHBTDEBUG
    cout << " hist pos ";
    cout << v0FromMuDst->topologyMapPos().numberOfHits(kTpcId); 
    cout << " hist neg ";
    cout << v0FromMuDst->topologyMapNeg().numberOfHits(kTpcId) << endl;
#endif
    hbtV0->SettpcHitsPos(v0FromMuDst->topologyMapPos().numberOfHits(kTpcId));
    hbtV0->SettpcHitsNeg(v0FromMuDst->topologyMapNeg().numberOfHits(kTpcId));
    hbtV0->SetTrackTopologyMapPos(0,v0FromMuDst->topologyMapPos().data(0));
    hbtV0->SetTrackTopologyMapPos(1,v0FromMuDst->topologyMapPos().data(1));
    hbtV0->SetTrackTopologyMapNeg(0,v0FromMuDst->topologyMapNeg().data(0));
    hbtV0->SetTrackTopologyMapNeg(1,v0FromMuDst->topologyMapNeg().data(1));
    hbtV0->SetkeyPos(v0FromMuDst->keyPos());
    hbtV0->SetkeyNeg(v0FromMuDst->keyNeg());
#ifdef STHBTDEBUG
    cout << " keyPos " << v0FromMuDst->keyPos() << endl;
    cout << " keyNeg " << v0FromMuDst->keyNeg() << endl;
#endif
    hbtV0->SetmomV0X(v0FromMuDst->momV0X());
    hbtV0->SetmomV0Y(v0FromMuDst->momV0Y());
    hbtV0->SetmomV0Z(v0FromMuDst->momV0Z());
#ifdef STHBTDEBUG
    cout << " alpha  ";
    cout << v0FromMuDst->alphaV0();
    cout << " ptArm  ";
    cout << v0FromMuDst->ptArmV0() << endl;
#endif
    hbtV0->SetalphaV0(v0FromMuDst->alphaV0());
    hbtV0->SetptArmV0(v0FromMuDst->ptArmV0());
    hbtV0->SeteLambda(v0FromMuDst->eLambda());
    hbtV0->SeteK0Short(v0FromMuDst->eK0Short());
    hbtV0->SetePosProton(v0FromMuDst->ePosProton());
    hbtV0->SetePosPion(v0FromMuDst->ePosPion());
    hbtV0->SeteNegPion(v0FromMuDst->eNegPion());
    hbtV0->SeteNegProton(v0FromMuDst->eNegProton());
    hbtV0->SetmassLambda(v0FromMuDst->massLambda());
    hbtV0->SetmassAntiLambda(v0FromMuDst->massAntiLambda());
    hbtV0->SetmassK0Short(v0FromMuDst->massK0Short());
    hbtV0->SetrapLambda(v0FromMuDst->rapLambda());
    hbtV0->SetrapK0Short(v0FromMuDst->rapK0Short());
    hbtV0->SetcTauLambda(v0FromMuDst->cTauLambda());
    hbtV0->SetcTauK0Short(v0FromMuDst->cTauK0Short());
    hbtV0->SetptV0(v0FromMuDst->ptV0());
    hbtV0->SetptotV0(v0FromMuDst->ptotV0());
    hbtV0->SetptPos(v0FromMuDst->ptPos());
    hbtV0->SetptotPos(v0FromMuDst->ptotPos());
    hbtV0->SetptNeg(v0FromMuDst->ptNeg());
    hbtV0->SetptotNeg(v0FromMuDst->ptotNeg());
    hbtV0->SetdedxPos(v0FromMuDst->dedxPos());
    hbtV0->SetdedxNeg(v0FromMuDst->dedxNeg());

    
    // By now, all track-wise information has been extracted and stored in hbtTrack
    // see if it passes any front-loaded event cut
    if (mV0Cut){
      if (!(mV0Cut->Pass(hbtV0))){                  // track failed - delete it and skip the push_back
	delete hbtV0;
	continue;
      }
    }
    
    
    hbtEvent->V0Collection()->push_back(hbtV0);
  } // end of loop over strangeness groups v0's
  //Store total number of v0s in v0Mudst so can start from there next time
#ifdef STHBTDEBUG
  cout << " StHbtStrangeMuDstEventReader::ReturnHbtEvent() - " << hbtEvent->V0Collection()->size();
  cout << " V0s pushed in collection " << endl;
#endif
  printf(" StHbtStrangeMuDstEventReader::ReturnHbtEvent() - %8i(%i) V0s pushed into collection \n",
	 hbtEvent->V0Collection()->size(),
	 mStrangeMuDstMaker->GetNV0());

  // There might be event cuts that modify the collections of Tracks or V0 in the event.
  // These cuts have to be done after the event is built. That's why we have the event cut
  // at this point for the second time.
  // An example of this kind of cuts will be an cut that removes spit tracks from the event.
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }
  
  return hbtEvent;
}





















