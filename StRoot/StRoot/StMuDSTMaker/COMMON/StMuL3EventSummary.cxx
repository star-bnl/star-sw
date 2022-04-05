/***************************************************************************
 *
 * $Id: StMuL3EventSummary.cxx,v 1.2 2002/03/14 04:12:55 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
/***************************************************************************
 *
 * $Log: StMuL3EventSummary.cxx,v $
 * Revision 1.2  2002/03/14 04:12:55  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/

#include "TClonesArray.h"

#include "StEvent/StEvent.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StL3EventSummary.h"
#include "StEvent/StL3AlgorithmInfo.h"
#include "StEvent/StPrimaryVertex.h"

#include "StMuEvent.h"
#include "StMuL3EventSummary.h"
#include "StMuException.hh"
#include "StMuDebug.h"

ClassImp(StMuL3EventSummary)

StMuL3EventSummary::StMuL3EventSummary() : mNumberOfProcessedEvents(0), mNumberReconstructedEvents(0),
  mNumberOfTracks(0), mNumberOfAlgorithms(0), mFlags(0), mL0TriggerWord(0), mUnbiasedPreScale(0) { 
  DEBUGMESSAGE("");
  clear();
}

 

void StMuL3EventSummary::fill(const StEvent* ev) {
  DEBUGMESSAGE("");
  clear();
  if ( !(ev->l3Trigger() && ev->l3Trigger()->l3EventSummary()) ) return;
  const StL3EventSummary* l3 = ev->l3Trigger()->l3EventSummary();
  mNumberOfProcessedEvents = l3->numberOfProcessedEvents();
  mNumberReconstructedEvents = l3->numberOfReconstructedEvents();
  mNumberOfTracks =  l3->numberOfTracks();
  mNumberOfAlgorithms = l3->numberOfAlgorithms();
  mFlags |= ( l3->zVertexTrigger()*__VERTEX__  );
  mFlags |= ( l3->unbiasedTrigger()*__UNBIASED__ );
  mL0TriggerWord = l3->l0TriggerWord();
  mUnbiasedPreScale = l3->unbiasedTriggerPreScale();
  if (ev->l3Trigger()->primaryVertex()) 
    mPrimaryVertex = ev->l3Trigger()->primaryVertex()->position();
}

StMuL3EventSummary::~StMuL3EventSummary(){
  DEBUGMESSAGE("");
}

void StMuL3EventSummary::clear(){
  DEBUGMESSAGE("");
  mNumberOfProcessedEvents = 0; 
  mNumberReconstructedEvents = 0;
  mNumberOfTracks = 0;  
  mNumberOfAlgorithms = 0; 
  mFlags = 0; 
  mL0TriggerWord = 0; 
  mUnbiasedPreScale = 0;
  mPrimaryVertex = StThreeVectorF(0.,0.,0.);
}

//! const TClonesArray* StMuL3EventSummary::algorithmsAcceptingEvent() { return StMuEvent::TCAfgAlgAccept; }
//! const TClonesArray* StMuL3EventSummary::algorithmsRejectingEvent() { return StMuEvent::TCAfgAlgReject; }
//! size_t StMuL3EventSummary::numberOfAcceptedAlgorithms() const { return StMuEvent::TCAfgAlgAccept->GetEntries();}
//! size_t StMuL3EventSummary::numberOfRejectedAlgorithms() const { return StMuEvent::TCAfgAlgReject->GetEntries();}
//!StMuL3AlgorithmInfo* StMuL3EventSummary::acceptedAlgorithm(int i) const {return (i>=0 && i<StMuEvent::TCAfgAlgAccept->GetEntries()) ? (StMuL3AlgorithmInfo*)StMuEvent::TCAfgAlgAccept->UncheckedAt(i) : 0;}
//!StMuL3AlgorithmInfo* StMuL3EventSummary::rejectedAlgorithm(int i) const {return (i>=0 && i<StMuEvent::TCAfgAlgReject->GetEntries()) ? (StMuL3AlgorithmInfo*)StMuEvent::TCAfgAlgReject->UncheckedAt(i)  : 0;}

/***************************************************************************
 *
 * $Log: StMuL3EventSummary.cxx,v $
 * Revision 1.2  2002/03/14 04:12:55  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
