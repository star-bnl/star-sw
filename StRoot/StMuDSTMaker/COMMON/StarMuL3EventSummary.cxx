/***************************************************************************
 *
 * $Id: StarMuL3EventSummary.cxx,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
/***************************************************************************
 *
 * $Log: StarMuL3EventSummary.cxx,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/

#include "TClonesArray.h"

#include "StEvent/StEvent.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StL3EventSummary.h"
#include "StEvent/StL3AlgorithmInfo.h"
#include "StEvent/StPrimaryVertex.h"

#include "StarMuEvent.h"
#include "StarMuL3EventSummary.h"
#include "StarMuException.hh"
#include "StarMuDebug.h"

ClassImp(StarMuL3EventSummary)

StarMuL3EventSummary::StarMuL3EventSummary() : mNumberOfProcessedEvents(0), mNumberReconstructedEvents(0),
  mNumberOfTracks(0), mNumberOfAlgorithms(0), mFlags(0), mL0TriggerWord(0), mUnbiasedPreScale(0) { 
  DEBUGMESSAGE("");
  clear();
}



void StarMuL3EventSummary::fill(const StEvent* ev) {
  DEBUGMESSAGE("");
  clear();
  if ( !(ev->l3Trigger() && ev->l3Trigger()->l3EventSummary()) ) return;
  const StL3EventSummary* l3 = ev->l3Trigger()->l3EventSummary();
  mNumberOfProcessedEvents = l3->numberOfProcessedEvents();
  mNumberReconstructedEvents = l3->numberOfReconstructedEvents();
  mNumberOfTracks =  l3->numberOfTracks();
  mNumberOfAlgorithms = l3->numberOfAlgorithms();
  mFlags |= (__VERTEX__ & l3->zVertexTrigger());
  mFlags |= (__UNBIASED__ & l3->unbiasedTrigger());
  mL0TriggerWord = l3->l0TriggerWord();
  mUnbiasedPreScale = l3->unbiasedTriggerPreScale();
  if (ev->l3Trigger()->primaryVertex()) 
    mPrimaryVertex = ev->l3Trigger()->primaryVertex()->position();
}

StarMuL3EventSummary::~StarMuL3EventSummary(){
  DEBUGMESSAGE("");
}

void StarMuL3EventSummary::clear(){
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

//! const TClonesArray* StarMuL3EventSummary::algorithmsAcceptingEvent() { return StarMuEvent::TCAfgAlgAccept; }
//! const TClonesArray* StarMuL3EventSummary::algorithmsRejectingEvent() { return StarMuEvent::TCAfgAlgReject; }
//! size_t StarMuL3EventSummary::numberOfAcceptedAlgorithms() const { return StarMuEvent::TCAfgAlgAccept->GetEntries();}
//! size_t StarMuL3EventSummary::numberOfRejectedAlgorithms() const { return StarMuEvent::TCAfgAlgReject->GetEntries();}
//!StarMuL3AlgorithmInfo* StarMuL3EventSummary::acceptedAlgorithm(int i) const {return (i>=0 && i<StarMuEvent::TCAfgAlgAccept->GetEntries()) ? (StarMuL3AlgorithmInfo*)StarMuEvent::TCAfgAlgAccept->UncheckedAt(i) : 0;}
//!StarMuL3AlgorithmInfo* StarMuL3EventSummary::rejectedAlgorithm(int i) const {return (i>=0 && i<StarMuEvent::TCAfgAlgReject->GetEntries()) ? (StarMuL3AlgorithmInfo*)StarMuEvent::TCAfgAlgReject->UncheckedAt(i)  : 0;}

/***************************************************************************
 *
 * $Log: StarMuL3EventSummary.cxx,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
