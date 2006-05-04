/***************************************************************************
 *
 * $Id: StMuTriggerIdCollection.cxx,v 1.3 2006/05/04 21:04:35 mvl Exp $
 *
 * Author: Frank Laue
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************/
#include "StMuTriggerIdCollection.h"

ClassImp(StMuTriggerIdCollection)

StMuTriggerIdCollection::StMuTriggerIdCollection(){}

StMuTriggerIdCollection::StMuTriggerIdCollection(const StTriggerIdCollection* c) { fill(c); }

StMuTriggerIdCollection::~StMuTriggerIdCollection(){}

void StMuTriggerIdCollection::fill(const StTriggerIdCollection* c) {
  if (!c) return;
  if ( c->l1() ) setL1( *(c->l1()) );
  if ( c->l2() ) setL2( *(c->l2()) );
  if ( c->l3() ) setL3( *(c->l3()) );
  if ( c->l3Expanded() ) setL3Expanded( *(c->l3Expanded()) );
  if ( c->nominal() ) setNominal( *(c->nominal()) );
}

bool StMuTriggerIdCollection::isEmpty(const StTriggerId& id){
    if ( id.triggerIds().size()==0) return true;
    return false;
}

const StTriggerId&
StMuTriggerIdCollection::nominal() const {return mNTriggerId;}

const StTriggerId&
StMuTriggerIdCollection::l1() const {return mL1TriggerId;}

const StTriggerId&
StMuTriggerIdCollection::l2() const {return mL2TriggerId;}

const StTriggerId&
StMuTriggerIdCollection::l3() const {return mL3TriggerId;}

const StTriggerId&
StMuTriggerIdCollection::l3Expanded() const {return mLETriggerId;}

void
StMuTriggerIdCollection::setL1(const StTriggerId val) {mL1TriggerId = val;}

void
StMuTriggerIdCollection::setL2(const StTriggerId val) {mL2TriggerId = val;}

void
StMuTriggerIdCollection::setL3(const StTriggerId val) {mL3TriggerId = val;}

void
StMuTriggerIdCollection::setL3Expanded(const StTriggerId val) {mLETriggerId = val;}

void
StMuTriggerIdCollection::setNominal(const StTriggerId val) {mNTriggerId = val;}
    

/**************************************************************************
 *
 * $Log: StMuTriggerIdCollection.cxx,v $
 * Revision 1.3  2006/05/04 21:04:35  mvl
 * Additions for extra L3 information (from Jamie)
 *
 * Revision 1.2  2003/03/19 18:58:04  laue
 * StMuChainMaker: updates for moved file catalog
 * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
 *
 * Revision 1.1  2003/02/20 15:50:30  laue
 * New. Wrapper around StEVent/StStriggerIdCollection
 *
 *
 **************************************************************************/
