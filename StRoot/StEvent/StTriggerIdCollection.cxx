/***************************************************************************
 *
 * $Id: StTriggerIdCollection.cxx,v 2.4 2006/05/04 19:07:49 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerIdCollection.cxx,v $
 * Revision 2.4  2006/05/04 19:07:49  ullrich
 * Added L3 trigger expansion.
 *
 * Revision 2.3  2003/03/14 21:44:47  jeromel
 * delete to avoid memory leak
 *
 * Revision 2.2  2003/01/30 18:37:19  ullrich
 * Changed name of method, now setNominal().
 *
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerIdCollection.h"

ClassImp(StTriggerIdCollection)

StTriggerIdCollection::StTriggerIdCollection()
{
    mL1TriggerId = 0;
    mL2TriggerId = 0;
    mL3TriggerId = 0;
    mL3ExpandedTriggerId = 0;
    mNominalTriggerId = 0;
}

StTriggerIdCollection::~StTriggerIdCollection()
{
    delete mL1TriggerId; mL1TriggerId = 0;
    delete mL2TriggerId; mL2TriggerId = 0;
    delete mL3TriggerId; mL3TriggerId = 0;
    delete mL3ExpandedTriggerId; mL3ExpandedTriggerId = 0;
    delete mNominalTriggerId; mNominalTriggerId = 0;
}

const StTriggerId*
StTriggerIdCollection::nominal() const {return mNominalTriggerId;}

const StTriggerId*
StTriggerIdCollection::l1() const {return mL1TriggerId;}

const StTriggerId*
StTriggerIdCollection::l2() const {return mL2TriggerId;}

const StTriggerId*
StTriggerIdCollection::l3() const {return mL3TriggerId;}

const StTriggerId*
StTriggerIdCollection::l3Expanded() const {return mL3ExpandedTriggerId;}

void
StTriggerIdCollection::setL1(StTriggerId* val) {mL1TriggerId = val;}

void
StTriggerIdCollection::setL2(StTriggerId* val) {mL2TriggerId = val;}

void
StTriggerIdCollection::setL3(StTriggerId* val) {mL3TriggerId = val;}

void
StTriggerIdCollection::setL3Expanded(StTriggerId* val) {mL3ExpandedTriggerId = val;}

void
StTriggerIdCollection::setNominal(StTriggerId* val) {mNominalTriggerId = val;}
    
