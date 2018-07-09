/***************************************************************************
 *
 * $Id: StETofCollection.cxx,v 2.1 2018/07/09 14:53:48 ullrich Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: This class collects eTOF objects for persistent
 * storage in StEvent. All eTOF stuff goes here expect the PiDTraits:
 *              - StETofDigi
 *              - StETofHit
 *              - StETofHeader
 *
 ***************************************************************************
 *
 * $Log: StETofCollection.cxx,v $
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StETofCollection.h"


StETofCollection::StETofCollection() {
    mETofHeader = 0;
}


StETofCollection::~StETofCollection() {
    if( mETofHeader ) delete mETofHeader;
}


StETofHeader*       StETofCollection::etofHeader()  { return mETofHeader; }
StSPtrVecETofDigi&  StETofCollection::etofDigis()   { return mETofDigis;  }
StSPtrVecETofHit&   StETofCollection::etofHits()    { return mETofHits;   }

const StETofHeader*         StETofCollection::etofHeader()  const   { return mETofHeader; }
const StSPtrVecETofDigi&    StETofCollection::etofDigis()   const   { return mETofDigis;  }
const StSPtrVecETofHit&	    StETofCollection::etofHits()    const   { return mETofHits;   }


void
StETofCollection::setHeader(StETofHeader* val){
    mETofHeader = val;
}


void
StETofCollection::addDigi( const StETofDigi* aDigi ) {
    if ( aDigi ) mETofDigis.push_back( aDigi );
}


void
StETofCollection::addHit( const StETofHit* aHit ) {
    if ( aHit ) mETofHits.push_back( aHit );
}


bool    StETofCollection::digisPresent()    const { return mETofDigis.size(); }
bool    StETofCollection::hitsPresent()     const { return mETofHits.size();  }