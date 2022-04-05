/***************************************************************************
 *
 * $Id: StETofCollection.h,v 2.1 2018/07/09 14:53:47 ullrich Exp $
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
 * $Log: StETofCollection.h,v $
 * Revision 2.1  2018/07/09 14:53:47  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#ifndef STETOFCOLLECTION_H
#define STETOFCOLLECTION_H

#include "StObject.h"
#include "StETofDigi.h"
#include "StETofHit.h"
#include "StETofHeader.h"

#include "StContainers.h"
#include "StEnumerations.h"

class StETofCollection : public StObject {
public: 
    StETofCollection();
    ~StETofCollection();

    const StETofHeader*         etofHeader() const;
    StETofHeader*               etofHeader();

    const StSPtrVecETofDigi&    etofDigis() const;
    StSPtrVecETofDigi& 	        etofDigis();

    const StSPtrVecETofHit&     etofHits() const;
    StSPtrVecETofHit&           etofHits();

    void setHeader( StETofHeader* );

    void addDigi( const StETofDigi* );
    void addHit(  const StETofHit*  );

    bool digisPresent()	   const;
    bool hitsPresent()     const;

private:
    StETofHeader*       mETofHeader;

    StSPtrVecETofDigi   mETofDigis;
    StSPtrVecETofHit    mETofHits;


    ClassDef( StETofCollection, 1 )
};

#endif // STETOFCOLLECTION_H

