/****************************************************************
 * $Id: StRichSimpleHitCollection.cxx,v 1.1 2000/05/18 12:24:08 lasiuk Exp $
 *
 * Description:
 *  Container for output of cluster finder.
 *
 ****************************************************************
 *
 * $Log: StRichSimpleHitCollection.cxx,v $
 * Revision 1.1  2000/05/18 12:24:08  lasiuk
 * nitial revision
 *
 ****************************************************************/

#include "StRichSimpleHitCollection.h"

StRichSimpleHitCollection::StRichSimpleHitCollection()
{ /* nopt */ }

StRichSimpleHitCollection::StRichSimpleHitCollection(HitVector& theData)
{
    mTheHits = theData;
}

StRichSimpleHitCollection::~StRichSimpleHitCollection()
{
    //
    // The cluster finder is responsible for the memory of the hits
    //
//     for(size_t ii=0; ii<mTheHits.size(); ii++) {
// 	delete (mTheHits[ii]);
//     }
}

