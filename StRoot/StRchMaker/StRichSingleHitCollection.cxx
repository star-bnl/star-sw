/****************************************************************
 * $Id: StRichSingleHitCollection.cxx,v 1.1 2000/04/05 16:39:48 lasiuk Exp $
 *
 * Description:
 *  Container for output of cluster finder.
 *
 ****************************************************************
 *
 * $Log: StRichSingleHitCollection.cxx,v $
 * Revision 1.1  2000/04/05 16:39:48  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:48  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include "StRichSingleHitCollection.h"

StRichSingleHitCollection::StRichSingleHitCollection()
{ /* nopt */ }

StRichSingleHitCollection::StRichSingleHitCollection(HitVector& theData)
{
    mTheHits = theData;
}

StRichSingleHitCollection::~StRichSingleHitCollection()
{
    //
    // The cluster finder is responsible for the memory of the hits
    //
//     for(size_t ii=0; ii<mTheHits.size(); ii++) {
// 	delete (mTheHits[ii]);
//     }
}

