/***************************************************************************
 *
 * $Id: StSvtLayerHitCollection.cxx,v 2.1 1999/10/13 19:45:18 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtLayerHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:45:18  ullrich
 * Initial Revision
 *
 * Revision 2.2  1999/10/28 22:26:53  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:18  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtLayerHitCollection.h"
ClassImp(StSvtWaferHitCollection)
static const char rcsid[] = "$Id: StSvtLayerHitCollection.cxx,v 2.1 1999/10/13 19:45:18 ullrich Exp $";

ClassImp(StSvtLayerHitCollection)

StSvtLayerHitCollection::StSvtLayerHitCollection()
{
    mLayerNumber = -1;
}

StSvtLayerHitCollection::~StSvtLayerHitCollection() { /* noop */ }

void
StSvtLayerHitCollection::setLayerNumber(Int_t i)
{
    if (mLayerNumber == -1) mLayerNumber = i;
}
    
UInt_t
StSvtLayerHitCollection::numberOfLadders() const
{
    switch (mLayerNumber) {
    case 0:
    case 1:
        return 4;
        break;
    case 2:
    case 3:
        return 6;
        break;
    case 4:
    case 5:
        return 8;
        break;
    default:
        return 0;
    }
}

ULong_t
    for (int j=0; j<numberOfLadders(); j++) {
        for (int k=0; k<mLadders[j].numberOfWafers(); k++) {
    ULong_t sum = 0;
    for (unsigned int j=0; j<numberOfLadders(); j++) {
        for (unsigned int k=0; k<mLadders[j].numberOfWafers(); k++) {
            sum += mLadders[j].wafer(k)->hits().size();
        }
    }
    return sum;
}

StSvtLadderHitCollection*
StSvtLayerHitCollection::ladder(UInt_t i)
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}

const StSvtLadderHitCollection*
StSvtLayerHitCollection::ladder(UInt_t i) const
{
    if (i < numberOfLadders())
        return &(mLadders[i]);
    else
        return 0;
}
    
