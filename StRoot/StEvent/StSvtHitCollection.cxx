/***************************************************************************
 *
 * $Id: StSvtHitCollection.cxx,v 2.5 2001/04/05 04:00:55 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.cxx,v $
 * Revision 2.5  2001/04/05 04:00:55  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2000/02/17 18:13:11  ullrich
 * Changed the SVT hit storage model. Hits are now stored according
 * to barrel/ladder/wafer not by layer/ladder/wafer.
 *
 * Revision 2.3  1999/12/13 20:16:24  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.2  1999/10/28 22:26:47  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtHitCollection.h"
#include "StSvtHit.h"

static const char rcsid[] = "$Id: StSvtHitCollection.cxx,v 2.5 2001/04/05 04:00:55 ullrich Exp $";

ClassImp(StSvtHitCollection)

StSvtHitCollection::StSvtHitCollection()
{
    //
    //  Barrel and ladder collections have to know
    //  their barrel number in order to return the
    //  proper numberOfLadders() and numberOfWafers().
    //
    for (int i=0; i<mNumberOfBarrels; i++) {
        mBarrels[i].setBarrelNumber(i);
        for (unsigned int j=0; j<mBarrels[i].numberOfLadders(); j++)
            mBarrels[i].ladder(j)->setBarrelNumber(i);
    }
}

StSvtHitCollection::~StSvtHitCollection() { /* noop */ }
    
unsigned int
StSvtHitCollection::numberOfBarrels() const { return mNumberOfBarrels; }

bool
StSvtHitCollection::addHit(StSvtHit* hit)
{
    unsigned int l, d, w;
    if (hit &&
        (l = hit->barrel()-1) < mNumberOfBarrels &&
        (d = hit->ladder()-1) < mBarrels[l].numberOfLadders() &&
        (w = hit->wafer()-1) < mBarrels[l].ladder(d)->numberOfWafers()) {
        mBarrels[l].ladder(d)->wafer(w)->hits().push_back(hit);
        return kTRUE;
    }
    else
        return kFALSE;
}

unsigned int
StSvtHitCollection::numberOfHits() const
{
    unsigned int sum = 0;
    for (int i=0; i<mNumberOfBarrels; i++)
        for (unsigned int j=0; j<mBarrels[i].numberOfLadders(); j++)
            for (unsigned int k=0; k<mBarrels[i].ladder(j)->numberOfWafers(); k++)
                sum += mBarrels[i].ladder(j)->wafer(k)->hits().size();
    return sum;
}

StSvtBarrelHitCollection*
StSvtHitCollection::barrel(unsigned int i)
{
    if (i < mNumberOfBarrels)
        return &(mBarrels[i]);
    else
        return 0;
}

const StSvtBarrelHitCollection*
StSvtHitCollection::barrel(unsigned int i) const
{
    if (i < mNumberOfBarrels)
        return &(mBarrels[i]);
    else
        return 0;
}

