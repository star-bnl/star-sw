/***************************************************************************
 *
 * $Id: StMcTpcSectorHitCollection.cc,v 2.0 1999/11/17 02:01:00 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Sector Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcSectorHitCollection.cc,v $
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcTpcSectorHitCollection.hh"

static const char rcsid[] = "$Id: StMcTpcSectorHitCollection.cc,v 2.0 1999/11/17 02:01:00 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcTpcSectorHitCollection)
#endif

StMcTpcSectorHitCollection::StMcTpcSectorHitCollection() { /* noop */ }

StMcTpcSectorHitCollection::~StMcTpcSectorHitCollection() { /* noop */ }
    
unsigned int
StMcTpcSectorHitCollection::numberOfPadrows() const { return mNumberOfPadrows; }

StMcTpcPadrowHitCollection* StMcTpcSectorHitCollection::padrow(unsigned int i)
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

const StMcTpcPadrowHitCollection*
StMcTpcSectorHitCollection::padrow(unsigned int i) const
{
    if (i < mNumberOfPadrows)
        return &(mPadrows[i]);
    else
        return 0;
}

unsigned long StMcTpcSectorHitCollection::numberOfHits() const
{
    unsigned long sum = 0;
    for (unsigned int i=0; i < mNumberOfPadrows; i++)
        sum += mPadrows[i].hits().size();
    return sum;
}
