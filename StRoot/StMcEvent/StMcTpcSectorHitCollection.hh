/***************************************************************************
 *
 * $Id: StMcTpcSectorHitCollection.hh,v 2.1 2000/03/06 18:05:23 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Sector Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcSectorHitCollection.hh,v $
 * Revision 2.1  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcTpcSectorHitCollection_hh
#define StMcTpcSectorHitCollection_hh

#include "StMcTpcPadrowHitCollection.hh"

class StMcTpcSectorHitCollection
{    
public:
    StMcTpcSectorHitCollection();
    ~StMcTpcSectorHitCollection();
    // StMcTpcSectorHitCollection(const StMcTpcSectorHitCollection&);            use default
    // StMcTpcSectorHitCollection& operator=(const StMcTpcSectorHitCollection&); use default
    
    unsigned long numberOfHits() const;
    unsigned int  numberOfPadrows() const;
    
    StMcTpcPadrowHitCollection*       padrow(unsigned int);
    const StMcTpcPadrowHitCollection* padrow(unsigned int) const;

private:
    enum { mNumberOfPadrows = 45 };
    StMcTpcPadrowHitCollection mPadrows[mNumberOfPadrows];
    
};
#endif
