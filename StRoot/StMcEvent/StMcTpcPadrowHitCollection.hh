/***************************************************************************
 *
 * $Id: StMcTpcPadrowHitCollection.hh,v 2.6 2012/03/22 00:54:37 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Padrow Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcPadrowHitCollection.hh,v $
 * Revision 2.6  2012/03/22 00:54:37  perev
 * private => protected
 *
 * Revision 2.5  2012/03/01 16:48:30  perev
 * method Browse() added
 *
 * Revision 2.4  2009/07/24 19:08:09  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.3  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcTpcPadrowHitCollection_hh
#define StMcTpcPadrowHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcTpcPadrowHitCollection : public StObject
{
public:
    StMcTpcPadrowHitCollection();
    virtual ~StMcTpcPadrowHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 

    // StMcTpcPadrowHitCollection(const StMcTpcPadrowHitCollection&); use default
    // const StMcTpcPadrowHitCollection&
    // operator=(const StMcTpcPadrowHitCollection&);                use default
    
    StSPtrVecMcTpcHit&       hits();
    const StSPtrVecMcTpcHit& hits() const;

protected:
    StSPtrVecMcTpcHit mHits;
    ClassDef(StMcTpcPadrowHitCollection,1)
};
#endif
