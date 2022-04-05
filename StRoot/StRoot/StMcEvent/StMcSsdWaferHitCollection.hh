/***************************************************************************
 *
 * $Id: StMcSsdWaferHitCollection.hh,v 2.4 2012/03/22 00:48:45 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Wafer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSsdWaferHitCollection.hh,v $
 * Revision 2.4  2012/03/22 00:48:45  perev
 * private => protected
 *
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2009/07/24 19:08:08  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.1  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.3  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcSsdWaferHitCollection_hh
#define StMcSsdWaferHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcSsdHit;

class StMcSsdWaferHitCollection : public StObject
{
public:
    StMcSsdWaferHitCollection();
    // StMcSsdWaferHitCollection(const StMcSsdWaferHitCollection&); use default
    // const StMcSsdWaferHitCollection& operator=(const StMcSsdWaferHitCollection&); use default
    virtual ~StMcSsdWaferHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    StSPtrVecMcSsdHit&       hits();
    const StSPtrVecMcSsdHit& hits() const;

protected:
    StSPtrVecMcSsdHit mHits;
    ClassDef(StMcSsdWaferHitCollection,1)
};
#endif
