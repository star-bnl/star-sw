/***************************************************************************
 *
 * $Id: StMcFtpcPlaneHitCollection.hh,v 2.5 2012/03/22 00:42:55 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ftpc Plane Hit Collection class
 *
 * The hits are stored here, and there is no Sector Hit collection, because
 * the sector assignment depends on the reconstruction, B-Field, etc.  The
 * Monte Carlo Data doesn't shouldn't assign the sector.
 *
 ***************************************************************************
 *
 * $Log: StMcFtpcPlaneHitCollection.hh,v $
 * Revision 2.5  2012/03/22 00:42:55  perev
 * private => protected
 *
 * Revision 2.4  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.3  2009/07/24 19:08:07  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcFtpcPlaneHitCollection_hh
#define StMcFtpcPlaneHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcFtpcHit;

class StMcFtpcPlaneHitCollection : public StObject
{
public:
    StMcFtpcPlaneHitCollection();
    virtual ~StMcFtpcPlaneHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    unsigned long numberOfHits() const;

    StSPtrVecMcFtpcHit&       hits();
    const StSPtrVecMcFtpcHit& hits() const; 

protected:
    StSPtrVecMcFtpcHit mHits;
    ClassDef(StMcFtpcPlaneHitCollection,1)
};
#endif
