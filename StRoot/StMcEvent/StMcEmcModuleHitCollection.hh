/***************************************************************************
 *
 * $Id: StMcEmcModuleHitCollection.hh,v 2.7 2012/03/22 00:35:22 perev Exp $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Module Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcModuleHitCollection.hh,v $
 * Revision 2.7  2012/03/22 00:35:22  perev
 * private => protected
 *
 * Revision 2.6  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.5  2007/10/05 00:01:21  calderon
 * Changes to include a EMC hit collection that does not care about
 * parent tracks, so that now there are two collections.  This
 * new collection will be useful to compare all the deposited energy in a hit tower
 * in a given event. The information about the track parentage is still
 * kept in the original collection unchanged.
 *
 * Revision 2.4  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2001/05/31 02:45:55  perev
 * const(ing)
 *
 * Revision 2.2  2000/08/30 14:52:03  calderon
 * New changes made by Aleksei.
 *
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#ifndef StMcEmcModuleHitCollection_hh
#define StMcEmcModuleHitCollection_hh
#include "StMcContainers.hh"
#include "TDataSet.h"

class StMcCalorimeterHit;

class StMcEmcModuleHitCollection : public TDataSet {
public:
    StMcEmcModuleHitCollection();
    StMcEmcModuleHitCollection(const unsigned int m);
    virtual ~StMcEmcModuleHitCollection();
    void Clear(const char* opt="");
virtual bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 

    void init(const unsigned int m);
    unsigned long numberOfHits() const;
    unsigned long numberOfDetectorHits() const;
    float sum() const;

    StSPtrVecMcCalorimeterHit&       hits();
    const StSPtrVecMcCalorimeterHit& hits() const;
    
    // detector hits are like hits, but there is at most one detector hit per
    // element (tower, preshower, or strip).  If multiple hits occur in a single
    // element their energy depositions are summed, regardless of the parent track.
    // Also, detector hits do not preserve information about the parent track of 
    // the hit, since there may be more than one.  APK - 09/07
    StSPtrVecMcCalorimeterHit&       detectorHits();
    const StSPtrVecMcCalorimeterHit& detectorHits() const;


    void operator()(const unsigned int m) { init(m); } 

protected:
    StSPtrVecMcCalorimeterHit mHits;
    StSPtrVecMcCalorimeterHit mDetectorHits;
    ClassDef(StMcEmcModuleHitCollection,1)
};
#endif
