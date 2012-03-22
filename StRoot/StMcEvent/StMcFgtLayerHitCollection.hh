/***************************************************************************
 *
 * $Id: StMcFgtLayerHitCollection.hh,v 2.4 2012/03/22 00:40:41 perev Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fgt Layer Hit Collection class from Kai
 *
 * The Fgt detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcFgtLayerHitCollection.hh,v $
 * Revision 2.4  2012/03/22 00:40:41  perev
 * private => protected
 *
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2009/07/24 19:08:07  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#ifndef StMcFgtLayerHitCollection_hh
#define StMcFgtLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcFgtHit;

class StMcFgtLayerHitCollection : public StObject
{
public:
    StMcFgtLayerHitCollection();
    virtual ~StMcFgtLayerHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    unsigned long numberOfHits() const;

    StSPtrVecMcFgtHit&       hits();
    const StSPtrVecMcFgtHit& hits() const; 

protected:
    StSPtrVecMcFgtHit mHits;
    ClassDef(StMcFgtLayerHitCollection,1)
};
#endif
