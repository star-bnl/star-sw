/***************************************************************************
 *
 * $Id: StMcSsdLadderHitCollection.hh,v 2.3 2012/03/22 00:47:25 perev Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Ladder Hit Collection class from Kai
 *
 * The ssd detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcSsdLadderHitCollection.hh,v $
 * Revision 2.3  2012/03/22 00:47:25  perev
 * private => protected
 *
 * Revision 2.2  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.1  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ssd classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#ifndef StMcSsdLadderHitCollection_hh
#define StMcSsdLadderHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

#include "StMcSsdWaferHitCollection.hh"

class StMcSsdLadderHitCollection : public StObject
{
public:
  StMcSsdLadderHitCollection() {}
  virtual ~StMcSsdLadderHitCollection() {}
protected:
    enum { mMaxNumberOfWafers = 16 };
    StMcSsdWaferHitCollection  mWafers[mMaxNumberOfWafers];
public:
    unsigned long numberOfHits() const;
  unsigned int  numberOfWafers() const {return mMaxNumberOfWafers;}
    
    StMcSsdWaferHitCollection*       wafer(unsigned int);
    const StMcSsdWaferHitCollection* wafer(unsigned int) const;
    
    ClassDef(StMcSsdLadderHitCollection,1)
};
#endif
