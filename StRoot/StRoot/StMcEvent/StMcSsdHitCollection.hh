/***************************************************************************
 *
 * $Id: StMcSsdHitCollection.hh,v 2.4 2012/03/22 01:08:54 perev Exp $
 * $Log: StMcSsdHitCollection.hh,v $
 * Revision 2.4  2012/03/22 01:08:54  perev
 * private => protected
 *
 * Revision 2.3  2005/11/22 21:44:52  fisyak
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
 **************************************************************************/
#ifndef StMcSsdHitCollection_hh
#define StMcSsdHitCollection_hh

#include "StMcSsdLadderHitCollection.hh"

class StMcSsdHit;

class StMcSsdHitCollection : public StObject {
public:

    StMcSsdHitCollection();
    virtual ~StMcSsdHitCollection();
    
    bool addHit(StMcSsdHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StMcSsdLadderHitCollection*       ladder(unsigned int);
    const StMcSsdLadderHitCollection* ladder(unsigned int) const;
protected:
    enum { mNumberOfLadders = 20 };
    StMcSsdLadderHitCollection mLadders[mNumberOfLadders];
    ClassDef(StMcSsdHitCollection,1)
};
#endif
