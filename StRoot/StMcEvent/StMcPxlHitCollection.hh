/***************************************************************************
 *
 * $Id: StMcPxlHitCollection.hh,v 2.1 2013/03/25 23:50:36 perev Exp $
 * $Log: StMcPxlHitCollection.hh,v $
 * Revision 2.1  2013/03/25 23:50:36  perev
 * Mustafa.Pxl add
 *
 *
 **************************************************************************/
#ifndef StMcPxlHitCollection_hh
#define StMcPxlHitCollection_hh

#include "StMcPxlSectorHitCollection.hh"
class StMcPxlHit;

class StMcPxlHitCollection : public StObject {
public:

    StMcPxlHitCollection();
    virtual ~StMcPxlHitCollection();
    
    bool addHit(StMcPxlHit*);
    unsigned int numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StMcPxlSectorHitCollection*       sector(unsigned int);
    const StMcPxlSectorHitCollection* sector(unsigned int) const;
protected:
    enum { kNumberOfSectors = 10 };
    StMcPxlSectorHitCollection mSectors[kNumberOfSectors];
    ClassDef(StMcPxlHitCollection,1)
};

inline unsigned int StMcPxlHitCollection::numberOfSectors() const {return kNumberOfSectors;}
#endif
