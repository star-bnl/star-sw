/*!
 * \class StEtrHitCollection 
 * \author Ming Shao, Jan 5, 2012
 */
/***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEtrHitCollection.h,v $
 * Revision 2.1  2012/01/24 03:06:13  perev
 * Add Etr
 *
 *
 * Revision 1.0  2012/01/05  Ming
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEtrHitCollection_hh
#define StEtrHitCollection_hh
   
#include "StObject.h"
#include "StContainers.h"

class StEtrHit;

class StEtrHitCollection : public StObject {
 public:
    StEtrHitCollection();
    ~StEtrHitCollection();
    
    bool          addHit(StEtrHit*);
    unsigned int  numberOfHits() const;
    unsigned int  numberOfLayers() const;
    unsigned int  numberOfSectors() const;
    
    StSPtrVecEtrHit&       hits();
    const StSPtrVecEtrHit& hits() const;

private:
    enum { mNumberOfLayers = 3 };
    enum { mNumberOfSectors = 12 };

    StSPtrVecEtrHit mHits;
    
    ClassDef(StEtrHitCollection,1)
};
#endif
