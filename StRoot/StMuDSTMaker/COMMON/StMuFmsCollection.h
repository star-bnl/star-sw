/***************************************************************************
 *
 * $Id: StMuFmsCollection.h,v 1.1 2010/01/25 03:57:39 tone421 Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.h,v $
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#ifndef StMuFmsCollection_hh
#define StMuFmsCollection_hh

#include "StObject.h"
#include "TClonesArray.h"

class StMuFmsHit;
//class StMuFmsCluster;
//class StMuFmsPoint;

class StMuFmsCollection : public StObject {
public:
    StMuFmsCollection();
    ~StMuFmsCollection();
    
    void          init();
    void          addHit();
    unsigned int  numberOfHits() const;
    void          setFmsHitArray(TClonesArray *array) {mHits=array;};

    StMuFmsHit* getHit(int hitId);
    TClonesArray* getHitArray() { return mHits; };
    
private:
    TClonesArray* mHits;
    
    ClassDef(StMuFmsCollection,1)
};
#endif
