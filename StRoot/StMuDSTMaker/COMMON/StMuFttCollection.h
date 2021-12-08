/***************************************************************************
 *
 * $Id: StMuFttCollection.h
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Ftt data interface to StMuFttRawHit, StMuFttCluster and StMuFttPoint
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFttCollection_hh
#define StMuFttCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFttRawHit;
class StMuFttCluster;
class StMuFttPoint;

class StMuFttCollection : public TObject {
public:
    StMuFttCollection();
    ~StMuFttCollection();
    
    void            init();
    StMuFttRawHit*  addRawHit();
    StMuFttCluster* addCluster();
    StMuFttPoint*   addPoint();

    unsigned int    numberOfRawHits() const;
    unsigned int    numberOfClusters() const;
    unsigned int    numberOfPoints() const;

    void            setFttHitArray(TClonesArray *array) {mHits=array;};
    void            setFttClusterArray(TClonesArray* array) {mClusters=array;}
    void            setFttPointArray(TClonesArray* array) {mPoints=array;}

    StMuFttRawHit*  getRawHit(int index);
    StMuFttCluster* getCluster(int index);
    StMuFttPoint*   getPoint(int index);

    TClonesArray*   getRawHitArray() { return mHits; };
    TClonesArray*   getClusterArray() { return mClusters; }
    TClonesArray*   getPointArray() { return mPoints; }

private:
    TClonesArray* mHits=0;
    TClonesArray* mClusters=0;
    TClonesArray* mPoints=0;

    ClassDef(StMuFttCollection,1)
};
#endif
