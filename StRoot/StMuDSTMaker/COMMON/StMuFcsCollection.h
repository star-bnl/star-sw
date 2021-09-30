/***************************************************************************
 *
 * $Id: StMuFcsCollection.h,v 1.6 2017/08/14 16:22:36 smirnovd Exp $
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Fcs data interface to StMuFcsHit, StMuFcsCluster and StMuFcsPoint
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFcsCollection_hh
#define StMuFcsCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFcsHit;
class StMuFcsCluster;
class StMuFcsPoint;

class StMuFcsCollection : public TObject {
public:
    StMuFcsCollection();
    ~StMuFcsCollection();
    
    void          init();
    StMuFcsHit*     addHit();
    StMuFcsCluster* addCluster();
    StMuFcsPoint*   addPoint();

    unsigned int  numberOfHits() const;
    unsigned int  numberOfClusters() const;
    unsigned int  numberOfPoints() const;

    void          setFcsHitArray(TClonesArray *array) {mHits=array;};
    void          setFcsClusterArray(TClonesArray* array) {mClusters=array;}
    void          setFcsPointArray(TClonesArray* array) {mPoints=array;}

    StMuFcsHit* getHit(int index);
    StMuFcsCluster* getCluster(int index);
    StMuFcsPoint* getPoint(int index);

    TClonesArray* getHitArray() { return mHits; };
    TClonesArray* getClusterArray() { return mClusters; }
    TClonesArray* getPointArray() { return mPoints; }

private:
    TClonesArray* mHits=0;
    TClonesArray* mClusters=0;
    TClonesArray* mPoints=0;

    ClassDef(StMuFcsCollection,1)
};
#endif
