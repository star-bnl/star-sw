/***************************************************************************
 *
 * $Id: StMuFmsCollection.h,v 1.5 2015/11/06 17:47:16 jdb Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.h,v $
 * Revision 1.5  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
 * Revision 1.4  2015/10/23 19:22:49  jdb
 * akio added mFmsReconstructionFlag and related getters and setters. pushed version number of StMuFmsCollection. Corresponding changes for reconstruction flag in StMuFmsUtil.cxx
 *
 * Revision 1.3  2015/10/16 18:13:28  jdb
 * incremented version # for StMuFmsCollection and StMuFmsPoint
 *
 * Revision 1.2  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#ifndef StMuFmsCollection_hh
#define StMuFmsCollection_hh

#include "St_base/StObject.h"
#include "TClonesArray.h"

class StMuFmsHit;
class StMuFmsCluster;
class StMuFmsPoint;
class StMuFmsInfo;

class StMuFmsCollection : public TObject {
public:
    StMuFmsCollection();
    ~StMuFmsCollection();
    
    void          init();
    void          addHit();
    void          addCluster();
    void          addInfo();
    StMuFmsPoint* addPoint();
    unsigned int  numberOfHits() const;
    unsigned int  numberOfClusters() const;
    unsigned int  numberOfPoints() const;
    void          setFmsHitArray(TClonesArray *array) {mHits=array;};
    void          setFmsClusterArray(TClonesArray* array) {mClusters=array;}
    void          setFmsPointArray(TClonesArray* array) {mPoints=array;}
    void          setFmsInfoArray(TClonesArray* array) {mInfo=array;}

    StMuFmsHit* getHit(int hitId);
    StMuFmsCluster* getCluster(int index);
    StMuFmsPoint* getPoint(int index);
    StMuFmsInfo* getInfo();
    TClonesArray* getHitArray() { return mHits; };
    TClonesArray* getClusterArray() { return mClusters; }
    TClonesArray* getPointArray() { return mPoints; }
    

    Int_t fmsReconstructionFlag();
    void setFmsReconstructionFlag(Int_t v);

private:
    TClonesArray* mHits;
    TClonesArray* mClusters;
    TClonesArray* mPoints;
    TClonesArray* mInfo;

    ClassDef(StMuFmsCollection,5)
};
#endif
