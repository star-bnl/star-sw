/**************************************************************************
 *
 * StFttPoint.h
 *
 * Author: jdb 2021
 **************************************************************************
 *
 * Description: Declaration of StFttPoint, the StEvent FTT point structure
 * Represents a "point" (cluster centroid etc) via cluster finding.
 *
 **************************************************************************/
#ifndef StFttPoint_h
#define StFttPoint_h

#include "StThreeVectorD.hh"
#include "StObject.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StFttCluster;

class StFttPoint : public StObject {
public:
    StFttPoint();
    ~StFttPoint();

    UChar_t plane()   const; // Detector plane.
    UChar_t quadrant()   const; // detector quadrant.
    float x() const;  // x position in cell unit at which point intersects the sub-detector in local coordinate
    float y() const;  // y position in cell unit at which point intersects the sub-detector in local coordinate
    float d1() const; // diagonal1
    float d2() const; // diagonal2
    int nClusters() const; // Number of points in the parent cluster.
    StFttCluster* cluster( size_t i); //  Parent cluster of the photon.
    const StThreeVectorD& xyz() const; // XYZ position in global STAR coordinate

    void setPlane(UChar_t plane);
    void setQuadrant(UChar_t quad);
    void setX(float x);
    void setY(float y);
    void setD1(float d1);
    void setD2(float d2);
    void addCluster(StFttCluster* cluster, UChar_t dir);
    void setXYZ(const StThreeVectorD& p3);

    
    void print(int option=0);

private:
    UChar_t mPlane=0;
    UChar_t mQuadrant=0;
    Float_t  mX=0.0;         // x-position in local coordinate
    Float_t  mY=0.0;         // y-position in local coordinate
    Float_t mD1=0.0;
    Float_t mD2=0.0;
    StFttCluster *mClusters[4];
    StThreeVectorD  mXYZ;    // Photon position in STAR coordinate

    ClassDef(StFttPoint, 1)
};

inline UChar_t StFttPoint::plane() const { return mPlane; }
inline UChar_t StFttPoint::quadrant() const { return mQuadrant; }
inline float StFttPoint::x() const { return mX; } // x position (cm) in local coords.
inline float StFttPoint::y() const { return mY; } // y position (cm) in local coords.
inline float StFttPoint::d1() const { return mD1; } // x position (cm) in local coords.
inline float StFttPoint::d2() const { return mD2; } // y position (cm) in local coords.
inline StFttCluster* StFttPoint::cluster( size_t i ) { if ( i < 4 ) return mClusters[i]; return nullptr; } //  Parent cluster of the photon.
inline const StThreeVectorD& StFttPoint::xyz() const { return mXYZ; }
inline void StFttPoint::setPlane(UChar_t plane) { mPlane = plane; }
inline void StFttPoint::setQuadrant(UChar_t quadrant) { mQuadrant = quadrant; }
inline void StFttPoint::setX(float xpos) { mX = xpos; }
inline void StFttPoint::setY(float ypos) { mY = ypos; }
inline void StFttPoint::setD1(float d1) { mD1 = d1; }
inline void StFttPoint::setD2(float d2) { mD2 = d2; }
inline void StFttPoint::addCluster(StFttCluster* cluster, UChar_t dir) { mClusters[dir] = (cluster); }
inline void StFttPoint::setXYZ(const StThreeVectorD& p3) { mXYZ = p3; }

#endif  // StFttPoint_h

