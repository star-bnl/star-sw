/**************************************************************************
 *
 * StMuFttPoint.h
 *
 * Author: jdb 2021
 **************************************************************************
 *
 * Description: Declaration of StMuFttPoint, the StEvent FTT point structure
 * Represents a "point" (cluster centroid etc) via cluster finding.
 *
 **************************************************************************/
#ifndef StMuFttPoint_h
#define StMuFttPoint_h

#include <TVector3.h>
#include <TObject.h>
#include <TRefArray.h>
#include "StEnumerations.h"

class StMuFttCluster;
class StFttPoint;

class StMuFttPoint : public TObject {
public:
    StMuFttPoint();
    ~StMuFttPoint();

    UChar_t plane()   const; // Detector plane.
    UChar_t quadrant()   const; // detector quadrant.
    float x() const;  // x position in cell unit at which point intersects the sub-detector in local coordinate
    float y() const;  // y position in cell unit at which point intersects the sub-detector in local coordinate
    int nParentClusters()   const; // Number of points in the parent cluster.
    // StMuFttCluster* cluster( size_t i); //  Parent cluster of the photon.
    const TVector3& xyz()   const; // XYZ position in global STAR coordinate

    void setPlane(UChar_t plane);
    void setQuadrant(UChar_t quad);
    void setX(float x);
    void setY(float y);
    // void addCluster(StMuFttCluster* cluster);
    void setXYZ(const TVector3& p3);

    
    void print(int option=0);

    void set( StFttPoint * point );

private:
    UChar_t mPlane;
    UChar_t mQuadrant;
    Float_t mX=0.0;         // x-position in local coordinate
    Float_t mY=0.0;         // y-position in local coordinate
    TRefArray mClusters=0; // parent clusters (could be up to 3?)
    TVector3  mXYZ;    // Photon position in STAR coordinate

    ClassDef(StMuFttPoint, 1)
};

inline UChar_t StMuFttPoint::plane() const { return mPlane; }
inline UChar_t StMuFttPoint::quadrant() const { return mQuadrant; }
inline float StMuFttPoint::x() const { return mX; } // x position (cm) in local coords.
inline float StMuFttPoint::y() const { return mY; } // y position (cm) in local coords.
inline int StMuFttPoint::nParentClusters() const { return mClusters.GetSize(); } // Number of points in parent cluster
// inline StMuFttCluster* StMuFttPoint::cluster( size_t i ) { return mClusters.At(i); } //  Parent cluster of the photon.
inline const TVector3& StMuFttPoint::xyz() const { return mXYZ; }
inline void StMuFttPoint::setPlane(UChar_t plane) { mPlane = plane; }
inline void StMuFttPoint::setQuadrant(UChar_t quadrant) { mQuadrant = quadrant; }
inline void StMuFttPoint::setX(float xpos) { mX = xpos; }
inline void StMuFttPoint::setY(float ypos) { mY = ypos; }
// inline void StMuFttPoint::addCluster(StMuFttCluster* cluster) { mClusters.Add( cluster ); }
inline void StMuFttPoint::setXYZ(const TVector3& p3) { mXYZ = p3; }

#endif  // StMuFttPoint_h

