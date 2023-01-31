#ifndef STMUFTTCLUSTER_H
#define STMUFTTCLUSTER_H

#include <TObject.h>
#include <TRefArray.h>
#include "StEnumerations.h"

class StMuFttRawHit;
class StMuFttPoint;
class StFttCluster;

class StMuFttCluster : public TObject {
public:
    StMuFttCluster();
    ~StMuFttCluster();
    
    int id() const; // Cluster ID
    UChar_t plane()   const; // Detector plane.
    UChar_t quadrant()   const; // detector quadrant.
    UChar_t orientation() const;
    int nStrips() const;
    int nPoints() const;
    int nRawHits() const; // == nStrips?? need both?
    int nNeighbors() const;
    float sumAdc() const;
    float x() const;  // Mean x ("center of gravity") in local grid coordinate (1st moment).
    float sigma() const; // Maximum 2nd moment (along major axis).

    void setId(int cluid);
    void setPlane(UChar_t plane);
    void setQuadrant(UChar_t quad);
    void setOrientation( UChar_t );
    void setNStrips(int numStrips);
    void setSumAdc(int theSumAdc);
    void setX(float x0);
    void setSigma(float sigma);

    TRefArray* rawHits();
    const TRefArray* rawHits() const;
    void addRawHit(StMuFttRawHit* p);
    void addNeighbor(StMuFttCluster* neighbor);
    TRefArray* neighbor();
    const TRefArray* neighbor() const;    
    TRefArray* points();
    const TRefArray* points() const;
    void addPoint(StMuFttPoint* p);
    // void print(Option_t *option="") const;

    void set( StFttCluster * clu );

private:
    Int_t mId=-1;             // Eventwise cluster ID
    UChar_t mPlane;
    UChar_t mQuadrant;
    UChar_t mOrientation = kFttUnknownOrientation;        // Orientation of cluster
    Int_t mNStrips=0;         // Number of strips
    Float_t mSumAdc=0.0;      // Total ADC (0th moment)
    Float_t mX=0.0;             // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigma=0.0;        // 2nd moment
    TRefArray mRawHits;            // Tower hits of the current cluster
    TRefArray mNeighbors;    // Neighbor clusters
    TRefArray mPoints;        // Fitted points (photons) in the cluster

    ClassDef(StMuFttCluster, 1)
};


inline int StMuFttCluster::id() const { return mId; } // Cluster ID
inline UChar_t StMuFttCluster::plane() const { return mPlane; }
inline UChar_t StMuFttCluster::quadrant() const { return mQuadrant; }
inline UChar_t StMuFttCluster::orientation() const { return mOrientation; }
inline int StMuFttCluster::nStrips() const { return mNStrips; }
inline int StMuFttCluster::nRawHits() const { return mRawHits.GetSize(); }
inline int StMuFttCluster::nNeighbors() const { return mNeighbors.GetSize(); }
inline int StMuFttCluster::nPoints() const { return mPoints.GetSize(); }
inline float StMuFttCluster::sumAdc() const { return mSumAdc; }
inline float StMuFttCluster::x() const { return mX; } // Mean x ("center of gravity") in local grid coordinate (1st moment).
inline float StMuFttCluster::sigma() const { return mSigma; } // 2nd moment

inline void StMuFttCluster::setPlane(UChar_t plane) { mPlane = plane; }
inline void StMuFttCluster::setQuadrant(UChar_t quadrant) { mQuadrant = quadrant; }
inline void StMuFttCluster::setOrientation( UChar_t so ) { mOrientation = so; }
inline void StMuFttCluster::setNStrips(int numStrips) { mNStrips = numStrips; }
inline void StMuFttCluster::setSumAdc(int theSumAdc) { mSumAdc = theSumAdc; }
inline void StMuFttCluster::setX(float x0) { mX = x0; }
inline void StMuFttCluster::setSigma(float sigma) { mSigma = sigma; }

inline void StMuFttCluster::setId(int cluid) { mId = cluid; }

inline TRefArray* StMuFttCluster::rawHits() { return &mRawHits; }
inline const TRefArray* StMuFttCluster::rawHits() const { return &mRawHits; }
inline TRefArray* StMuFttCluster::neighbor() { return &mNeighbors; }
inline const TRefArray* StMuFttCluster::neighbor() const { return &mNeighbors; }
inline TRefArray* StMuFttCluster::points() { return &mPoints; }
inline const TRefArray* StMuFttCluster::points() const { return &mPoints; }

#endif  // STMUFTTCLUSTER_H
