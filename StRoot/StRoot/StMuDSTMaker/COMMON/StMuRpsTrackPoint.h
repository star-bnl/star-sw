#ifndef __StMuRpsTrackPoint_hh__
#define __StMuRpsTrackPoint_hh__

#include "TObject.h"
#include "TVector3.h"

// #include "StEvent/StRpsTrackPoint.h"

class StRpsTrackPoint;

class StMuRpsTrackPoint : public TObject {

public:
    enum StMuRpsTrackPointQuality {rpsNormal, rpsGolden, rpsNotSet};
    enum {mNumberOfPmtsInRp = 2, mNumberOfPlanesInRp = 4};
protected:

	// position (x, y, z)
	TVector3	mPosition;
	// 	RP# (e.g. E1U=0)
	Int_t		mRpId;
	// IDs of clusters in RpsCollection
	Int_t 		mClusterId[mNumberOfPlanesInRp];
	// Track Point Quality
    StMuRpsTrackPointQuality mQuality;
    // time
    double mTime[mNumberOfPmtsInRp];

public:
	StMuRpsTrackPoint();
    StMuRpsTrackPoint(const StMuRpsTrackPoint&);
	~StMuRpsTrackPoint();

    StMuRpsTrackPoint& operator=(const StMuRpsTrackPoint&);

	TVector3 positionVec() const;
    Int_t rpId() const;
    Int_t clusterId(unsigned int planeId ) const;
    double time(unsigned int) const;
    StMuRpsTrackPointQuality quality() const;
    
    double x() const;
    double y() const;
    double z() const;
    
    void setPosition(const TVector3&);
    void setRpId(Int_t);
    void setClusterId(Int_t, unsigned int);
    void setQuality(StMuRpsTrackPointQuality quality);
    unsigned int planesUsed() const;

    void setTime(double, unsigned int);


private:

	ClassDef(StMuRpsTrackPoint,1)
};

inline TVector3 StMuRpsTrackPoint::positionVec() const { return mPosition; }
inline Int_t StMuRpsTrackPoint::rpId() const { return mRpId; }
inline Int_t StMuRpsTrackPoint::clusterId(unsigned int planeId ) const {
    return planeId<mNumberOfPlanesInRp ? mClusterId[planeId] : -1;
}
inline StMuRpsTrackPoint::StMuRpsTrackPointQuality StMuRpsTrackPoint::quality() const { return mQuality; }
inline double StMuRpsTrackPoint::x() const { return mPosition.x(); }
inline double StMuRpsTrackPoint::y() const { return mPosition.y(); }
inline double StMuRpsTrackPoint::z() const { return mPosition.z(); }

inline void StMuRpsTrackPoint::setPosition(const TVector3& position){
    mPosition = position;
}
inline void StMuRpsTrackPoint::setRpId(Int_t rpId) { mRpId = rpId; }
inline void StMuRpsTrackPoint::setClusterId(int clusterId, unsigned int planeId) {
    if( planeId<mNumberOfPlanesInRp )
        mClusterId[planeId] = clusterId;
}
inline void StMuRpsTrackPoint::setQuality(StMuRpsTrackPointQuality quality ) {
    mQuality = quality;
}
inline double StMuRpsTrackPoint::time(unsigned int pmtId) const {
    return pmtId<mNumberOfPmtsInRp ? mTime[pmtId] : -1;
}
inline void StMuRpsTrackPoint::setTime(double timeVal, unsigned int pmtId){
    if( pmtId<mNumberOfPmtsInRp ) mTime[pmtId] = timeVal;
}



#endif