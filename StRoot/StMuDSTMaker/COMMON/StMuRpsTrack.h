#ifndef __StMuRpsTrack_hh__
#define __StMuRpsTrack_hh__

#include "TObject.h"
#include "TVector3.h"
#include "TRef.h"
#include <vector>

using namespace std;

class StRpsTrackPoint;
class StRpsTrack;

class StMuRpsTrack : public TObject {
public:
	enum StMuRpsTrackType { rpsLocal, rpsGlobal, rpsUndefined };
    enum StMuRpsAngles { rpsAngleThetaX, rpsAngleThetaY, rpsAngleTheta, mNumberOfAngleTypes };
    enum {mNumberOfStationsInBranch = 2};

protected:
    TRef mTrackPoints[mNumberOfStationsInBranch];   // reference to track points (local tracks)
    TVector3 mP;								    // three-vector with reconstructed track momentum
    Int_t          mBranch;                         // detectors branch, EU=0, ED=1, WU=2, WD=3 
    StMuRpsTrackType mType;                         // type of the track
    
public:
    StMuRpsTrack();
    StMuRpsTrack(const StMuRpsTrack&);
    ~StMuRpsTrack();

    StMuRpsTrack& operator=(const StMuRpsTrack&);

    const StMuRpsTrackPoint* trackPoint(unsigned int)const ;
    TVector3 pVec() const;
    int branch() const;
    StMuRpsTrackType type() const;
    unsigned int planesUsed() const;
    
    double theta(unsigned int = rpsAngleTheta) const;
    double thetaRp(unsigned int = rpsAngleTheta) const;
    double phi() const;
    double phiRp() const;
    double t(double) const;
    double xi(double) const;
    double p() const;
    double pt() const;
    double eta() const;
    double time() const;

    void setTrackPoint( StMuRpsTrackPoint*, unsigned int);
    void setP(const TVector3&);
    void setBranch(int);
    void setType(StMuRpsTrackType type);
    
    ClassDef(StMuRpsTrack, 1)
};

inline const StMuRpsTrackPoint* StMuRpsTrack::trackPoint(unsigned int station) const {
    return station < mNumberOfStationsInBranch ? static_cast<const StMuRpsTrackPoint*>(mTrackPoints[station].GetObject()) : nullptr;
}
inline TVector3 StMuRpsTrack::pVec() const { return mP; }
inline int StMuRpsTrack::branch() const { return mBranch; }
inline StMuRpsTrack::StMuRpsTrackType StMuRpsTrack::type() const { return mType; }
inline double StMuRpsTrack::phi() const { return mP.Phi(); }
inline double StMuRpsTrack::t(double beamMomentum) const {
  return -2*beamMomentum*beamMomentum*(1-xi(beamMomentum))*(1-cos(theta(rpsAngleTheta)));
}
inline double StMuRpsTrack::xi(double beamMomentum) const {
    return (beamMomentum - mP.Mag())/beamMomentum;
}
inline double StMuRpsTrack::p() const { return mP.Mag(); }
inline double StMuRpsTrack::pt() const { return mP.Perp(); }
inline double StMuRpsTrack::eta() const { return mP.PseudoRapidity(); }

inline void StMuRpsTrack::setTrackPoint(StMuRpsTrackPoint* trackPoint, unsigned int station) {
    if (station<mNumberOfStationsInBranch)
        mTrackPoints[station] = trackPoint;
}
inline void StMuRpsTrack::setP(const TVector3& P) { mP = P; }
inline void StMuRpsTrack::setBranch(int branch) { mBranch = branch; }
inline void StMuRpsTrack::setType(StMuRpsTrackType type) { mType = type; }

#endif
