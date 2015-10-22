/***************************************************************************
 * 
 * $Id: StRpsTrack.h,v 2.4 2015/10/22 20:31:31 ullrich Exp $
 *
 * Author: Rafal Sikora, 1 Oct 2015
 *
 ***************************************************************************
 *
 * Description: StRpsTrack class representing reconstructed track in 
 * the Roman Pot system, with all associated observables, such as 
 * momentum (px, py, pz) etc..
 *
 ***************************************************************************
 *
 * $Log: StRpsTrack.h,v $
 * Revision 2.4  2015/10/22 20:31:31  ullrich
 * StRpsTrack.cxx
 *
 * Revision 2.3  2015/10/08 20:53:34  ullrich
 * Changed comment of mBranch
 *
 * Revision 2.2  2015/10/07 17:30:13  ullrich
 * Changed const to enums and related changes.
 *
 * Revision 2.1  2015/10/02 19:48:14  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#ifndef StRpsTrack_hh
#define StRpsTrack_hh

#include "StObject.h"
#include "StContainers.h"
#include "StThreeVectorF.hh"

class StRpsTrackPoint;

class StRpsTrack : public StObject {
public:
    StRpsTrack();
    StRpsTrack(const StRpsTrack&);
    ~StRpsTrack();

    StRpsTrack& operator=(const StRpsTrack&);
    enum StRpsTrackType { rpsLocal, rpsGlobal, rpsUndefined };
    enum StRpsAngles { rpsAngleThetaX, rpsAngleThetaY, rpsAngleTheta, mNumberOfAngleTypes };

    StRpsTrackPoint* trackPoint(unsigned int) const;
    StThreeVectorF pVec() const;
    int branch() const;
    StRpsTrackType type() const;
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

    void setTrackPoint(StRpsTrackPoint*, unsigned int);
    void setP(const StThreeVectorF&);
    void setBranch(int);
    void setType(StRpsTrackType);

    enum {mNumberOfStationsInBranch = 2};

private:
    StPtrVecRpsTrackPoint mTrackPoints;	 // pointers to track points (local tracks)
    StThreeVectorF mP;				// three-vector with reconstructed track momentum
    Int_t          mBranch;			// detectors branch, EU=0, ED=1, WU=2, WD=3 
    StRpsTrackType mType;			// type of the track
    
    ClassDef(StRpsTrack, 1)
};

inline StRpsTrackPoint* StRpsTrack::trackPoint(unsigned int station) const
{
    return station < mNumberOfStationsInBranch ? mTrackPoints[station] : nullptr;
}
inline StThreeVectorF StRpsTrack::pVec() const { return mP; }
inline int StRpsTrack::branch() const { return mBranch; }
inline StRpsTrack::StRpsTrackType StRpsTrack::type() const { return mType; }
inline double StRpsTrack::phi() const { return mP.phi(); }
inline double StRpsTrack::t(double beamMomentum) const
{
  return -2*beamMomentum*beamMomentum*(1-xi(beamMomentum))*(1-cos(theta(rpsAngleTheta)));
}
inline double StRpsTrack::xi(double beamMomentum) const
{
    return (beamMomentum - mP.mag())/beamMomentum;
}
inline double StRpsTrack::p() const { return mP.mag(); }
inline double StRpsTrack::pt() const { return mP.perp(); }
inline double StRpsTrack::eta() const { return mP.pseudoRapidity(); }

inline void StRpsTrack::setTrackPoint(StRpsTrackPoint* trackPoint, unsigned int station)
{
    if (station<mNumberOfStationsInBranch)
        mTrackPoints[station] = trackPoint;
}
inline void StRpsTrack::setP(const StThreeVectorF& P) { mP = P; }
inline void StRpsTrack::setBranch(int branch) { mBranch = branch; }
inline void StRpsTrack::setType(StRpsTrack::StRpsTrackType type) { mType = type; }

#endif
