/***************************************************************************
 * 
 * $Id: StRpsTrack.h,v 2.1 2015/10/02 19:48:14 ullrich Exp $
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

    void setTrackPoint(StRpsTrackPoint* const, unsigned int);
    void setP(const StThreeVectorF&);
    void setBranch(int);
    void setType(StRpsTrackType);

private:
    static const unsigned int mNumberOfStationsInBranch = 2;  //!
  
    StPtrVecRpsTrackPoint mTrackPoints;	 // pointers to track points (local tracks)
    StThreeVectorF mP;				// three-vector with reconstructed track momentum
    Int_t          mBranch;			// detectors branch, EU=0, WU=1, ED=2, WD=3
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
inline double StRpsTrack::xi(double beamMomentum) const
{
    return (beamMomentum - mP.mag())/beamMomentum;
}
inline double StRpsTrack::p() const { return mP.mag(); }
inline double StRpsTrack::pt() const { return mP.perp(); }
inline double StRpsTrack::eta() const { return mP.pseudoRapidity(); }

inline void StRpsTrack::setTrackPoint(StRpsTrackPoint* const trackPoint, unsigned int station)
{
    if (station<mNumberOfStationsInBranch)
        mTrackPoints[station] = trackPoint;
}
inline void StRpsTrack::setP(const StThreeVectorF& P) { mP = P; }
inline void StRpsTrack::setBranch(int branch) { mBranch = branch; }
inline void StRpsTrack::setType(const StRpsTrack::StRpsTrackType type) { mType = type; }

#endif
