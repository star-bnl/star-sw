//StMuTrackFourVec.h
//M.L. Miller (Yale Software)
//07/02

#ifndef StMuTrackFourVec_HH
#define StMuTrackFourVec_HH

#include <iostream>
#include <string>
using namespace std;

#include "StJetFinder/FourVec.h"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StDetectorId.h"

class StMuTrackFourVec : public AbstractFourVec
{
public:
    
    StMuTrackFourVec(StMuTrack* track, StLorentzVectorF P, Int_t i, StDetectorId detId);
    StMuTrackFourVec(StMuTrack*);
    StMuTrackFourVec(StMuTrack*, Int_t i);
    StMuTrackFourVec();
    virtual ~StMuTrackFourVec() {};
    
    ///momenta
    virtual double pt() const;
    virtual double px() const;
    virtual double py() const;
    virtual double pz() const;
    virtual double p() const;

    ///angles
    virtual double theta() const;
    virtual double phi() const;
    virtual double eta() const;
    virtual double rapidity() const;

    ///4-th component
    virtual double eT() const;
    virtual double eZ() const;
    virtual double e() const;
    virtual double mass() const;

    ///charge
    virtual double charge() const;

    ////Mu Track (null if it's an emc tower/hit/point) this will change soon
    StMuTrack* particle() const {return mTrack;}

    ///Index of the track/tower/cluster/point in the container that it came from
    Int_t getIndex(void) const { return index; }
    
    ///Id of the detector that generated this 4-vector
    StDetectorId detectorId() const {return mDetId;}
    
    void Init(StMuTrack* track, StLorentzVectorF P, Int_t i, StDetectorId detId);

    const StLorentzVectorF& vec() const {return mVec;}
    
protected:
    StMuTrack* mTrack;
    StLorentzVectorF mVec;
    Int_t index;
    StDetectorId mDetId;
};

// --- inlines

inline double StMuTrackFourVec::pt() const
{
    return mVec.perp();
}

inline double StMuTrackFourVec::px() const
{
    return mVec.px();
}

inline double StMuTrackFourVec::py() const
{
    return mVec.py();
}

inline double StMuTrackFourVec::pz() const
{
    return mVec.pz();
}

inline double StMuTrackFourVec::p() const
{
    return mVec.vect().mag();
}

//angles
inline double StMuTrackFourVec::theta() const
{
    return mVec.theta();
}

inline double StMuTrackFourVec::phi() const
{
    return mVec.phi();
}

inline double StMuTrackFourVec::eta() const
{
    return mVec.pseudoRapidity();
}

inline double StMuTrackFourVec::rapidity() const
{
    return mVec.rapidity();
}

//4-th component
inline double StMuTrackFourVec::eT() const
{
    return ::sqrt(e()*e()*pt()*pt()/(p()*p()));
}

inline double StMuTrackFourVec::eZ() const
{
    return eT()*sinh(eta());
}

inline double StMuTrackFourVec::e() const
{
    return mVec.e();
}

inline double StMuTrackFourVec::mass() const
{
    return mVec.m();
}

//charge
inline double StMuTrackFourVec::charge() const
{
    if(!mTrack) return 0;
    return static_cast<double>( mTrack->charge() );
}

inline ostream& operator<<(ostream& os, const StMuTrackFourVec& f)
{
    string idstring;
    StDetectorId mDetId = f.detectorId();
    if (mDetId==kTpcId) {
	idstring = "kTpcId";
    }
    else if (mDetId==kBarrelEmcTowerId) {
	idstring = "kBarrelEmcTowerId";
    }
    else if (mDetId==kEndcapEmcTowerId) {
	idstring = "kEndcapEmcTowerId";
    }
    else {
	idstring = "kUnknown";
    }
    
    return os <<"index:\t"<<f.getIndex()<<"\tP:\t"<<f.vec()<<"\tdetId:\t"<<f.detectorId()<<"\t"<<idstring;

}
#endif
