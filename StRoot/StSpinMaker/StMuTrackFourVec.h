//StMuTrackFourVec.h
//M.L. Miller (Yale Software)
//07/02

#ifndef StMuTrackFourVec_HH
#define StMuTrackFourVec_HH

#include "StJetFinder/FourVec.h"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StarClassLibrary/StLorentzVectorF.hh"

class StMuTrackFourVec : public AbstractFourVec
{
public:
    
    StMuTrackFourVec(StMuTrack* track, StLorentzVectorF P, Int_t i);
    StMuTrackFourVec(StMuTrack*);
    StMuTrackFourVec(StMuTrack*, Int_t i);
    StMuTrackFourVec();
    virtual ~StMuTrackFourVec() {};
    
    //momenta
    virtual double pt() const;
    virtual double px() const;
    virtual double py() const;
    virtual double pz() const;
    virtual double p() const;

    //angles
    virtual double theta() const;
    virtual double phi() const;
    virtual double eta() const;
    virtual double rapidity() const;

    //4-th component
    virtual double eT() const;
    virtual double eZ() const;
    virtual double e() const;
    virtual double mass() const;

    //charge
    virtual double charge() const;

    StMuTrack* particle() const {return mTrack;}

    Int_t getIndex(void) { return index; };
    
    void Init(StMuTrack* track, StLorentzVectorF P, Int_t i);

protected:
    StMuTrack* mTrack;
    StLorentzVectorF mVec;
    Int_t index;
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

#endif
