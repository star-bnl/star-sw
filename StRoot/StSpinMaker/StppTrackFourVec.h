//StppTrackFourVec.h
//M.L. Miller (Yale Software)
//07/02

#ifndef StppTrackFourVec_HH
#define StppTrackFourVec_HH

#include "StJetFinder/FourVec.h"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StppTrack.h"

class StppTrackFourVec : public AbstractFourVec
{
public:
    
    StppTrackFourVec(StppTrack*);
    virtual ~StppTrackFourVec() {};
    
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

    StppTrack* particle() const {return mTrack;}
    
protected:
    StppTrackFourVec(); //not implemented
    StppTrack* mTrack;
    StLorentzVectorF mVec;
};

// --- inlines

inline double StppTrackFourVec::pt() const
{
    return mVec.perp();
}

inline double StppTrackFourVec::px() const
{
    return mVec.px();
}

inline double StppTrackFourVec::py() const
{
    return mVec.py();
}

inline double StppTrackFourVec::pz() const
{
    return mVec.pz();
}

inline double StppTrackFourVec::p() const
{
    return mVec.vect().mag();
}

//angles
inline double StppTrackFourVec::theta() const
{
    return mVec.theta();
}

inline double StppTrackFourVec::phi() const
{
    return mVec.phi();
}

inline double StppTrackFourVec::eta() const
{
    return mVec.pseudoRapidity();
}

inline double StppTrackFourVec::rapidity() const
{
    return mVec.rapidity();
}

//4-th component
inline double StppTrackFourVec::eT() const
{
    return sqrt(e()*e()*pt()*pt()/(p()*p()));
}

inline double StppTrackFourVec::eZ() const
{
    return eT()*sinh(eta());
}

inline double StppTrackFourVec::e() const
{
    return mVec.e();
}

inline double StppTrackFourVec::mass() const
{
    return mVec.m();
}

//charge
inline double StppTrackFourVec::charge() const
{
    return static_cast<double>( mTrack->charge );
}

#endif
