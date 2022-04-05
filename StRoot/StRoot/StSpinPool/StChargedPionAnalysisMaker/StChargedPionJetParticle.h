#ifndef ST_CHARGED_PION_JET_PARTICLE_HH
#define ST_CHARGED_PION_JET_PARTICLE_HH

// $Id: StChargedPionJetParticle.h,v 1.4 2009/01/04 17:48:10 kocolosk Exp $

#include "Math/PtEtaPhiE4D.h"
#include "Math/LorentzVector.h"
using namespace ROOT::Math;

#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEvent/StEnumerations.h"

#include "TLorentzVector.h"

class StChargedPionJetParticle : public LorentzVector< PtEtaPhiE4D<Double32_t> > {
public:
    StChargedPionJetParticle();
    virtual ~StChargedPionJetParticle();
    
    int index();
    StDetectorId detectorId();
    
    int charge();
    unsigned int nHits();
    unsigned int nHitsPoss();
    unsigned int nHitsFit();
    unsigned int nHitsDEdx();
    double nSigmaPion();
    
    StThreeVectorF&         globalDca();
    const StThreeVectorF&   globalDca() const;
    
    double z(const TLorentzVector&) const;
    
    void setIndex(short);
    void setDetectorId(StDetectorId);
    
    void setCharge(char);
    void setNHits(unsigned char);
    void setNHitsFit(unsigned char);
    void setNHitsPoss(unsigned char);
    void setNHitsDEdx(unsigned char);
    void setNSigmaPion(float);
    
    void setGlobalDca(StThreeVectorF&);
    
private:
    Short_t mIndex;
    Char_t mDetectorId;
    Char_t mCharge;
    UChar_t mNHits;
    UChar_t mNHitsFit;
    UChar_t mNHitsDEdx;
    UChar_t mNHitsPoss;
    Float_t mNSigmaPion;
    StThreeVectorF mGlobalDca;
    
    ClassDef(StChargedPionJetParticle, 2)
};

inline int StChargedPionJetParticle::index() { return mIndex; }
inline StDetectorId StChargedPionJetParticle::detectorId() { return StDetectorId(mDetectorId); }
inline int StChargedPionJetParticle::charge() { return mCharge; }
inline unsigned int StChargedPionJetParticle::nHits() { return mNHits; }
inline unsigned int StChargedPionJetParticle::nHitsPoss() { return mNHitsPoss; }
inline unsigned int StChargedPionJetParticle::nHitsFit() { return mNHitsFit; }
inline unsigned int StChargedPionJetParticle::nHitsDEdx() { return mNHitsDEdx; }
inline double StChargedPionJetParticle::nSigmaPion() { return mNSigmaPion; }
inline StThreeVectorF& StChargedPionJetParticle::globalDca() { return mGlobalDca; }
inline const StThreeVectorF& StChargedPionJetParticle::globalDca() const { return mGlobalDca; }

inline void StChargedPionJetParticle::setIndex(short a) { mIndex = a; }
inline void StChargedPionJetParticle::setDetectorId(StDetectorId a) { mDetectorId = char(a); }
inline void StChargedPionJetParticle::setCharge(char a) { mCharge = a; }
inline void StChargedPionJetParticle::setNHits(unsigned char a) { mNHits = a; }
inline void StChargedPionJetParticle::setNHitsFit(unsigned char a) { mNHitsFit = a; }
inline void StChargedPionJetParticle::setNHitsPoss(unsigned char a) { mNHitsPoss = a; }
inline void StChargedPionJetParticle::setNHitsDEdx(unsigned char a) { mNHitsDEdx = a; }
inline void StChargedPionJetParticle::setNSigmaPion(float a) { mNSigmaPion = a; }
inline void StChargedPionJetParticle::setGlobalDca(StThreeVectorF & a) { mGlobalDca = a; }

#endif

/*****************************************************************************
 * $Log: StChargedPionJetParticle.h,v $
 * Revision 1.4  2009/01/04 17:48:10  kocolosk
 * include StEnumerations.h instead of deprecated StDetectorId.h
 *
 * Revision 1.3  2008/12/29 15:58:30  kocolosk
 * removed commented code and added Id and Log as needed
 *
 *****************************************************************************/
