#ifndef ST_CHARGED_PION_JET_HH
#define ST_CHARGED_PION_JET_HH

// $Id: StChargedPionJet.h,v 1.7 2012/11/09 03:31:34 perev Exp $

#include <vector>
using std::vector;

#include "TLorentzVector.h"
#include "TClonesArray.h"

#include "StChargedPionJetParticle.h"

class StChargedPionJet : public TLorentzVector {
public:
    StChargedPionJet();
    StChargedPionJet(const StChargedPionJet&);
    virtual ~StChargedPionJet();
    
    void Clear(Option_t* = "");
    
    int charge() const;
    unsigned int nTpcTracks() const;
    unsigned int nBarrelTowers() const;
    unsigned int nEndcapTowers() const;
    
    double tpcEtSum() const;
    double barrelEtSum() const;
    double endcapEtSum() const;
    
    double vertexZ() const;
    
    float detectorEta() const;
    float detectorEta(float vz, float r=231.72) const;
    
    bool isTrigger(unsigned int trigId) const;
    
    vector<StChargedPionJetParticle>&       particles();
    const vector<StChargedPionJetParticle>& particles() const;
    
    const StChargedPionJetParticle& leadingParticle() const;
    
    void setCharge(char);
    void setNTpcTracks(unsigned short);
    void setNBarrelTowers(unsigned short);
    void setNEndcapTowers(unsigned short);
    void setTpcEtSum(float);
    void setBarrelEtSum(float);
    void setEndcapEtSum(float);
    void setVertexZ(float);
    void addTrigger(unsigned int trigId);
    void addParticle(StChargedPionJetParticle*);
    
private:
    /// all of these are possibly superfluous if we keep the particles
    Char_t mCharge;
    UShort_t mTpcCount;
    UShort_t mBtowCount;
    UShort_t mEtowCount;
    Float_t mTpcEtSum;
    Float_t mBtowEtSum;
    Float_t mEtowEtSum;
    
    // ok, this is probably useful
    Float_t mVertexZ;
    
    // something for geom triggers?
    UInt_t mGeomTriggers;
    
    // MUST keep these guys slim
    vector<StChargedPionJetParticle> mParticles;
    
    ClassDef(StChargedPionJet, 2)
};

inline int StChargedPionJet::charge() const { return mCharge; }
inline unsigned int StChargedPionJet::nTpcTracks() const { return mTpcCount; }
inline unsigned int StChargedPionJet::nBarrelTowers() const { return mBtowCount; }
inline unsigned int StChargedPionJet::nEndcapTowers() const { return mEtowCount; }
inline double StChargedPionJet::tpcEtSum() const { return mTpcEtSum; }
inline double StChargedPionJet::barrelEtSum() const { return mBtowEtSum; }
inline double StChargedPionJet::endcapEtSum() const { return mEtowEtSum; }
inline double StChargedPionJet::vertexZ() const { return mVertexZ; }
inline vector<StChargedPionJetParticle>& StChargedPionJet::particles() { return mParticles; }
inline const vector<StChargedPionJetParticle>& StChargedPionJet::particles() const { return mParticles; }

inline void StChargedPionJet::setCharge(char a) { mCharge = a; }
inline void StChargedPionJet::setNTpcTracks(unsigned short a) { mTpcCount = a; }
inline void StChargedPionJet::setNBarrelTowers(unsigned short a) { mBtowCount = a; }
inline void StChargedPionJet::setNEndcapTowers(unsigned short a) { mEtowCount = a; }
inline void StChargedPionJet::setTpcEtSum(float a) { mTpcEtSum = a; }
inline void StChargedPionJet::setBarrelEtSum(float a) { mBtowEtSum = a; }
inline void StChargedPionJet::setEndcapEtSum(float a) { mEtowEtSum = a; }
inline void StChargedPionJet::setVertexZ(float a) { mVertexZ = a; }

#endif

/*****************************************************************************
 * $Log: StChargedPionJet.h,v $
 * Revision 1.7  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.6  2008/12/29 15:58:29  kocolosk
 * removed commented code and added $Id: StChargedPionJet.h,v 1.7 2012/11/09 03:31:34 perev Exp $/$Log: StChargedPionJet.h,v $
 * removed commented code and added $Id$/Revision 1.7  2012/11/09 03:31:34  perev
 * removed commented code and added $Id$/Cleanup
 * removed commented code and added $Id$/ as needed
 *
 *****************************************************************************/
