#ifndef ST_CHARGED_PION_JET_HH
#define ST_CHARGED_PION_JET_HH

#include <vector>
using std::vector;

#include "TLorentzVector.h"
#include "TClonesArray.h"

#include "StChargedPionJetParticle.h";

class StChargedPionJet : public TLorentzVector {
public:
    StChargedPionJet();
    StChargedPionJet(const StChargedPionJet&);
    virtual ~StChargedPionJet();
    
    void Clear(Option_t* = "");
    
    int charge();
    unsigned int nTpcTracks();
    unsigned int nBarrelTowers();
    unsigned int nEndcapTowers();
    
    double tpcEtSum();
    double barrelEtSum();
    double endcapEtSum();
    
    double vertexZ();
    
    vector<StChargedPionJetParticle>&       particles();
    const vector<StChargedPionJetParticle>& particles() const;
    
    void setCharge(char);
    void setNTpcTracks(unsigned short);
    void setNBarrelTowers(unsigned short);
    void setNEndcapTowers(unsigned short);
    void setTpcEtSum(float);
    void setBarrelEtSum(float);
    void setEndcapEtSum(float);
    void setVertexZ(float);
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
    
    // MUST keep these guys slim
    //TClonesArray* mParticles;
    vector<StChargedPionJetParticle> mParticles;
    
    ClassDef(StChargedPionJet, 1)
};

inline int StChargedPionJet::charge() { return mCharge; }
inline unsigned int StChargedPionJet::nTpcTracks() { return mTpcCount; }
inline unsigned int StChargedPionJet::nBarrelTowers() { return mBtowCount; }
inline unsigned int StChargedPionJet::nEndcapTowers() { return mEtowCount; }
inline double StChargedPionJet::tpcEtSum() { return mTpcEtSum; }
inline double StChargedPionJet::barrelEtSum() { return mBtowEtSum; }
inline double StChargedPionJet::endcapEtSum() { return mEtowEtSum; }
inline double StChargedPionJet::vertexZ() { return mVertexZ; }
//inline unsigned int StChargedPionJet::nParticles() { return mParticles->GetEntriesFast(); }
//inline TClonesArray* StChargedPionJet::particles() { return mParticles; }
//inline const TClonesArray* StChargedPionJet::particles() const { return mParticles; }
//inline unsigned int StChargedPionJet::nParticles() { return mParticles.size(); }
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