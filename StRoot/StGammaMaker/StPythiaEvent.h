// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//

#ifndef ST_PYTHIA_EVENT
#define ST_PYTHIA_EVENT

#include <vector>
using std::vector;

#include "TParticle.h"
#include "TClonesArray.h"

#include "tables/St_particle_Table.h"

class StPythiaEvent : public TObject {
public:
  StPythiaEvent();
  ~StPythiaEvent();

  int runId() const;
  int eventId() const;
  int processId() const;
  TVector3& vertex();
  const TVector3& vertex() const;
  float s() const;
  float t() const;
  float u() const;
  float pt() const;
  float cosTheta() const;
  float x1() const;
  float x2() const;
  float Q2() const;
  float aLL() const;
  float ALL_LO() const;
  float ALL_NLO() const;
  float ALL_NLO_g0() const;
  float ALL_NLO_gmax() const;
  float ALL_NLO_gmin() const;
  TClonesArray* particles();
  TParticle* particle(int i);
  int numberOfParticles() const;
  vector<TLorentzVector>& pion04Mom();
  vector<TLorentzVector>& prompt4Mom();
  vector<TLorentzVector>& decay4Mom();
  vector<TLorentzVector>& frag4Mom();
  vector<TLorentzVector>& initial4Mom();
  vector<TLorentzVector>& final4Mom();

  void Clear(Option_t* option = "");
  void setRunId(int id);
  void setEventId(int id);
  void setProcessId(int id);
  void setVertex(const TVector3& v);
  void setS(float s);
  void setU(float u);
  void setT(float t);
  void setPt(float pt);
  void setCosTheta(float cosTheta);
  void setX1(float x1);
  void setX2(float x2);
  void setALL(float a);
  void setALL_LO(float a);
  void setALL_NLO(float a);
  void setALL_NLO_g0(float a);
  void setALL_NLO_gmax(float a);
  void setALL_NLO_gmin(float a);
  void addParticle(const particle_st& particle);

private:
  int mRunId;
  int mEventId;
  int mProcessId;
  TVector3 mVertex;
  float mS;
  float mT;
  float mU;
  float mPt;
  float mCosTheta;
  float mX1;
  float mX2;
  float mALL;
  float mALL_LO;
  float mALL_NLO;
  float mALL_NLO_g0;
  float mALL_NLO_gmax;
  float mALL_NLO_gmin;
  TClonesArray* mParticles;

  vector<TLorentzVector> mPion04Mom;    // Four-momentum of pions in pythia record which may or may not decay in geant record
  vector<TLorentzVector> mPrompt4Mom;   // Four-momentum of prompt photons in pythia record
  vector<TLorentzVector> mDecay4Mom;    // Four-momentum of decay photon from pythia and geant record
  vector<TLorentzVector> mFrag4Mom;     // Four-momentum of fragmentation photons
  vector<TLorentzVector> mInitial4Mom;  // Four-momentum of initial state radiation
  vector<TLorentzVector> mFinal4Mom;    // Four-momentum of final state radiation  

  ClassDef(StPythiaEvent, 1);
};

inline int StPythiaEvent::runId() const { return mRunId; }
inline int StPythiaEvent::eventId() const { return mEventId; }
inline int StPythiaEvent::processId() const { return mProcessId; }
inline TVector3& StPythiaEvent::vertex() { return mVertex; }
inline const TVector3& StPythiaEvent::vertex() const { return mVertex; }
inline float StPythiaEvent::s() const { return mS; }
inline float StPythiaEvent::t() const { return mT; }
inline float StPythiaEvent::u() const { return mU; }
inline float StPythiaEvent::pt() const { return mPt; }
inline float StPythiaEvent::cosTheta() const { return mCosTheta; }
inline float StPythiaEvent::x1() const { return mX1; }
inline float StPythiaEvent::x2() const { return mX2; }
inline float StPythiaEvent::Q2() const { return mPt * mPt; }
inline float StPythiaEvent::aLL() const { return mALL; }
inline float StPythiaEvent::ALL_LO() const { return mALL_LO; }
inline float StPythiaEvent::ALL_NLO() const { return mALL_NLO; }
inline float StPythiaEvent::ALL_NLO_g0() const { return mALL_NLO_g0; }
inline float StPythiaEvent::ALL_NLO_gmax() const { return mALL_NLO_gmax; }
inline float StPythiaEvent::ALL_NLO_gmin() const { return mALL_NLO_gmin; }
inline TClonesArray* StPythiaEvent::particles() { return mParticles; }
inline TParticle* StPythiaEvent::particle(int i) { return (TParticle*)mParticles->At(i); }
inline int StPythiaEvent::numberOfParticles() const { return mParticles->GetEntriesFast(); }
inline vector<TLorentzVector>& StPythiaEvent::pion04Mom() { return mPion04Mom; }
inline vector<TLorentzVector>& StPythiaEvent::prompt4Mom() { return mPrompt4Mom; }
inline vector<TLorentzVector>& StPythiaEvent::decay4Mom() { return mDecay4Mom; }
inline vector<TLorentzVector>& StPythiaEvent::frag4Mom() { return mFrag4Mom; }
inline vector<TLorentzVector>& StPythiaEvent::initial4Mom() { return mInitial4Mom; }
inline vector<TLorentzVector>& StPythiaEvent::final4Mom() { return mFinal4Mom; }

inline void StPythiaEvent::addParticle(const particle_st& particle)
{
  new ((*mParticles)[mParticles->GetEntriesFast()])
    TParticle(particle.idhep,
	      particle.isthep,
              particle.jmohep[0],
	      particle.jmohep[1],
              particle.jdahep[0],
              particle.jdahep[1],
              TLorentzVector(particle.phep),
              TLorentzVector(particle.vhep));
}

inline void StPythiaEvent::Clear(Option_t* option) { mParticles->Clear(option); }
inline void StPythiaEvent::setRunId(int id) { mRunId = id; }
inline void StPythiaEvent::setEventId(int id) { mEventId = id; }
inline void StPythiaEvent::setProcessId(int id) { mProcessId = id; }
inline void StPythiaEvent::setVertex(const TVector3& v) { mVertex = v; }
inline void StPythiaEvent::setS(float s) { mS = s; }
inline void StPythiaEvent::setT(float t) { mT = t; }
inline void StPythiaEvent::setU(float u) { mU = u; }
inline void StPythiaEvent::setPt(float pt) { mPt = pt; }
inline void StPythiaEvent::setCosTheta(float cosTheta) { mCosTheta = cosTheta; }
inline void StPythiaEvent::setX1(float x1) { mX1 = x1; }
inline void StPythiaEvent::setX2(float x2) { mX2 = x2; }
inline void StPythiaEvent::setALL(float a) { mALL = a; }
inline void StPythiaEvent::setALL_LO(float a) { mALL_LO = a; }
inline void StPythiaEvent::setALL_NLO(float a) { mALL_NLO = a; }
inline void StPythiaEvent::setALL_NLO_g0(float a) { mALL_NLO_g0 = a; }
inline void StPythiaEvent::setALL_NLO_gmax(float a) { mALL_NLO_gmax = a; }
inline void StPythiaEvent::setALL_NLO_gmin(float a) { mALL_NLO_gmin = a; }

#endif
