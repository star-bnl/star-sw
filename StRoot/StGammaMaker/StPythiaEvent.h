// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//

#ifndef ST_PYTHIA_EVENT
#define ST_PYTHIA_EVENT

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
  TClonesArray* partons();
  TParticle* parton(int i);
  int numberOfPartons() const;
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
  void addParton(const particle_st& parton);

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
  TClonesArray* mPartons;

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
inline TClonesArray* StPythiaEvent::partons() { return mPartons; }
inline TParticle* StPythiaEvent::parton(int i) { return (TParticle*)mPartons->At(i); }
inline int StPythiaEvent::numberOfPartons() const { return mPartons->GetEntriesFast(); }

inline void StPythiaEvent::addParton(const particle_st& parton)
{
  new ((*mPartons)[mPartons->GetEntriesFast()])
    TParticle(parton.idhep,
	      parton.isthep,
              parton.jmohep[0],
	      parton.jmohep[1],
              parton.jdahep[0],
              parton.jdahep[1],
              TLorentzVector(parton.phep),
              TLorentzVector(parton.vhep));
}

inline void StPythiaEvent::Clear(Option_t* option) { mPartons->Clear(option); }

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
