// -*- mode: C++ -*-
// $Id: StPythiaEvent.h,v 1.14 2012/12/10 21:52:46 pibero Exp $

// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//
// $Log: StPythiaEvent.h,v $
// Revision 1.14  2012/12/10 21:52:46  pibero
// More simplifications...
//
// Revision 1.13  2012/12/10 21:40:06  pibero
// Simplify code
//
// Revision 1.12  2012/12/06 20:45:41  pibero
// const correctess
//
// Revision 1.11  2012/12/06 20:37:26  pibero
// Print PYTHIA record
//
// Revision 1.10  2012/11/25 21:56:58  pibero
// small bug fix
//
// Revision 1.9  2012/11/24 17:16:03  pibero
// small bug fix
//
// Revision 1.8  2012/11/24 03:12:43  pibero
// Add LSS2010 and BB2010
//
// Revision 1.7  2012/01/18 18:11:36  pibero
// Added PYTHIA variables: MSTU(72), MSTU(73), and MSTP(111)
//
// Revision 1.6  2011/09/13 20:03:51  pibero
// DSSV2009 -> DSSV2009a
//
// Revision 1.5  2011/09/13 16:24:21  pibero
// Added DSSV2009 grid
//
// Revision 1.4  2010/10/04 19:18:29  pibero
// Fix copy constructor and assignment operator. Thanks, Alice!
//
// Revision 1.3  2009/12/14 01:22:47  pibero
// const correctness
//
// Revision 1.2  2009/12/08 15:14:24  pibero
// Added Pythia tune per Helen Caines request.
//
// Revision 1.1  2008/06/01 05:31:42  tai
// moved StPythiaEvent to StSpinPool/StJetSkimEvent
//
// Revision 1.9  2008/06/01 04:59:22  tai
// removed the dependency of StPythiaEvent on St_particle_Table.h
//
// Revision 1.8  2008/06/01 04:33:33  tai
// *** empty log message ***
//
// Revision 1.7  2008/05/02 02:36:48  rfatemi
// update ClassDef
//
// Revision 1.6  2008/05/01 01:36:39  rfatemi
// check in D. Staszak modifications - additional grids
//
// Revision 1.5  2008/02/03 01:27:17  rfatemi
// Included Gehrmann-Stirling PDFs
//
// Revision 1.4  2007/11/17 20:11:33  kocolosk
// remove printf
//
// Revision 1.3  2007/11/01 02:48:38  rfatemi
// Dave Staszak update with additional GRSV grids
//
// Revision 1.2  2007/08/06 17:06:05  rfatemi
// set default GRSV to standard
//
// Revision 1.1  2007/07/19 01:40:41  kocolosk
// use Pibero's StPythiaEvent class to supply mcAsymMaker results to user
//

#ifndef ST_PYTHIA_EVENT
#define ST_PYTHIA_EVENT

#include <cstdio>
#include <algorithm>
#include "TParticle.h"
#include "TClonesArray.h"

using std::fill;

class StPythiaEvent : public TObject {
public:
  StPythiaEvent();
  virtual ~StPythiaEvent();
  StPythiaEvent(const StPythiaEvent& other);
  StPythiaEvent& operator=(const StPythiaEvent& rhs);
  enum { NPDF=34 };
  enum PDF { LO=0, NLO=1, STD=1, ZERO=2, MAX=3, MIN=4, M015=5, M030=6, M045=7, M060=8, M075=9, M090=10, M105=11, P030=12, P045=13, P060=14, P070=15, GS_NLOA=16, GS_NLOB=17, GS_NLOC=18, DSSV=19, LSS1=20, LSS2=21, LSS3=22, AAC1=23, AAC2=24, AAC3=25, BB1=26, BB2=27, DNS1=28, DNS2=29, DSSV2009a=30, LSS2010_delGpos=31, LSS2010_chsign_delG=32, BB2010=33 };
  int runId() const;
  int eventId() const;
  int processId() const;
  int tune() const;
  const TVector3& vertex() const;
  float s() const;
  float t() const;
  float u() const;
  float pt() const;
  float cosTheta() const;
  float x1() const;
  float x2() const;
  int mstu72() const;
  int mstu73() const;
  int mstp111() const;
  float Q2() const;
  float partonALL() const;
  float dF1(PDF scenario = STD) const;
  float dF2(PDF scenario = STD) const;
  float f1(PDF scenario = STD) const;
  float f2(PDF scenario = STD) const;
  float ALL(PDF scenario = STD) const;

  const TClonesArray* particles() const;
  int numberOfParticles() const;
  const TParticle* particle(int i) const;
  
  void Clear(Option_t* option = "");
  void setRunId(int id);
  void setEventId(int id);
  void setProcessId(int id);
  void setTune(int tune);
  void setVertex(const TVector3& v);
  void setS(float s);
  void setT(float t);
  void setU(float u);
  void setPt(float pt);
  void setCosTheta(float cosTheta);
  void setX1(float x1);
  void setX2(float x2);
  void setMstu72(int mstu72);
  void setMstu73(int mstu73);
  void setMstp111(int mstp111);
  void setPartonALL(float a);
  void setDF1(PDF scenario, float val);
  void setDF2(PDF scenario, float val);
  void setF1(PDF scenario, float val);
  void setF2(PDF scenario, float val);
  
  void addParticle(const TParticle& particle);
  void print() const;
  void printHelper(int first, int last) const;
  
private:
  int mRunId;
  int mEventId;
  int mProcessId;
  int mTune;
  TVector3 mVertex;
  float mS;
  float mT;
  float mU;
  float mPt;
  float mCosTheta;
  float mX1;
  float mX2;
  int mMstu72;
  int mMstu73;
  int mMstp111;
  float mPartonALL;
  float mDF1[NPDF];  //[LO][NLO][ZERO][MAX][MIN][M015][M030][M045][M060][M075][M090][M105][P030][P045][P060][P070][NLOA][NLOB][NLOC][DSSV][LSS1][LSS2][LSS3][AAC1][AAC2][AAC3][BB1][BB2][DNS1][DNS2][DSSV2009a][LSS2010_delGpos][LSS2010_chsign_delG][BB2010]
  float mDF2[NPDF];  //[LO][NLO][ZERO][MAX][MIN][M015][M030][M045][M060][M075][M090][M105][P030][P045][P060][P070][NLOA][NLOB][NLOC][DSSV][LSS1][LSS2][LSS3][AAC1][AAC2][AAC3][BB1][BB2][DNS1][DNS2][DSSV2009a][LSS2010_delGpos][LSS2010_chsign_delG][BB2010]
  float mF1[2];   //[LO][NLO]
  float mF2[2];   //[LO][NLO]
  
  TClonesArray* mParticles;
  
  ClassDef(StPythiaEvent,7);
};

inline int StPythiaEvent::runId() const { return mRunId; }
inline int StPythiaEvent::eventId() const { return mEventId; }
inline int StPythiaEvent::processId() const { return mProcessId; }
inline int StPythiaEvent::tune() const { return mTune; }
inline const TVector3& StPythiaEvent::vertex() const { return mVertex; }
inline float StPythiaEvent::s() const { return mS; }
inline float StPythiaEvent::t() const { return mT; }
inline float StPythiaEvent::u() const { return mU; }
inline float StPythiaEvent::pt() const { return mPt; }
inline float StPythiaEvent::cosTheta() const { return mCosTheta; }
inline float StPythiaEvent::x1() const { return mX1; }
inline float StPythiaEvent::x2() const { return mX2; }
inline int StPythiaEvent::mstu72() const { return mMstu72; }
inline int StPythiaEvent::mstu73() const { return mMstu73; }
inline int StPythiaEvent::mstp111() const { return mMstp111; }
inline float StPythiaEvent::Q2() const { return mPt * mPt; }
inline float StPythiaEvent::partonALL() const { return mPartonALL; }
inline float StPythiaEvent::dF1(PDF scenario) const { return mDF1[scenario]; }
inline float StPythiaEvent::dF2(PDF scenario) const { return mDF2[scenario]; }

inline float StPythiaEvent::f1(PDF scenario) const 
{
  return (scenario == LO) ? mF1[LO] : mF1[NLO];
}

inline float StPythiaEvent::f2(PDF scenario) const 
{
  return (scenario == LO) ? mF2[LO] : mF2[NLO];
}

inline float StPythiaEvent::ALL(PDF scenario) const
{
  return (dF1(scenario)*dF2(scenario)*partonALL())/(f1(scenario)*f2(scenario));
}

inline const TClonesArray* StPythiaEvent::particles() const { return mParticles; }
inline int StPythiaEvent::numberOfParticles() const { return mParticles->GetEntriesFast(); }
inline const TParticle* StPythiaEvent::particle(int i) const { return (TParticle*)mParticles->At(i); }

inline void StPythiaEvent::addParticle(const TParticle& particle)
{
  new ((*mParticles)[mParticles->GetEntriesFast()]) TParticle(particle);
}

inline void StPythiaEvent::Clear(Option_t* option)
{
    mRunId = 0;
    mEventId = 0;
    mProcessId = 0;
    mTune = 0;
    mVertex.SetXYZ(0, 0, 0);
    mS = 0;
    mT = 0;
    mU = 0;
    mPt = 0;
    mCosTheta = 0;
    mX1 = 0;
    mX2 = 0;
    mMstu72 = 0;
    mMstu73 = 0;
    mMstp111 = 0;
    mPartonALL = 0;
    fill(mDF1,mDF1+NPDF,0);
    fill(mDF2,mDF2+NPDF,0);
    mF1[0] = 0; mF1[1] = 0;
    mF2[0] = 0; mF2[1] = 0;

    if (mParticles) mParticles->Clear(option);
}

inline void StPythiaEvent::setRunId(int id) { mRunId = id; }
inline void StPythiaEvent::setEventId(int id) { mEventId = id; }
inline void StPythiaEvent::setProcessId(int id) { mProcessId = id; }
inline void StPythiaEvent::setTune(int tune) { mTune = tune; }
inline void StPythiaEvent::setVertex(const TVector3& v) { mVertex = v; }
inline void StPythiaEvent::setS(float s) { mS = s; }
inline void StPythiaEvent::setT(float t) { mT = t; }
inline void StPythiaEvent::setU(float u) { mU = u; }
inline void StPythiaEvent::setPt(float pt) { mPt = pt; }
inline void StPythiaEvent::setCosTheta(float cosTheta) { mCosTheta = cosTheta; }
inline void StPythiaEvent::setX1(float x1) { mX1 = x1; }
inline void StPythiaEvent::setX2(float x2) { mX2 = x2; }
inline void StPythiaEvent::setMstu72(int mstu72) { mMstu72 = mstu72; }
inline void StPythiaEvent::setMstu73(int mstu73) { mMstu73 = mstu73; }
inline void StPythiaEvent::setMstp111(int mstp111) { mMstp111 = mstp111; }
inline void StPythiaEvent::setPartonALL(float a) { mPartonALL = a; }
inline void StPythiaEvent::setDF1(PDF scenario, float val) { mDF1[scenario] = val; }
inline void StPythiaEvent::setDF2(PDF scenario, float val) { mDF2[scenario] = val; }

inline void StPythiaEvent::setF1(PDF scenario, float val) 
{
    if(scenario == LO) mF1[0] = val;
    mF1[1] = val;
}

inline void StPythiaEvent::setF2(PDF scenario, float val) 
{
    if(scenario == LO) mF2[0] = val;
    mF2[1] = val;
}


inline void StPythiaEvent::print() const
{
  // Header
  puts("                            Event listing (standard)\n");
  puts("    I  particle/jet  K(I,1)   K(I,2) K(I,3)     K(I,4)      K(I,5)       P(I,1)       P(I,2)       P(I,3)       P(I,4)       P(I,5)\n");

  // Colliding protons
  printHelper(0,2);
  puts(" ==================================================================================================================================");

  // Hard collision
  printHelper(2,mstu72());
  puts(" ==================================================================================================================================");

  // Fragmentation
  printHelper(mstu72(),mstu73());
  puts(" ==================================================================================================================================");

  // Hadronization
  printHelper(mstu73(),numberOfParticles());
  puts(" ==================================================================================================================================");
}

inline void StPythiaEvent::printHelper(int first, int last) const
{
  for (int i = first; i < last; ++i) {
    const TParticle* part = particle(i);
    TLorentzVector mom;
    part->Momentum(mom);
    printf("%5d%14s%8d%9d%7d%11d%12d%13f%13f%13f%13f%13f\n",i+1,part->GetName(),part->GetStatusCode(),part->GetPdgCode(),part->GetFirstMother(),part->GetFirstDaughter(),part->GetLastDaughter(),mom.Px(),mom.Py(),mom.Pz(),mom.E(),mom.M());
  }
}

#endif
