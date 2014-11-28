#ifndef evansPairCut_hh
#define evansPairCut_hh

//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"
#include "StHbtMaker/Infrastructure/StParityAnalysis.h"
#include "StHbtMaker/Infrastructure/StParityTypes.hh"

class evansPairCut : public StHbtPairCut{
public:
  evansPairCut();
  evansPairCut(const evansPairCut&);
  //~evansPairCut();

  virtual bool Pass(const StHbtPair*);
  void SetAngleCut(double);
  void SetPCutDistance(double);
  virtual void ParityPairCuts(ParityBuff*, ParityBuff*);
  virtual StHbtString Report();
  evansPairCut* Clone();


private:
  long mNPairsPassed;
  long mNPairsFailed;
  double AngleCut;  //  (In radians) see comments in .cxx code
  double pCutDistance; // (In Meters ) see comments in .cxx code
  double Bfield;       // (In Tesla for the distance cut) (see .cxx code)


#ifdef __ROOT__
  ClassDef(evansPairCut, 1)
#endif
};

inline evansPairCut::evansPairCut(const evansPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}

inline void evansPairCut::SetAngleCut(double CutValue) {
  AngleCut = CutValue;
}

inline void evansPairCut::SetPCutDistance(double CutValue) {
  pCutDistance = CutValue;
}

inline evansPairCut* evansPairCut::Clone() { evansPairCut* c = new evansPairCut(*this); return c;}

#endif
