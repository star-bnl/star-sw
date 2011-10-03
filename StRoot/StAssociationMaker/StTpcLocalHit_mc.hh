/* Generated local hits in the TPC - MALisa 9jun99 */

#ifndef StTpcLocalHit_mc_HH
#define StTpcLocalHit_mc_HH

#include "StLocalHit.hh"
#include <iostream.h>

class StMcTpcHit;

class StTpcLocalHit_mc : public StLocalHit {

public:
  StTpcLocalHit_mc(const StMcTpcHit*, float, float);
  ~StTpcLocalHit_mc();

  StMcTpcHit* globalHitPtr(){return mHit;};

private:
  StMcTpcHit* mHit;   // points to the hit in the StMcTpcHit collection
};
ostream& operator<<(ostream&, const StTpcLocalHit_mc&);

#endif
