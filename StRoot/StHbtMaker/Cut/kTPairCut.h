//kT pair cut

#ifndef kTPairCut_hh
#define kTPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class kTPairCut : public StHbtPairCut{
public:
  kTPairCut();
  kTPairCut(const kTPairCut&);
  //~kTPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  kTPairCut* Clone();

  void SetkTRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mkTLo;
  double mkTHi;

#ifdef __ROOT__
  ClassDef(kTPairCut, 1)
#endif
};

inline kTPairCut::kTPairCut(const kTPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline kTPairCut* kTPairCut::Clone() { kTPairCut* c = new kTPairCut(*this); return c;}

#endif
