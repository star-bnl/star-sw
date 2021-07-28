/***************************************************************************
 *
 * $Id: kTPairCut.h,v 1.2 2002/05/17 14:45:50 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on kT of the pair
 *
 ***************************************************************************
 *
 * $Log: kTPairCut.h,v $
 * Revision 1.2  2002/05/17 14:45:50  mercedes
 * Pair cut on kT, k is four-momentum of the pair
 *
 *
 **************************************************************************/

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
