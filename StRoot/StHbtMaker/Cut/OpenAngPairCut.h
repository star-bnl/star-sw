/***************************************************************************
 *
 * $Id: OpenAngPairCut.h,v 1.1 2000/10/26 16:09:11 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Opening angle of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: OpenAngPairCut.h,v $
 * Revision 1.1  2000/10/26 16:09:11  lisa
 * Added OpeningAngle PairCut class and method to StHbtPair
 *
 *
 **************************************************************************/


#ifndef OpenAngPairCut_hh
#define OpenAngPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class OpenAngPairCut : public StHbtPairCut{
public:
  OpenAngPairCut();
  OpenAngPairCut(const OpenAngPairCut&);
  //~OpenAngPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  OpenAngPairCut* Clone();

  void SetOpenAngRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mLo;
  double mHi;

#ifdef __ROOT__
  ClassDef(OpenAngPairCut, 1)
#endif
};

inline OpenAngPairCut::OpenAngPairCut(const OpenAngPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline OpenAngPairCut* OpenAngPairCut::Clone() { OpenAngPairCut* c = new OpenAngPairCut(*this); return c;}

#endif
