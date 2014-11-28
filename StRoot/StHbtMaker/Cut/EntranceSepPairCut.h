/***************************************************************************
 *
 * $Id: EntranceSepPairCut.h,v 1.1 2000/07/31 01:19:24 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Entrance Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: EntranceSepPairCut.h,v $
 * Revision 1.1  2000/07/31 01:19:24  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 *
 **************************************************************************/


#ifndef EntranceSepPairCut_hh
#define EntranceSepPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class EntranceSepPairCut : public StHbtPairCut{
public:
  EntranceSepPairCut();
  EntranceSepPairCut(const EntranceSepPairCut&);
  //~EntranceSepPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  EntranceSepPairCut* Clone();

  void SetEntranceSepRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mEntSepLo;
  double mEntSepHi;

#ifdef __ROOT__
  ClassDef(EntranceSepPairCut, 1)
#endif
};

inline EntranceSepPairCut::EntranceSepPairCut(const EntranceSepPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline EntranceSepPairCut* EntranceSepPairCut::Clone() { EntranceSepPairCut* c = new EntranceSepPairCut(*this); return c;}

#endif
