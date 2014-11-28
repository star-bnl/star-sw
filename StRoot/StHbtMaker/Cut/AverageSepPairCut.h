/***************************************************************************
 *
 * $Id: AverageSepPairCut.h,v 1.1 2000/10/05 23:09:02 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on average Entrance Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: AverageSepPairCut.h,v $
 * Revision 1.1  2000/10/05 23:09:02  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 *
 *
 **************************************************************************/


#ifndef AverageSepPairCut_hh
#define AverageSepPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class AverageSepPairCut : public StHbtPairCut{
public:
  AverageSepPairCut();
  AverageSepPairCut(const AverageSepPairCut&);
  //~AverageSepPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  AverageSepPairCut* Clone();

  void SetAveSepRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mAveSepLo;
  double mAveSepHi;

#ifdef __ROOT__
  ClassDef(AverageSepPairCut, 1)
#endif
};

inline AverageSepPairCut::AverageSepPairCut(const AverageSepPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline AverageSepPairCut* AverageSepPairCut::Clone() { AverageSepPairCut* c = new AverageSepPairCut(*this); return c;}

#endif
