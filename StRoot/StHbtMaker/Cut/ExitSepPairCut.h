/***************************************************************************
 *
 * $Id: ExitSepPairCut.h,v 1.1 2000/09/14 18:36:58 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Exit Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: ExitSepPairCut.h,v $
 * Revision 1.1  2000/09/14 18:36:58  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 **************************************************************************/


#ifndef ExitSepPairCut_hh
#define ExitSepPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class ExitSepPairCut : public StHbtPairCut{
public:
  ExitSepPairCut();
  ExitSepPairCut(const ExitSepPairCut&);
  //~ExitSepPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  ExitSepPairCut* Clone();

  void SetExitSepRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mEntSepLo;
  double mEntSepHi;

#ifdef __ROOT__
  ClassDef(ExitSepPairCut, 1)
#endif
};

inline ExitSepPairCut::ExitSepPairCut(const ExitSepPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline ExitSepPairCut* ExitSepPairCut::Clone() { ExitSepPairCut* c = new ExitSepPairCut(*this); return c;}

#endif
