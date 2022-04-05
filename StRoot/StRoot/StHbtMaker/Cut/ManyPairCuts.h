/***************************************************************************
 *
 * $Id: ManyPairCuts.h,v 1.1 2000/07/31 01:19:24 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   ManyPairCuts is a StHbtPairCut that simply owns a collection
 *   of StHbtPairCut objects, and returns the AND of their return values
 *
 ***************************************************************************
 *
 * $Log: ManyPairCuts.h,v $
 * Revision 1.1  2000/07/31 01:19:24  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 *
 **************************************************************************/


#ifndef ManyPairCuts_hh
#define ManyPairCuts_hh

#include "StHbtMaker/Infrastructure/StHbtPairCutCollection.hh"
#include "StHbtMaker/Base/StHbtPairCut.h"

class ManyPairCuts : public StHbtPairCut{
public:
  ManyPairCuts();
  ManyPairCuts(const ManyPairCuts&);
  //~ManyPairCuts();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  ManyPairCuts* Clone();

  void AddPairCut(StHbtPairCut*);


private:
  long mNPairsPassed;
  long mNPairsFailed;
  StHbtPairCutCollection mPairCutCollection;


#ifdef __ROOT__
  ClassDef(ManyPairCuts, 0)
#endif
};

inline ManyPairCuts::ManyPairCuts(const ManyPairCuts& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline ManyPairCuts* ManyPairCuts::Clone() { ManyPairCuts* c = new ManyPairCuts(*this); return c;}

inline void ManyPairCuts::AddPairCut(StHbtPairCut* pc){mPairCutCollection.push_back(pc);}

#endif
