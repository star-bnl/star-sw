/***************************************************************************
 *
 * $Id: StHbtLikeSignCorrFctn.hh,v 1.2 2001/06/21 19:06:49 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    base class for a STAR correlation function.  Users should inherit 
 *    from this and must implement constructor, destructor, Report(),
 *    AddMixedPair(), AddRealPair(), AddLikeSignPositivePair(),
 *    AddLikeSignNegativePair(), Finish()
 *
 **************************************************************************/

#ifndef StHbtLikeSignCorrFctn_hh
#define StHbtLikeSignCorrFctn_hh

class StHbtPair;
#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class StHbtLikeSignCorrFctn : public StHbtCorrFctn {

public:
  StHbtLikeSignCorrFctn(){/* no-op */};
  StHbtLikeSignCorrFctn(const StHbtLikeSignCorrFctn& );
  virtual ~StHbtLikeSignCorrFctn(){/* no-op */};

  virtual void AddLikeSignPositivePair(const StHbtPair*) = 0;
  virtual void AddLikeSignNegativePair(const StHbtPair*) = 0;

  virtual StHbtLikeSignCorrFctn* Clone() { return 0;}

  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtLikeSignAnalysis;

};

inline StHbtLikeSignCorrFctn::StHbtLikeSignCorrFctn(const StHbtLikeSignCorrFctn& c) { myAnalysis =0; }

#endif
