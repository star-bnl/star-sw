/***************************************************************************
 *
 * $Id: StHbtLikeSignCorrFctn.hh,v 1.1 2000/09/01 18:40:53 laue Exp $
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

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
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
