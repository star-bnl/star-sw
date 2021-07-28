/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * special CorrFctn used for Parity Violation studies
 *
 ***************************************************************************/

#ifndef ParityCorrFctn_hh
#define ParityCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include "StHbtMaker/Cut/ParityEventCut.h"


class ParityCorrFctn : public StHbtCorrFctn {
public:
  ParityCorrFctn(ParityEventCut* PEC);
  virtual ~ParityCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*); 
  virtual void AddMixedPair(const StHbtPair*); 

  virtual void Finish(); 


private:
  ParityEventCut* mParityEventCut;  // this is used to access the Event-wise quantity and increment it
#ifdef __ROOT__
  ClassDef(ParityCorrFctn, 0)
#endif
};


#endif

