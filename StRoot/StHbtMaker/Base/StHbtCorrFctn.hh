/***************************************************************************
 *
 * $Id: StHbtCorrFctn.hh,v 1.3 1999/12/03 22:24:33 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    base class for a STAR correlation function.  Users should inherit 
 *    from this and must implement constructor, destructor, Report(),
 *    AddMixedPair(), AddRealPair(), Finish()
 *
 ***************************************************************************
 *
 * $Log: StHbtCorrFctn.hh,v $
 * Revision 1.3  1999/12/03 22:24:33  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.2  1999/07/06 22:33:18  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtCorrFctn_hh
#define StHbtCorrFctn_hh

//#include<string>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

#include "StHbtMaker/Infrastructure/StHbtPair.hh"

class StHbtCorrFctn{

public:
  StHbtCorrFctn(){/* no-op */};
  virtual ~StHbtCorrFctn(){/* no-op */};

  //  virtual string Report() = 0;
  virtual StHbtString Report() = 0;

  virtual void AddRealPair(const StHbtPair*) = 0;
  virtual void AddMixedPair(const StHbtPair*) = 0;

  virtual void Finish() = 0;

  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtAnalysis;
  StHbtAnalysis* HbtAnalysis(){return myAnalysis;};

protected:
  StHbtAnalysis* myAnalysis;

private:

};

#endif
