/***************************************************************************
 *
 * $Id: StHbtCorrFctn.hh,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtCorrFctn_hh
#define StHbtCorrFctn_hh

#include<string>

#include "StHbtMaker/Infrastructure/StHbtPair.hh"

class StHbtCorrFctn{

public:
  StHbtCorrFctn(){/* no-op */};
  virtual ~StHbtCorrFctn(){/* no-op */};

  virtual string Report() = 0;

  virtual void AddRealPair(const StHbtPair*) = 0;
  virtual void AddMixedPair(const StHbtPair*) = 0;

  virtual void Finish() = 0;


private:

};

#endif
