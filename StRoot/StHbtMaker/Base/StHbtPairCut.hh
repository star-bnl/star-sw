/***************************************************************************
 *
 * $Id: StHbtPairCut.hh,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *         base class for pair-wise cuts
 *         Users inherit from this class and must add their own quantities
 *
 ***************************************************************************
 *
 * $Log: StHbtPairCut.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtPairCut_hh
#define StHbtPairCut_hh

#include <string>

#include "StHbtMaker/Infrastructure/StHbtPair.hh"

class StHbtPairCut{

public:

  StHbtPairCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtPairCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtPair* pair) =0;  // true if passes, false if not

  virtual string Report() =0;    // user-written method to return string describing cuts

};

#endif
