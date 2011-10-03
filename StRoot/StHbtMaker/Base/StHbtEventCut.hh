
/***************************************************************************
 *
 * $Id: StHbtEventCut.hh,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       base class for Eventwise cuts
 *      Users inherit from this class and must add their own quantities        
 *
 ***************************************************************************
 *
 * $Log: StHbtEventCut.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtEventCut_hh
#define StHbtEventCut_hh

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include <string>

class StHbtEventCut{

public:

  StHbtEventCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtEventCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtEvent* event) =0;  // true if passes, false if not

  virtual string Report() =0;    // user-written method to return string describing cuts

};

#endif
