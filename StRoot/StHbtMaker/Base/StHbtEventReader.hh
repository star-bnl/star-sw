/***************************************************************************
 *
 * $Id: StHbtEventReader.hh,v 1.3 1999/09/03 22:39:14 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for a StHbtEventReader             
 *   All HbtEventReaders should inherit from this.
 *   Objects of these classes are required
 *   to obtain the data and convert it somehow to the STAR StEvent object
 * 
 * A major change is that on 3sep99, the StHbtReader classes _MUST_ implement
 *   a Report() method, like the Cuts and CorrFctns.
 * Also, a StHbtReader MAY implement a WriteHbtEvent(StHbtEvent*) method.
 *
 ***************************************************************************
 *
 * $Log: StHbtEventReader.hh,v $
 * Revision 1.3  1999/09/03 22:39:14  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.2  1999/06/29 17:50:26  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtEventReader_hh
#define StHbtEventReader_hh
#include "StMaker.h"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtEventReader {

public:
  // even tho it's only a base class and never constructed, if you don't have an implementation,
  // you get "StHbtEventReader type_info node" upon dynamical loading
  StHbtEventReader(){/* no-op*/};
  virtual ~StHbtEventReader(){/* no-op */};

  virtual StHbtEvent* ReturnHbtEvent() =0;

  virtual StHbtString Report() =0;    // user-written method to return string describing reader
                                      // Including whatever "early" cuts are being done

  // this next method does NOT need to be implemented, in which case the 
  // "default" method below is executed
  virtual void WriteHbtEvent(StHbtEvent*){cout << "No WriteHbtEvent implemented";}

};

#endif

