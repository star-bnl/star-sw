/***************************************************************************
 *
 * $Id: StHbtEventReader.hh,v 1.2 1999/06/29 17:50:26 fisyak Exp $
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
 ***************************************************************************
 *
 * $Log: StHbtEventReader.hh,v $
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

class StHbtEventReader {

public:
  // even tho it's only a base class and never constructed, if you don't have an implementation,
  // you get "StHbtEventReader type_info node" upon dynamical loading
  StHbtEventReader(){/* no-op*/};
  virtual ~StHbtEventReader(){/* no-op */};

  virtual StHbtEvent* ReturnHbtEvent() =0;
};

#endif

