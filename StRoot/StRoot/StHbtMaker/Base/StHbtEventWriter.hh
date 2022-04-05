/***************************************************************************
 *
 * $Id: StHbtEventWriter.hh,v 1.1 2000/02/18 21:25:00 laue Exp $
 *
 * Author: Frank, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for a StHbtEventWriter             
 **************************************************************************/

#ifndef StHbtEventWriter_hh
#define StHbtEventWriter_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"

typedef StHbtEventReader StHbtEventWriter;//!  // yes, because our writer are reader-writers

#endif

