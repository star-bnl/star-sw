/***************************************************************************
 *
 * $Id: StSvtHybridDaqPed.hh,v 1.1 2001/07/11 23:29:48 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDaqPed.hh,v $
 * Revision 1.1  2001/07/11 23:29:48  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 *
 **************************************************************************/

#ifndef STSVTHYBRIDDAQPED_HH
#define STSVTHYBRIDDAQPED_HH

#include "StSvtClassLibrary/StSvtHybridPed.hh"

class StSVTReader;

class StSvtHybridDaqPed: public StSvtHybridPed
{
public:
  StSvtHybridDaqPed(int barrel, int ladder, int wafer, int hybrid, StSVTReader* reader=0);

  int setHybridPed(StSVTReader* reader);
  int setHybridRMSPed(StSVTReader* reader);

protected:

  ClassDef(StSvtHybridDaqPed,1)
};

#endif
