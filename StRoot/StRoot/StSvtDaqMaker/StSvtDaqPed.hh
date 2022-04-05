/***************************************************************************
 *
 * $Id: StSvtDaqPed.hh,v 1.1 2001/07/11 23:29:48 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DAQ Ped class
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqPed.hh,v $
 * Revision 1.1  2001/07/11 23:29:48  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 *
 **************************************************************************/

#ifndef STSVTDAQPED_HH
#define STSVTDAQPED_HH

#include "StSvtClassLibrary/StSvtHybridCollection.hh"

class StSvtHybridDaqPed;
class StSVTReader;

class StSvtDaqPed: public StSvtHybridCollection
{
public:
  StSvtDaqPed();
  StSvtDaqPed(const char* config, StSVTReader* reader=0, int run=0);
  StSvtDaqPed(StSvtConfig* config, StSVTReader* reader=0, int run=0);

  void setRunNumber(int run)   { mRunNumber = run;}
  int setPed(StSVTReader* reader, const char* type="PED");

protected:
  StSvtHybridDaqPed* mPed; // Hybrid Daq Ped Object
  int mRunNumber;   // Run Number
  
  ClassDef(StSvtDaqPed,1)
};

#endif
