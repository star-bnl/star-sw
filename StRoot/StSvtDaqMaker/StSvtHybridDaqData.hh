/***************************************************************************
 *
 * $Id: StSvtHybridDaqData.hh,v 1.1 2000/06/13 20:42:06 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDaqData.hh,v $
 * Revision 1.1  2000/06/13 20:42:06  caines
 * StRoot/StSvtDaqMaker
 *
 **************************************************************************/

#ifndef STSVTHYBRIDDAQDATA_HH
#define STSVTHYBRIDDAQDATA_HH

#include "StSvtClassLibrary/StSvtHybridData.hh"

class StSVTReader;

class StSvtHybridDaqData: public StSvtHybridData
{
public:
  StSvtHybridDaqData(int barrel, int ladder, int wafer, int hybrid, StSVTReader* reader=0, char* option="ZS");

  int setHybridData(StSVTReader* reader, char* option = "ZS");

protected:

  ClassDef(StSvtHybridDaqData,1)
};

#endif
