/***************************************************************************
 *
 * $Id: StSvtDaqData.hh,v 1.1 2000/06/13 20:42:05 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqData.hh,v $
 * Revision 1.1  2000/06/13 20:42:05  caines
 * StRoot/StSvtDaqMaker
 *
 **************************************************************************/

#ifndef STSVTDAQDATA_HH
#define STSVTDAQDATA_HH

#include "StSvtClassLibrary/StSvtData.hh"

class StSvtHybridDaqData;
class StSVTReader;

class StSvtDaqData: public StSvtData
{
public:
  StSvtDaqData(char* config=0, StSVTReader* reader=0, char* option = "ZS", int run=0, int event=0, int trigger=0);

  int setData(StSVTReader* reader, char* option = "ZS");

protected:

  ClassDef(StSvtDaqData,1)
};

#endif
