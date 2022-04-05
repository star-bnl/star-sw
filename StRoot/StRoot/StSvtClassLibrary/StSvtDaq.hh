/***************************************************************************
 *
 * $Id: StSvtDaq.hh,v 1.2 2004/03/30 20:20:00 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Daq parameters object. 
 *
 ***************************************************************************
 *
 * $Log: StSvtDaq.hh,v $
 * Revision 1.2  2004/03/30 20:20:00  caines
 * Add functions to Daq code from Petr
 *
 * Revision 1.1  2004/01/30 00:13:03  munhoz
 * daq parameters object
 *
 *
 **************************************************************************/

#ifndef STSVTDAQ_HH
#define STSVTDAQ_HH

#define MAX_NUMBER_OF_BARRELS 3

#include "StObject.h"
#include "TString.h"

class StSvtDaq: public StObject
{
public:
  StSvtDaq();
  virtual ~StSvtDaq();

  StSvtDaq(const StSvtDaq&);
  StSvtDaq& operator = (const StSvtDaq&);

  void setClearedTimeBins(long value){clearedTimeBins=value;}   // 
  void setSavedBlackAnodes(long value, int i){savedBlackAnodes[i]=value;}   // 
  void setPixelsBefore(long value){pixelsBefore=value;}
  void setPixelsAfter(long value){pixelsAfter=value;}
  void setPedOffset(long value) {pedOffset=value;}
  void setSeqLo(long value){seqLo=value;}
  void setSeqHi(long value){seqHi=value;}
  void setThreshLo(long value){threshLo=value;}
  void setThreshHi(long value){threshHi=value;}

  long getClearedTimeBins(){return clearedTimeBins;}   // 
  long getSavedBlackAnodes(int i){return savedBlackAnodes[i];}   // 
  long getPixelsBefore(){return pixelsBefore;}
  long getPixelsAfter(){return pixelsAfter;}
  long getPedOffset(){return pedOffset;}
  long getSeqLo(){return seqLo;}
  long getSeqHi(){return seqHi;}
  long getThreshLo(){return threshLo;}
  long getThreshHi(){return threshHi;}

protected:
  long clearedTimeBins;   // 
  long savedBlackAnodes[4];   // 
  long pixelsBefore;
  long pixelsAfter;
  long pedOffset;
  long seqLo;
  long seqHi;
  long threshLo;
  long threshHi;

  ClassDef(StSvtDaq,1)
};

#endif
