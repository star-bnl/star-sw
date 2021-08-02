#ifndef MTDtriggerinfoHistogramGroup_h
#define MTDtriggerinfoHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
#define nMTDtrays 3

class MTDtriggerinfoHistogramGroup : public HistogramGroup {

public:
  MTDtriggerinfoHistogramGroup();
  MTDtriggerinfoHistogramGroup(const char* group, const char* subGroup="MTDtrigger", const char* trigger="any", const char* detector="MTD");

  ~MTDtriggerinfoHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:
  TH1* MTD_adc[nMTDtrays][2];// 3 trays; 0 east, 1 west
  TH1* MTD_tac[nMTDtrays][2];

  ClassDef(MTDtriggerinfoHistogramGroup,1) ;


};


#endif
