#ifndef MTDtriggerinfoHistogramGroup_h
#define MTDtriggerinfoHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
#define ntray 3

class MTDtriggerinfoHistogramGroup : public HistogramGroup {

public:
  MTDtriggerinfoHistogramGroup();
  MTDtriggerinfoHistogramGroup(const char* group, const char* subGroup="MTDtrigger", const char* trigger="any", const char* detector="MTD");

  ~MTDtriggerinfoHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:
  TH1* MTD_adc[ntray][2];// 3 trays; 0 east, 1 east
  TH1* MTD_tac[ntray][2];

  ClassDef(MTDtriggerinfoHistogramGroup,1) ;


};


#endif
