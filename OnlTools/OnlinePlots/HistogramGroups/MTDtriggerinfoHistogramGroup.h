#ifndef MTDtriggerinfoHistogramGroup_h
#define MTDtriggerinfoHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
class MTDtriggerinfoHistogramGroup : public HistogramGroup {

public:
  MTDtriggerinfoHistogramGroup();
  MTDtriggerinfoHistogramGroup(const char* group, const char* subGroup="MTDtrigger", const char* trigger="any", const char* detector="MTD");

  ~MTDtriggerinfoHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* MTD_adc[2];
  TH1* MTD_tac[2];
  TH2* MTD_eastTac_vs_westTac;
  TH2* MTD_aveTac_vs_vpd_aveTac;

  ClassDef(MTDtriggerinfoHistogramGroup,1) ;


};


#endif
