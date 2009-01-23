#ifndef UPCHistogramZdcGroup_h
#define UPCHistogramZdcGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


class UPCHistogramZdcGroup : public HistogramGroup {

public:

  UPCHistogramZdcGroup(const char* group="upc", const char* subGroup="zdc", const char* trigger="upc", const char* detector="zdc");
  ~UPCHistogramZdcGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* h_zdce_sum;             // spectrum of ZDCE unattanuated sum 
  TH1* h_zdcw_sum;             // spectrum of ZDCW unattanuated sum 
  TH2* h_zdce_sum_vs_ctb_sum;  // ZDCE unattanuated sum vs. CTB ADC sum
  TH2* h_zdcw_sum_vs_ctb_sum;  // ZDCW unattanuated sum vs. CTB ADC sum

  ClassDef(UPCHistogramZdcGroup,1) ;

};


#endif
