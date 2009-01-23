#ifndef UPCHistogramGroup_h
#define UPCHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


class UPCHistogramGroup : public HistogramGroup {
 public:
  UPCHistogramGroup(const char* group="upc", const char* subGroup="first", const char* trigger="upc", const char* detector="any");
  ~UPCHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
 TH1* h_zdc_time_east;
 TH1* h_zdc_time_west;
 TH1* h_zdc_timediff_east_west;
 TH1* h_zdc_time_east_vs_west;
 TH1* h_zdc_unatt_east;
 TH1* h_zdc_unatt_west;
  ClassDef(UPCHistogramGroup,1) ;
};


#endif
