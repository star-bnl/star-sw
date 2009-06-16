#ifndef FMSHistogramGroup_h
#define FMSHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"

class FMSHistogramGroup : public HistogramGroup {

public:

  FMSHistogramGroup();
  FMSHistogramGroup(const char* group, const char* subGroup="fms", const char* trigger="any", const char* detector="fms");
  ~FMSHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:
  void createHistos();
  void deleteHistos();

  TH1* h_fms_quad_sum[4];           // # of QT quadrant sum  

  enum {
    mMaxCrate    = 4,
    mMaxAddr     = 11,
    mMaxCh       = 32,
  };

  ClassDef(FMSHistogramGroup,1) ;
};

#endif
