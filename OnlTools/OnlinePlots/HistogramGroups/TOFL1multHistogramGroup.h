#ifndef TOFL1multHistogramGroup_h
#define TOFL1multHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
class TOFL1multHistogramGroup : public HistogramGroup {

public:
  TOFL1multHistogramGroup();
  TOFL1multHistogramGroup(const char* group, const char* subGroup="TOF L1 multiplicity", const char* trigger="any", const char* detector="TOF");

  ~TOFL1multHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH2* TOF_L1mult_vs_ZDCadcsum;

  //
  ClassDef(TOFL1multHistogramGroup,1) ;


};


#endif
