#ifndef L2UpsilonMassHistogramGroup_h
#define L2UpsilonMassHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"

class L2UpsilonMassHistogramGroup : public HistogramGroup {
 public:
  L2UpsilonMassHistogramGroup(const char* group="L2 upsilon", const char* subGroup="mass", const char* trigger="ups", const char* detector="any");
  ~L2UpsilonMassHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
 TH1* hEnergyL0;
 TH1* hEnergyL2;
 TH1* hMass;
 TH1* hCosTheta;
 ClassDef(L2UpsilonMassHistogramGroup,1) ;
};


#endif
