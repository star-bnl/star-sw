#ifndef L2UpsilonTowersHistogramGroup_h
#define L2UpsilonTowersHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"



class L2UpsilonTowersHistogramGroup : public HistogramGroup {
 public:
  L2UpsilonTowersHistogramGroup();
  L2UpsilonTowersHistogramGroup(const char* group, const char* subGroup="mass", const char* trigger="ups", const char* detector="any");
  ~L2UpsilonTowersHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
 TH1* hTriggerTowerIdL0;
 TH1* hTriggerTowerIdL2;
 TH1* hNumberOfTowersL0;
 TH1* hNumberOfTowersL2;
 TH2* hEtaPhiL0;
 TH2* hEtaPhiL2;
 ClassDef(L2UpsilonTowersHistogramGroup,1) ;
};


#endif
