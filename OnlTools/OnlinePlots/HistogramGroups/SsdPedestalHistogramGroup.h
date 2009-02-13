#ifndef SsdPedestalHistogramGroup_h
#define SsdPedestalHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


#define __MAXBOARDS__ 14


class SsdPedestalHistogramGroup : public HistogramGroup {
protected:
   SsdPedestalHistogramGroup():HistogramGroup(),h1SsdMeanPedestal(0),h1SsdMeanNoise(0)
                              ,good_ssd(19562009){}
public:
  SsdPedestalHistogramGroup(const char* group, const char* subGroup="Pedestal", const char* trigger="pedestal", const char* detector="ssd");
  ~SsdPedestalHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
  virtual void beginRun(evpReader* evp, char* datap);
  virtual void endRun();
public:
  TH1* h1SsdMeanPedestal;
  TH1* h1SsdMeanNoise;
  int good_ssd;
  void SsdTH1Setup(TH1 *rHisto,const char *rTitleX, const char *rTitleY,int rMax,int rFillColor, int rLineColor=0);
  //  unsigned int mEvents;
  //  unsigned int mRunNumber;
  ClassDef(SsdPedestalHistogramGroup,1) ;
};


#endif
