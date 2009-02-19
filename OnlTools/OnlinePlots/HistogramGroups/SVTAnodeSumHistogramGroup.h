#ifndef SVTAnodeSumHistogramGroup_h
#define SVTAnodeSumHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


#define __MAXBOARDS__ 14


class SVTAnodeSumHistogramGroup : public HistogramGroup {
public:
  SVTAnodeSumHistogramGroup();
  SVTAnodeSumHistogramGroup(const char* group, const char* subGroup="Anode Sum", const char* trigger="any", const char* detector="svt");
  ~SVTAnodeSumHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
  virtual void beginRun(evpReader* evp, char* datap);
  virtual void endRun();
public:
  TH1* hSVT[2];
  unsigned int mEvents;
  unsigned int mRunNumber;
  ClassDef(SVTAnodeSumHistogramGroup,1) ;
};


#endif
