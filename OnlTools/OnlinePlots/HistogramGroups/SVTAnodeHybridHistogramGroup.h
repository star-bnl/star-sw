#ifndef SVTAnodeHybridHistogramGroup_h
#define SVTAnodeHybridHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


#define __MAXBOARDS__ 14


class SVTAnodeHybridHistogramGroup : public HistogramGroup {
public:
  SVTAnodeHybridHistogramGroup();
  SVTAnodeHybridHistogramGroup(unsigned int board, const char* group="SVT", const char* subGroup="Anode vs Hybrid", const char* trigger="any", const char* detector="svt");
  ~SVTAnodeHybridHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
  static void blwh2rma(int b, int l,int w, int h, int& recBoard, int& mezz, int& mz_hyb);

public:
  TH2D* hSVT[4];
  unsigned int mEvents;
  unsigned int mBoard;
  static unsigned int mFirstHybrid[__MAXBOARDS__+1];
  static const char* mNames[4];

  ClassDef(SVTAnodeHybridHistogramGroup,1) ;
};


#endif
