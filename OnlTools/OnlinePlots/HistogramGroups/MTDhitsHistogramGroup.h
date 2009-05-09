#ifndef MTDhitsHistogramGroup_h
#define MTDhitsHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
class MTDhitsHistogramGroup : public HistogramGroup {

public:
  MTDhitsHistogramGroup();
  MTDhitsHistogramGroup(const char* group, const char* subGroup="hits", const char* trigger="any", const char* detector="MTD");

  ~MTDhitsHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* MTD_hitmap[2];
  TH2* MTD_ToT;
  TH2* MTD_eastT_vs_westT;
  TH1* MTD_eastT_westT;
  TH2* MTD_hits_vs_TOF_hits;
  int tdcchan2MTDchan(int,int);
  int tdcchan2mrpcchan(int);
  //
  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;
  

  ClassDef(MTDhitsHistogramGroup,1) ;


};


#endif
