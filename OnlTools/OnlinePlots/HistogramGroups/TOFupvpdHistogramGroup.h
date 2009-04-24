#ifndef TOFupvpdHistogramGroup_h
#define TOFupvpdHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
class TOFupvpdHistogramGroup : public HistogramGroup {

public:
  TOFupvpdHistogramGroup();
  TOFupvpdHistogramGroup(const char* group, const char* subGroup="upvpd", const char* trigger="any", const char* detector="TOF");

  ~TOFupvpdHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* upvpd_hitmap[2];
  TH2* upvpd_ToT;
  TH2* upvpd_eastT_vs_westT;
  int tdcchan2upvpdPMTchan(int,int,int);
  //
  double numberforsort;
  vector<double> leadinghits;
  vector<double> trailinghits;
  

  ClassDef(TOFupvpdHistogramGroup,1) ;


};


#endif
