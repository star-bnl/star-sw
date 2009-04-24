#ifndef TOFtrayHistogramGroup_h
#define TOFtrayHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"

#define MAXTRAYS 30

class TOFtrayHistogramGroup : public HistogramGroup {

public:
  TOFtrayHistogramGroup();
  TOFtrayHistogramGroup(unsigned int ipart,const char* group, const char* subGroup="Tray", const char* trigger="any", const char* detector="TOF");

  ~TOFtrayHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* TOF_Tray_LEhitmap[MAXTRAYS];
  TH1* TOF_Tray_TEhitmap[MAXTRAYS];

  //int tdcchan2mrpcchan(int);
  //
  int mPart;
  int mNtray;
  int actualTrayNum[MAXTRAYS];

  bool Tray_NotInRun(int);

  ClassDef(TOFtrayHistogramGroup,1) ;


};


#endif
