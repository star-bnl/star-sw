#ifndef TOFL0HistogramGroup_h
#define TOFL0HistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"

#define MAXTRAYS 30

class TOFL0HistogramGroup : public HistogramGroup {

public:
  TOFL0HistogramGroup();
  TOFL0HistogramGroup(unsigned int ipart,const char* group, const char* subGroup="TOF L0", const char* trigger="any", const char* detector="trg");

  ~TOFL0HistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* TOF_L0_hit[MAXTRAYS];
  TH1* TOF_L0_trg[MAXTRAYS];

  //int tdcchan2mrpcchan(int);
  //
  int mPart;
  int mNtray;
  int actualTrayNum[MAXTRAYS];

  bool NotActiveTray[128];  // Highest TOF tray  number is MTD: 124, leave some rooms here. 
  static char* mTrayList;
  void ReadTrayList();

  bool MaskoutTray[128];  
  static char* mTraymaskoutList;
  void ReadTraymaskoutList();

  ClassDef(TOFL0HistogramGroup,1) ;


};


#endif
