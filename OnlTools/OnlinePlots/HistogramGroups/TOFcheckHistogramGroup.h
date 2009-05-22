#ifndef TOFcheckHistogramGroup_h
#define TOFcheckHistogramGroup_h

#include "Rtypes.h"
#include "HistogramGroup.h"
class TOFcheckHistogramGroup : public HistogramGroup {

public:
  TOFcheckHistogramGroup();
  TOFcheckHistogramGroup(const char* group, const char* subGroup="Error check", const char* trigger="any", const char* detector="TOF");

  ~TOFcheckHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  TH1* TOF_Error1;
  TH1* TOF_Error2;
  TH1* TOF_Error3;
  TH1* TOF_EventCount;
  //TH1F* TOF_Tray_hits1;
  //TH1F* TOF_Tray_hits2;

  bool ValidDataword(int);
  int ValidBunchid(int,int,int,int);
  //
  bool NotActiveTray[128];  // Highest TOF tray  number is MTD: 124, leave some rooms here. 
  static char* mTrayList;
  void ReadTrayList();
  // 
  static char* mBunchShiftList;
  void ReadValidBunchidPhase();
  int mReferenceTray;
  int mValidShiftTray[2][4];
  int mValidShift121[2][2];
  int mValidShift122[2][2];
  int mValidShift124[2];

  //bool Tray_NotInRun(int);

  ClassDef(TOFcheckHistogramGroup,1) ;


};


#endif
