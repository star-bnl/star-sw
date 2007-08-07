//
//  StBemcTriggerSimu.h,v 0.01
//
#ifndef STAR_StBemcTriggerSimu
#define STAR_StBemcTriggerSimu

#include <TObject.h> 
#include <vector>


class StBemcTriggerSimu {
 private:
  int mMCflag; // 0 for real data
  TObjArray *mHList;

 public:
  StBemcTriggerSimu();
  virtual     ~StBemcTriggerSimu();
  void Init();
  void InitRun();
  void Clear();
  void Make();
  void setMC(int x) {mMCflag=x;}
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList( void * );
  ClassDef(StBemcTriggerSimu, 1)
 };


#endif
