#ifndef STSVTONLINESEQADJSIM_HH
#define STSVTONLINESEQADJSIM_HH

#include "Stiostream.h"
#include <stdlib.h>
#include "TObject.h"

class StSvtHybridPixelsC;
class StSvtHybridCollection;
class StSvtHybridSimData;

//there is no bad anode list in this adjuster compared to reality
//this doesn't mater as long as the offline anode list constains the whole online anode list

//also there is no inverse product criterium implememented
class StSvtOnlineSeqAdjSim
{
public:
  StSvtOnlineSeqAdjSim();
  ~StSvtOnlineSeqAdjSim();

  void  Make();

  void SetPixelData(StSvtHybridPixelsC* data);
  StSvtHybridPixelsC* GetPixelData(){return mData;} 

  void SetRawData(StSvtHybridSimData *data);
  StSvtHybridSimData *GetRawData(){return mRawData;}


  void SetKillBadAnodes(bool doit){mKillBadAnodes=doit;}
  bool GetKillBadAnodes(){return mKillBadAnodes;}

  void SetSaveAnode2Raw(bool doit){mSaveAnode2Raw=doit;}
  bool GetSaveAnode2Raw(){return mSaveAnode2Raw;}

  void SetSaveAnode239Raw(bool doit){mSaveAnode239Raw=doit;}
  bool GetSaveAnode239Raw(){return mSaveAnode239Raw;}
  
  void SetNumberTBinsToClear(int n){mNumberTBinsToClear=n;}
  int GetNumberTBinsToClear(){return mNumberTBinsToClear;}
  
  void SetExtraPixelsBefore(int num){mExtraBefore=num;}
  int GetExtraPixelsBefore(){return mExtraBefore;}

  void SetExtraPixelsAfter(int num){mExtraAfter=num;}
  int GetExtraPixelsAfter(){return mExtraAfter;}

  void Set_n_seq_lo(int num){m_n_seq_lo=num;}
  int Get_n_seq_lo(){return m_n_seq_lo;}

  void Set_n_seq_hi(int num){m_n_seq_hi=num;}
  int Get_n_seq_hi(){return m_n_seq_hi;}
  
  void Set_thresh_lo(int num){m_thresh_lo=num;}
  int Get_thresh_lo(){return m_thresh_lo;}

  void Set_thresh_hi(int num){m_thresh_hi=num;}
  int Get_thresh_hi(){return m_thresh_hi;}

  void SetPedOffset(int num){mPedOffset=num;}
  int GetPedOffset(){return mPedOffset;}
  
  
  void SetBadAnodes(StSvtHybridCollection* anodes){mSvtBadAnodes=anodes;}
private:
  StSvtHybridPixelsC* mData;
  StSvtHybridSimData *mRawData;
  StSvtHybridCollection* mSvtBadAnodes;
  bool  mKillBadAnodes;
  int   mNumberTBinsToClear;   //number of first time bins which are set to 0 by DAQ - default 2
  bool  mSaveAnode2Raw;
  bool  mSaveAnode239Raw;

  //number of extra pixels around the sequence to save
  int   mExtraBefore;
  int   mExtraAfter;

  //parameters for seq adjusting
  int m_n_seq_lo;
  int m_n_seq_hi;
  int m_thresh_lo;
  int m_thresh_hi;
  int mPedOffset;
  
  Bool_t mMask[128*240]; 
  void  KillBadAnodes();
  void  ClearMask();
  void  RawAnodes();
  void  ClearFirstTbins();
  void  SequenceSearch();
  void  WriteMask();
  void  WriteSequence(int anode,int begins, int ends, int NumOfHigh);
  void  FillRawData();
};

#endif
