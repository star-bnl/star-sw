// $Id: StSvtSeqAdjMaker.h,v 1.7 2001/05/02 02:07:55 caines Exp $
// $Log: StSvtSeqAdjMaker.h,v $
// Revision 1.7  2001/05/02 02:07:55  caines
// Fix function declaration for Solaris
//
// Revision 1.6  2001/05/01 00:23:58  caines
//  Update h files for use with zsp data
//
// Revision 1.5  2000/11/30 20:45:56  caines
// Dynamically calc prob values, use database
//
// Revision 1.4  2000/08/24 04:23:50  caines
// Improved histograms
//
// Revision 1.3  2000/08/21 12:57:31  caines
// Now opens and reads in ped using CalibMaker
//
// Revision 1.2  2000/07/16 22:32:23  caines
// Now also saves RAW data
//
// Revision 1.1  2000/06/15 20:04:54  caines
// Initial versions of sequence adjusting codes
//
//
#ifndef STAR_StSvtSeqAdj
#define STAR_StSvtSeqAdj
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtSeqAdj base class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <iostream.h>
#include <fstream.h>

class TH1D;
 
class StSequence;
class StSvtData;
class StSvtHybridData;
class StSvtInverseProducts;
class StSvtPedSub;
class TObjectSet;
class StSvtHybridCollection;
class StSvtBadAnode;
class StSvtProbValues;

class StSvtSeqAdjMaker : public StMaker
{
 public: 
  StSvtSeqAdjMaker(const char *name = "SvtSeqAdj");
  StSvtSeqAdjMaker(StSvtSeqAdjMaker& analmaker);
  ~StSvtSeqAdjMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  Int_t SetSvtData();
  Int_t GetSvtRawData();
  Int_t GetSvtPedestals();
  Int_t GetBadAnodes();
  Int_t GetPedOffset(){return mPedOffSet;};
  Int_t AdjustSequences1( int iAnode, int Anode); // Find sequences  based on ASICS
  Int_t AdjustSequences2(int iAnode, int Anode); //adjust sequences base on LowInvProd

  Int_t CreateHist(Int_t tNuOfHyb);
  void  MakeHistogramsProb(int index,int Anode);
  void  MakeHistogramsAdc(StSvtHybridData* hybridData, int index,int Anode, int Count);

  Int_t SetMinAdcLevels( int MinAdc1,  int MinAbove1, int MinAdc2, int MinAbove2, int PedOffset); // Set the 2 thresholds for a sequence
  Int_t SetPedestalFile(const char* PedFile);
  Int_t SetLowInvProd(int LowInvProd);// Set the low threshold based on the frequency distribution
    
 protected:
  St_ObjectSet* mSvtDataSet;             //!
  St_ObjectSet* mSvtBadAnodeSet;          //!
  StSvtData* mSvtRawData;            //!
  StSvtData* mSvtAdjData;    //!
  StSvtHybridData* mHybridRawData ;       //!
  StSvtHybridData* mHybridAdjData ;   //!

  StSvtHybridCollection* mSvtBadAnodes;   //!
  StSvtHybridCollection* mSvtPedColl;     //!

  StSvtInverseProducts* mInvProd;         //!
  StSvtPedSub* mSvtPedSub;                //!
  StSvtProbValues* mProbValue;            //!
  StSequence* tempSeq1;                   //!

  const char* mPedFile;   //!

  TH1D** mInvProdSeqAdj;  //!
  TH1F** mRawAdc;  //!
  TH1F** mAdcAfter;  //!

  int mNumOfSeq;
  int m_n_seq_lo;
  int m_n_seq_hi;
  int m_thresh_lo;
  int m_inv_prod_lo;
  int m_thresh_hi;
  
  int mTotalNumberOfHybrids;
  int mPedOffSet;

 private:

  ClassDef(StSvtSeqAdjMaker,1)   //virtual base class for Makers

};


#endif
