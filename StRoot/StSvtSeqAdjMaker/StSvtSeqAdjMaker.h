// $Id: StSvtSeqAdjMaker.h,v 1.1 2000/06/15 20:04:54 caines Exp $
// $Log: StSvtSeqAdjMaker.h,v $
// Revision 1.1  2000/06/15 20:04:54  caines
// Initial versions of sequence adjusting codes
//
//
#ifndef STAR_StSvtSeqAdj
#define STAR_StSvtSeqAdj
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtSeqAdj base class                                     //
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


class StSvtSeqAdjMaker : public StMaker
{
 public: 
  StSvtSeqAdjMaker(const char *name = "SvtSeqAdj");
  StSvtSeqAdjMaker(StSvtSeqAdjMaker& analmaker);
  ~StSvtSeqAdjMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();


  Int_t GetSvtEvent();
  Int_t SetInputFiles(char* table, char* PedFile,int PedOffset);
  Int_t CreateHist(Int_t tNuOfHyb);
  void  MakeHistograms(int index,int Anode);
  Int_t SetMinAdcLevels( int MinAdc1,  int MinAbove1, int MinAdc2, int MinAbove2 ); // Set the 2 thresholds for a sequence
  Int_t SetLowInvProd(int LowInvProd);// Set the low threshold based on the frequency distribution
  Int_t AdjustSequences1( int Anode); // Find sequences  based on ASICS
  Int_t AdjustSequences2(int Anode); //adjust sequences base on LowInvProd
    
 protected:
  StSvtData* mSvtEvent;      //!
  StSvtHybridData* mHybridData ;           //!
  StSvtInverseProducts* mInvProd;  //!
  StSvtPedSub* mSvtPedSub;   //!
  StSequence* tempSeq1; //!
  char* mPedFile;

  TH1D** mInvProdSeqAdj;  //!

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
