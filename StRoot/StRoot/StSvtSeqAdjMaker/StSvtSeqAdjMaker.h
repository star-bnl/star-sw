// $Id: StSvtSeqAdjMaker.h,v 1.27 2014/08/06 11:43:46 jeromel Exp $
// $Log: StSvtSeqAdjMaker.h,v $
// Revision 1.27  2014/08/06 11:43:46  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.26  2007/07/12 20:10:35  fisyak
// Add forward declaration for ROOT 5.16
//
// Revision 1.25  2005/07/23 03:37:34  perev
// IdTruth + Cleanup
//
// Revision 1.24  2004/03/18 04:05:02  caines
// Remove from global scope variables used in debug mode as they shouldnt be there and caused erratic behaviour, also initialise some variables that valgrind was complaining about - didnt really need to as they are sent back from function which initialises them properly always but doesnt hurt
//
// Revision 1.23  2003/09/10 19:47:36  perev
// ansi corrs
//
// Revision 1.22  2003/09/02 17:59:08  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.21  2003/07/18 17:15:40  caines
// Fix Pedoffset to be 20 not 10 change variables to int from floats to avoid casting problems, fix that when pedestal goes negative we dont
//
// Revision 1.20  2003/01/21 01:27:44  jeromel
// hfile->write(0 while NULL caused spurious crash.
// Took the oportunity to add GetCVS()
// Some maniaco-compulsive //! alignement fixes ...
//
// Revision 1.19  2002/09/20 19:35:25  caines
// Change building of file name
//
// Revision 1.18  2002/09/19 16:17:49  caines
// Add code to do Juns gain calibration
//
// Revision 1.17  2002/05/09 16:55:08  munhoz
// add reading bad anodes from DB
//
// Revision 1.16  2002/04/22 14:52:54  caines
// Add the .h too
//
// Revision 1.15  2002/01/11 22:49:22  caines
// Fix sequence merging bugs-hopefully
//
// Revision 1.14  2001/12/13 03:08:17  caines
// Can now subtract common mode noise via black anodes 239 and 2
//
// Revision 1.13  2001/10/19 23:31:34  caines
// Correct problem that if anodes were missing didnt do average common mode noise calc
//
// Revision 1.12  2001/09/26 18:42:48  caines
// Fix 2 anode subtraction routines
//
// Revision 1.11  2001/09/16 22:24:26  caines
// Fix for when SVT isnt in every event
//
// Revision 1.10  2001/08/24 20:57:46  caines
// Do common mode noise suppression from first two anodes
//
// Revision 1.9  2001/07/25 14:47:47  caines
// Fix filling histogram only when debug is on
//
// Revision 1.8  2001/07/22 20:31:29  caines
// Better tuning for real data. Common mode noise calc and sub. Avoid overlapping seq. Fill histograms only in debug
//
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

#include <Stiostream.h>
#include "Stiostream.h"
#include <string>

class TH1D;
class TH2F;
class TFile; 
class StSequence;
class StSvtData;
class StSvtHybridData;
class StSvtInverseProducts;
class StSvtPedSub;
class TObjectSet;
class StSvtHybridCollection;
class StSvtBadAnode;
class StSvtProbValues;
class StMCTruth;

class StSvtSeqAdjMaker : public StMaker
{
 public: 
  StSvtSeqAdjMaker(const char *name = "SvtSeqAdj");
  StSvtSeqAdjMaker(StSvtSeqAdjMaker& analmaker);
  ~StSvtSeqAdjMaker();

  virtual Int_t Init();
  virtual Int_t InitRun( int runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();

  Int_t SetSvtData();
  Int_t GetSvtRawData();
  Int_t GetSvtPedestals();
  Int_t GetBadAnodes();
  Int_t GetPedOffset(){return mPedOffSet;};
  void CommonModeNoiseCalc(int iAnode);
  void CommonModeNoiseSub(int iAnode);
  void SubtractFirstAnode(int iAnode);
  Int_t FindBlackAnodes(); // Find Black anodes on each hybrid
  Int_t AdjustSequences1( int iAnode, int Anode); // Find sequences  based on ASICS
  Int_t AdjustSequences2(int iAnode, int Anode); //adjust sequences base on LowInvProd
  Int_t MergeSequences(StSequence* Seq, int nSeq,StMCTruth *Tru=0); // Merge overlapping sequences
  Int_t CreateHist(Int_t tNuOfHyb);
  void  MakeHistogramsProb(int index,int Anode);
  void  MakeHistogramsAdc(StSvtHybridData* hybridData, int index,int Anode, int Count);
  Int_t Reset();

  Int_t SetMinAdcLevels( int MinAdc1,  int MinAbove1, int MinAdc2, int MinAbove2, int PedOffset); // Set the 2 thresholds for a sequence
  Int_t SetPedestalFile(const char* PedFile);
  Int_t SetLowInvProd(int LowInvProd);// Set the low threshold based on the frequency distribution

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSvtSeqAdjMaker.h,v 1.27 2014/08/06 11:43:46 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
    
 protected:
  St_ObjectSet* mSvtDataSet;              //!
  StSvtData* mSvtRawData;                 //!
  StSvtData* mSvtAdjData;                 //!
  StSvtHybridData* mHybridRawData ;       //!
  StSvtHybridData* mHybridAdjData ;       //!

  StSvtHybridCollection* mSvtBadAnodes;   //!
  StSvtHybridCollection* mSvtPedColl;     //!

  StSvtInverseProducts* mInvProd;         //!
  StSvtPedSub* mSvtPedSub;                //!
  StSvtProbValues* mProbValue;            //!

  const char* mPedFile;                   //!

  int* anolist;   //!
  TFile *hfile;   //!
  unsigned long Evt_counts;  //!

  TH1F* mOcupancyHisto;                   //!
  TH1F* EventOccupancy;                   //!
  TH1D** mInvProdSeqAdj;                  //!
  TH1F** mRawAdc;                         //!
  TH1F** mAdcAfter;                       //!
  TH1F* mCommonModePitch;                 //!
  TH1F* mCommonModeCount;                 //!
  TH2F** mTimeAn;                         //!
  TH1F** mRawPixel;                       //!
  int mNumOfSeq;
  int m_n_seq_lo;
  int m_n_seq_hi;
  int m_thresh_lo;
  int m_inv_prod_lo;
  int m_thresh_hi;
  
  int mTotalNumberOfHybrids;
  int mPedOffSet;
  int mCommonModeNoise[128];
  int mCommonModeNoiseAn[128];
  int mNAnodes;
  int doCommon;
  int adcCommon[128];

 private:
  string buildFileName(string dir, string fileName, string extention);
  string baseName(string s);
  ClassDef(StSvtSeqAdjMaker,0)   //virtual base class for Makers

};


#endif
