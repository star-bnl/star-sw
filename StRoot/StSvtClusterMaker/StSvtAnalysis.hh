/***************************************************************************
 *
 * $Id: StSvtAnalysis.hh,v 1.9 2005/07/23 03:37:33 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysis.hh,v $
 * Revision 1.9  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.8  2003/01/28 20:27:49  munhoz
 * new filters for clusters
 *
 * Revision 1.7  2002/05/09 16:55:40  munhoz
 * add reading bad anodes from DB
 *
 * Revision 1.6  2002/04/25 20:34:50  caines
 * Pass bad anode information into cluster fitter
 *
 * Revision 1.5  2000/10/31 16:20:57  caines
 * Added more functions to make the code more readable
 *
 * Revision 1.4  2000/08/24 04:27:56  caines
 * Fixed casting warnings so compiles without errors on linux
 *
 * Revision 1.3  2000/08/21 13:06:58  caines
 * Much improved hit finding and fitting
 *
 **************************************************************************/


#ifndef STSVTANALYSIS_HH
#define STSVTANALYSIS_HH
#include  <string.h> 

#include  "TArrayC.h" 
#include  "myPoint.h" 
#include  "StSvtHybridCluster.hh"

class StSvtHybridData;
class StSvtHybridBadAnodes;
class StSequence;
class StSvtAnalysisAux
{
public:		
  int mCluFirstAnode;                     //!
  int mCluLastAnode;                      //!
  int mCluFirstTimeBin;                   //!
  int mCluLastTimeBin;                    //!
  int mCluFlag;                           //!
  int m_oneortwo_flag;                    // added by JT
  int mCluPeakAdc;                        //!
  int mCluNumPixels;                      //!
  int mCluNumAnodes;                      //!
  int mHybridNum;                         //!
  int mCluID;                             //!
  int mCluDeconvID;                       //!
  StSvtClusterMemberInfo* mInfo;          //!
  int mTruth;
  double mCluCharge;                      //!
  double mMeanClusterTimeBin;             //!
  double mMeanClusterAnode;               //!
  double mSecondMomClusterTimeBin;        //!
  double mSecondMomClusterAnode;          //!
  double mCluXCov;                        //!
  double mCluYCov;                        //!
};
class StSvtAnalysis 
{

public:
  StSvtAnalysis(int numOfHybrids);
  virtual ~StSvtAnalysis();

  void   SetPointers(StSvtHybridData* hybAdjData,StSvtHybridData* hybRawData,
                     StSvtHybridCluster* hybClu, StSvtHybridBadAnodes* SvtBadAnode,int numOfHybrids,
		     int PedOffset);
  void setArrays(int TotalNumberOfHybrids);
  void setMemory();
  void calcMoments(int clu);
  void oneOrTwoAnodeMoments(int clu, int peakPosTim);
  void finalMoments(int clu , int numAnodes);
  void newCluster(int clu, int numAnodes,int igt3);
  void   FirstAndLastAnodes();
  void   CluFirstTimeBin();
  void   CluLastTimeBin();
  void   MomentAnalysis();
  int    GetFirstAnode(int clu);
  int    GetLastAnode(int clu);
  int    GetFirstTimeBin(int clu);
  int    GetLastTimeBin(int clu);
  int    GetCluFlag(int clu);
  int    GetDeconvFlag(int clu);
  int    return_oneortwoanode_flag(int clu);
  int    GetCluPeakAdc(int clu);
  int    GetCluNumAnodes(int clu);
  int    GetCluNumPixels(int clu);
  int    GetnSvtClu();
  int    GetCluID(int clu);
  int    GetCluDeconvID(int clu);
  int    GetTruth(int clu);
  double    GetCluCharge(int clu);
  double GetMeanClusterAnode(int clu);
  double GetMeanClusterTimeBin(int clu);
  double GetSecondMomClusterAnode(int clu);
  double GetSecondMomClusterTimeBin(int clu);
  double GetCluXCov(int clu);
  double GetCluYCov(int clu);
  void   Report(int index);
//void   ResetMeanValues();
  void   SetBadAnTb(int numClus);
  void   LoadAnodeGains();

  int    Print_Pixels(int iRows, int iCols, int clu);
  int    Fill_Pixel_Array(int clu);
  void   free_matrix_d (int**, int);
  int    **malloc_matrix_d (int iRows, int iCols);
  POINT  *Find_Peaks (int iRows, int iCols, int *iNumPeaks);
  float  IsValidPeak (int iRows, int iCols, POINT  *Peaks, int iNumPeaks);
  int    BlockOut (int x, int y);
  int    Fit_Peaks(int iRows, int iCols, int iNumPeaks, POINT *Peaks, int clu);
  int    CatagorizeCluster(int iRows, int iCols, int igt3, int clu);
  int    Deconvolve_Cluster(int iRows, int iCols, int clu);
  void   SetHybIndex(int index);
  int    FillRawAdc();
  void   ClearRawAdc();
  void   updateTruth();

private:

  int m_clu;                               //!
  int m_row_p;                             //!
  int m_col_p;                             //!
  int m_adc_p;                             //!
  int m_SvtEvt;                            //!

  int m_deconv;                            //!
  int m_nWrkBkt;                           //!
  int m_nGt8;                              //!
  int m_nUndBkt;                           //! 
  int m_nSig;                              //! 

  int m_hybIndex;                          //!

  int mHitId;
  int mMyflag;                             // added by JT
  int mNeff;
  int mNumOfClusters, mNumOfMembers;
  int mNumPixels, mPeakADC, mSumAdc;

  double mDriftMom1, mAnodeMom1;
  double mDriftMom2, mAnodeMom2, mMom0;
  double mX_err, mY_err;  
  double mAnodeGain[433][241];             // added by JT

  StSvtHybridData* mHybridData;            //!
  StSvtHybridData* mHybridRawData;         //!
  StSvtHybridCluster* mHybridCluster;      //!
  StSvtHybridBadAnodes* mSvtBadAnode;             //!
  StSequence* mSvtSequence;                //!
  
  TArrayC mAuxArr;
  int mMaxClu;
  StSvtAnalysisAux *mAux;                  //!

  int** m_countBadAn;                      //!
  int** m_countBadTb;                      //!

  StSvtClusterMemberInfo** tempMemberInfo; //!

  int** m_Pixels;                          //!
  int** m_Shadow;                          //!
  int** m_Raw;                             //!
   
  int mPedOffset;

  };

#endif
