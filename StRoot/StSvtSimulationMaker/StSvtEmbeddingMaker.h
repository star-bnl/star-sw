/***************************************************************************
 *
 * $Id: StSvtEmbeddingMaker.h,v 1.3 2003/11/30 20:51:48 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Embedding Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtEmbeddingMaker.h,v $
 * Revision 1.3  2003/11/30 20:51:48  caines
 * New version of embedding maker and make OnlSeqAdj a stand alone maker
 *
 * Revision 1.2  2003/09/10 19:47:37  perev
 * ansi corrs
 *
 * Revision 1.1  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 **************************************************************************/
#ifndef STAR_StSvtEmbeddingMaker
#define STAR_StSvtEmbeddingMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StSvtHybridData;
class StSvtData;
class StSvtHybridCollection;
class StSvtHybridPixelsD;
class TH2F;
class TFile;

class StSvtEmbeddingMaker : public StMaker
{

public:
  
  StSvtEmbeddingMaker(const char* name = "SvtEmbedding");
  virtual ~StSvtEmbeddingMaker();
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual Int_t InitRun(int runumber);
    
  void setBackGround(Bool_t backgr,double backgSigma);
 
private:
  void GetSvtData();
  void GetPedRMS();
  void ClearMask();
  void AddRawData();
  void CreateBackground();
 
  //for debugging
  void bookHistograms();
  //void fillHistograms();
  double  MakeGaussDev(double sigma);

  StSvtData*               mSimPixelColl;   //!
  StSvtData*               mRealDataColl;       //!
  StSvtHybridCollection*   mPedColl;            //!
  StSvtHybridCollection*   mPedRMSColl;         //!

  double mBackGSigma;  //default value if individiual RMS are not available 
  Bool_t mBackGrOption;
  Bool_t mMask[128*240];

//global variables for temporary store in the loop
  StSvtHybridPixelsD  *mCurrentPixelData;
  int mCurrentIndex;


  //+++++++++++
  TH2F** mDataHist;      //!
  TFile* mFile;          //!
  
  ClassDef(StSvtEmbeddingMaker,1)

};

#endif


