/***************************************************************************
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Ssd Add Maker class
 *
 ***************************************************************************
 */
#ifndef STAR_StSsdEmbeddingMaker
#define STAR_StSsdEmbeddingMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdUtil/StSsdWafer.hh"

class TNtuple;
class TFile;
class StSsdBarrel;
class St_ssdDimensions;
class StDAQReader;
class St_spa_strip;
class StEvent;
class StSsdEmbeddingMaker : public StMaker 
{
  private :
  St_ssdDimensions *m_dimensions;//!
  Int_t            MODE;
 public:
  StSsdEmbeddingMaker(const char* name = "SsdEmbed");
  virtual ~StSsdEmbeddingMaker();
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual Int_t InitRun(int);
  void          setDoEmbedding(Bool_t doIt);                      
  void          setPlainSimEvenIfNoSSD(Bool_t doIt);
  Int_t         GetSsdData();
  Int_t         AddRawData();
  Int_t         NoSsd();
  void          CheckTables();
  Int_t         idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  void          setSsdParameters(ssdDimensions_st *geom_par);  
  
 private:
  Bool_t           mDoEmbedding;     //shell I try to run embedding if posssible 
  Bool_t           mPlainSimIfNoSSD; // if true it will run plain simulation insted of embedding if there's no SSD in real data
  Bool_t           mRunningEmbedding;// can I realy run embedding - ie. missing DAQ maker?
  Int_t            mSsdLayer;
  Int_t            mNLadder;
  Int_t            mNWaferPerLadder;
  Int_t            mNStripPerSide;
  Float_t          mDetectorLargeEdge;
  Float_t          mDetectorSmallEdge;
  Float_t          mStripPitch;
  Float_t          mTheta;
  St_spa_strip     *mSsdSimuData;
  St_spa_strip     *mSsdrealData;
  St_spa_strip     *mSsdSimuReal;
  ssdDimensions_st *mDimensions;
  protected :
    
  StEvent             *mCurrentEvent;   //!
  TH1F                *hStripsSimu;//!
  TH1F                *hStripsReal;//!
  TH1F                *hStripsCommon;//!
  TFile               *myfile;  //!
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSsdEmbeddingMaker.h,v 1.2 2014/08/06 11:43:43 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StSsdEmbeddingMaker,1)
};
#endif

