// $Id: StSvtSimulationMaker.h,v 1.3 2001/02/18 00:10:42 caines Exp $
// $Log: StSvtSimulationMaker.h,v $
// Revision 1.3  2001/02/18 00:10:42  caines
// Improve and use StSvtConfig
//
// Revision 1.2  2001/02/07 19:13:51  caines
// Small fixes to allow to run without setup from command line
//
// Revision 1.1  2000/11/30 20:47:49  caines
// First version of Slow Simulator - S. Bekele
//
//
#ifndef STAR_StSvtSimulationMaker
#define STAR_StSvtSimulationMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtSeqAdj base class                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;
class TFile;
class TNtuple;
class TH1D;
class TH2D;
class TString;

class St_g2t_svt_hit;
class svg_shape_st;
class srs_srspar_st;
class svg_geom_st;

class StSvtHybridCollection;
class StSvtConfig;
class StSvtGeantHits;
class StSvtHybridPixels;
class StSvtHybridSimData;
class StSvtEnergySim;
class StSvtSimulation;
class StSvtSignal;
class StSvtElectronCloud;
class StSvtAngles;
class StSvtWaferCoordinate;
class StSvtCoordinateTransform;

class StSvtSimulationMaker : public StMaker
{
 public:
  StSvtSimulationMaker(const char* name = "SvtSimulator");
  ~StSvtSimulationMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  Int_t setOneHit(int oneHit = 0, double anode = 0, double time = 0,double energy = 0,double theta = 0,double phi = 0);
  Int_t setOptions(char* option1, int option2, int option3, int option4);
  Int_t setConst(char* backgr,double backgSigma, double timBinSize, double anodeSize );
  Int_t setConfig(const char* config);
  Int_t setConfig(StSvtConfig* config);
  Int_t setEval();
  Int_t setSvtRawData(); 
  Int_t setTables();
  Int_t setHybrid();

  void  calcAngles(svg_geom_st *geom_st,int hardWarePosition);
  void  fillBuffer(double mAnHit, double mTimeHit, int hybIndex);
  Int_t doOneHit(StSvtHybridPixels* mSvtSimDataPixels);
  Int_t fillEval(int barrel, int ladder, int wafer, int yybrid, StSvtWaferCoordinate& waferCoord);

  Int_t CreateHistograms();
  void  MakeHistograms1();
  void  MakeHistograms2();
  void  histTimeDist();
  void  histChargeDist();

 private:
  TString                      mConfigString;  
  
  StSvtConfig                  *mConfig;             //!
  StSvtAngles                  *mSvtAngles;          //!
  StSvtElectronCloud           *mElectronCloud;      //!
  StSvtSimulation              *mSvtSimulation;      //!
  StSvtSignal                  *mSvtSignal;          //!
  StSvtEnergySim               *mEnergySim;          //!
  StSvtCoordinateTransform     *mCoordTransform;     //!

  StSvtHybridCollection        *mSvtSimPixelColl;    //!
  StSvtHybridCollection        *mSvtSimDataColl;     //!
  StSvtHybridCollection        *mSvtGeantHitColl;    //!


  StSvtGeantHits               *mSvtGeantHit;        //!
  StSvtHybridPixels            *mBackGrDataPixels;   //!
  StSvtHybridPixels            *mSvtSimDataPixels;   //!
  StSvtHybridSimData           *mSimHybridData;      //!

  St_ObjectSet                 *mGeantHitSet;        //!
  St_ObjectSet                 *mSimDataSet;         //!

  St_g2t_svt_hit               *mG2tSvtHit;       //!
  svg_shape_st                 *mSvtShape;        //!
  svg_geom_st                  *mSvtGeom;         //!
  srs_srspar_st                *mSvtSrsPar;       //!

  TFile                        *mNtFile;          //! 
  TNtuple                      *mNTuple;          //!

  Int_t mNumOfHybrids;
  char* mBackGrOption ;    //! 
  char* mExpOption;    //!  
  Int_t mWrite;        // for signal width outputs
  Int_t mFineDiv;      // mFineDiv = 0 to use finer scales for signal width outputs 
  Int_t mSigOption;
  Int_t mOneHit;

  double mAnode;
  double mTime;    

  double mTimeBinSize;
  double mAnodeSize;    
  double mDriftVelocity;
  double mEnergy;
  double mTheta;
  double mPhi;
   
  double mBackGSigma;

  TH1D* mTimeDist[7];          //!
  TH1D* mChargeDist;           //!
  TH2D** hit_plus_backgr;      //!
  TH2D** geant_hit;            //!

  virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtSimulationMaker.h,v 1.3 2001/02/18 00:10:42 caines Exp $ built "__DATE__" "__TIME__; return cvs;}

  ClassDef(StSvtSimulationMaker,1)

};

#endif
