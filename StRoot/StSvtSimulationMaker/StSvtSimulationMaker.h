// $Id: StSvtSimulationMaker.h,v 1.9 2003/11/30 20:51:48 caines Exp $
// $Log: StSvtSimulationMaker.h,v $
// Revision 1.9  2003/11/30 20:51:48  caines
// New version of embedding maker and make OnlSeqAdj a stand alone maker
//
// Revision 1.8  2003/11/13 16:25:00  caines
// Further improvements to get simulator looking like reality
//
// Revision 1.6  2001/08/13 15:34:19  bekele
// Debugging tools added
//
// Revision 1.5  2001/03/19 22:25:53  caines
// Catch wrong wafer ids more elegantly
//
// Revision 1.4  2001/03/15 15:12:09  bekele
// added a method to fill the whole SVT hybrid with background
//
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
// StSvtSeqAdj base class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H  
#include "StMaker.h"
#endif

#include "StThreeVector.hh"

class StChain;
class TFile;
//class TDirectory;
class TNtuple;
class TH1D;
class TH2D;
class TString;

//class St_g2t_svt_hit;
class StSvtGeometry;

class StSvtHybridCollection;
class StSvtData;
class StSvtHybridData;
class StSvtConfig;
class StSvtGeantHits;
class StSvtHybridPixelsC;
class StSvtHybridPixelsD;
class StSvtSimulation;
class StSvtElectronCloud;
class StSvtAngles;
class StSvtWaferCoordinate;
class StSvtCoordinateTransform;
class StSvtT0;


class StSvtSimulationMaker : public StMaker
{
 public:
  StSvtSimulationMaker(const char* name = "SvtSimulator");
  virtual ~StSvtSimulationMaker(); //** destructor should be virtual

  //seting different options and configurations
 
  Int_t setOptions(char* option1, int option2, int option3, int option4);
  Int_t setConst(double timBinSize, double anodeSize, int offset );
  Int_t setBackGround(Bool_t backgr,double backgSigma);
  
  //t+ Int_t setEval(Bool_t key);
  //t+ Int_t setEmbedding(Bool_t key);
 
  //inherited maker routines
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(const char *opt);
  virtual Int_t Finish();
  virtual Int_t InitRun(int runumber); 
   
  void setElectronLifeTime(double tLife);
  void setTrappingConst(double trapConst);
  void setDiffusionConst(double diffConst);

  void  setSvtPixelData();
  void  setGeantData();

  void resetPixelData();
  void createBackGrData();
  void FillGeantHit(int barrel, int ladder, int wafer, int hybrid,
                    StSvtWaferCoordinate& waferCoord,StThreeVector<double>& VecG,
                    StThreeVector<double>& VecL, double peak);

  Int_t getConfig();
  Int_t getSvtGeometry();
  Int_t getSvtDriftSpeeds();
  Int_t getSvtT0();
  void  getPedRMS();
  Int_t getBadAnodes();

//+++++++++++++++++++++++
  void MakePixelHistos();
  void MakeGeantHitsHistos();
  void CreateHistograms();
 
  Int_t GetNumOfHybrids(){return mNumOfHybrids;}
  //TH2D* GetHitPlusBkgrHist(int i){ return hit_plus_backgr[i];}
  //TH2D* GetGeantHitHist(int i){ return geant_hit[i];}
  TNtuple* GetNTuple(){return mNTuple;}

 private:

  // initial options
  double mLifeTime;
  double mTrapConst;
  double mDiffusionConst;
  double mTimeBinSize;
  double mAnodeSize;    
  int    mPedOffset;

  double mBackGSigma;  //default value if individiual RMS are not available 
  Bool_t mBackGrOption;

  char* mExpOption;    //!  
  Int_t mWrite;        // for signal width outputs
  Int_t mFineDiv;      // mFineDiv = 0 to use finer scales for signal width outputs 
  Int_t mSigOption;
  
  //t+ Bool_t  mDoEmbedding;
  //Bool_t  mDoEval;
 

  Int_t mNumOfHybrids;   //could be used to override number of simulated hybrids
  double mDefaultDriftVelocity;  //should be from database

  //data for whole simulation
  StSvtConfig                  *mConfig;              //! created in constructor (desctructor kills)

  //tools-owned by maker
  StSvtAngles                  *mSvtAngles;          //! created in Init (desctructor kills)
  StSvtElectronCloud           *mElectronCloud;      //! created in Init (desctructor kills)
  StSvtSimulation              *mSvtSimulation;      //! created in Init (desctructor kills)
  StSvtCoordinateTransform     *mCoordTransform;     //! created in Init (desctructor kills)
  
  //data for each run
  StSvtGeometry                *mSvtGeom;            //! read for each run in InitRun(owned by SvtDbMaker - don't kill)
  StSvtHybridCollection        *mDriftSpeedColl;     //!
  StSvtT0                      *mT0;                 //!
  StSvtHybridCollection        *mPedColl;            //!
  StSvtHybridCollection        *mPedRMSColl;         //!
  StSvtHybridCollection        *mSvtBadAnodes;       //!
 
  //data for each event
  StSvtData                         *mSvtSimPixelColl;    //! the simulated data - created for each run InitRun{in beginAnalyses} 
  StSvtData                         *mSvt8bitPixelColl;   //! simulated final result written to 8 bits
  StSvtData                         *mSvtGeantHitColl;    //!
  
  //  TFile                        *mDebugFile;         //!
  TFile                        *mNtFile;                //! 
  TNtuple                      *mNTuple;                //! 
    

  virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtSimulationMaker.h,v 1.9 2003/11/30 20:51:48 caines Exp $ built "__DATE__" "__TIME__; return cvs;}

  ClassDef(StSvtSimulationMaker,3)

  
};

#endif
