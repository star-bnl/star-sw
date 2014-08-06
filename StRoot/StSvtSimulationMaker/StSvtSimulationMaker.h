// $Id: StSvtSimulationMaker.h,v 1.19 2014/08/06 11:43:46 jeromel Exp $
// $Log: StSvtSimulationMaker.h,v $
// Revision 1.19  2014/08/06 11:43:46  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.18  2007/12/24 17:37:20  fisyak
// Add protection from missing geometry
//
// Revision 1.17  2007/11/01 19:56:16  caines
// Added routines to move SVT hits from GEANT geometry to real geometry
//
// Revision 1.16  2005/07/23 03:37:34  perev
// IdTruth + Cleanup
//
// Revision 1.15  2005/02/09 14:33:36  caines
// New electron expansion routine
//
// Revision 1.12  2004/03/30 21:27:12  caines
// Remove asserts from code so doesnt crash if doesnt get parameters it just quits with kStErr
//
// Revision 1.11  2004/02/24 15:53:22  caines
// Read all params from database
//
// Revision 1.10  2004/01/22 16:30:47  caines
// Getting closer to a final simulation
//
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

/*!
 *
 * \class  StSvtSimulationMaker
 * \author Chaloupka
 * \date   2004/07/29
 * \brief SVT Slow Simulator
 *        Simulates hits in SVT based on geant data and database information  
 */


class StSvtSimulationMaker : public StMaker
{
 public:
  StSvtSimulationMaker(const char* name = "SvtSimulator");
  virtual ~StSvtSimulationMaker(); //** destructor should be virtual

  ///seting different options and configurations - for testing purposes
   Int_t setOptions(int SigOption);
  Int_t setConst(double timBinSize, double anodeSize);
 
  ///inherited maker routines
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(const char *opt);
  virtual Int_t Finish();
  virtual Int_t InitRun(int runumber); 
  virtual Int_t FinishRun(int oldrunumber);

  /// only these three constants influence the physics of electron cloud expansion 
  void setElectronLifeTime(double tLife);
  void setTrappingConst(double trapConst);
  void setDiffusionConst(double diffConst);

  void  setSvtPixelData();
  void  setGeantData();

  void resetPixelData();
  
  void FillGeantHit(int barrel, int ladder, int wafer, int hybrid,
                    StSvtWaferCoordinate* waferCoord,StThreeVector<double>* VecG,
		    StThreeVector<double>* VecL, double peak,int idtrk);  


  Int_t getConfig();
  Int_t getSvtGeometry();
  Int_t getSvtDriftSpeeds();
  Int_t getSvtT0();
  Int_t getPedestalOffset();
 

//+++++++++++++++++++++++
  Int_t GetNumOfHybrids(){return mNumOfHybrids;}
  TNtuple* GetNTuple(){return mNTuple;}

 private:
  
  ClassDef(StSvtSimulationMaker,4)       //!
 
  Int_t ideal2RealTranslation(StThreeVector<double> *pos, StThreeVector<double> *mtm, double charge, int *wafId);

  // initial options
  double mLifeTime;        //!
  double mTrapConst;       //!
  double mDiffusionConst;  //!
  double mTimeBinSize;     //!
  double mAnodeSize;       //! 
  int    mPedOffset;       //!  not absolutely necesary to be already here - could be added in EmbeddingMaker, but it works

  Int_t mSigOption;    //!
   
  Int_t  mNumOfHybrids;         //!could be used to override number of simulated hybrids
  double mDefaultDriftVelocity; //! obsolete? - used if no database, error might be better
  double mBField; // z component of BField;

  ///data for whole simulation
  StSvtConfig                  *mConfig;              //! created in constructor (desctructor kills)
  Bool_t                        mDoEmbedding;         //! embedding or plain simulation?

  ///tools-owned by maker
  StSvtAngles                  *mSvtAngles;          //! created in Init (desctructor kills)
  StSvtElectronCloud           *mElectronCloud;      
  StSvtSimulation              *mSvtSimulation;      
  StSvtCoordinateTransform     *mCoordTransform;     //! created in Init (desctructor kills)
  
  //data for each run
  StSvtGeometry                *mSvtGeom;            //! read for each run in InitRun(owned by SvtDbMaker - don't kill)
  StSvtHybridCollection        *mDriftSpeedColl;     //!
  StSvtT0                      *mT0;                 //! 
          
 
  ///data for each event - get deleted in ::Clear(), including the data inside
  StSvtData                         *mSvtSimPixelColl;    //! the simulated data - created for each run InitRun{in beginAnalyses} 
  StSvtData                         *mSvt8bitPixelColl;   //! simulated final result written to 8 bits - would be cleaner if owned by OnlSeqAdj
  StSvtData                         *mSvtGeantHitColl;    //!
  
  ///for debugging
  int                          *counter;                //!
  TFile                        *mNtFile;                //! 
  TNtuple                      *mNTuple;                //! 
  

    
    
  virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtSimulationMaker.h,v 1.19 2014/08/06 11:43:46 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}
    

  
};

#endif
