 /***************************************************************************
 *
 * $Id: StSvtSimulationMaker.cxx,v 1.49 2013/03/26 15:56:00 genevb Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Slow Simulator Maker class
 *
 ***************************************************************************
  * Revision 1.23  2004/04/08 15:11:27  caines
 * Ensure array is initialised to zeros
 *
 * Revision 1.22  2004/04/06 20:18:19  caines
 * Initialise variable counter in the constructor to NULL to avoid crash
 *
 * Revision 1.21  2004/03/30 21:27:12  caines
 * Remove asserts from code so doesnt crash if doesnt get parameters it just quits with kStErr
 *
 * $Log: StSvtSimulationMaker.cxx,v $
 * Revision 1.49  2013/03/26 15:56:00  genevb
 * Replace agufld(x,b) with direct call to StarMagField::Instance()->BField(x,b)
 *
 * Revision 1.48  2013/02/18 16:30:42  fisyak
 * gufld => agufld
 *
 * Revision 1.47  2010/09/25 16:17:50  caines
 * Add iteration to transformation routines so local -> wafer followed by wafer->local come to same point, needed because of dirft velocity assumptions
 *
 * Revision 1.46  2010/09/22 13:44:25  caines
 * Record is StMcHit the shifted hit position after caling ideal2real routines
 *
 * Revision 1.45  2010/08/27 17:46:28  perev
 * WarnOff
 *
 * Revision 1.44  2009/06/28 04:04:06  baumgart
 * Increase of trapping constant to compensate for changes in electron cloud shape
 *
 * Revision 1.43  2009/06/11 23:22:07  baumgart
 * Decrease of cTrapConst to reflect proper time evolution of hits
 *
 * Revision 1.42  2009/02/21 14:18:45  caines
 * change trapping const to better reproduce data
 *
 * Revision 1.41  2009/01/28 23:03:42  fisyak
 * Fix wafId
 *
 * Revision 1.40  2009/01/22 23:19:21  fine
 * Prptection against fo the crash
 *
 * Revision 1.39  2008/12/13 01:12:57  caines
 * Check that ladder index not out of range in translation routine
 *
 * Revision 1.38  2008/11/07 20:42:06  caines
 * Fix some mistakes in new way of initializing variables. lifetime was missing
 *
 * Revision 1.37  2008/10/21 21:13:30  fine
 * Initialize the class data-members see bug #1294
 *
 * Revision 1.36  2008/10/21 21:05:51  fine
 * Initialize the class data-members see bug #1294
 *
 * Revision 1.35  2007/12/24 17:37:19  fisyak
 * Add protection from missing geometry
 *
 * Revision 1.34  2007/11/01 19:56:12  caines
 * Added routines to move SVT hits from GEANT geometry to real geometry
 *
 * Revision 1.33  2007/07/12 20:18:18  fisyak
 * read Db by deman
 *
 * Revision 1.32  2007/03/21 17:25:51  fisyak
 * Ivan Kotov's drift velocities, TGeoHMatrix
 *
 * Revision 1.31  2005/10/21 02:46:07  caines
 * Improve PASA shift to 220 microns to place hit residuals in MC closer to zero

 * Revision 1.30  2005/10/19 21:22:30  caines
 * Make PASA shift 200 microns
 *
 * Revision 1.29  2005/10/18 18:59:24  caines
 * Addtional code change for improving PASA shift
 *
 * Revision 1.28  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.26  2005/02/09 14:33:35  caines
 * New electron expansion routine
 *
 * Revision 1.20  2004/02/24 15:53:22  caines
 * Read all params from database
 *
 * Revision 1.19  2004/01/27 02:45:42  perev
 * LeakOff
 *
 * Revision 1.18  2004/01/22 16:30:47  caines
 * Getting closer to a final simulation
 *
 * Revision 1.17  2003/11/30 20:51:48  caines
 * New version of embedding maker and make OnlSeqAdj a stand alone maker
 *
 * Revision 1.16  2003/11/15 20:24:29  caines
 * fixes to remove warnings at compilation
 *
 * Revision 1.15  2003/11/14 17:33:19  caines
 * DOnt read from pedmaker for now
 *
 * Revision 1.14  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.12  2003/07/31 19:18:10  caines
 * Petrs improved simulation code
 *
 * Revision 1.10  2001/11/06 20:12:06  caines
 * Add include for new compiler
 *
 * Revision 1.9  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.7  2001/04/12 20:34:54  caines
 * Add check for if nosvt hits present
 *
 * Revision 1.6  2001/04/03 15:24:24  caines
 * Increase hit space size again
 *
 * Revision 1.5  2001/03/19 22:25:53  caines
 * Catch wrong wafer ids more elegantly
 *
 * Revision 1.4  2001/03/15 15:12:09  bekele
 * added a method to fill the whole SVT hybrid with background
 *
 * Revision 1.3  2001/02/18 00:10:42  caines
 * Improve and use StSvtConfig
 *
 * Revision 1.2  2001/02/07 19:13:51  caines
 * Small fixes to allow to run without setup from command line
 *
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/


#include <string>
#include <cmath>
using namespace std;

#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"
#include "TH1.h"
#include "TH2.h"
#include "TString.h"

#include "TFile.h"
#include "TDirectory.h"
#include "TNtuple.h"

#include "StSequence.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
//#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsD.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtDaq.hh"
#include "StSvtDaqMaker/StSvtHybridDaqPed.hh"
#include "StSvtCalibMaker/StSvtPedMaker.h"
#include "StSvtAngles.hh"
#include "StSvtElectronCloud.hh"
#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"
#include "StSvtGeantHits.hh"

#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"

#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
//#include "StSvtConversionTable.h"

#include "StSvtSimulationMaker.h"
#include "StDbUtilities/St_svtCorrectionC.h"
#include "StDbUtilities/St_svtHybridDriftVelocityC.h"
#include "StMcEvent/StMcSvtHitCollection.hh"
#include "StParticleDefinition.hh"
#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcSvtHit.hh"
#include "StarMagField.h"

ClassImp(StSvtSimulationMaker)

  /*! hardvired constants
   * mTimeBinSize = 0.04;  // Micro Secs -  this is quite accurate according to Dave Lynn
   * mDefaultDriftVelocity = 1.E-5*675000;  // [mm/us] used only if there is no database
   * mDiffusionConst=0.0035;   // [mm**2/micro seconds] default=0.0035 (for silicon)
   * mLifeTime=1000000.0;     // [us]   default =1000000.0
   * mTrapConst=4.0e-5;       // [us]   default =0
  */

#define cTimeBinSize 0.04
#define cDefaultDriftVelocity 1.E-5*675000
#define cDiffusionConst 0.0035
#define cLifeTime 1000000.0
//#define cTrapConst 4.0e-5
#define cTrapConst 5.0e-5 // Stephens tuning to real data
//___________________________________________________________________________
/// the only place where electron cloud expansioin constants are set
StSvtSimulationMaker::StSvtSimulationMaker(const char *name):StMaker(name)
 , mLifeTime(cLifeTime)          // [us]   //default =1000000.0
 , mTrapConst(cTrapConst)           // [us]   //default =0
 , mDiffusionConst(cDiffusionConst) // [mm**2/micro seconds] default=0.0035 (for silicon)
 , mTimeBinSize(cTimeBinSize)          // [us]   //default =0.04 
 , mAnodeSize(-1956)
 , mPedOffset(-1956)                //  not absolutely necesary to be already here - could be added in EmbeddingMaker, but it works
 , mSigOption(0)                    // use both PASA codes, mNumOfHybrids(-1956)             //!could be used to override number of simulated hybrids
 , mDefaultDriftVelocity(-1956)     // obsolete? - used if no database, error might be better
 , mBField(-1956)                   // z component of BField;
 , mConfig(0)                       // created in constructor (desctructor kills)
 , mDoEmbedding(kFALSE)             // embedding or plain simulation?
 , mSvtAngles(0)                    // created in Init (desctructor kills)
 , mElectronCloud(0)
 , mSvtSimulation(0)
 , mCoordTransform(0)               // created in Init (desctructor kills)
 , mSvtGeom(0)                      // read for each run in InitRun(owned by SvtDbMaker - don't kill)
 , mDriftSpeedColl(0)
 , mT0(0)
 , mSvtSimPixelColl(0)             // the simulated data - created for each run InitRun{in beginAnalyses} 
 , mSvt8bitPixelColl(0)            // simulated final result written to 8 bits - would be cleaner if owned by OnlSeqAdj
 , mSvtGeantHitColl(0)             //
 , counter(0)
 , mNtFile(0)
 , mNTuple(0)
{ 
  LOG_DEBUG << "StSvtSimulationMaker::constructor"<<endm;
  
  //initial cleanup
  mNumOfHybrids = 0; 
  LOG_DEBUG << "StSvtSimulationMaker::constructor...END"<<endm;
}

//____________________________________________________________________________
StSvtSimulationMaker::~StSvtSimulationMaker()
{
  LOG_DEBUG << "StSvtSimulationMaker::destructor"<<endm;
 
  SafeDelete(mSvtAngles);
  SafeDelete(mSvtSimulation);
  SafeDelete(mElectronCloud);
  SafeDelete(mCoordTransform);

  LOG_DEBUG << "StSvtSimulationMaker::destructor...END"<<endm; 
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setConst(double timBinSize, double anodeSize)
{
  mTimeBinSize = timBinSize ;
  mAnodeSize = anodeSize;    
  return kStOK;
}


//____________________________________________________________________________

void StSvtSimulationMaker::setElectronLifeTime(double tLife)
{
  mLifeTime = tLife;  //  [micro seconds] 

}
//____________________________________________________________________________
void StSvtSimulationMaker::setTrappingConst(double trapConst)
{
  mTrapConst = trapConst;  //  [micro seconds] 
}

//____________________________________________________________________________
void StSvtSimulationMaker::setDiffusionConst(double diffConst)
{
  mDiffusionConst = diffConst;  
}

//____________________________________________________________________________
//mainly for debugging
Int_t StSvtSimulationMaker::setOptions(int SigOption)
{
  mSigOption = SigOption;

  return kStOK;
}


//____________________________________________________________________________
Int_t StSvtSimulationMaker::Init()
{
      
  LOG_DEBUG << "In StSvtSimulationMaker::Init() ..."<<endm;
    

  LOG_DEBUG << "In StSvtSimulationMaker::Init() -End"<<endm;

  return  StMaker::Init();
}

//__________________________________________________________________________
///all database dependent data are read here 
Int_t StSvtSimulationMaker::InitRun(int runumber)
{ //when the run changes
 LOG_DEBUG << "StSvtSimulationMaker::InitRun()"<<endm;
  
  //read from database
  Int_t res;
  getConfig();
  if ((res=getSvtGeometry())!=kStOk) return res;
#if 0
  if ((res=getSvtDriftSpeeds())!=kStOk) return res;
#endif
  if ((res=getSvtT0())!=kStOk) return res;
  if ((res=getPedestalOffset())!=kStOk) return res; 

  setSvtPixelData();
  //Set up coordinate transformation 
  if (! mCoordTransform) mCoordTransform=new StSvtCoordinateTransform();  
   
  mCoordTransform->setParamPointers(mSvtGeom, mConfig,mDriftSpeedColl,mT0);
  
  //mTimeBinSize = 1.E6/mSvtSrsPar->fsca;  // Micro Secs
  //mAnodeSize = mSvtSrsPar->pitch*10;  // mm
  // *****values hard wired for the time being - should be in database
  mTimeBinSize = cTimeBinSize;  // Micro Secs - Petr: this is quite accurate according to Dave
  mAnodeSize = mSvtGeom->getAnodePitch()*10;  // mm
  mDefaultDriftVelocity = cDefaultDriftVelocity;  // used only if there is no database
  //set default drift speeds - if drift speed data exist it will be overriden later
  if (! mElectronCloud) mElectronCloud = new StSvtElectronCloud();
  mElectronCloud->setDriftVelocity(mDefaultDriftVelocity);
  mElectronCloud->setElectronLifeTime(mLifeTime);
  mElectronCloud->setDiffusionConst(mDiffusionConst);
  mElectronCloud->setTrappingConst(mTrapConst);
  if (! mSvtSimulation) mSvtSimulation = new StSvtSimulation();
  mSvtSimulation->setOptions(mSigOption);
  mSvtSimulation->setAnodeTimeBinSizes(mTimeBinSize , mAnodeSize);
  mSvtSimulation->setDriftVelocity(mDefaultDriftVelocity);
  mSvtSimulation->setElCloud(mElectronCloud);

  LOG_INFO <<"StSvtSimulationMaker::InitRun info:"<<endm;
  LOG_INFO <<"  Anode size="<<mAnodeSize<<" ,time bin size="<<mTimeBinSize<<endm;
  LOG_INFO <<"  default drift velocity="<<mDefaultDriftVelocity<<endm;
  LOG_INFO <<"  pedestal offset(from database)="<<mPedOffset<<endm;
  LOG_INFO <<"  T0(from database)= "<<mT0->getT0(1)<<endm;

  // Get BField;
  Float_t x[3] = {0,0,0};
  Float_t b[3];
  StarMagField::Instance()->BField(x,b);
  mBField = b[2]*tesla;

  LOG_DEBUG << "StSvtSimulationMaker::InitRun()-END"<<endm;
 
  return StMaker::InitRun(runumber);
}

//____________________________________________________________________________

Int_t  StSvtSimulationMaker:: FinishRun(int oldrunumber){
  LOG_INFO <<"StSvtSimulationMaker::FinishRun()"<<endm;
 
  TDataSet *set;
  if ((set=GetDataSet("StSvtPixelData"))) delete set;
  if ((set=GetDataSet("StSvt8bitPixelData"))) delete set;   
 
  LOG_INFO <<"StSvtSimulationMaker::FinishRun() - END"<<endm;
  return StMaker::FinishRun(oldrunumber);
}


//____________________________________________________________________________

void  StSvtSimulationMaker::resetPixelData(){
  //this resets mSvtSimPixelColl and mSvt8bitPixelColl
 
   StSvtHybridPixelsD* tmpPixels;
   StSvtHybridPixelsC* tmp8bitPixels;
   if (mSvtSimPixelColl) {
   for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {
     for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {
       for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {
         for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){
           
           int index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
           if( index < 0) continue; 
           
           tmpPixels  = (StSvtHybridPixelsD*)mSvtSimPixelColl->at(index);
           tmp8bitPixels = (StSvtHybridPixelsC*)mSvt8bitPixelColl->at(index);

           if(!tmpPixels) {
             tmpPixels = new StSvtHybridPixelsD(Barrel, Ladder, Wafer, Hybrid);
             mSvtSimPixelColl->put_at(tmpPixels,index);
           }
	   if(!tmp8bitPixels) {
             tmp8bitPixels = new StSvtHybridPixelsC(Barrel, Ladder, Wafer, Hybrid);
             mSvt8bitPixelColl->put_at(tmp8bitPixels,index);
           }

           tmpPixels->setPedOffset(mPedOffset);
           tmpPixels->reset();

         }
       }
     }
   }
   }
}


//____________________________________________________________________________
///create output data and put them into the chain
void  StSvtSimulationMaker::setSvtPixelData()
{ //add pixeldata to chain->.data
  if (GetDataSet("StSvtPixelData")) 
     LOG_ERROR <<"Error: Found StSvtSimPIxels in the chain - should have been deleted"<<endm;
     
  St_ObjectSet *set = new St_ObjectSet("StSvtPixelData");
  AddConst(set); 
  mSvtSimPixelColl = new StSvtData(mConfig->getConfiguration());
  set->SetObject((TObject*)mSvtSimPixelColl);
  
  set = new St_ObjectSet("StSvt8bitPixelData");
  AddConst(set); 
  mSvt8bitPixelColl = new StSvtData(mConfig->getConfiguration());
  set->SetObject((TObject*)mSvt8bitPixelColl);

  mNumOfHybrids = mSvtSimPixelColl->getTotalNumberOfHybrids(); 
}


//__________________________________________________________________________________________________
void  StSvtSimulationMaker::setGeantData()
{
  St_ObjectSet* set=(St_ObjectSet*)GetDataSet("StSvtGeantHits");

  if (set) {
    LOG_DEBUG <<"Found StSvtGeantHits in the chain- replacing"<<endm;
    set->SetObject(0);
  } 
  else{
    set =  new St_ObjectSet("StSvtGeantHits");
    AddData(set);
  }

  if (mSvtGeantHitColl) {
         LOG_ERROR <<"!!!!!!m SvtGeantHitColl already exists in SvtSimulationMaker.cxx:setEval"<<endm;
   } else{
    //owned by the SvtData
    mSvtGeantHitColl = new StSvtData(mConfig->getConfiguration());
    set->SetObject((TObject*)mSvtGeantHitColl);
  }

//+++++++++++++++++
//for debugging purposes
  if(!counter){
    counter = new int[mNumOfHybrids];
    for( int ii=0; ii<mNumOfHybrids; ii++){
      counter[ii] = 0;
    }
  }
}

//__________________________________________________________________________________________________
Int_t  StSvtSimulationMaker::getSvtGeometry()
{
    
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtGeometry");
  if (!dataSet){
    LOG_ERROR <<"BIG TROUBLE:No SVT geometry -impossible to run!!!!"<<endm;
    return kStFatal;
  }
 

  mSvtGeom = (StSvtGeometry*)dataSet->GetObject();
  if (!mSvtGeom){
    LOG_ERROR << "BIG TROUBLE:No SVT geometry -impossible to run!!!!"<<endm;
    return kStFatal;
    }
  
  
  return kStOk;
}

//__________________________________________________________________________________________________
Int_t  StSvtSimulationMaker::getPedestalOffset()
{
    
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtDaq");
  if (!dataSet){
    LOG_ERROR <<"BIG TROUBLE:No DAQ parameters for SVT!!!!"<<endm;
    return kStFatal;
  }
 

  StSvtDaq *daq = (StSvtDaq*)dataSet->GetObject();
  if (daq==NULL){
    LOG_ERROR << "BIG TROUBLE:No DAQ parameters are empty!!!!"<<endm;
    return kStFatal;
    }


  mPedOffset=daq->getPedOffset();

  return kStOk;
}
#if 0
//____________________________________________________________________________
Int_t StSvtSimulationMaker::getSvtDriftSpeeds()
{
  mDriftSpeedColl =NULL;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtDriftVelocity");
  if (!dataSet){
    cout<<"Warning: no SVT drift velocity data available - using default drift speed:"<<mDefaultDriftVelocity<<endl;
    return kStErr;
  } //this might be obsolete, maybe it's better to give an error instead of running on

  mDriftSpeedColl = (StSvtHybridCollection*)dataSet->GetObject();
  if (! mDriftSpeedColl) cout<<"Warning: SVT drift velocity data empty - using default drift speed:"<<mDefaultDriftVelocity<<endl;
    
  return kStOk;
}
#endif

//____________________________________________________________________________
Int_t StSvtSimulationMaker::getSvtT0()
{
  mT0=NULL;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtT0");
  if (!dataSet){
    LOG_WARN <<"no SVT T0 data available -using defalt T0 = 0"<<endm;
    return kStErr;
  } //this might be obsolete, maybe it's better to give an error instead of running on
  
  mT0 = (StSvtT0*)dataSet->GetObject();
  if (! mT0)   LOG_WARN <<" SVT T0 data empty - using default T0 = 0"<<endm;
  
  return kStOk;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::getConfig()
{
  mConfig=NULL;
  St_DataSet *dataSet = NULL;
  dataSet = GetDataSet("StSvtConfig");
  
  if (!dataSet)
    {
      LOG_WARN <<" No SvtConfig  data set" << endm;
      dataSet = new St_ObjectSet("StSvtConfig");                                                               
      AddConst(dataSet);
      mConfig=NULL;
    }
  
  mConfig=((StSvtConfig*)(dataSet->GetObject()));
 
  if (!mConfig) {
    LOG_WARN <<"SvtConfig data set is empty- seting default full configuration" << endm;
    mConfig=new StSvtConfig();
    mConfig->setConfiguration("FULL");
    dataSet->SetObject(mConfig);
  } 

  return kStOk;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::ideal2RealTranslation( StThreeVector<double> *pos,  StThreeVector<double> *mtm, double charge, int *wafId){
  StGlobalCoordinate globalCor(0,0,0);
  StThreeVector<double> x;
  // Find wafer geometry of wafer track passed through in Ideal Geom
  int index = mSvtGeom->getWaferIndex(*wafId);
  if (index < 0) {
    LOG_DEBUG << "StSvtSimulationMaker::ideal2RealTranslation: illegal wafId = " << *wafId << endl;
    return kStSkip;
  }
  //cout << "pos going in : " << *pos << endl;

  // Get normal and center position of the REAL wafer geom
    StSvtWaferGeometry* waferGeom = (StSvtWaferGeometry*)mSvtGeom->at(index);
    if (! waferGeom) return kStSkip;
    StThreeVectorD wafCent(waferGeom->x(0),waferGeom->x(1),waferGeom->x(2));
    StThreeVectorD norm(waferGeom->n(0),waferGeom->n(1),waferGeom->n(2));
    
    // Move helix of track from IDEAL geom to find where it hit REAL wafer geom
    
    StPhysicalHelixD tHelix( *mtm, *pos, mBField, charge);
    double s = tHelix.pathLength(wafCent, norm);
    x = tHelix.at(s); 
    pos->setX(x.x());
    pos->setY(x.y());
    pos->setZ(x.z());
    globalCor.setPosition(*pos);  
    
    
    if( mCoordTransform->IsOnWaferZ(*pos,*wafId) && mCoordTransform->IsOnWaferR(*pos,*wafId)){
      //cout << " Coming out " << *pos << endl;
      x = tHelix.momentumAt(s,mBField);
      mtm->setX(x.x());
      mtm->setY(x.y());
      mtm->setZ(x.z());
      return kStOk;
    }
    

    // If the hit is now on a different wafer look for it by looping
    // over one ladder before and one after

    int ladder = *wafId%100;
    int layer = *wafId/1000;
    int barrel = (layer-1)/2 +1;
    int iladder;
    for( iladder = ladder-1; iladder <= ladder+1; iladder++){
      if( iladder==0) continue;
      if( iladder > mConfig->getNumberOfLadders(barrel)) break;
      for( int iwaf = 1;  iwaf <= mConfig->getNumberOfWafers(barrel); iwaf++){
	//wafId = 1000*layer+ 100*iwaf + iladder;
	index = mSvtGeom->getWaferIndex(barrel, iladder, iwaf);
      
	// Get normal and center position of the REAL wafer geom
	waferGeom = (StSvtWaferGeometry*)mSvtGeom->at(index);
	if (! waferGeom) continue;
	wafCent.setX(waferGeom->x(0));
	wafCent.setY(waferGeom->x(1));
	wafCent.setZ(waferGeom->x(2));
	norm.setX(waferGeom->n(0));
	norm.setY(waferGeom->n(1));
	norm.setZ(waferGeom->n(2));
	
	// Move helix of track from IDEAL geom to find where it hit REAL wafer geom
	s = tHelix.pathLength(wafCent, norm);
	x = tHelix.at(s); 
	pos->setX(x.x());
	pos->setY(x.y());
	pos->setZ(x.z());
	globalCor.setPosition(*pos);  
	*wafId = 1000*mConfig->getLayer(index) + 100*iwaf + iladder;
	if( mCoordTransform->IsOnWaferZ(*pos,*wafId) && mCoordTransform->IsOnWaferR(*pos,*wafId)){
	  //cout << " Coming out " << *pos << endl;
	  x = tHelix.momentumAt(s,mBField);
	  mtm->setX(x.x());
	  mtm->setY(x.y());
	  mtm->setZ(x.z());
	  return kStOk;
	}
      }
    }

    if( ladder ==1){
      iladder = mConfig->getNumberOfLadders(barrel);
      for( int iwaf = 1;  iwaf <= mConfig->getNumberOfWafers(barrel); iwaf++){
	//wafId = 1000*layer+ 100*iwaf + iladder;
	index = mSvtGeom->getWaferIndex(barrel, iladder, iwaf);
      
	// Get normal and center position of the REAL wafer geom
	waferGeom = (StSvtWaferGeometry*)mSvtGeom->at(index);
	if (! waferGeom) continue;
	wafCent.setX(waferGeom->x(0));
	wafCent.setY(waferGeom->x(1));
	wafCent.setZ(waferGeom->x(2));
	norm.setX(waferGeom->n(0));
	norm.setY(waferGeom->n(1));
	norm.setZ(waferGeom->n(2));	

	// Move helix of track from IDEAL geom to find where it hit REAL wafer geom
	s = tHelix.pathLength(wafCent, norm);
	x = tHelix.at(s); 
	pos->setX(x.x());
	pos->setY(x.y());
	pos->setZ(x.z());
	globalCor.setPosition(*pos);  
	*wafId = 1000*mConfig->getLayer(index) + 100*iwaf + iladder;
	if( mCoordTransform->IsOnWaferZ(*pos,*wafId) && 
	    mCoordTransform->IsOnWaferR(*pos,*wafId)){
	  //cout << " Coming out " << *pos << endl;
	  x = tHelix.momentumAt(s,mBField);
	  mtm->setX(x.x());
	  mtm->setY(x.y());
	  mtm->setZ(x.z());
	  return kStOk;
	}
      }  
    }
    //cout << " Coming out " << *pos << endl;
    return kStSkip;
}
//____________________________________________________________________________
Int_t StSvtSimulationMaker::Make()
{
  LOG_DEBUG << "In StSvtSimulationMaker::Make()" << endm;

  int volId ,barrel, layer, ladder, wafer, hybrid;
  // Int_t NumOfHitsPerHyb=0;
  StThreeVector<double> VecG(0,0,0);
  StThreeVector<double> VecL(0,0,0);
  StThreeVector<double> mtm(0,0,0);


  StSvtHybridPixelsD *svtSimDataPixels;
 
  //########## initiating data structures ##########################
  resetPixelData();
  setGeantData(); //if this removed geant data need to be dealocated in Clear() 
  
  StSvtWaferCoordinate waferCoord (0,0,0,0,0,0);
  StSvtLocalCoordinate localCoord (0,0,0), localCoordb(0,0,0);
  StGlobalCoordinate globalCor(0,0,0);

  St_svtHybridDriftVelocityC *driftVel = St_svtHybridDriftVelocityC::instance();
  assert(driftVel);

 int tmpBadCount=0;
  
  StMcEvent* mcEvent = 0;
  mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
  StMcSvtHitCollection *mcCol  = 0; 
  //  StMcSvtHitCollection *mcCol1 = 0; 
  if(mcEvent)
    {
      mcCol  = mcEvent->svtHitCollection();
      LOG_INFO <<"mNumOfGeantHits = "<<mcCol->numberOfHits()<<endm;
      for (unsigned int iBarrel=0; iBarrel< mcCol->numberOfBarrels(); iBarrel++) {
	for (unsigned int iLadder=0; iLadder<mcCol->barrel(iBarrel)->numberOfLadders(); iLadder++) {
	  for (unsigned int iWafer = 0; iWafer < mcCol->barrel(iBarrel)->ladder(iLadder)->numberOfWafers(); iWafer++) {
	    for (StMcSvtHitIterator iter = mcCol->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits().begin();
		 iter != mcCol->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits().end();
		 iter++) {
	      StMcSvtHit   *mchit = dynamic_cast<StMcSvtHit   *> (*iter);      
	      
	      VecG.setX( mchit->position().x());
	      VecG.setY( mchit->position().y());
	      VecG.setZ( mchit->position().z());
	      mtm.setX(mchit->localMomentum().x());
	      mtm.setY(mchit->localMomentum().y());
	      mtm.setZ(mchit->localMomentum().z());
	      volId = 1000*mchit->layer()+100*mchit->wafer()+mchit->ladder();
	      
	      // Translate hit position from IDEAL geom coords to REAL geom coords in
	      // global coordinates
	      //  cout << "Going in : " << VecG << endl;
	      
	      
	      Int_t iok = ideal2RealTranslation(&VecG, &mtm, (double)mchit->parentTrack()->particleDefinition()->charge(), &volId);
	      if (iok != kStOK) continue;
	      mchit->setPosition(VecG);
	      mchit->setLocalMomentum(mtm);
	      
	      globalCor.setPosition(VecG);	     
	      mCoordTransform->operator()(globalCor,localCoord,volId);
	      	      
	      
	      if (! driftVel->p(localCoord.barrel(),localCoord.ladder(),localCoord.wafer(),localCoord.hybrid())) continue;
	     
	      // need to djust drift time because the drift velocity assumptions are different in the conversion routines
	      // to and from the wafer coordinate.
	      mCoordTransform->operator()(localCoord,waferCoord);
	      mCoordTransform->operator()(waferCoord,localCoordb);
	      double tnew, told;
	      int mtry=0;
	      double scale = 1;
	      double diffx = localCoordb.position().x()- localCoord.position().x();
	      while( fabs(diffx) > 0.002){
		
		told = waferCoord.timebucket();
		tnew =waferCoord.timebucket()+ scale*waferCoord.timebucket()*(localCoordb.position().x()- localCoord.position().x())/localCoord.position().x();
		if( told-tnew > 4) tnew = told-0.5;
		else if( told-tnew < -4) tnew = told+0.5;
		waferCoord.setTimeBucket(tnew);
		mCoordTransform->operator()(waferCoord,localCoordb);
		if( fabs(diffx) < fabs(localCoordb.position().x()- localCoord.position().x())){
		  waferCoord.setTimeBucket(told);
		  scale *= 0.5;
		}
		else{
		  diffx = localCoordb.position().x()- localCoord.position().x();
		}
		//	cout << "try "<< mtry << " " <<  scale << " " << diffx << " " << localCoord.position() << " " << localCoordb.position() << " " << told << " " << tnew << endl;
		mtry++;
		if( mtry > 10000) diffx = 0;

	      }
	      // cout << "Final best guess " << "try "<< mtry << " " <<  diffx << " " << scale << " " << told << " " << tnew << endl;

	      VecL.setX(localCoord.position().x());
	      VecL.setY(localCoord.position().y());
	      VecL.setZ(localCoord.position().z());	      
	      
	      //mCoordTransform->operator()(localCoord,waferCoord);
	      double anode,time;
	      barrel = waferCoord.barrel();
	      layer = waferCoord.layer(); ladder = waferCoord.ladder();
	      wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid();     
	      time = waferCoord.timebucket();
	      double driftTime=time - mT0->getT0();
	      anode = waferCoord.anode();
	      
	      if(driftTime < 0.0 || time > 128.0 || anode < 0.0 || anode > 240.0)
		{ tmpBadCount++; continue;}
#if 0     
	      
	      
	      //########### get barrel and ladder numbers correctly #################
	      
	      static const int barrels[]={3,1,1,2,2};
	      barrel = 3;
	      if (layer<=4) barrel = barrels[layer];
	      
	      if ( !strncmp(mConfig->getConfiguration(), "Y1L", strlen("Y1L")) ) {
		if ((wafer == 1) || (wafer == 2) || (wafer == 3))
		  ladder = 2;
	      }	   
	      
	      //if(Debug()) mNTuple->Fill(time,anode,trk_st[j].x[0],trk_st[j].x[1],trk_st[j].x[2],0 ,0,0,0,0,0);
#endif      
	      if( 1000*layer+100*wafer+ladder != volId){
		LOG_INFO << "trouble - skipping hit" << volId <<"\t"<< mchit->position().x() << "\t" 
			 << mchit->position().y() <<"\t and our calc"<<"\t" << layer << " " 
			 << wafer << "\t" << ladder <<endm;
		continue;
	      }
	      
	      int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);
	      if( index < 0) continue; 
	      svtSimDataPixels  = (StSvtHybridPixelsD*)mSvtSimPixelColl->at(index);
	      if (! mSvtAngles) mSvtAngles =  new StSvtAngles();
	      mSvtAngles->calcAngles(mSvtGeom,mtm.x(),mtm.y(),mtm.z(),layer,ladder,wafer);
	      double theta = mSvtAngles->getTheta();
	      double phi = mSvtAngles->getPhi();
	      //seting drift speed for simulation
	      double vd=-1;
#if 0
	      if (mDriftSpeedColl && mDriftSpeedColl->at(index)){
		vd = ((StSvtHybridDriftVelocity*)mDriftSpeedColl->at(index))->getV3(1);
		if (vd<=0) vd=mDefaultDriftVelocity;
		else vd=vd*1e-5;
	      }
	      //cout<<"drift velocity used: = "<<vd<<" (default would be "<<mDefaultDriftVelocity<<")"<<endl;
#else
	      vd = driftVel->DriftVelocity(barrel,ladder,wafer,hybrid)*1e-5;
#endif
	      if (vd <= 0) continue;
	      // double PasaShift=0.1/vd*25.; //100 um shift from pasa
	      double PasaShift=0.22/vd*25.; //220 um shift from pasa
	      
	      mSvtSimulation->setDriftVelocity(vd);
	      double  energy = mchit->dE()*1e9; 
	      mSvtSimulation->doCloud(driftTime,energy,theta,phi,mchit->parentTrack()->key());
	      mSvtSimulation->fillBuffer(anode,time-PasaShift,svtSimDataPixels);
	      
	      if (Debug()) {
		FillGeantHit(barrel,ladder,wafer,hybrid,&waferCoord,&VecG,&VecL,mSvtSimulation->getPeak(),mchit->parentTrack()->key());
	      }
	      
	      
	    }
	  }
	}
      }
    }
  else
    LOG_INFO << "There's a problem no MC event" << endm;
  
  int nSimDataPixels = mSvtSimPixelColl->size();
  for (int index=0;index<nSimDataPixels;index++) {
    svtSimDataPixels  = (StSvtHybridPixelsD*)mSvtSimPixelColl->at(index);
    if (!svtSimDataPixels) continue;
    svtSimDataPixels->updateTruth();
  }
  LOG_DEBUG <<"bad hits:"<<tmpBadCount<<endm;
  LOG_DEBUG << "In StSvtSimulationMaker::Make()...END" << endm;
  return kStOK;
}


//____________________________________________________________________________
void StSvtSimulationMaker::FillGeantHit(int barrel, int ladder, int wafer, int hybrid,
                    StSvtWaferCoordinate* waferCoord,StThreeVector<double>* VecG,
		    StThreeVector<double>* VecL, double peak,int idtrk)
{ 
  StSvtGeantHits* geantHit;
  
  int index = mSvtGeantHitColl->getHybridIndex(barrel,ladder,wafer,hybrid);
  if (index < 0) return;
  
  geantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
  if(!geantHit) { //actually, it should be empty
    geantHit = new StSvtGeantHits(barrel,ladder,wafer,hybrid);
    mSvtGeantHitColl->put_at(geantHit,index);
  }
  
  geantHit->setGeantHit(counter[index],waferCoord);
  geantHit->setLocalCoord(counter[index],VecL);
  geantHit->setGlobalCoord(counter[index],VecG);
  geantHit->setPeak(counter[index],peak);
  geantHit->setTrackId(counter[index],idtrk);
  ++counter[index];
  geantHit->setNumOfHits(counter[index]);
}


//_____________________________________________________________________________
void StSvtSimulationMaker::Clear(const char*)
{
 
  LOG_DEBUG << "In StSvtSimulationMaker::Clear" << endm;
  
  
  //all will be deleted by StMaker::Clear()
  mSvtGeantHitColl = NULL;

  if (counter) delete counter; counter=NULL;

  StMaker::Clear();
  
  LOG_DEBUG << "In StSvtSimulationMaker::Clear..END" << endm; 
}


//____________________________________________________________________________

Int_t StSvtSimulationMaker::Finish()
{
  LOG_DEBUG << "In StSvtSimulationMaker::Finish()"<< endm;

  /*
  if (Debug()&&(mNtFile)) {
    mNtFile->Write(); 
    mNtFile->Close(); 
  }
  */
  //mSvtSimulation->closeFiles(); 
  //mElectronCloud->closeFiles();

  LOG_DEBUG << "In StSvtSimulationMaker::Finish() ...END"<< endm;
  return kStOK;
}

//____________________________________________________________________________



