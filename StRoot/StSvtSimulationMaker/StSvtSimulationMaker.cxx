 /***************************************************************************
 *
 * $Id: StSvtSimulationMaker.cxx,v 1.19 2004/01/27 02:45:42 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Slow Simulator Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulationMaker.cxx,v $
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


#include <string.h>
#include <math.h>


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
#include "StSvtDaqMaker/StSvtHybridDaqPed.hh"
#include "StSvtCalibMaker/StSvtPedMaker.h"
#include "StSvtAngles.hh"
#include "StSvtElectronCloud.hh"
#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"
#include "StSvtGeantHits.hh"
#include "StSvtSimulationMaker.h"



#include "tables/St_g2t_svt_hit_Table.h"
//#include "StSvtConversionTable.h"


ClassImp(StSvtSimulationMaker)

int* counter = 0;



//___________________________________________________________________________
StSvtSimulationMaker::StSvtSimulationMaker(const char *name):StMaker(name)
{ 
   if (Debug()) gMessMgr->Info() << "StSvtSimulationMaker::constructor"<<endm;
 
  mPedOffset = 20;     //value taken from StSeqAdjMakeru- hardwired value, but could possibly change run to run
 
  //electron cloud settings - these can be tuned;
  mDiffusionConst=0.0035;   // [mm**2/micro seconds]
  mLifeTime=1000000.0;     // [us]
  mTrapConst=0;            // [us]

  //options - can be set by setOptions
  mExpOption = "both";     // both, coulomb, diffusion
  mWrite = 0;              // Debug option
  mFineDiv = 0;            // Debug option
  mSigOption = 0;          // Debug option 
  
  
  //initial cleanup
  mNumOfHybrids = 0; 

  mSvtGeantHitColl = NULL;
  mSvtSimPixelColl = NULL;
  mSvt8bitPixelColl = NULL; //final data 
  mDriftSpeedColl=NULL;
  
  mCoordTransform = NULL;

  //this should be part of the chain
  //if (!GetMaker("StSvtPedMaker")) new StSvtPedMaker("StSvtPedMaker");   //this should be later removed when Pedmaker is standard part of BFC
  //else gMessMgr->Info() << "StSvtPedMaker already exists in the chain"<<endm;

  if (Debug()) gMessMgr->Info() << "StSvtSimulationMaker::constructor...END"<<endm;
 
}

//____________________________________________________________________________
StSvtSimulationMaker::~StSvtSimulationMaker()
{
  if (Debug()) gMessMgr->Info() << "StSvtSimulationMaker::destructor"<<endm;

  if (mSvtAngles) delete mSvtAngles;
  if (mSvtSimulation) delete mSvtSimulation;
  if (mElectronCloud) delete mElectronCloud;
  if (mCoordTransform) delete mCoordTransform;

  if (Debug()) gMessMgr->Info() << "StSvtSimulationMaker::destructor...END"<<endm; 
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setConst(double timBinSize, double anodeSize, int offset)
{
  mTimeBinSize = timBinSize ;
  mAnodeSize = anodeSize;    
  mPedOffset = offset;
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
Int_t StSvtSimulationMaker::setOptions(char* option1, int option2, int option3, int option4)
{
  mExpOption = option1;  
  mWrite = option2;
  mFineDiv = option3;
  mSigOption = option4;

  return kStOK;
}


//____________________________________________________________________________
Int_t StSvtSimulationMaker::Init()
{
    
  if(Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Init() ..."<<endm;
    
  // init objects that do parts of simulation
  mSvtAngles =  new StSvtAngles();
 
  mElectronCloud = new StSvtElectronCloud(mExpOption,mWrite,mFineDiv);
  mElectronCloud->setSiliconProp();
  mElectronCloud->setElectronLifeTime(mLifeTime);
  mElectronCloud->setDiffusionConst(mDiffusionConst);

  mSvtSimulation = new StSvtSimulation();
  mSvtSimulation->setOptions(mSigOption);
  mSvtSimulation->setPointers(mElectronCloud, mSvtAngles);
  mSvtSimulation->setTrappingConst(mTrapConst); 

  mCoordTransform=new StSvtCoordinateTransform();  
   
  if(Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Init() -End"<<endm;

  return  StMaker::Init();
}

//__________________________________________________________________________
Int_t StSvtSimulationMaker::InitRun(int runumber)
{ //when the run changes
  if(Debug()) gMessMgr->Info() <<"StSvtSimulationMaker::InitRun()"<<endm;
 
  //((StSvtPedMaker*) GetMaker("StSvtPedMaker"))->ReadFromFile("data/ped/pedestal_4065001.root");
  //((StSvtPedMaker*) GetMaker("StSvtPedMaker"))->ReadRMSFromFile("data/ped/rms2_pedestal_4065001.root");
  
  //read from database
  getConfig();
  getSvtGeometry();
  getSvtDriftSpeeds();
  getSvtT0();

  setSvtPixelData();
  //Set up coordinate transformation 
  mCoordTransform->setParamPointers(mSvtGeom, mConfig,mDriftSpeedColl,mT0);
  
  //mTimeBinSize = 1.E6/mSvtSrsPar->fsca;  // Micro Secs
  //mAnodeSize = mSvtSrsPar->pitch*10;  // mm
  //mDriftVelocity = 1.E-5*mSvtSrsPar->vd;  // mm/MicroSec (?)
  // *****values hard wired for the time being - should be in database
  mTimeBinSize = 0.04;  // Micro Secs - Petr: this is quite accurate according to Dave
  mAnodeSize = mSvtGeom->getAnodePitch()*10;  // mm
  mDefaultDriftVelocity = 1.E-5*675000;  // used only if there is no database
  
  //set default drift speeds - if drift speed data exist it will be overriden later
  mSvtSimulation->setAnodeTimeBinSizes(mTimeBinSize , mAnodeSize);
  mSvtSimulation->setDriftVelocity(mDefaultDriftVelocity);
 
  //set size of hits-otherwise default is false,8
  //mSvtSimulation->setPasaSigAttributes(kFALSE,8)

  
  //if(Debug()) CreateHistograms();
  cout<<"StSvtSimulationMaker::InitRun info:"<<endl;
  cout<<"  Anode size="<<mAnodeSize<<" ,time bin size="<<mTimeBinSize<<endl;
  cout<<"  pedestal offset="<<mPedOffset<<endl;
  cout<<"  default drift velocity="<<mDefaultDriftVelocity<<endl;
  cout<<"  T0(from database)= "<<mT0->getT0(1)<<endl;
  

  if(Debug()) gMessMgr->Info()<<"StSvtSimulationMaker::InitRun()-END"<<endm;
 
  return StMaker::InitRun(runumber);
}




//____________________________________________________________________________

void  StSvtSimulationMaker::resetPixelData(){
  //this resets mSvtSimPixelColl and mSvt8bitPixelColl
 
   StSvtHybridPixelsD* tmpPixels;
   StSvtHybridPixelsC* tmp8bitPixels;

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


//____________________________________________________________________________
void  StSvtSimulationMaker::setSvtPixelData()
{ //add pixeldata to chain->.data
  if (GetDataSet("StSvtPixelData")) cout<<"Error: Found StSvtSimPIxels in the chain - should have been deleted"<<endl;
     
  St_ObjectSet *set = new St_ObjectSet("StSvtPixelData");
  AddConst(set); 
  mSvtSimPixelColl = new /*StSvtHybridCollection*/StSvtData(mConfig->getConfiguration());
  set->SetObject((TObject*)mSvtSimPixelColl);
  
  set = new St_ObjectSet("StSvt8bitPixelData");
  AddConst(set); 
  mSvt8bitPixelColl = new /*StSvtHybridCollection*/StSvtData(mConfig->getConfiguration());
  set->SetObject((TObject*)mSvt8bitPixelColl);

  mNumOfHybrids = mSvtSimPixelColl->getTotalNumberOfHybrids(); 
}


//__________________________________________________________________________________________________
void  StSvtSimulationMaker::setGeantData()
{
  St_ObjectSet* set=(St_ObjectSet*)GetDataSet("StSvtGeantHits");

  if (set) {
    cout<<"Found StSvtGeantHits in the chain- replacing"<<endl;
    set->SetObject(0);
  } 
  else{
    set =  new St_ObjectSet("StSvtGeantHits");
    AddData(set);
  }

  if (mSvtGeantHitColl) cout<<"!!!!!!m SvtGeantHitColl already exists in SvtSimulationMaker.cxx:setEval"<<endl;
  else{
    mSvtGeantHitColl = new /*StSvtHybridCollection*/StSvtData(mConfig->getConfiguration());
    set->SetObject((TObject*)mSvtGeantHitColl);
  }

//+++++++++++++++++
  if(!counter)
    counter = new int[mNumOfHybrids];

}

//____________________________________________________________________________
void StSvtSimulationMaker::CreateHistograms()
{ 
  //mNtFile = new TFile("spacepoints.root","RECREATE","SpacePoints");
  
  //mNTuple = new TNtuple("SpacePoints","SpacePoints","xl:yl:x:y:z:peak:unShoot:sumAdc:widthInTime:widthInAnode:shiftInTime");

 
}

//__________________________________________________________________________________________________
Int_t  StSvtSimulationMaker::getSvtGeometry()
{
    
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtGeometry");
  assert(dataSet);

  mSvtGeom = (StSvtGeometry*)dataSet->GetObject();
  assert(mSvtGeom);
  
 

  //+++++++++++++ 
  //why it's not local? and why here - gets open for each run again
  //outGeantSvtGeom.open("geantSvtGeom.dat",ios::out);
  //outDbSvtGeom.open("dbSvtGeom.dat",ios::out);
  //outSvtTrans.open("transSvtGeom.dat",ios::out);
  return kStOk;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::getSvtDriftSpeeds()
{
  mDriftSpeedColl =NULL;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtDriftVelocity");
  if (!dataSet){
    cout<<"Warning: no SVT drift velocity data available - using default drift speed:"<<mDefaultDriftVelocity<<endl;
    return kStWarn;
  }

  mDriftSpeedColl = (StSvtHybridCollection*)dataSet->GetObject();
  if (! mDriftSpeedColl) cout<<"Warning: SVT drift velocity data empty - using default drift speed:"<<mDefaultDriftVelocity<<endl;
    
  return kStOk;
}


//____________________________________________________________________________
Int_t StSvtSimulationMaker::getSvtT0()
{
  mT0=NULL;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtT0");
  if (!dataSet){
    cout<<"Warning: no SVT T0 data available -using defalt T0 = 0"<<endl;
    return kStWarn;
  }
  
  mT0 = (StSvtT0*)dataSet->GetObject();
  if (! mT0) cout<<"Warning: SVT T0 data empty - using default T0 = 0"<<endl;
  
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
      gMessMgr->Warning() << " No SvtConfig  data set" << endm;
      dataSet = new St_ObjectSet("StSvtConfig");                                                               
      AddConst(dataSet);
      mConfig=NULL;
    }
  
  mConfig=((StSvtConfig*)(dataSet->GetObject()));
 
  if (!mConfig) {
    gMessMgr->Warning() << "SvtConfig data set is empty- seting default full configuration" << endm;
    mConfig=new StSvtConfig();
    mConfig->setConfiguration("FULL");
    dataSet->SetObject(mConfig);
  } 

  return kStOk;
}


//____________________________________________________________________________
Int_t StSvtSimulationMaker::Make()
{
  if (Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Make()" << endm;
   
  int volId ,barrel, layer, ladder, wafer, hybrid;
  double px,py,pz;
  Int_t NumOfHitsPerHyb=0;
  StThreeVector<double> VecG(0,0,0);
  StThreeVector<double> VecL(0,0,0);

  StSvtHybridPixelsD *svtSimDataPixels;
 
  //########## initiating data structures ##########################
  resetPixelData();
  setGeantData(); //if this removed geant data need to be dealocated in Clear() 
  
  StSvtWaferCoordinate waferCoord (0,0,0,0,0,0);
  StSvtLocalCoordinate localCoord (0,0,0);
  StGlobalCoordinate globalCor(0,0,0);

  //
  //################  get geant hit table ##########################
  // 
  
  St_DataSet *g2t_svt_hit =  GetDataSet("g2t_svt_hit");
  St_DataSetIter g2t_svt_hit_it(g2t_svt_hit);
  St_g2t_svt_hit *g2t_SvtHit = (St_g2t_svt_hit *) g2t_svt_hit_it.Find("g2t_svt_hit");

  g2t_svt_hit_st *trk_st=0;
  if( !g2t_SvtHit) {
    gMessMgr->Warning() << "No SVT hits" << endm;
    NumOfHitsPerHyb = 0;
  }
  else{  
  trk_st = g2t_SvtHit->GetTable();    
  NumOfHitsPerHyb = g2t_SvtHit->GetNRows();
  }

  //
  //################  Loop over geant hits ##########################
  //
 
  cout<<"mNumOfGeantHits = "<<NumOfHitsPerHyb<<endl;
  int tmpBadCount=0;
  if (NumOfHitsPerHyb>0) 
   for (int j=0;j<NumOfHitsPerHyb ;j++)
    {
      double anode,time;
      volId = trk_st[j].volume_id;
      //cout <<"genat hit #"<<j<<" volumeID="<< volId << " x=" << trk_st[j].x[0] << " y=" << trk_st[j].x[1] << " z=" <<  trk_st[j].x[2]<<endl;
      //outGeantSvtGeom<< volId <<endl;
      if( volId > 7000) continue; // SSD hit
      /*
        if (int(volId/1000) == 3)
        volId = volId - 3000 + 4000;
        else if (int(volId/1000) == 4)
        volId = volId - 4000 + 3000;
      */

   
      VecG.setX( trk_st[j].x[0]);VecG.setY( trk_st[j].x[1]);VecG.setZ( trk_st[j].x[2]);
      px = trk_st[j].p[0];  py = trk_st[j].p[1];  pz = trk_st[j].p[2];
      //double energy = 96000.; 
      double  energy = trk_st[j].de*1e9; 
      globalCor.setPosition(VecG);

      mCoordTransform->operator()(globalCor,waferCoord);
      

      layer = waferCoord.layer(); ladder = waferCoord.ladder();
      wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid();     
      time = waferCoord.timebucket();
      anode = waferCoord.anode();
      //cout<<"time pos of hit(according to CoordTransform):"<<time<<" ,anodepos:"<<anode<<endl;
    
      if(time < 0.0 || time > 128.0 || anode < 0.0 || anode > 240.0)
	{ tmpBadCount++; continue;}
      
      mCoordTransform->operator()(globalCor,localCoord);
      VecL.setX(localCoord.position().x());
      VecL.setY(localCoord.position().y());
      VecL.setZ(localCoord.position().z());
      
      
      //########### get barrel and ladder numbers correctly #################
      
      if(layer == 1 || layer == 2)
        barrel = 1;
      else if(layer == 3 || layer == 4)
        barrel = 2;
      else
        barrel = 3;
      if ( !strncmp(mConfig->getConfiguration(), "Y1L", strlen("Y1L")) ) {
        if ((wafer == 1) || (wafer == 2) || (wafer == 3))
          ladder = 2;
      }	   
        
      //if(Debug()) mNTuple->Fill(time,anode,trk_st[j].x[0],trk_st[j].x[1],trk_st[j].x[2],0 ,0,0,0,0,0);
      
      if( 1000*layer+100*wafer+ladder != volId){
        cout << "trouble - skipping hit" << volId <<"\t"<< trk_st[j].x[0] << "\t" 
             << trk_st[j].x[1] <<"\t and our calc"<<"\t" << layer << " " 
             << wafer << "\t" << ladder << "\t" << j <<endl;
        continue;
      }
      
      int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);
      if( index < 0) continue; 
      svtSimDataPixels  = (StSvtHybridPixelsD*)mSvtSimPixelColl->at(index);
	     
      mSvtAngles->calcAngles(mSvtGeom,px,py,pz,layer,ladder,wafer);
      double theta = mSvtAngles->getTheta();
      double phi = mSvtAngles->getPhi();

      //seting drift speed for simulation
      double vd=0;
      if (mDriftSpeedColl){
	vd = ((StSvtHybridDriftVelocity*)mDriftSpeedColl->at(index))->getV3(1);
	if (vd<=0) vd=mDefaultDriftVelocity;
	else vd=vd*1e-5;
      }
      //cout<<"drift velocity used: = "<<vd<<" (default would be "<<mDefaultDriftVelocity<<")"<<endl;
     
      mSvtSimulation->setDriftVelocity(vd);
      mSvtSimulation->doCloud(time,energy,theta,phi);
      mSvtSimulation->fillBuffer(anode,time,svtSimDataPixels);
           
      //FillGeantHit(barrel,ladder,wafer,hybrid,waferCoord,VecG,VecL,mSvtSimulation->getPeak());
      
    }
  
  
  /*
   if (mDoBigOutput){
    cout<<"!!!!!!!!!!!!!Watch out:making big output of histograms"<<endl;
    MakeRawDataHistos();
    MakeGeantHitsHistos();
    oldDir->cd();
  }
  */

  if (Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Make()...END" << endm;
  return kStOK;
}


//____________________________________________________________________________
void StSvtSimulationMaker::FillGeantHit(int barrel, int ladder, int wafer, int hybrid,
                    StSvtWaferCoordinate& waferCoord,StThreeVector<double>& VecG,
                    StThreeVector<double>& VecL, double peak)
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
  ++counter[index];
  geantHit->setNumOfHits(counter[index]);
}

//____________________________________________________________________________
void StSvtSimulationMaker::MakePixelHistos()
{
  if (Debug()) gMessMgr->Message() << "Making histograms from PixelData" << endm;
  TH1D *mult=new TH1D("ADC distribution","ADC distribution",500,-10.0,10.0);  

  for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {    
        for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {      
          for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {	
            for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){
              
              int index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
              if( index < 0) continue;

              StSvtHybridPixelsD* tmpPixels = (StSvtHybridPixelsD *)mSvtSimPixelColl->at(index);
              if(!tmpPixels) continue;

              char name[50];
              char title[50];
              sprintf(name,"hyb: %.3i Backgr",index);
              sprintf(title,"Background for hybrid %i",index);

              TH2D *hist = new TH2D(name,title,240,0.0,240.0,128,0.0,128.0);
              double adc;
              for(int tim = 0; tim < 128; tim++)
                for(int an = 0; an < 240; an++)
                  {
                    adc = tmpPixels->getPixelContent(an + 1,tim);
                    //adc-= mPedOffset;
                    //if(adc != 0.) cout <<adc<<endl;
                    //cout<<"adc"<<adc<<endl;
                    hist->Fill(an,tim,adc);
                    mult->Fill(adc);
                  }
              hist->SetDrawOption("ncolz");
              hist->Write();
              delete hist;              
            }
          }
        }
  }
  mult->Write();
  delete mult;
}

//_____________________________________________________________________________
void StSvtSimulationMaker::MakeGeantHitsHistos()
{
  if (Debug()) gMessMgr->Message() << "Making histograms from GeantData" << endm;
  //TH1D *mult=new TH1D("ADC distribution","ADC distribution",500,-10.0,10.0);  

  for(int Barrel = 1;Barrel <= mSvtGeantHitColl->getNumberOfBarrels();Barrel++) {    
        for (int Ladder = 1;Ladder <=mSvtGeantHitColl->getNumberOfLadders(Barrel);Ladder++) {      
          for (int Wafer = 1;Wafer <= mSvtGeantHitColl->getNumberOfWafers(Barrel);Wafer++) {	
            for( int Hybrid = 1;Hybrid <= mSvtGeantHitColl->getNumberOfHybrids();Hybrid++){

              int index=mSvtGeantHitColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
              StSvtGeantHits* geantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
              if( !geantHit) continue;
              if (geantHit->numberOfHits()==0) continue;

              char name[50];
              char title[50];
              sprintf(name,"hyb: %.3i Geant",index);
              sprintf(title,"Geant hits for hybrid %i",index);

              TH2D *hist = new TH2D(name,title,240,0.0,240.0,128,0.0,128.0);

              for( int gHit = 0; gHit < geantHit->numberOfHits(); gHit++){ 
                float  time =  geantHit->waferCoordinate()[gHit].timebucket();
                float  anode = geantHit->waferCoordinate()[gHit].anode();
                hist->Fill(anode,time);
              }
              hist->SetDrawOption("ncolz");
              hist->Write();
              delete hist;              
            }
          }
        }
  }           
}

//_____________________________________________________________________________
void StSvtSimulationMaker::Clear(const char*)
{
 
  if (Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Clear" << endm;
  
  
  //all will be deleted by StMaker::Clear()
  mSvtGeantHitColl = NULL;

  if (counter) delete counter;
  counter=NULL;

  StMaker::Clear();
  
  if (Debug()) gMessMgr->Info() << "In StSvtSimulationMaker::Clear..END" << endm; 
}


//____________________________________________________________________________

Int_t StSvtSimulationMaker::Finish()
{
  if (Debug()) gMessMgr->Info()<< "In StSvtSimulationMaker::Finish()"<< endm;

  /*
  if (Debug()&&(mNtFile)) {
    mNtFile->Write(); 
    mNtFile->Close(); 
  }
  */
  //mSvtSimulation->closeFiles(); 
  //mElectronCloud->closeFiles();

  if (Debug()) gMessMgr->Info()<< "In StSvtSimulationMaker::Finish() ...END"<< endm;
  return kStOK;
}

//____________________________________________________________________________



