/***************************************************************************
 *
 * $Id: StSvtSimulationMaker.cxx,v 1.10 2001/11/06 20:12:06 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Slow Simulator Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulationMaker.cxx,v $
 * Revision 1.10  2001/11/06 20:12:06  caines
 * Add include for new compiler
 *
 * Revision 1.9  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.7  2001/04/12 20:34:54  caines
 * Add check for if no svt hits present
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
#include <iostream.h>
#include <string.h>
#include <fstream.h>
#include <math.h>

#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"
#include "TH1.h"
#include "TH2.h"
#include "TString.h"

#include "TFile.h"
#include "TNtuple.h"

#include "StSequence.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtHybridSimData.hh"
#include "StSvtAngles.hh"
#include "StSvtElectronCloud.hh"
#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"
#include "StSvtGeantHits.hh"
#include "StSvtSimulationMaker.h"

#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_srs_srspar_Table.h"
#include "tables/St_svg_shape_Table.h"

ClassImp(StSvtSimulationMaker)

int* counter = 0;
int svtAttr[5];
float anTime[2];
PasaSignalAttributes* mPasaSignals;

//___________________________________________________________________________
StSvtSimulationMaker::StSvtSimulationMaker(const char *name):StMaker(name)
{
  mConfigString = TString("FULL");  // SVT config
  mNumOfHybrids = 0;          
  mExpOption = "both";     // both, coulomb, diffusion
  mWrite = 0;              // Debug option
  mFineDiv = 0;            // Debug option
  mSigOption = 0;          // Debug option 
  mTimeBinSize = 0.04;  //micro sec
  mAnodeSize =  0.25;  //mm
  mBackGSigma = 2.1;
  mBackGrOption = "noHitBackGr";
  mCheckOneHybrid = 0;
  mCheckAllHybrids = 0;
  mDoDriftTimeShift = 0;
  mDoEmbedding = "noEmbedding";
}

//____________________________________________________________________________
StSvtSimulationMaker::~StSvtSimulationMaker()
{}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setConst(char * backgr,double backgSigma, double timBinSize, double anodeSize)
{
 mBackGrOption = backgr;
 mBackGSigma = backgSigma;
 mTimeBinSize = timBinSize ;
 mAnodeSize = anodeSize;    

 return kStOK;

}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::setEmbedding(char* embedding)
{
  mDoEmbedding = embedding;
  return kStOK;
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
Int_t StSvtSimulationMaker::setDriftTimeShift(int driftTimeShift){

  mDoDriftTimeShift = driftTimeShift;

  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::setHybridToCheck(int oneHybrid, int allHybrids, int numOfHitsperHyb,int pasaSigAttributes){

 mCheckOneHybrid = oneHybrid;
 mCheckAllHybrids = allHybrids;
 mNumOfHitsPerHyb = numOfHitsperHyb;
 mPasaSigAttributes = pasaSigAttributes;

 return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setHitAttributes(double anode, double time,double energy, double theta, double phi)
{

  mAnode = anode;
  mTime = time;
  mEnergy = energy;
  mTheta = theta;
  mPhi = phi;

 return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSimulationMaker::setConfig(const char* config)
{
  gMessMgr->Message() <<"StSvtSim:Setting configuration to "<< config << endm;
  mConfigString = TString(config);

  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSimulationMaker::setConfig(StSvtConfig* config)
{
  mConfigString = TString(config->getConfiguration());
  mConfig = config;
  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::Init()
{
   if(Debug()) gMessMgr->Debug() << "In StSvtSimulationMaker::Init() ..."
				 << GetName() <<endm;
   /*
  St_DataSet *dataSet = GetDataSet("StSvtConfig");

  if (dataSet){
    setConfig((StSvtConfig*)(dataSet->GetObject()));
  }
  else{
    dataSet = new St_ObjectSet("StSvtConfig");
    AddConst(dataSet);  
    mConfig = new StSvtConfig();
    mConfig->setConfiguration(mConfigString.Data());
    dataSet->SetObject((TObject*)mConfig);

  }
   */

  
  setTables();

  mTimeBinSize = 1.E6/mSvtSrsPar->fsca;  // Micro Secs
  mAnodeSize = mSvtSrsPar->pitch*10;  // mm
  mDriftVelocity = 1.E-5*mSvtSrsPar->vd;  // mm/MicroSec (?)


  mSvtAngles =  new StSvtAngles();

  mElectronCloud = new StSvtElectronCloud(mExpOption,mWrite,mFineDiv);
  //mElectronCloud->openFiles();
  mElectronCloud->setSiliconProp();

  mSvtSignal = new StSvtSignal();
  mSvtSignal->setOption(mSigOption);
  mSvtSignal->setParam(mTimeBinSize,mAnodeSize,mDriftVelocity);
  mSvtSignal->pasaRelatedStuff();

  mSvtSimulation = new StSvtSimulation();
  mSvtSimulation->setPasaSigAttributes(mPasaSigAttributes,mNumOfHitsPerHyb);
  //mSvtSimulation->openFiles();
  mSvtSimulation->setOptions(mBackGrOption);
  mSvtSimulation->setPointers(mElectronCloud,mSvtSignal,mSvtAngles);
  mSvtSimulation->setAnodeTime(mTimeBinSize , mAnodeSize);
  
 
   setSvtPixelData();
   setEval();

   mCoordTransform =  new StSvtCoordinateTransform();
   //checkCoordTrans();
 
  if(!strncmp(mBackGrOption,"doHybBackGr",strlen("doHybBackGr")))
    createBackGrData(mBackGSigma); 

  CreateHistograms();
  
  return  StMaker::Init();
}

//____________________________________________________________________________
Int_t  StSvtSimulationMaker::setSvtPixelData()
{

  if ( !strncmp(mDoEmbedding, "doEmbedding", strlen("doEmbedding"))){
  mSimPixelSet = new St_ObjectSet("StSvtSimPixels");
  //cout<<"mSimPixelSet = "<<mSimPixelSet<<endl;
  AddData(mSimPixelSet);  
  SetOutput(mSimPixelSet); //Declare for output
    }

  mSvtSimPixelColl = new StSvtHybridCollection(mConfigString.Data());
  //cout<<" mSvtSimPixelColl = "<<mSvtSimPixelColl<<endl;

  if ( !strncmp(mDoEmbedding, "doEmbedding", strlen("doEmbedding"))){
  mSimPixelSet->SetObject((TObject*)mSvtSimPixelColl);
  assert(mSvtSimPixelColl);
  } else {

    setSvtRawData();
  }

  mNumOfHybrids = mSvtSimPixelColl->getTotalNumberOfHybrids(); 

  if(!counter)
   counter = new int[mNumOfHybrids];

 return kStOK;

}

//____________________________________________________________________________
Int_t  StSvtSimulationMaker::setSvtRawData()
{
  mSimDataSet = new St_ObjectSet("StSvtRawData");
  AddData(mSimDataSet);  

  //mSvtSimDataColl = new StSvtHybridCollection(mConfig);
  mSvtSimDataColl = new StSvtHybridCollection(mConfigString.Data());
  mSimDataSet->SetObject((TObject*)mSvtSimDataColl); 
  assert(mSvtSimDataColl);

  if(!mNumOfHybrids)
   mNumOfHybrids = mSvtSimDataColl->getTotalNumberOfHybrids(); 

  return kStOK;

}

//__________________________________________________________________________________________________
Int_t StSvtSimulationMaker::setTables()
{


  // Create tables
  St_DataSetIter       local(GetInputDB("svt"));

   St_srs_srspar* SvtSrsPar=0;

   SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
  

  if (!SvtSrsPar) { 
    gMessMgr->Error() << "SVT- StSvtSimulationMaker:svt srspar do not exist" << endm;
    return kStErr;
  }

 //get svt geometry table and parameter tables
  mSvtSrsPar = SvtSrsPar->GetTable();

   return kStOK;
}

//__________________________________________________________________________________________________

Int_t  StSvtSimulationMaker::checkCoordTrans()
{
       StThreeVector<double> VecG(0,0,0);
       StThreeVector<double> VecL(0,0,0);
 
      St_svg_shape* SvtShape=0;
      St_svg_geom* SvtGeom=0;
      St_srs_srspar* SvtSrsPar=0;
      St_DataSetIter       local(GetInputDB("svt"));

      SvtShape = (St_svg_shape   *) local("svgpars/shape");
      
      //cout<<"mConfigString : "<<mConfigString<<endl;
      //cout<<"mConfigString.Data() : "<<mConfigString.Data()<<endl;

      if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
	SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
      else
	SvtGeom = (St_svg_geom    *) local("svgpars/geom");
      
      SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
    

      mSvtSrsPar = SvtSrsPar->GetTable();
      mSvtShape  = SvtShape->GetTable();
      mSvtGeom   = SvtGeom->GetTable();

      mTimeBinSize = 1.E6/mSvtSrsPar->fsca;  // Micro Secs
      mAnodeSize = mSvtSrsPar->pitch*10;  // mm
      mDriftVelocity = 1.E-5*mSvtSrsPar->vd;  // mm/MicroSec (?)
     

      mCoordTransform->setParamPointers(mSvtSrsPar, mSvtGeom, mSvtShape, mSvtSimDataColl->getSvtConfig());

      StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
      StSvtLocalCoordinate localCoord;
      StGlobalCoordinate globalCor(0,0,0);

      int layer,ladder ,wafer ,hybrid ;
      float xPos,yPos,zPos ;
      float tim[12*6],an[12*6]; 

      xPos = 6.125;                   //cm
      zPos = mSvtSrsPar->pitch*(5 + 0.5);  //cm
      yPos = mDriftVelocity*mTimeBinSize*(8 + 0.5)/10.0;    //cm
      

      for(int i = 0; i < 12; i++){
        
       yPos = mDriftVelocity*mTimeBinSize*(8 + 0.5)/10.0;    //cm

       for(int j = 0; j < 6; j++){
     
	 if(zPos < 6.0 && yPos < 3.0){

	  VecG.setX(xPos);
	  VecG.setY(yPos);
	  VecG.setZ(zPos);
	  
          globalCor.setPosition(VecG);
          
          mCoordTransform->operator()(globalCor,waferCoord);

	  layer = waferCoord.layer(); ladder = waferCoord.ladder();
	  wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid();     

	  mTime = waferCoord.timebucket(); mAnode = waferCoord.anode();

          tim[j + i*6] = mTime; an[j + i*6] = mAnode;

	  yPos += 20.0*mDriftVelocity*mTimeBinSize/10.0;
           
	 }
      
       }

       zPos += 20.0*mSvtSrsPar->pitch;
      }
      
      for(int i = 0; i < 12; i++){

       for(int j = 0; j < 6; j++){
      
	waferCoord.setLayer(layer);
	waferCoord.setLadder(ladder);
	waferCoord.setWafer(wafer);
        waferCoord.setHybrid(hybrid);
    
        waferCoord.setAnode(an[j + i*6]);
        waferCoord.setTimeBucket(tim[j + i*6]);

        cout<<"\n############ Wafer Coordinates #################################"<<endl;
        cout<<"layer = "<<waferCoord.layer()<<"\tladder = "<<waferCoord.ladder()
            <<"\twafer = "<<waferCoord.wafer()<<"\thybrid = "<<waferCoord.hybrid()<<endl;
        //cout<<"anode = "<<anode*0.025<<"\ttime = "<<time*0.027<<endl;
        cout<<"anode = "<<an[j + i*6]<<"\ttime = "<<tim[j + i*6]<<endl;
        cout<<"##################################################################"<<endl;
        cout<<"\n"<<endl;
        cout<<"$$$$$$$$$$$$  Local Coordinates $$$$$$$$$$$$$$$"<<endl;
        cout<<"\n";

        mCoordTransform->operator()(waferCoord,localCoord);

        cout<<"layer = "<<localCoord.layer()<<"\tladder = "<<localCoord.ladder()
            <<"\twafer = "<<localCoord.wafer()<<"\thybrid = "<<localCoord.hybrid()<<endl;
        cout<<"local x = "<<localCoord.position().x()<<endl;
        cout<<"local y = "<<localCoord.position().y()<<endl;
        cout<<"local z = "<<localCoord.position().z()<<endl;
        cout<<"\n";
        cout<<"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"<<endl;
        cout<<"\n";
        cout<<"@@@@@@@@@@@@  Global Coordinates @@@@@@@@@@@@@@"<<endl;
        cout<<"\n";

        mCoordTransform->operator()(localCoord,globalCor);

        cout<<"global x = "<<globalCor.position().x()<<endl;
        cout<<"global y = "<<globalCor.position().y()<<endl;
        cout<<"global z = "<<globalCor.position().z()<<endl;
        cout<<"\n";
        cout<<"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;
       }
      }

 
      return kStOK;
}
//__________________________________________________________________________________________________

Int_t  StSvtSimulationMaker::setEval()
{
 
  mGeantHitSet =  new St_ObjectSet("StSvtGeantHits");
  AddData(mGeantHitSet);

  //mSvtGeantHitColl = new StSvtHybridCollection(mConfig);
  mSvtGeantHitColl = new StSvtHybridCollection(mConfigString.Data());
  mGeantHitSet->SetObject((TObject*)mSvtGeantHitColl);
  assert(mSvtGeantHitColl);

  if(!counter)
   counter = new int[mNumOfHybrids];

  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::CreateHistograms()
{ 
  mNtFile = new TFile("spacepoints.root","RECREATE","SpacePoints");
  
  mNTuple = new TNtuple("SpacePoints","SpacePoints","xl:yl:x:y:z:peak:unShoot:sumAdc:widthInTime:widthInAnode:shiftInTime");

  char  Index[4];
  char geantHit[25];
  char hitBackGr[25];
  char dBeforeSeqAdj[25];

  char* geantHitTitle; 
  char* hitBackGTitle;
  char* dataBefSeqAdjTitle;

  if(mCheckOneHybrid){

    char timeDist[25];
    char chargeDist[25];

    char* timeDistTitle;
    char* chargeDistTitle;
  
    
    for(Int_t i = 0; i < 7;i++)
      {
	sprintf(timeDist,"mTimeDist");
	sprintf(Index,"%d", i);   
	timeDistTitle = strcat(timeDist,Index);
      
	mTimeDist[i] = new TH1D(timeDistTitle, "counts vs time",128,-0.5,127.5);
      }
 
    sprintf(chargeDist,"mChargeDist");
    sprintf(Index,"%d", 0); 
    chargeDistTitle = strcat(chargeDist,Index);
    mChargeDist = new TH1D(chargeDistTitle,"charge versus anode",240,0.0,240.0);

  int Barrel = 1;
  int layer = 1;
  int Ladder = 2;
  int Wafer = 3;
  int Hybrid = 2;

  svtAttr[0] = Barrel; svtAttr[1] = layer; svtAttr[2] = Ladder; svtAttr[3] = Wafer;svtAttr[4] = Hybrid;

  int index = mSvtSimPixelColl->getHybridIndex(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);

  if( index < 0) return kStErr;
            
  hit_plus_backgr = new TH2D*[1];
  geant_hit = new TH2D*[1];
  mDataBeforeSeqAdj = new TH2D*[1];

  sprintf(hitBackGr,"hit_plus_backgr");
  sprintf(geantHit,"geant_hit");
  sprintf(dBeforeSeqAdj,"dataBeforeSeqAdj");
  sprintf(Index,"%d", index);
      
  hitBackGTitle = strcat(hitBackGr,Index);
  geantHitTitle = strcat(geantHit,Index);
  dataBefSeqAdjTitle = strcat( dBeforeSeqAdj,Index);

  hit_plus_backgr[0] = new TH2D(hitBackGTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
  geant_hit[0] = new TH2D(geantHitTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
  mDataBeforeSeqAdj[0] = new TH2D(dataBefSeqAdjTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
  
 } else {

  hit_plus_backgr = new TH2D*[mNumOfHybrids];
  geant_hit = new TH2D*[mNumOfHybrids];
  mDataBeforeSeqAdj = new TH2D*[mNumOfHybrids];

  for(Int_t hyb = 0; hyb < mNumOfHybrids; hyb++)
    {
      sprintf(hitBackGr,"hit_plus_backgr");
      sprintf(geantHit,"geant_hit");
      sprintf(dBeforeSeqAdj,"dataBeforeSeqAdj");
      sprintf(Index,"%d", hyb);
      
      hitBackGTitle = strcat(hitBackGr,Index);
      geantHitTitle = strcat(geantHit,Index);
      dataBefSeqAdjTitle = strcat( dBeforeSeqAdj,Index);

      hit_plus_backgr[hyb] = new TH2D(hitBackGTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
      geant_hit[hyb] = new TH2D(geantHitTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
      mDataBeforeSeqAdj[hyb] = new TH2D(dataBefSeqAdjTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
    }
  }
  
  return  StMaker::Init();
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::createBackGrData(double backgsigma)
{
   for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {
    
    for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {
      
      for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {
	
	for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){

            int index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

	    //cout << "Barrel = " << Barrel << ", Ladder = " << Ladder << ", Wafer = " << Wafer << ", Hybrid = " << Hybrid << endl;
            //cout<<"index = "<<index<<endl;

            if( index < 0) continue; 

            for(int an = 0; an < 240; an++)
             for(int tim = 0; tim < 128; tim++)
	       {
		 //cout<<"an = "<<an<<" "<<"time = "<<tim<<endl;
		 //cout<<mSvtSimulation->makeGausDev(backgsigma)<<endl;

                double back;
                 back = mSvtSimulation->makeGausDev(backgsigma);
                 mAdcArray[an*128 + tim] = back;
 		 
               }

          mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);

          if(!mSvtSimDataPixels )
            mSvtSimDataPixels  = new StSvtHybridPixels(Barrel, Ladder, Wafer, Hybrid, 128*240, mAdcArray);
            
            mSvtSimPixelColl->at(index) = mSvtSimDataPixels ;
          //(*mSvtSimPixelColl)[index] = mSvtSimDataPixels ;

	    //cout << "mSvtSimDataPixels = " << mSvtSimDataPixels << endl;

	}
      }
    }
  } 

   return kStOK; 

}
//____________________________________________________________________________

Int_t StSvtSimulationMaker::Make()
{
  if(Debug()) gMessMgr->Debug() <<"In StSvtSimulationMaker::Make()"<<endm;
  cout<<"In StSvtSimulationMaker::Make()"<<endl;
  int volId ,barrel, layer, ladder, wafer, hybrid;
  double px,py,pz;
  StThreeVector<double> VecG(0,0,0);
  StThreeVector<double> VecL(0,0,0);

  if (counter){
    for(int i = 0; i < mNumOfHybrids; i++){
      counter[i] = 0;
      if( mSvtGeantHitColl->at(i)){
	delete mSvtGeantHitColl->at(i);
	mSvtGeantHitColl->at(i) = NULL;
      }
    }
  }

  if(mWrite){
   mSvtSimulation->doCloud(127.5,96000,0,0);
  }
  else if(mCheckOneHybrid)
    {
      St_svg_shape* SvtShape=0;
      St_svg_geom* SvtGeom=0;
      St_srs_srspar* SvtSrsPar=0;
      St_DataSetIter       local(GetInputDB("svt"));

      
      SvtShape = (St_svg_shape   *) local("svgpars/shape");
      
      //cout<<"mConfigString : "<<mConfigString<<endl;
      //cout<<"mConfigString.Data() : "<<mConfigString.Data()<<endl;

      if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
	SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
      else
	SvtGeom = (St_svg_geom    *) local("svgpars/geom");
      
      SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
    
      mSvtSrsPar = SvtSrsPar->GetTable();
      mSvtShape  = SvtShape->GetTable();
      mSvtGeom   = SvtGeom->GetTable();

      //cout << mSvtGeom << endl;

      mCoordTransform->setParamPointers(mSvtSrsPar, mSvtGeom, mSvtShape, mSvtSimPixelColl->getSvtConfig());

      int index = mSvtSimPixelColl->getHybridIndex(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);
 
      if( index < 0) return kStErr; 

      mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);

 
      if(!mSvtSimDataPixels) {
	mSvtSimDataPixels = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
	mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
      }
 
      mSvtSimDataPixels->reset();
 
      checkOneHybrid(mSvtSimDataPixels);
 
      //MakeHistograms1();

      setHybrid();

      //MakeHistograms2();
    }

  else
    {
      
      St_svg_shape* SvtShape=0;
      St_svg_geom* SvtGeom=0;
      St_srs_srspar* SvtSrsPar=0;
      St_DataSetIter       local(GetInputDB("svt"));

      
      SvtShape = (St_svg_shape   *) local("svgpars/shape");
      
      //cout<<"mConfigString : "<<mConfigString<<endl;
      //cout<<"mConfigString.Data() : "<<mConfigString.Data()<<endl;

      if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
	SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
      else
	SvtGeom = (St_svg_geom    *) local("svgpars/geom");
      
      SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
    
      

      mSvtSrsPar = SvtSrsPar->GetTable();
      mSvtShape  = SvtShape->GetTable();
      mSvtGeom   = SvtGeom->GetTable();

      //cout << mSvtGeom << endl;

      mCoordTransform->setParamPointers(mSvtSrsPar, mSvtGeom, mSvtShape, mSvtSimPixelColl->getSvtConfig());

      if(mCheckAllHybrids){

        checkAllHybrids();

        MakeHistograms1();

        setHybrid();

        //MakeHistograms2();

      } else {

      // get geant hit table
      St_DataSet *g2t_svt_hit =  GetDataSet("g2t_svt_hit");
      St_DataSetIter g2t_svt_hit_it(g2t_svt_hit);
      mG2tSvtHit = (St_g2t_svt_hit *) g2t_svt_hit_it.Find("g2t_svt_hit");
      if( !mG2tSvtHit) {
	gMessMgr->Warning() << "No SVT hits" << endm;
	return kStOK;
      }

      g2t_svt_hit_st *trk_st = mG2tSvtHit->GetTable();

      if(Debug()) gMessMgr->Debug() << "number of particles = "<<mG2tSvtHit->GetNRows() << endm;
      
 
      StSvtWaferCoordinate waferCoord (0,0,0,0,0,0);
      StSvtLocalCoordinate localCoord (0,0,0);
      StGlobalCoordinate globalCor(0,0,0);
 
      //
      // Loop over geant hits
      //
      mNumOfHitsPerHyb = mG2tSvtHit->GetNRows();
      mSvtSimulation->setPasaSigAttributes(mPasaSigAttributes, mNumOfHitsPerHyb);

      for (int j=0;j<mNumOfHitsPerHyb ;j++)
	{
	  volId = trk_st[j].volume_id;
	  cout << volId << " " << trk_st[j].x[0] << " " << trk_st[j].x[1] << " " << endl;
	  if( volId > 7000) continue; // SSD hit
	  VecG.setX( trk_st[j].x[0]);
	  VecG.setY( trk_st[j].x[1]);
	  VecG.setZ( trk_st[j].x[2]);
	  
	  px = trk_st[j].p[0];  py = trk_st[j].p[1];  pz = trk_st[j].p[2];
	  // mEnergy =  trk_st[j].de*1e9;
	  mEnergy =  96000;

          globalCor.setPosition(VecG);
          
          mCoordTransform->operator()(globalCor,waferCoord);
          
          mCoordTransform->operator()(globalCor,localCoord);

          VecL.setX(localCoord.position().x());
          VecL.setY(localCoord.position().y());
          VecL.setZ(localCoord.position().z());
	  

	  layer = waferCoord.layer(); ladder = waferCoord.ladder();
	  wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid();     

	  mTime = waferCoord.timebucket(); mAnode = waferCoord.anode();
          
          if(mTime < 0.0 || mTime > 128.0 || mAnode < 0.0 || mAnode > 240.0)
            continue;
          
	  // get barrel and ladder numbers correctly
	  if(layer == 1 || layer == 2)
	    barrel = 1;
	  else if(layer == 3 || layer == 4)
	    barrel = 2;
	  else
	    barrel = 3;
	  if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) ) {
	    if ((wafer == 1) || (wafer == 2) || (wafer == 3))
	      ladder = 2;
	  }	   

          
	  if( 1000*layer+100*wafer+ladder != volId){
	    cout << "trouble " << volId << " and our calc" << layer << " " 
		 << wafer << " " << ladder << " " << j <<endl;
	    continue;
	  }
	  int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if( index < 0) continue; 
	  if(index == 50)
	    cout<<"mTime = "<<mTime<<",  mAnode = "<<mAnode<<endl;

 	  mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);


	  if(!mSvtSimDataPixels) {
	    mSvtSimDataPixels = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
	    mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
	  }
	 
	  fillEval(barrel,ladder,wafer,hybrid,waferCoord,VecG,VecL);

	  mSvtAngles->calcAngles(mSvtGeom,px,py,pz,layer,ladder,wafer);
      
	  mTheta = mSvtAngles->getTheta();
	  mPhi = mSvtAngles->getPhi();

	  mSvtSimulation->doCloud(mTime,mEnergy,mTheta,mPhi);
 
	  mSvtSignal->getCloud(mElectronCloud);
       
	  mSvtSignal->setPeakAndUnderShoot();

          if(mPasaSigAttributes)
	    mSvtSimulation->resetPasaSignalAttributes();
 
	  mSvtSimulation->fillBuffer(mAnode,mTime,mBackGSigma,mSvtSimDataPixels);

	  //if(Hybrid == 2)
	  // locPx += 20.0*mDriftVelocity*mTimeBinSize/10.0;
	  //else
	  // locPx -= 20.0*mDriftVelocity*mTimeBinSize/10.0;
                  
	  if(mPasaSigAttributes){

	    mPasaSignals = mSvtSimulation->getPasaSigAttributes();
	    float peakVal = 0,timeCenter = 0,timeWidth =0;
	    float unShootVal = 0;

	    for(int i = 0; i < 7; i++){
	      if(mPasaSignals[j].mPeak[i] > peakVal){
		peakVal = mPasaSignals[j].mPeak[i];
	      }
	      if(mPasaSignals[j].mUnderShoot[i] > unShootVal){
		unShootVal = mPasaSignals[j].mUnderShoot[i];
	      }
	      
	    }
           cout<<"peakVal = "<<peakVal<<endl;
           cout<<"unShootVal = "<<unShootVal<<endl;
           mNTuple->Fill(mTime,mAnode,trk_st[j].x[0],trk_st[j].x[1],trk_st[j].x[2],peakVal ,unShootVal,0,0,0,0);
	  }

	  
	}

      //MakeHistograms1();
      
      if (!strncmp(mDoEmbedding, "noEmbedding", strlen("noEmbedding"))){
        setHybrid();
        //MakeHistograms2();
      }

      cout<<"Done with Simulation"<<endl;
     }
    }

  return kStOK;

}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::setHybrid()

{

 if(mCheckOneHybrid)
    {
     
      int index = mSvtSimPixelColl->getHybridIndex(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);


      if( index < 0) return kStErr;
            
      mSimHybridData = (StSvtHybridSimData *)mSvtSimDataColl->at(index);
      //if(mSimHybridData)
      // delete mSimHybridData;
      if(!mSimHybridData ) {
	mSimHybridData = new StSvtHybridSimData(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);

	mSvtSimDataColl->put_at(mSimHybridData,index);
       }
        
     mSvtSimDataPixels = (StSvtHybridPixels *)mSvtSimPixelColl->at(index);
	  
     if(mSvtSimDataPixels)
     mSimHybridData->setSimHybridData(mSvtSimDataPixels);
             
     mSvtSimDataPixels->reset();
     

    } else {


    for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {
     for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {	
	for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){

	  int index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);


	  if( index < 0) continue;
            
	  mSvtSimDataPixels = (StSvtHybridPixels *)mSvtSimPixelColl->at(index);

	  
	  if(!mSvtSimDataPixels)
	    continue;
          
	  mSimHybridData = (StSvtHybridSimData *)mSvtSimDataColl->at(index);
	  //if(mSimHybridData)
	  //delete mSimHybridData;
	  if(!mSimHybridData ) {
	    mSimHybridData = new StSvtHybridSimData(Barrel, Ladder, Wafer, Hybrid);
	    mSvtSimDataColl->put_at(mSimHybridData,index);
	  }

	  mSimHybridData->setSimHybridData(mSvtSimDataPixels);
             
	  mSvtSimDataPixels->reset();
	}
       }
      }
     }
    }

 return kStOK;
}


//____________________________________________________________________________
Int_t StSvtSimulationMaker::checkOneHybrid(StSvtHybridPixels* SvtSimDataPixels)
{  
   cout<<"StSvtSimulationMaker::doOneHit"<<endl;

   if(mDoDriftTimeShift){
     doDriftTimeShift(SvtSimDataPixels);
   } else {

      StThreeVector<double> VecG(0,0,0);
 
      StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
      StGlobalCoordinate globalCor(0,0,0);

      int layer,ladder ,wafer ,hybrid ;
      float xPos,yPos,zPos ;

      xPos = 6.125;                        //cm
      zPos = mSvtSrsPar->pitch*(3 + 0.5);  //cm
      //zPos = mSvtSrsPar->pitch*0.5;  //cm

      cout<<" xPos = "<<xPos<<endl;
     
      VecG.setX(xPos);

      for(int i = 0; i < 12; i++){
        
       yPos = mDriftVelocity*mTimeBinSize*(8 + 0.5)/10.0;    //cm
 
       VecG.setZ(zPos);

       for(int j = 0; j < 6; j++){
     
	 if(zPos < 6.0 &&  yPos< 3.0){

	  VecG.setY(yPos);
	  
          globalCor.setPosition(VecG);
          
          mCoordTransform->operator()(globalCor,waferCoord);

	  layer = waferCoord.layer(); ladder = waferCoord.ladder();
	  wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid(); 

          cout<<"layer = "<<layer<<"\tladder = "<<ladder
            <<"\twafer = "<<wafer<<"\thybrid = "<<hybrid<<endl;    

	  anTime[0] = waferCoord.anode(); anTime[1] = waferCoord.timebucket();

          cout<<" zPos = "<<zPos<<",  mAnode = "<<anTime[0]<<",  yPos = "<<yPos<<", mTime = "<<anTime[1]<<endl;

          fillEval(svtAttr,anTime);

	  mSvtSimulation->doCloud(anTime[1],mEnergy,mTheta,mPhi);
 
	  mSvtSignal->getCloud(mElectronCloud);
 
	  mSvtSignal->setPeakAndUnderShoot();
 
          if(mPasaSigAttributes)
	    mSvtSimulation->resetPasaSignalAttributes();

	  mSvtSimulation->fillBuffer(anTime[0],anTime[1],mBackGSigma,SvtSimDataPixels);

          if(mPasaSigAttributes){

	    mPasaSignals = mSvtSimulation->getPasaSigAttributes();
	    float peakVal = 0,timeCenter = 0,timeWidth =0;
	    float unShootVal = 0;

	    for(int i = 0; i < 7; i++){
	      if(mPasaSignals[j].mPeak[i] > peakVal){
		peakVal = mPasaSignals[j].mPeak[i];
	      }
	      if(mPasaSignals[j].mUnderShoot[i] > unShootVal){
		unShootVal = mPasaSignals[j].mUnderShoot[i];
	      }
	      
	    }
           cout<<"peakVal = "<<peakVal<<endl;
           cout<<"unShootVal = "<<unShootVal<<endl;

	   yPos += 20.0*mDriftVelocity*mTimeBinSize/10.0;
	  }
	 }
       }
	
       zPos += 20.0*mSvtSrsPar->pitch;
      }
   }
  
      return kStOK;
}
//____________________________________________________________________________

Int_t StSvtSimulationMaker::checkAllHybrids()
{  
   cout<<"StSvtSimulationMaker::checkAllHybrids()"<<endl;
   
   int k, HardWarePos,Layer;
   int idShape = 0;
   int layer,ladder ,wafer ,hybrid ;
   float locPx, locPy ,locPz;
   float xPos,yPos,zPos ;
   float wafPosX,wafPosY,wafPosZ;

   StThreeVector<double> VecG(0,0,0);
   StThreeVector<double> VecL(0,0,0);
 
   StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
   StSvtLocalCoordinate localCoord(0,0,0); 
   StGlobalCoordinate globalCor(0,0,0);

   for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {
    
    for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {
      
      for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {
	
	for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){

          

            int index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

            if( index < 0) continue; 

	    mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);

 
	    if(!mSvtSimDataPixels) {
	      mSvtSimDataPixels = new StSvtHybridPixels(Barrel,Ladder,Wafer,Hybrid);
	      mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
	    }
 
	    mSvtSimDataPixels->reset();

            Layer = mSvtSimDataPixels->getLayerID();

            svtAttr[0] = Barrel; svtAttr[1] = Layer; svtAttr[2] = Ladder; svtAttr[3] = Wafer;svtAttr[4] = Hybrid;

	    HardWarePos = 1000*Layer+100*Wafer+Ladder;
	    
	    for( k=0; k< 216; k++){
	      if( mSvtGeom[k].id ==  HardWarePos)
		break;
	    } 
	    
   
	     wafPosX = mSvtGeom[k].x[0];
             wafPosY = mSvtGeom[k].x[1];
             wafPosZ = mSvtGeom[k].x[2];

	     // if(Layer == 1 && Ladder == 2 && Wafer == 3 && Hybrid == 2){

             //cout<<"wafPosX = "<< wafPosX<<",  wafPosY = "<< wafPosY<<",  wafPosZ = "<< wafPosZ<<endl; 
	   
	     //}

             locPz = 0.015;                    //cm   normal direction 
	     //mSvtShape[idShape].shape[1] == 3                                   
	     //locPy = mSvtSrsPar->pitch*(3 + 0.5) - mSvtShape[idShape].shape[1];  //cm  transverse direction

             
	     mSvtSimulation->setPasaSigAttributes(mPasaSigAttributes, mNumOfHitsPerHyb);

	     for(int i = 0; i < mNumOfHitsPerHyb; i++){

	       //if(Hybrid == 2)
	       // locPx = mDriftVelocity*mTimeBinSize*(6 + 0.5)/10.0;    //cm
	       //else
	       // locPx = (-1)*mDriftVelocity*mTimeBinSize*(6 + 0.5)/10.0;    //cm
   

                if(Hybrid == 2)
		  locPx = 2.9*((float)rand()/(float)RAND_MAX);
		else
		  locPx = (-2.9)*((float)rand()/(float)RAND_MAX);

		locPy = 5.6*((float)rand()/(float)RAND_MAX) - 2.8; 
     
		 if(fabs(locPy) < 3.0 && fabs(locPx) < 3.0){

		   xPos = wafPosX + locPx*mSvtGeom[k].d[0] + locPy*mSvtGeom[k].t[0] + locPz*mSvtGeom[k].n[0] ;  //cm
		   yPos = wafPosY + locPx*mSvtGeom[k].d[1] + locPy*mSvtGeom[k].t[1] + locPz*mSvtGeom[k].n[1] ;  //cm
		   zPos = wafPosZ + locPx*mSvtGeom[k].d[2] + locPy*mSvtGeom[k].t[2] + locPz*mSvtGeom[k].n[2] ;  //cm

		   VecG.setX(xPos);
		   VecG.setY(yPos);
		   VecG.setZ(zPos);
	  
		   globalCor.setPosition(VecG);
          
		   mCoordTransform->operator()(globalCor,waferCoord);

                   mCoordTransform->operator()(globalCor,localCoord);

                   VecL.setX(localCoord.position().x());             //drift direction
                   VecL.setY(localCoord.position().y());             //transverse direction
                   VecL.setZ(localCoord.position().z());             //normal direction

		   layer = waferCoord.layer(); ladder = waferCoord.ladder();
		   wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid();     

		   //cout<<"layer = "<<layer<<"\tladder = "<<ladder<<"\twafer = "<<wafer<<"\thybrid = "<<hybrid<<endl;    

		  mAnode = waferCoord.anode(); mTime = waferCoord.timebucket();

		  //if(Layer == 1 && Ladder == 2 && Wafer == 3 && Hybrid == 2){
		    
		  //cout<<"localCoord.position().z() = "<<localCoord.position().z()<<endl;
		  //cout<<"xPos = "<<xPos<<",  yPos = "<<yPos<<",  zPos = "<<zPos<<",  mAnode = "<<anTime[0]<<", mTime = "<<anTime[1]<<endl;
		  //}


                  fillEval(Barrel,Ladder,Wafer,Hybrid,waferCoord,VecG,VecL);

		  mSvtSimulation->doCloud(mTime ,mEnergy,mTheta,mPhi);
 
		  mSvtSignal->getCloud(mElectronCloud);
 
                  if(mPasaSigAttributes)
		   mSvtSimulation->resetPasaSignalAttributes();
 
		  mSvtSimulation->fillBuffer(mAnode ,mTime ,mBackGSigma,mSvtSimDataPixels);

                 
                  if(mPasaSigAttributes){

		    mPasaSignals = mSvtSimulation->getPasaSigAttributes();
                    float peakVal = 0;
                    float unShootVal = 0;

		    for(int j = 0; j < 7; j++){
                      if(mPasaSignals[i].mPeak[j] > peakVal)
			peakVal = mPasaSignals[i].mPeak[j];
                      if(mPasaSignals[i].mUnderShoot[j] > unShootVal)
			unShootVal = mPasaSignals[i].mUnderShoot[j];

                    mNTuple->Fill(mTime,mAnode,xPos,yPos,zPos,peakVal ,unShootVal,0,0,0,0);

		    }
		  }
                
		 }
	     }
	}
      }
    }
   }
  
      return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::doDriftTimeShift(StSvtHybridPixels* SvtSimDataPixels){

  // fstream outputR1;
  //outputR1.open("driftshift.dat",ios::out);

 StThreeVector<double> VecG(0,0,0);
 
 StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
 StGlobalCoordinate globalCor(0,0,0);

 int layer,ladder ,wafer ,hybrid ;
 float xPos,yPos,zPos,tBin,aBin,zStep,yStep;
 //float driftTimeShift[128][100];

 xPos = 6.125;                        //cm
 aBin = mSvtSrsPar->pitch;      //cm
 zStep = aBin/10.0;
 zPos = 8*zStep + aBin*120; 

 //cout<<" zPos = "<<zPos<<endl;
        
 tBin = (mDriftVelocity*mTimeBinSize)/10.0;    //cm 
 yStep = tBin/100.0;
 yPos = 128*tBin; 

 int numOfSteps = (int)(128*tBin/yStep);

 VecG.setX(xPos + 0.015);

 //cout<<"yPos"<<"\t"<<"anode"<<"\t"<<"Time"<<"\t"<<"actualAnode"<<"\t"<<"timeCenter"<<"\t"<<"timeShift"<<"\t"<<"timeWidth"<<"\t"<<"peak"<<"\t"<<"undershoot"<<"\t"<<"chargeOnAnode"<<endl;

 //for(int i = 0; i < 10; i++){

   VecG.setZ(zPos);

   cout<<"numOfSteps = "<<numOfSteps<<endl;
   for(int j = numOfSteps; j > 0; j--){
     
     yPos -= yStep;
    if(yPos > 0.0 && yPos < 3.0){
   
     VecG.setY(yPos);

     globalCor.setPosition(VecG);
          
     mCoordTransform->operator()(globalCor,waferCoord);

     layer = waferCoord.layer(); ladder = waferCoord.ladder();
     wafer = waferCoord.wafer(); hybrid = waferCoord.hybrid(); 

     // cout<<"layer = "<<layer<<"\tladder = "<<ladder<<"\twafer = "<<wafer<<"\thybrid = "<<hybrid<<endl;    

     anTime[0] = waferCoord.anode(); anTime[1] = waferCoord.timebucket();
     if(j > 400){
       //cout<<" zPos = "<<zPos<<",  mAnode = "<<anTime[0]<<",  yPos = "<<yPos<<", mTime = "<<anTime[1]<<endl;

     }
     fillEval(svtAttr,anTime);

     mSvtSimulation->doCloud(anTime[1],mEnergy,mTheta,mPhi);
 
     mSvtSignal->getCloud(mElectronCloud);
 
     mSvtSignal->setPeakAndUnderShoot();
 
     if(mPasaSigAttributes)
       mSvtSimulation->resetPasaSignalAttributes();

     mSvtSimulation->fillBuffer(anTime[0],anTime[1],mBackGSigma,SvtSimDataPixels);

     if(mPasaSigAttributes){

       mPasaSignals = mSvtSimulation->getPasaSigAttributes();
       float peakVal = 0,timeCenter = 0,timeShift = 0;
       float unShootVal = 0;

       float sumAdc = 0;
       float sumAdcSq = 0;
       float sumAnPos = 0;
       float sumTimPos = 0;
       float sumAnPosSq = 0;
       float sumTimPosSq = 0;

       for(int i = 0; i < 7; i++){

       // mPasaSignals[0].anode[i];                   //actual anode
//        mPasaSignals[0].mPeak[i];
//        mPasaSignals[0].mTimeCenter[i];
//        mPasaSignals[0].mTimeWidth[i];
//        mPasaSignals[0].mUnderShoot[i];
//        mPasaSignals[0].mCharge[i];
	 if(mPasaSignals[0].mPeak[i] > peakVal){
	   peakVal = mPasaSignals[0].mPeak[i];
	 }
	 if(mPasaSignals[0].mUnderShoot[i] < unShootVal){
	   unShootVal = mPasaSignals[0].mUnderShoot[i];
	 }

           for(int j = 0; j < 128; j++){
     
             float adc = mPasaSignals[0].mTempBuffer[i][j];

             if(adc > 0){
                sumAdc += adc;
                sumAdcSq += adc*adc;
                float wAnPos = adc*(float(mPasaSignals[0].anode[i]) - 0.5);
                float wTimPos = adc*(float(j) + 0.5);
                sumAnPos += wAnPos;
                sumAnPosSq += wAnPos*(float(mPasaSignals[0].anode[i]) - 0.5);
                sumTimPos += wTimPos;
                sumTimPosSq += wTimPos*(float(j) + 0.5);
               }
              }

	   //timeShift = (mPasaSignals[0].mTimeCenter[i]/mTimeBinSize) - anTime[1];
	   // cout<<yPos<<"\t"<<anTime[0]<<"\t"<<anTime[1]<<"\t"<<mPasaSignals[0].anode[i]<<"\t"<<mPasaSignals[0].mTimeCenter[i]<<"\t"<<timeShift<<"\t"<<mPasaSignals[0].mTimeWidth[i]<<"\t"<<mPasaSignals[0].mPeak[i]<<"\t"<<mPasaSignals[0].mUnderShoot[i]<<"\t"<<mPasaSignals[0].mCharge[i]<<endl;
         
       }
          
       float meanAnPos = sumAnPos/sumAdc;
       float meanAnPosSq = sumAnPosSq/sumAdc;
       float meanTimPos = sumTimPos/sumAdc;
       float meanTimPosSq = sumTimPosSq/sumAdc;
          
       float shiftInTime = meanTimPos - anTime[1];
       float ratio = (sumAdc*sumAdc)/sumAdcSq;

       // int n = (int)anTime[1];
//        int m = (int)((anTime[1] - float(n))*100);

//        driftTimeShift[n][m] = shiftInTime;
//        if(j > 400){
// 	 //cout<<"driftTimeShift"<<"\["<<n<<"]["<<m<<"] = "<<shiftInTime<<endl;
//        }

       // if(m < 99)       
//        outputR1<<shiftInTime<<"\t";
//        else outputR1<<shiftInTime<<endl;

       //widthAnode = mElectronCloud->getSigma1()*(1000.0/250.0);
       //widthTime = mElectronCloud->getSigma2()*(1000.0/240.0);
          
       float widthAnode = sqrt(meanAnPosSq - meanAnPos*meanAnPos);
       float widthTime = sqrt(meanTimPosSq - meanTimPos*meanTimPos);

       if(ratio > 1){
	 widthAnode = widthAnode*sqrt(ratio/(ratio - 1));
	 widthTime = widthTime*sqrt(ratio/(ratio - 1));
       }
          

       mNTuple->Fill(anTime[1],anTime[0],xPos,yPos,zPos,peakVal,unShootVal,sumAdc,widthTime,widthAnode,shiftInTime);

      
	     
     }
       // cout<<"peakVal = "<<peakVal<<endl;
       //cout<<"unShootVal = "<<unShootVal<<endl;
    }
   
    //yPos -= yStep;

   }

    // cout<<"tBin = "<<tBin<<" , aBin = "<<aBin<<endl;
//     cout<<"totalDriftLength = "<<128*tBin<<endl;
//     cout<<"yStep = "<<yStep <<endl;
//     cout<<"numOfSteps = "<<numOfSteps<<endl;
   // zPos += zStep;
   // }
   //outputR1.close();
      return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::fillEval(int* svtAttrib, float* anodeTim)
{
 int index = mSvtGeantHitColl->getHybridIndex(svtAttrib[0],svtAttrib[2],svtAttrib[3],svtAttrib[4]);
  
  if (index < 0) return kStErr;
  
  mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
  if(!mSvtGeantHit) {
    mSvtGeantHit = new StSvtGeantHits(svtAttrib[0],svtAttrib[2],svtAttrib[3],svtAttrib[4]);
    mSvtGeantHitColl->put_at(mSvtGeantHit,index);
  }

  mSvtGeantHit->setGeantHit(counter[index],svtAttrib,anodeTim);
  ++counter[index];
  mSvtGeantHit->setNumOfHits(counter[index]);
   
  return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::fillEval(int barrel,int ladder,int wafer,int hybrid, StSvtWaferCoordinate& waferCoord, StThreeVector<double>& VecG,StThreeVector<double>& VecL)
{ 
  int index = mSvtGeantHitColl->getHybridIndex(barrel,ladder,wafer,hybrid);
  
  if (index < 0) return kStErr;
  
  mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
  if(!mSvtGeantHit) {
    mSvtGeantHit = new StSvtGeantHits(barrel,ladder,wafer,hybrid);
    mSvtGeantHitColl->put_at(mSvtGeantHit,index);
  }

  mSvtGeantHit->setGeantHit(counter[index],waferCoord);
  mSvtGeantHit->setLocalCoord(counter[index],VecL);
  mSvtGeantHit->setGlobalCoord(counter[index],VecG);
  ++counter[index];
  mSvtGeantHit->setNumOfHits(counter[index]);
    
  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::MakeHistograms1()
{
  cout<<"In StSvtSimulationMaker::MakeHistograms1()"<<endl;
  int index; float adc;

 if(mCheckOneHybrid)
    {
     
      int index = mSvtSimPixelColl->getHybridIndex(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);

      if( index < 0) return kStErr;

      mSvtSimDataPixels = (StSvtHybridPixels *)mSvtSimPixelColl->at(index);

      if(!mSvtSimDataPixels) return kStWarn;
   
      mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
      if( !mSvtGeantHit) return kStWarn;

      geant_hit[0]->Reset();

       for( int gHit = 0; gHit < mSvtGeantHit->numberOfHits(); gHit++){
              
         float  time =  mSvtGeantHit->waferCoordinate()[gHit].timebucket();
          float  anode = mSvtGeantHit->waferCoordinate()[gHit].anode();

	   geant_hit[0]->Fill(anode,time);
	 }
	     
       hit_plus_backgr[0]->Reset();
       for(int tim = 0; tim < 128; tim++)
       for(int an = 0; an < 240; an++)
         {
          //adc = ((StSvtHybridPixels *)mSvtSimPixelColl->at(index))->getPixelContent(an + 1,tim);
           adc = mSvtSimDataPixels->getPixelContent(an + 1,tim);
           hit_plus_backgr[0]->Fill(an,tim,adc);
          }
      
      } else {

         for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {    
          for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {      
           for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {	
	    for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){

             index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

             if( index < 0) continue; 
            
              mSvtSimDataPixels = (StSvtHybridPixels *)mSvtSimPixelColl->at(index);

             if(!mSvtSimDataPixels)
               continue;
	     //cout<<index<<endl;
             mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
              if( !mSvtGeantHit) continue;

               geant_hit[index]->Reset();

             for( int gHit = 0; gHit < mSvtGeantHit->numberOfHits(); gHit++){
              
                float  time =  mSvtGeantHit->waferCoordinate()[gHit].timebucket();
                float  anode = mSvtGeantHit->waferCoordinate()[gHit].anode();

	         geant_hit[index]->Fill(anode,time);
	     }
	     
            hit_plus_backgr[index]->Reset();
            for(int tim = 0; tim < 128; tim++)
             for(int an = 0; an < 240; an++)
               {
                //adc = ((StSvtHybridPixels *)mSvtSimPixelColl->at(index))->getPixelContent(an + 1,tim);
                adc = mSvtSimDataPixels->getPixelContent(an + 1,tim);
               
                hit_plus_backgr[index]->Fill(an,tim,adc);
               }
	 }
        }
      }
    }

  }
 
 return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::MakeHistograms2()
{
  int* anolist; 
  int mSequence,index;
  int stTimeBin,len,status;
  unsigned char* adc;

 StSequence* svtSequence;

 if(mCheckOneHybrid)
    {
     
      int index = mSvtSimDataColl->getHybridIndex(svtAttr[0],svtAttr[2],svtAttr[3],svtAttr[4]);

      if( index < 0) return kStErr;

      mSimHybridData = (StSvtHybridSimData *)mSvtSimDataColl->at(index);
      if(!mSimHybridData) return kStWarn;

      mDataBeforeSeqAdj[0]->Reset();
       for(int anode = 0; anode < mSimHybridData->getAnodeList(anolist); anode++)
        {
         int ianode = anolist[anode];
          status = mSimHybridData->getListSequences(anode,mSequence,svtSequence);

          for(int mSeq = 0; mSeq < mSequence; mSeq++) 
           {
            stTimeBin = svtSequence[mSeq].startTimeBin; 
            len = svtSequence[mSeq].length;
            adc = svtSequence[mSeq].firstAdc;
            for(int j = 0 ; j < len; j++)
             {
               float c = (float) adc[j] - mSimHybridData->getOffSet();

                mDataBeforeSeqAdj[0]->Fill(ianode - 1,stTimeBin + j,c);
	    }
	 }
      }
 
   } else {
    

     for(int Barrel = 1;Barrel <= mSvtSimDataColl->getNumberOfBarrels();Barrel++) {    
      for (int Ladder = 1;Ladder <= mSvtSimDataColl->getNumberOfLadders(Barrel);Ladder++) {      
       for (int Wafer = 1;Wafer <= mSvtSimDataColl->getNumberOfWafers(Barrel);Wafer++) {	
	 for( int Hybrid = 1;Hybrid <= mSvtSimDataColl->getNumberOfHybrids();Hybrid++){

             index = mSvtSimDataColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

            
             if( index < 0) continue; 
            
              mSimHybridData = (StSvtHybridSimData *)mSvtSimDataColl->at(index);
              if(!mSimHybridData)
               continue;

             mDataBeforeSeqAdj[index]->Reset();
             
             for(int anode = 0; anode < mSimHybridData->getAnodeList(anolist); anode++)
               {
                 int ianode = anolist[anode];
                 status = mSimHybridData->getListSequences(anode,mSequence,svtSequence);

                 for(int mSeq = 0; mSeq < mSequence; mSeq++) 
                  {
                   stTimeBin = svtSequence[mSeq].startTimeBin; 
                   len = svtSequence[mSeq].length;
                   adc = svtSequence[mSeq].firstAdc;
                   for(int j = 0 ; j < len; j++)
                     {
                       float c = (float) adc[j] - mSimHybridData->getOffSet();

                       mDataBeforeSeqAdj[index]->Fill(ianode - 1,stTimeBin + j,c);
		     }
                   }
	       }
	 } //hybrid loop
        }
      }
     } // barrel loop
   }

return kStOK;
 
}
//____________________________________________________________________________
 void StSvtSimulationMaker::histTimeDist(int hit)
{
  //cout<<"In StSvtSimulationMaker::histTimeDist"<<endl;

  if(mPasaSigAttributes){

    mPasaSignals = mSvtSimulation->getPasaSigAttributes();

    for(int i = 0; i < 7; i++){
    
      //cout<<"mTimeDist[i]"<<mTimeDist[i]<<endl;
      mTimeDist[i]->Reset();
   
      for(int j = 0; j < 128; j++){
	float adc = mPasaSignals[hit].mTempBuffer[i][j];
	mTimeDist[i]->Fill(j,adc);
      }
    }
  }
}


//____________________________________________________________________________
 void StSvtSimulationMaker::histChargeDist(int hit)
{
  //cout<<"In StSvtSimulationMaker::histChargeDist"<<endl;
  int i = 0; double charge;

  mChargeDist->Reset();
  int anode = (int) mAnode + 1;

  if(mPasaSigAttributes){

    mPasaSignals = mSvtSimulation->getPasaSigAttributes();
   
  for(int an = -3; an <= 3; an++)
   {
    if(anode + an > 0 && anode + an <= 240)
       {
         charge = mPasaSignals[hit].mCharge[i];
         mChargeDist->Fill(anode + an - 0.5, log(charge));
      }
    ++i;
   }
  }
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::Finish()
{
   if (Debug()) gMessMgr->Debug() << "In StSvtSimulationMaker::Finish() ..."
			       << endm;


   mNtFile->Write();
   mNtFile->Close();

  //mSvtSimulation->closeFiles(); 
  //mElectronCloud->closeFiles();

  return kStOK;
}

//____________________________________________________________________________
