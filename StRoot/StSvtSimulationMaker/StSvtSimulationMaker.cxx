/***************************************************************************
 *
 * $Id: StSvtSimulationMaker.cxx,v 1.7 2001/04/12 20:34:54 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Slow Simulator Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulationMaker.cxx,v $
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
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtHybridSimData.hh"
#include "StSvtClusterMatrix.hh"
#include "StSvtAngles.hh"
#include "StSvtEnergySim.hh"
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
  mBackGrOption = "doHitBackGr";
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
Int_t StSvtSimulationMaker::setOptions(char* option1, int option2, int option3, int option4)
{
 mExpOption = option1;  
 mWrite = option2;
 mFineDiv = option3;
 mSigOption = option4;
 
 return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::setOneHit(int oneHit, double anode, double time,double energy, double theta, double phi)
{
  mOneHit = oneHit;
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



  mCoordTransform =  new StSvtCoordinateTransform();
  
  setTables();

     St_svg_shape* SvtShape=0;
     St_svg_geom* SvtGeom=0;
     St_DataSetIter       local(GetInputDB("svt"));
     SvtShape = (St_svg_shape   *) local("svgpars/shape");
     
     if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
       SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
     else
       SvtGeom = (St_svg_geom    *) local("svgpars/geom");
     
  

      cout << mSvtGeom << endl;

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
  //mSvtSimulation->openFiles();
  mSvtSimulation->setOptions(mBackGrOption);
  mSvtSimulation->setPointers(mElectronCloud,mSvtSignal,mSvtAngles);
  mSvtSimulation->setParam(mTimeBinSize , mAnodeSize);

  mSvtSimPixelColl = new StSvtHybridCollection(mConfig);
  setSvtRawData();
  setEval();
  
 /*
  if(doEmbbeding){
    setSvtPixelData();
    setSvtRawData();
   } 
  else{
    mSvtSimPixelColl = new StSvtHybridCollection(mConfig.Data());
    setSvtRawData();
  }
 */

  if(!strncmp(mBackGrOption,"doHybBackGr",strlen("doHybBackGr")))
    createBackGrData(mBackGSigma); 

  //CreateHistograms();

  return  StMaker::Init();
}

//____________________________________________________________________________
Int_t  StSvtSimulationMaker::setSvtPixelData()
{

  mSimPixelSet = new St_ObjectSet("StSvtSimPixels");
  AddData(mSimPixelSet);  
  SetOutput(mSimPixelSet); //Declare for output

  mSvtSimPixelColl = new StSvtHybridCollection(mConfig);
  //cout<<" mSvtSimPixelColl = "<<mSvtSimPixelColl<<endl;
  mSimPixelSet->SetObject((TObject*)mSvtSimPixelColl);
  assert(mSvtSimPixelColl);

 return kStOK;

}

//____________________________________________________________________________
Int_t  StSvtSimulationMaker::setSvtRawData()
{
  mSimDataSet = new St_ObjectSet("StSvtRawData");
  AddData(mSimDataSet);  

  mSvtSimDataColl = new StSvtHybridCollection(mConfig);
  mSimDataSet->SetObject((TObject*)mSvtSimDataColl); 
  assert(mSvtSimDataColl);

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
    gMessMgr->Error() << "SVT- StSvtHitMaker:svt srspar do not exist" << endm;
    return kStErr;
  }

 //get svt geometry table and parameter tables
  mSvtSrsPar = SvtSrsPar->GetTable();

   return kStOK;
}

//__________________________________________________________________________________________________

Int_t  StSvtSimulationMaker::setEval()
{
 
  mGeantHitSet =  new St_ObjectSet("StSvtGeantHits");
  AddData(mGeantHitSet);

  mSvtGeantHitColl = new StSvtHybridCollection(mConfig);
  mGeantHitSet->SetObject((TObject*)mSvtGeantHitColl);
  assert(mSvtGeantHitColl);

  counter = new int[mNumOfHybrids];

  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::CreateHistograms()
{ 
  mNtFile = new TFile("spacepoints.root","RECREATE","SpacePoints");
  
  mNTuple = new TNtuple("SpacePoints","SpacePoints","xl:yl:x:y:z:peak:unShoot");

  char geantHit[25];
  char hitBackGr[25];
  char timeDist[25];
  char chargeDist[25];
  
  char  Index[4];
  char* hitBackGTitle;
  char* timeDistTitle;
  char* geantHitTitle;
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
 
  hit_plus_backgr = new TH2D*[mNumOfHybrids];
  geant_hit = new TH2D*[mNumOfHybrids];
  
  for(Int_t hyb = 0; hyb < mNumOfHybrids; hyb++)
    {
      sprintf(hitBackGr,"hit_plus_backgr");
      sprintf(geantHit,"geant_hit");
      sprintf(Index,"%d", hyb);
      
      hitBackGTitle = strcat(hitBackGr,Index);
      geantHitTitle = strcat(geantHit,Index);
      
      hit_plus_backgr[hyb] = new TH2D(hitBackGTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
      geant_hit[hyb] = new TH2D(geantHitTitle,"Counts vs time and anode",240,0.0,240.0,128,0.0,128.0);
      
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

  int volId ,barrel, layer, ladder, wafer, hybrid;
  double px,py,pz;
  StThreeVector<double> xVec(0,0,0);

  if (counter)
    for(int i = 0; i < mNumOfHybrids; i++)
      counter[i] = 0;

  if(mWrite)
   mSvtSimulation->doCloud(127.5,96000,0,0);

  else if(mOneHit)
    {
      int barrel = 3;
      int ladder = 1;
      int wafer = 7;
      int hybrid = 1;

      int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);
      
      if( index < 0) return kStErr; 

      mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);


      if(!mSvtSimDataPixels) {
	mSvtSimDataPixels = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
	mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
      }
     
      //mSvtSimDataPixels->reset();

      //doOneHit(mSvtSimDataPixels);
           
      setHybrid();
    }

  else
    {
      // get geant hit table
      St_DataSet *g2t_svt_hit =  GetDataSet("g2t_svt_hit");
      St_DataSetIter g2t_svt_hit_it(g2t_svt_hit);
      mG2tSvtHit = (St_g2t_svt_hit *) g2t_svt_hit_it.Find("g2t_svt_hit");
      if( !mG2tSvtHit) {
	gMessMgr->Warning() << "No SVT hits" << endm;
	return kStOK;
      }
      g2t_svt_hit_st *trk_st = mG2tSvtHit->GetTable();
      St_svg_shape* SvtShape=0;
      St_svg_geom* SvtGeom=0;
      St_srs_srspar* SvtSrsPar=0;
      St_DataSetIter       local(GetInputDB("svt"));
      SvtShape = (St_svg_shape   *) local("svgpars/shape");
      
      if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
	SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
      else
	SvtGeom = (St_svg_geom    *) local("svgpars/geom");
      
      SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");


      cout << mSvtGeom << endl;
      mSvtSrsPar = SvtSrsPar->GetTable();
      mSvtShape  = SvtShape->GetTable();
      mSvtGeom   = SvtGeom->GetTable();
      cout << mSvtGeom << endl;
      mCoordTransform->setParamPointers(mSvtSrsPar, mSvtGeom, mSvtShape, mSvtSimDataColl->getSvtConfig());


     if(Debug()) gMessMgr->Debug() << "number of particles = "<<mG2tSvtHit->GetNRows() << endm;
 
      StSvtWaferCoordinate *waferCoordArray = new StSvtWaferCoordinate[mG2tSvtHit->GetNRows()];


      //
      // Loop over geant hits
      //
      for (int j=0;j<mG2tSvtHit->GetNRows() ;j++)
	{
	  volId = trk_st[j].volume_id;
	  if( volId > 7000) continue; // SSD hit
	  xVec.setX( trk_st[j].x[0]);
	  xVec.setY( trk_st[j].x[1]);
	  xVec.setZ( trk_st[j].x[2]);
	  
	  px = trk_st[j].p[0];  py = trk_st[j].p[1];  pz = trk_st[j].p[2];
	  // mEnergy =  trk_st[j].de*1e9;
	  mEnergy =  96000;

	  waferCoordArray[j] = mSvtSimulation->toLocalCoord(xVec,mCoordTransform); 

	  layer = waferCoordArray[j].layer(); ladder = waferCoordArray[j].ladder();
	  wafer = waferCoordArray[j].wafer(); hybrid = waferCoordArray[j].hybrid();     

	  mTime = waferCoordArray[j].timebucket(); mAnode = waferCoordArray[j].anode();

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


	  if( 1000*layer+100*wafer+ladder !=volId){
	    cout << "trouble " << volId << " and our calc" << layer << " " 
		 << wafer << " " << ladder << " " << j <<endl;
	    continue;
	  }
	  int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if( index < 0) continue; 

 	  mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);


	  if(!mSvtSimDataPixels) {
	    mSvtSimDataPixels = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
	    mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
	  }
	 
	  fillEval(barrel,ladder,wafer,hybrid,waferCoordArray[j],xVec);

	  mSvtSimulation->calcAngles(mSvtGeom,px,py,pz,layer,ladder,wafer);
      
	  mTheta = mSvtAngles->getTheta();
	  mPhi = mSvtAngles->getPhi();

	  mSvtSimulation->doCloud(mTime,mEnergy,mTheta,mPhi);
 
	  mSvtSignal->getCloud(mElectronCloud);
       
	  mSvtSignal->setPeak();

	  mSvtSimulation->fillBuffer(mAnode,mTime,mBackGSigma,mSvtSimDataPixels);

	  //mNTuple->Fill(mTime,mAnode,x,y,z,mSvtSignal->getPeak(),mSvtSignal->getMinUnderShoot());
	}
      
      //MakeHistograms1();

      setHybrid();

      //MakeHistograms2();
    }
 
  return kStOK;

}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setHybrid()

{

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
	  // delete mSimHybridData;
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

 return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::doOneHit(StSvtHybridPixels* SvtSimDataPixels)
{
      mSvtSimulation->doCloud(mTime,mEnergy,mTheta,mPhi);
 
      mSvtSignal->getCloud(mElectronCloud);
       
      mSvtSignal->setPeak();

      mSvtSimulation->reset();

      mSvtSimulation->fillBuffer(mAnode,mTime,mBackGSigma,SvtSimDataPixels);

      //histTimeDist();
      //histChargeDist();

      return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::fillEval(int barrel,int ladder,int wafer,int hybrid, StSvtWaferCoordinate& waferCoord, StThreeVector<double>& xVec)
{ 
  int index = mSvtGeantHitColl->getHybridIndex(barrel,ladder,wafer,hybrid);
  
  if (index < 0) return kStWarn;
  
  mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
  if(!mSvtGeantHit) {
    mSvtGeantHit = new StSvtGeantHits(barrel,ladder,wafer,hybrid);
    mSvtGeantHitColl->put_at(mSvtGeantHit,index);
  }

  mSvtGeantHit->setGeantHit(counter[index],waferCoord);
  ++counter[index];
  mSvtGeantHit->setNumOfHits(counter[index]);
  mSvtGeantHit->setGlobalCoord(counter[index],xVec);
    
  return kStOK;
}

//____________________________________________________________________________
void StSvtSimulationMaker::MakeHistograms1()
{
  int index; float adc;


 for(int Barrel = 1;Barrel <= mSvtSimPixelColl->getNumberOfBarrels();Barrel++) {    
    for (int Ladder = 1;Ladder <= mSvtSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mSvtSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {	
	for( int Hybrid = 1;Hybrid <= mSvtSimPixelColl->getNumberOfHybrids();Hybrid++){

             index = mSvtSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

             if( index < 0) continue; 
            
              mSvtSimDataPixels = (StSvtHybridPixels *)mSvtSimPixelColl->at(index);

             if(!mSvtSimDataPixels)
               continue;
             
             mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
              if( !mSvtGeantHit) continue;

               hit_plus_backgr[index]->Reset();
               geant_hit[index]->Reset();

             for( int gHit = 0; gHit < mSvtGeantHit->numberOfHits(); gHit++){
              
                float  time =  mSvtGeantHit->waferCoordinate()[gHit].timebucket();
                float  anode = mSvtGeantHit->waferCoordinate()[gHit].anode();

	         geant_hit[index]->Fill(anode,time);
	     }

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

//____________________________________________________________________________
 void StSvtSimulationMaker::MakeHistograms2()
{
  int* anolist; 
  int mSequence,index;
  int stTimeBin,len,status;
  unsigned char* adc;

 StSequence* svtSequence;

 for(int Barrel = 1;Barrel <= mSvtSimDataColl->getNumberOfBarrels();Barrel++) {    
    for (int Ladder = 1;Ladder <= mSvtSimDataColl->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mSvtSimDataColl->getNumberOfWafers(Barrel);Wafer++) {	
	for( int Hybrid = 1;Hybrid <= mSvtSimDataColl->getNumberOfHybrids();Hybrid++){

             index = mSvtSimDataColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

            
             if( index < 0) continue; 
            
              mSimHybridData = (StSvtHybridSimData *)mSvtSimDataColl->at(index);
              if(!mSimHybridData)
               continue;

             hit_plus_backgr[index]->Reset();
             for(int anode = 0; anode < mSimHybridData->getAnodeList(anolist); anode++)
               {
                 status = mSimHybridData->getListSequences(anode,mSequence,svtSequence);

                 for(int mSeq = 0; mSeq < mSequence; mSeq++) 
                  {
                   stTimeBin = svtSequence[mSeq].startTimeBin; 
                   len = svtSequence[mSeq].length;
                   adc = svtSequence[mSeq].firstAdc;
                   for(int j = 0 ; j < len; j++)
                     {
                       float c = (float) adc[j] - 100.0;

                       hit_plus_backgr[index]->Fill(anode,stTimeBin + j,c);
		     }
                   }
	       }
	}
      }
    }
 }
 
}
//____________________________________________________________________________
 void StSvtSimulationMaker::histTimeDist()
{

  for(int i = 0; i < 7; i++){
     mTimeDist[i]->Reset();
     for(int j = 0; j < 128; j++){
      float adc = mSvtSimulation->getTempBuffer(i,j);
      mTimeDist[i]->Fill(j,adc);
     }
  }

}


//____________________________________________________________________________
 void StSvtSimulationMaker::histChargeDist()
{
  int i = 0; double charge;

  mChargeDist->Reset();
  int anode = (int) mAnode + 1;

  for(int an = -3; an <= 3; an++)
   {
    if(anode + an > 0 && anode + an <= 240)
       {
         charge = mSvtSimulation->getCharge(i);
         mChargeDist->Fill(anode + an - 0.5, log(charge));
      }
    ++i;
   }

}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::Finish()
{
   if (Debug()) gMessMgr->Debug() << "In StSvtSimulationMaker::Finish() ..."
			       << endm;

  //mNtFile->Write();
  //mNtFile->Close();

  //mSvtSimulation->closeFiles(); 
  //mElectronCloud->closeFiles();

  return kStOK;
}

//____________________________________________________________________________
