/***************************************************************************
 *
 * $Id: StSvtSimulationMaker.cxx,v 1.1 2000/11/30 20:47:49 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Slow Simulator Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulationMaker.cxx,v $
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
  mConfig = TString("Y1L");
  mNumOfHybrids = 0;
}

//____________________________________________________________________________
StSvtSimulationMaker::~StSvtSimulationMaker()
{}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::setConst(char * backgr,double backgSigma, double driftVel, double timBinSize, double anodeSize)
{
 mBackGrOption = backgr;
 mBackGSigma = backgSigma;
 mDriftVel = driftVel;
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
  mConfig = TString(config);

  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSimulationMaker::setConfig(StSvtConfig* config)
{
  mConfig = TString(config->getConfiguration());

  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtSimulationMaker::Init()
{
   if(Debug()) gMessMgr->Debug() << "In StSvtSimulationMaker::Init() ..."
				 << GetName() <<endm;

  St_DataSet *dataSet = GetDataSet("StSvtConfig");

  if (dataSet)
    setConfig((StSvtConfig*)(dataSet->GetObject()));

  mCoordTransform =  new StSvtCoordinateTransform();

  mSvtAngles =  new StSvtAngles();

  mElectronCloud = new StSvtElectronCloud(mExpOption,mWrite,mFineDiv);
  //mElectronCloud->openFiles();
  mElectronCloud->setSiliconProp();

  mSvtSignal = new StSvtSignal();
  mSvtSignal->setOption(mSigOption);
  mSvtSignal->setParam(mTimeBinSize,mAnodeSize,mDriftVel);
  mSvtSignal->pasaRelatedStuff();

  mSvtSimulation = new StSvtSimulation();
  //mSvtSimulation->openFiles();
  mSvtSimulation->setOptions(mBackGrOption);
  mSvtSimulation->setPointers(mElectronCloud,mSvtSignal,mSvtAngles);
  mSvtSimulation->setParam(mTimeBinSize , mAnodeSize);

  mSvtSimPixelColl = new StSvtHybridCollection(mConfig.Data());

  setSvtRawData();
  setTables();
  setEval();

  //CreateHistograms();

  return  StMaker::Init();
}

//____________________________________________________________________________

Int_t  StSvtSimulationMaker::setSvtRawData()
{
  mSimDataSet = new St_ObjectSet("StSvtRawData");
  AddData(mSimDataSet);  

  mSvtSimDataColl = new StSvtHybridCollection(mConfig.Data());
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

  mSvtShape = (St_svg_shape   *) local("svgpars/shape");
  
  if ( !strncmp(mConfig.Data(), "Y1L", strlen("Y1L")) )
    mSvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
  else
    mSvtGeom = (St_svg_geom    *) local("svgpars/geom");
  
  mSvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
  
  if ((!mSvtGeom) || (!mSvtShape) || (!mSvtSrsPar)) { 
    gMessMgr->Error() << "SVT- StSvtHitMaker:svt srspar geom or shapes do not exist" << endm;
    return kStErr;
  }

   return kStOK;
}

//__________________________________________________________________________________________________

Int_t  StSvtSimulationMaker::setEval()
{
 
  mGeantHitSet =  new St_ObjectSet("StSvtGeantHits");
  AddData(mGeantHitSet);

  mSvtGeantHitColl = new StSvtHybridCollection(mConfig.Data());
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
Int_t StSvtSimulationMaker::Make()
{
  if(Debug()) gMessMgr->Debug() <<"In StSvtSimulationMaker::Make()"<<endm;

  int volId ,barrel, layer, ladder, wafer, hybrid;
  double x,y,z,px,py,pz;

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
      g2t_svt_hit_st *trk_st = mG2tSvtHit->GetTable();

      //get svt geometry table
      srs_srspar_st *srs_par = mSvtSrsPar->GetTable();
      svg_shape_st *shape_st = mSvtShape->GetTable();
      svg_geom_st *geom_st = mSvtGeom->GetTable();

      mCoordTransform->setParamPointers(&srs_par[0], &geom_st[0], &shape_st[0], mSvtSimDataColl->getSvtConfig());
     if(Debug()) gMessMgr->Debug() << "number of particles = "<<mG2tSvtHit->GetNRows() << endm;
 
      StSvtWaferCoordinate *waferCoordArray = new StSvtWaferCoordinate[mG2tSvtHit->GetNRows()];

      //
      // Loop over geant hits
      //
      for (int j=0;j<mG2tSvtHit->GetNRows() ;j++)
	{
	  volId = trk_st[j].volume_id;  
	  x = trk_st[j].x[0];    y = trk_st[j].x[1];   z = trk_st[j].x[2];
	  px = trk_st[j].p[0];  py = trk_st[j].p[1];  pz = trk_st[j].p[2];
	  // mEnergy =  trk_st[j].de*1e9;
	  mEnergy =  96000;

	  waferCoordArray[j] = mSvtSimulation->toLocalCoord(x,y,z,mCoordTransform); 

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
	  if ( !strncmp(mConfig.Data(), "Y1L", strlen("Y1L")) ) {
	    if ((wafer == 1) || (wafer == 2) || (wafer == 3))
	      ladder = 2;
	  }	   

	  int index = mSvtSimPixelColl->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if( index < 0) continue; 

	  mSvtSimDataPixels  = (StSvtHybridPixels*)mSvtSimPixelColl->at(index);


	  if(!mSvtSimDataPixels) {
	    mSvtSimDataPixels = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
	    mSvtSimPixelColl->put_at(mSvtSimDataPixels,index);
	  }
	 
	  fillEval(barrel,ladder,wafer,hybrid,waferCoordArray[j]);

	  mSvtSimulation->calcAngles(geom_st,px,py,pz,layer,ladder,wafer);
      
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
Int_t StSvtSimulationMaker::doOneHit(StSvtHybridPixels* mSvtSimDataPixels)
{
      mSvtSimulation->doCloud(mTime,mEnergy,mTheta,mPhi);
 
      mSvtSignal->getCloud(mElectronCloud);
       
      mSvtSignal->setPeak();

      mSvtSimulation->reset();

      mSvtSimulation->fillBuffer(mAnode,mTime,mBackGSigma,mSvtSimDataPixels);

      //histTimeDist();
      //histChargeDist();

      return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSimulationMaker::fillEval(int barrel,int ladder,int wafer,int hybrid, StSvtWaferCoordinate& waferCoord)
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
