/***************************************************************************
 *
 * $Id: StSvtEmbeddingMaker.cxx,v 1.7 2004/07/01 13:54:29 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Embedding Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtEmbeddingMaker.cxx,v $
 * Revision 1.7  2004/07/01 13:54:29  caines
 * Changes to the simulation maker from the review
 *
 * Revision 1.5  2004/02/24 15:53:21  caines
 * Read all params from database
 *
 * Revision 1.4  2004/01/22 16:30:47  caines
 * Getting closer to a final simulation
 *
 * Revision 1.3  2003/11/30 20:51:46  caines
 * New version of embedding maker and make OnlSeqAdj a stand alone maker
 *
 * Revision 1.2  2003/09/07 03:49:06  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.1  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 **************************************************************************/

#include "Stiostream.h"
#include <cmath>
using namespace std;

#include "TH2.h"
#include "TFile.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StSequence.hh"
#include "StSvtEmbeddingMaker.h"
#include "StMessMgr.h"

#include "StSvtDbMaker/StSvtDbMaker.h"

#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsD.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"

ClassImp(StSvtEmbeddingMaker)
  /*!
   *   we use default value for RMS of the pedestal,
   *   if nothing else is available from the database
   *
   */
#define cDefaultBckgRMS 1.8


//____________________________________________________________________________
StSvtEmbeddingMaker::StSvtEmbeddingMaker(const char *name):StMaker(name)
{
///By default we want to use maxim information contained in the dabase and run embedding if possible.
  mDoEmbedding=kTRUE;     
  setBackGround(kTRUE,cDefaultBckgRMS);//sets to default true and sigma 1.8
  SetPedRmsPreferences(kTRUE, kTRUE);  //read database

 
  mSimPixelColl = NULL;
  mRealDataColl = NULL;
  mPedColl = NULL;
  mPedRMSColl = NULL;
}

//____________________________________________________________________________
StSvtEmbeddingMaker::~StSvtEmbeddingMaker()
{

}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Init()
{
  return StMaker::Init();
}

//____________________________________________________________________________
///All database dependent data are read here. If SvtDaqMaker is found inside of the main chain
///the simulation is run as an embedding, otherwise the EmbeddingMaker creates
//only white noise as a background.
Int_t StSvtEmbeddingMaker::InitRun(int runumber)
{
  ReadPedRMSfromDb();
  GetPedRMS();
  if (mDoEmbedding) //now decide if it's true embedding into raw data or just simple background
   {
    mRunningEmbedding= (GetMaker("SvtDaq")!=NULL);
    if (!mRunningEmbedding) gMessMgr->Info()<<"StSvtEmbeddingMaker::InitRun: No SvtDaq Maker found - SWITCHING TO PLAIN SIMULATION "<<endm;
   }
  else mRunningEmbedding=kFALSE;

  //write out the state of simulation
  char st[20];
  if (mRunningEmbedding) sprintf(st,"EMBEDDING");
  else sprintf(st,"PLAIN SIMULATION");
  gMessMgr->Info()<<"SVT SlowSimualtion is running in the state of :"<<st<<endm;
  
  return StMaker::InitRun(runumber);
}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Make()
{
 
  GetSvtData();
  ClearMask(); //it has to be cleared here - needed for plain simulation

  if (mSimPixelColl==NULL) return kStErr;

  for(int Barrel = 1;Barrel <= mSimPixelColl->getNumberOfBarrels();Barrel++) {
    for (int Ladder = 1;Ladder <= mSimPixelColl->getNumberOfLadders(Barrel);Ladder++) {
      for (int Wafer = 1;Wafer <= mSimPixelColl->getNumberOfWafers(Barrel);Wafer++) {
        for( int Hybrid = 1;Hybrid <= mSimPixelColl->getNumberOfHybrids();Hybrid++){
          
          mCurrentIndex = mSimPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
          if( mCurrentIndex < 0) continue; 
          
          mCurrentPixelData  = (StSvtHybridPixelsD*)mSimPixelColl->at(mCurrentIndex);
          
          if(!mCurrentPixelData) { //no data from simulation Maker
            gMessMgr->Info()<<"Error  in StSvtEmbeddingMaker::Make(): Something is wrong, no data from simulator for hybrid index:"<<mCurrentIndex<<endm;
            mCurrentPixelData = new StSvtHybridPixelsD(Barrel, Ladder, Wafer, Hybrid);
            mSimPixelColl->put_at(mCurrentPixelData,mCurrentIndex);
          }
	   
          if (mRunningEmbedding){  //do the embedding
            ClearMask();
            AddRawData();
          }
          
          if (mBackGrOption) CreateBackground();
	}
      }
    }
  }
  
  return kStOK;
}

//____________________________________________________________________________
///Reads information about background from the database.
///First it tries to find RMS values for individual anodes.
///If that's not avilable then it tries to find values for individual hybrids.
///If not even this is available then one default value for RMS is used.
void StSvtEmbeddingMaker::ReadPedRMSfromDb()
{
  StSvtDbMaker *svtDb =(StSvtDbMaker*)GetMaker("svtDb");
  if (svtDb ==NULL) {
    gMessMgr->Error()<<"StSvtEmbeddingMaker::ReadPedRMSfromDb() - NO SvtDbMaker in chain !"<<endm;
    return;
  }

  if (mUsePixelRMS){
    gMessMgr->Error()<<"StSvtEmbeddingMaker - reading individual pixel RMS values from database "<<endm;
    if ( !GetData("StSvtRMSPedestal") ) svtDb->setSvtRms();
    svtDb->readSvtRms();
  }

if (mUseHybridRMS){
    gMessMgr->Error()<<"StSvtEmbeddingMaker - reading individual hybrid RMS values from database "<<endm;
    if ( !GetData("StSvtPedestal") ) svtDb->setSvtPedestals();
    svtDb->readSvtPedestals();
  }
}

//____________________________________________________________________________
void  StSvtEmbeddingMaker::GetPedRMS()
{
  mPedRMSColl=NULL;
  mPedColl=NULL;

  St_DataSet* dataSet=NULL;
  dataSet = GetDataSet("StSvtRMSPedestal");
  if (dataSet)  mPedRMSColl= (StSvtHybridCollection*)dataSet->GetObject();
  if (mPedRMSColl) cout<<"StSvtEmbeddingMaker: Found RMS values for individual pixels."<<endl;
    else cout<<"StSvtEmbeddingMaker: NO RMS values for individual pixels."<<endl;
      
  dataSet=NULL;
  dataSet = GetDataSet("StSvtPedestal");
  if (dataSet) mPedColl= (StSvtHybridCollection*)dataSet->GetObject();
  if (mPedColl) cout<<"StSvtEmbeddingMaker: Found RMS values for individual hybrids."<<endl;
    else cout<<"StSvtEmbeddingMaker: NO RMS values for individual hybrids."<<endl;

  if ((!mPedRMSColl)&&(!mPedColl))
    cout<<"Warning: no SVT RMS information available from Chain - using default backgroung:"<<mBackGSigma<<endl;
}


//____________________________________________________________________________
void StSvtEmbeddingMaker::GetSvtData()
{
  //EmbeddingMaker requires some data(at least empty) from the SimulationMaker
  mSimPixelColl=NULL;
  mRealDataColl=NULL;
   
  St_DataSet* dataSet = GetDataSet("StSvtPixelData");
  if (dataSet==NULL){
    gMessMgr->Error()<<"BIG TROUBLE:No data from simulator to work with!!!!"<<endm;
    assert(dataSet); //for safety reasons, we could theoreticaly go on, but...
    //return;
  }

  mSimPixelColl= (StSvtData*)(dataSet->GetObject());
  if ( mSimPixelColl==NULL){
    gMessMgr->Error()<<"BIG TROUBLE:Data from simulator is empty!!!!"<<endm;
    return;
  }
  
  if (!mRunningEmbedding) return; //in case it's forbiden to embed
  dataSet = GetDataSet("StSvtRawData");
  if (dataSet) mRealDataColl= (StSvtData*)(dataSet->GetObject());
  if (!mRealDataColl)      //switching to plain simulation, because there is no raw data
     gMessMgr->Info()<<"Note: StSvtEmbeddingMaker is set to do embbeding, but found no raw data."<<endm;
}
 
//____________________________________________________________________________
///Mixes raw data into simulated data nad sets mask for background.
void StSvtEmbeddingMaker::AddRawData()
{ 
  
  StSequence* Sequence;
  
  int numOfSeq;
  int* anolist;
  
  StSvtHybridData* realData = (StSvtHybridData *)mRealDataColl->at(mCurrentIndex); 
    
  double *adcArray=mCurrentPixelData->GetArray();      
  double offset =  mCurrentPixelData->getPedOffset(); //this could be problem if offset differs between real and simulated data!!!
  
  anolist = NULL;
  if (realData!=NULL)
    for (int iAnode= 0; iAnode<realData->getAnodeList(anolist); iAnode++){
      
      int Anode = anolist[iAnode]; //goes from 1
      realData->getSequences(Anode,numOfSeq,Sequence);
      
      for (int nSeq=0; nSeq< numOfSeq ; nSeq++){ 
        unsigned char* adc=Sequence[nSeq].firstAdc;
        int length = Sequence[nSeq].length;
        int startTimeBin=Sequence[nSeq].startTimeBin;
        
        for(int t = 0; t<length; t++){
          int time = startTimeBin + t;
          unsigned char adcVal = adc[t];
          //pixelIndex = mSimPixelData->getPixelIndex(Anode,time);
          //less clean but faster
          int pixelIndex=(Anode-1)*128+time;
          //there already is pedestal offset
          adcArray[pixelIndex]=adcArray[pixelIndex]+(double)adcVal-offset;
          
	//don't do noise here
          mMask[pixelIndex]=kFALSE;
        }
      }
    }
  
  //now clear rest of the mask
  for(int an = 0; an < 240; an++){
    for(int tim = 0; tim < 128; tim++){
      int pIndex=an*128 + tim;
      if (adcArray[pIndex]==offset) mMask[pIndex]=kFALSE; //don't make extra noise outside of hits
    }
  }
  
}

//____________________________________________________________________________
double StSvtEmbeddingMaker::MakeGaussDev(double sigma)
{
  
  double fac,rsq,v1,v2;
  
  do {
    v1 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
    v2 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
    rsq = v1*v1 + v2*v2;
  } while(rsq >= 1.0 );
  
  fac = sigma*::sqrt(-2.0*::log(rsq)/rsq);
  
  // gset = v1*fac;  // gset = 3.0*::sqrt(-2.0*::log(rsq))*(v1/::sqrt(rsq))
   return v2*fac;
 
}

//____________________________________________________________________________
///Creates background depending on what information was retrieved from the database.
void StSvtEmbeddingMaker::CreateBackground()
{
  const double rmsScale=16.;
  double *adcArray=mCurrentPixelData->GetArray(); // array of [128*240]
  
  //find out what background to use
  StSvtHybridPixels* pedRms=NULL;
  if (mPedRMSColl)
    { 
      pedRms = (StSvtHybridPixels*)mPedRMSColl->at(mCurrentIndex);
      if (pedRms == NULL) cout<<"Warning: Individual pixel RMS info is empty for hybrid "<<mCurrentIndex<<" =>have to use other method "<<endl;
    }
  
  if(pedRms)
    {// I have rms for each pixel
      for(int an = 0; an < 240; an++)  for(int tim = 0; tim < 128; tim++){
        //cout<<pedRms<<"indiv rms="<<pedRms->At(pedRms->getPixelIndex(an+1,tim))/rmsScale<<endl;
        int pIndex=an*128 + tim;
        if (mMask[pIndex]) adcArray[pIndex]+=MakeGaussDev(pedRms->At(pedRms->getPixelIndex(an+1,tim))/rmsScale);// !! mAdcArray already contains PedOffset
      }
    }
  else {
    //one value for hybrid
    double backgsigma;
    StSvtHybridPed *ped=NULL;
    if (mPedColl){ 
      ped=(StSvtHybridPed *)mPedColl->at(mCurrentIndex);
      if (ped == NULL) cout<<"Warning: hybrid  RMS info is empty for hybrid "<<mCurrentIndex<<" =>using default value "<<mBackGSigma<<endl;
	}
    if (ped) backgsigma=ped->getRMS(); else  backgsigma=mBackGSigma; //the default value
    if ((backgsigma<=0.)||(backgsigma>=6.)){ //check for obviously bad values 
      cout<<"Warnig for index "<<mCurrentIndex<<" pedestal RMS is:"<<backgsigma<<" => seting to default "<<mBackGSigma<<endl;
      backgsigma=mBackGSigma;
    }
    if (Debug()) cout<<"for index "<<mCurrentIndex<<" pedestal RMS is:"<< backgsigma<<endl;
    
    for(int an = 0; an < 240; an++){
      for(int tim = 0; tim < 128; tim++){
        int pIndex=an*128 + tim;
        if (mMask[pIndex]) adcArray[pIndex]+=MakeGaussDev(backgsigma);// !! mAdcArray already contains PedOffset             
      }
    }
  }
}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Finish()
{ 
  return kStOK;
}


//____________________________________________________________________________
void StSvtEmbeddingMaker::SetPedRmsPreferences(Bool_t usePixelRMS, Bool_t useHybridRMS)
{ // allows to disable reading RMS from database
  mUsePixelRMS=usePixelRMS;
  mUseHybridRMS=useHybridRMS;
}


//____________________________________________________________________________
void  StSvtEmbeddingMaker::setDoEmbedding(Bool_t doIt){
  mDoEmbedding = doIt;
}

//____________________________________________________________________________
void  StSvtEmbeddingMaker::setBackGround(Bool_t backgr,double backgSigma){
  mBackGrOption = backgr;
  mBackGSigma = backgSigma;
}

//____________________________________________________________________________
void  StSvtEmbeddingMaker::ClearMask()
{
  for (int  i=0;i<128*240;i++)mMask[i]=kTRUE;
}

