 /***************************************************************************
 *
 * Author: Bouchet Jonathan
 ***************************************************************************
 *
 * Description: SsdEmbeddingMaker class
 *
 **************************************************************************/

#include "Stiostream.h"
#include <cmath>
using namespace std;

#include "TH2.h"
#include "TFile.h"
#include "TNtuple.h"

#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StSsdEmbeddingMaker.h"
#include "StMessMgr.h"

#include "tables/St_sls_strip_Table.h"
#include "tables/St_spa_strip_Table.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StSSDReader.h"
#include "StSsdSimulationMaker/St_spa_Maker.h"
#include "StSsdPointMaker/StSsdPointMaker.h"

ClassImp(StSsdEmbeddingMaker)

//____________________________________________________________________________
StSsdEmbeddingMaker::StSsdEmbeddingMaker(const char *name):StMaker(name)
{
  ///By default we want to use maxim information contained in the dabase and run embedding if possible.
  mDoEmbedding     = kTRUE;                  ///embedding mode set as default    
  mPlainSimIfNoSSD = kFALSE;    
  /// if true it will run plain simulation insted of embedding if there's no SSD in real data
  mSsdrealData = 0;
  mSsdSimuData = 0;
 }
//____________________________________________________________________________
StSsdEmbeddingMaker::~StSsdEmbeddingMaker()
{
}
//____________________________________________________________________________
Int_t StSsdEmbeddingMaker::Init()
{
  if(IAttr(".histos")){
    hStripsSimu   = new TH1F("hStripsSimu","Log_{10} of the number of strip after dAQ cut in simulation",100,0,10);
    hStripsReal   = new TH1F("hStripsReal","Log_{10} of the number of strip after dAQ cut in real data",100,0,10);
    hStripsCommon = new TH1F("hStripsCommon","number of strips in common from real file and simu file",1000,0,1000);
  }
  return StMaker::Init();
}
//____________________________________________________________________________
///All database dependent data are read here. 
Int_t StSsdEmbeddingMaker::InitRun(int RunNo)
{
  assert(StSsdBarrel::Instance());
  TDataSet *ssdparams = GetInputDB("Geometry/ssd");
  if (! ssdparams) {
    LOG_ERROR << "No  access to Geometry/ssd parameters" << endm;
    return kStFatal;
  }
  TDataSetIter    local(ssdparams);
  m_dimensions  = (St_ssdDimensions     *)local("ssdDimensions");
  ssdDimensions_st     *dimensions = m_dimensions->GetTable(); 
  setSsdParameters(dimensions);
  return kStOk;
}
//____________________________________________________________________________
Int_t StSsdEmbeddingMaker::Make()
{
  LOG_INFO<<"##################################"<<endm;
  LOG_INFO<<"####  START OF SSD EMBEDDING  ####"<<endm;
  //now decide in which mode to realy run, based on options
  //this could be done in InitRun, but could there be mixed event in one run with SSD and without SSD, ie.. some trigger mix
  mRunningEmbedding=mDoEmbedding;
  
  if (mDoEmbedding && NoSsd() ){//check if SSD is present if not skip simulation

    if (mPlainSimIfNoSSD){//run plain simulation instead of embedding
      mRunningEmbedding=kFALSE; //run plain simulation
    }
    else 
      { //clear data and get out
	LOG_INFO <<"SSD SlowSimulation: SKIPPING THIS EVENT - no SSD in real data!!"<<endm;
	return kStOk;
      }  
  }
  Int_t res;
  res = GetSsdData();
  if (res!=kStOk) return res;
  //write out the state of simulation
  char st[20];
  if (mRunningEmbedding) sprintf(st,"EMBEDDING");
  else sprintf(st,"PLAIN SIMULATION");
  LOG_INFO<<"SSD SlowSimulation is running in the state of :"<<st<<endm;
  if (mRunningEmbedding){  //do the embedding
    MODE = 1;
    Int_t SIZEREAL = mSsdrealData->GetNRows();
    Int_t SIZESIMU = mSsdSimuData->GetNRows();
    LOG_INFO <<"####    NUMBER OF STRIPS (SIMU) : "<<SIZESIMU <<" ####"<<endm;
    LOG_INFO <<"####    NUMBER OF STRIPS (REAL) : "<<SIZEREAL <<" ####"<<endm;
    if(Debug())CheckTables();
    Int_t numberofstrips_in_common = AddRawData();
    LOG_INFO <<"####  MERGE SIMU+REAL : "<<numberofstrips_in_common << "####" << endm;
    if (IAttr(".histos")){
      if(SIZESIMU>0){hStripsSimu->Fill(TMath::Log10(SIZESIMU));}
      if(SIZEREAL>0){hStripsReal->Fill(TMath::Log10(SIZEREAL));}
      hStripsCommon->Fill(numberofstrips_in_common);
    }
  }     
  return kStOk;
}
//____________________________________________________________________________
Int_t StSsdEmbeddingMaker::GetSsdData()
{
  //EmbeddingMaker requires some data(at least empty) from the SimulationMaker
  if(!mRunningEmbedding) return kStOk;//dont read real data if you don't need them
  mSsdSimuData = (St_spa_strip*)GetDataSet("SsdSimuData");
  //mSsdSimuData = (St_spa_strip*)GetDataSet("spa_strip/.data/spa_strip");
  if (!mSsdSimuData || mSsdSimuData->GetNRows()==0){ 
    LOG_ERROR << "GetSsdData : no simu input (fired strip for the SSD)"<<endm;   
    return kStErr;
  }
  if(mSsdSimuData) LOG_INFO << "Numbers of strips (simu) = " << mSsdSimuData->GetNRows()<< endm;
  
  mSsdrealData = (St_spa_strip*)GetDataSet("SsdRealData");
  if (!mSsdrealData || mSsdrealData->GetNRows()==0){ 
    LOG_ERROR << "GetSsdData : no real input (fired strip for the SSD)"<<endm;   
    return kStErr;
  }
  if(mSsdrealData) LOG_INFO << "Numbers of strips (real) = " << mSsdrealData->GetNRows()<< endm;
  return kStOk;
}
//____________________________________________________
Int_t StSsdEmbeddingMaker::AddRawData(){
  mSsdSimuReal = new St_spa_strip("spa_strip",50000);   
  AddData(mSsdSimuReal);
  spa_strip_st out_strip ;
  Int_t idWafSimu         = 0;
  Int_t iWafSimu          = 0;
  Int_t iLadSimu          = 0;
  Int_t nStripSimu        = 0;
  Int_t iSideSimu         = 0;
  spa_strip_st *strip_simu = mSsdSimuData->GetTable();
  Int_t idWafReal         = 0;
  Int_t iWafReal          = 0;
  Int_t iLadReal          = 0;
  Int_t nStripReal        = 0;
  Int_t iSideReal         = 0;
  spa_strip_st *strip_real = mSsdrealData->GetTable();
  Int_t currRecord        = 0;
  Int_t currRecordSimu    = 0;
  Int_t currRecordReal    = 0;
  Int_t currRecordCommon  = 0;
  Int_t IdCommon[mSsdSimuData->GetNRows()];
  for(Int_t ll=0;ll<mSsdSimuData->GetNRows();ll++) IdCommon[ll]=0;  
  for (Int_t i = 0 ; i < mSsdrealData->GetNRows(); i++)
    {
      nStripReal  = (int)(strip_real[i].id_strip/100000.);
      idWafReal   = strip_real[i].id_strip-10000*((int)(strip_real[i].id_strip/10000.));
      iWafReal    = idWaferToWafer(idWafReal);
      iLadReal    = (int)(idWafReal - mSsdLayer*1000 - (iWafReal+1)*100 - 1);
      iSideReal   = (strip_real[i].id_strip - nStripReal*100000 - idWafReal)/10000;
      Int_t in =0 ;
      for (Int_t j = 0 ; j < mSsdSimuData->GetNRows(); j++)
	{
	  nStripSimu  = (int)(strip_simu[j].id_strip/100000.);
	  idWafSimu   = strip_simu[j].id_strip-10000*((int)(strip_simu[j].id_strip/10000.));
	  iWafSimu    = idWaferToWafer(idWafSimu);
	  iLadSimu    = (int)(idWafSimu - mSsdLayer*1000 - (iWafSimu+1)*100 - 1);
	  iSideSimu   = (strip_simu[j].id_strip - nStripSimu*100000 - idWafSimu)/10000;
	  
	  if((nStripReal == nStripSimu) && (idWafReal == idWafSimu) && (iSideReal == iSideSimu)) // we found the same strip in simu and real
	    {
	      LOG_DEBUG<<Form("simu side=%d idWafer=%d Ladder=%d wafer=%d nstrip=%d signal=%d",iSideSimu,idWafSimu,iLadSimu,iWafSimu,nStripSimu,strip_simu[j].adc_count)<<endm;
	      LOG_DEBUG<<Form("real side=%d idWafer=%d Ladder=%d wafer=%d nstrip=%d signal=%d",iSideReal,idWafReal,iLadReal,iWafReal,nStripReal,strip_real[i].adc_count)<<endm;
	      out_strip.id          = currRecord + 1;
	      out_strip.adc_count   = (int)(strip_simu[j].adc_count + strip_real[i].adc_count);
	      out_strip.id_strip    = 10000*(10*nStripSimu + iSideSimu)+idWafSimu;
	      for (Int_t e = 0 ; e < 5 ; e++)
		{
		  out_strip.id_mchit[e] = strip_simu[j].id_mchit[e];
		  out_strip.id_mctrack[e] = strip_simu[j].id_mctrack[e];
		}
	      mSsdSimuReal->AddAt(&out_strip);
	      IdCommon[currRecordCommon] = (int)strip_real[i].id_strip; // we keep this id 
	      currRecord++;
	      currRecordCommon++;
	      in = 1;
	      LOG_DEBUG<<Form("# of strips in common =%d id=%d",currRecordCommon,(int)strip_real[i].id_strip)<<endm;
	      if(Debug())
		{
		  for(Int_t ii=0;ii<5;ii++){
		    LOG_INFO <<"mchit["<<ii<<"]="<<strip_simu[j].id_mchit[ii]<<endm;
		    LOG_INFO <<"mctrack["<<ii<<"]="<<strip_simu[j].id_mctrack[ii]<<endm;
		    LOG_INFO << " "<< endm;
		  }
		}
	    }
	  break; // break the loop
	}
      if(in==0) //no common strips, now we fill the real strip 
	{
	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = (int)(strip_real[i].adc_count);
	  out_strip.id_strip    = 10000*(10*nStripReal + iSideReal)+idWafReal;
	  for (Int_t e = 0 ; e < 5 ; e++)
	    {
	      out_strip.id_mchit[e]   = 0;
	      out_strip.id_mctrack[e] = 0;
	    }
	  mSsdSimuReal->AddAt(&out_strip);
	  currRecordReal++;
	  currRecord++;
	} 
    }
  // we continue to fill the table with simu strips not found (in common with the real)
  Int_t found ;
  for(Int_t k=0; k <mSsdSimuData->GetNRows();k++)
    {
      found = 0;
      for (Int_t kk=0;kk<currRecordCommon;kk++)
	{		
	  if((int)strip_simu[k].id_strip == IdCommon[kk]){
	    LOG_DEBUG<<Form("k = %d strip id =%d",k,(int)strip_simu[k].id_strip)<<endm;
	    found = 1;
	  }
	}
      if(found==0){
	nStripSimu  = (int)(strip_simu[k].id_strip/100000.);
	idWafSimu   = strip_simu[k].id_strip-10000*((int)(strip_simu[k].id_strip/10000.));
	iWafSimu    = idWaferToWafer(idWafSimu);
	iLadSimu    = (int)(idWafSimu - mSsdLayer*1000 - (iWafSimu+1)*100 - 1);
	iSideSimu   = (strip_simu[k].id_strip - nStripSimu*100000 - idWafSimu)/10000;
	out_strip.id          = currRecord + 1;
	out_strip.adc_count   = (int)(strip_simu[k].adc_count);
	out_strip.id_strip    = 10000*(10*nStripSimu + iSideSimu)+idWafSimu;
	for (Int_t e = 0 ; e < 5 ; e++)
	  {
	    out_strip.id_mchit[e] = strip_simu[k].id_mchit[e];
	    out_strip.id_mctrack[e] = strip_simu[k].id_mctrack[e];
	  }
	mSsdSimuReal->AddAt(&out_strip);
	currRecord++;
	currRecordSimu++;
      }
    }
  LOG_INFO<<Form("# in common = %d totstrips after add = %d totstrips for simu only = %d for real only = %d",currRecordCommon,currRecord,currRecordSimu,currRecordReal)<<endm;
  return currRecord;
}
//____________________________________________________________________________
Int_t StSsdEmbeddingMaker::Finish()
{ 
  return kStOK;
}
//____________________________________________________________________________
void  StSsdEmbeddingMaker::setDoEmbedding(Bool_t doIt){
  mDoEmbedding = doIt;
}
//____________________________________________________________________________
void StSsdEmbeddingMaker::setPlainSimEvenIfNoSSD(Bool_t doIt){
  mPlainSimIfNoSSD= doIt;
}
//____________________________________________________________________________
Int_t StSsdEmbeddingMaker::NoSsd()
{  
  St_DataSet *dataSet;

  dataSet = GetDataSet("StDAQReader");
  if(!dataSet){
    LOG_ERROR<<("BIG TROUBLE: cannot find StDAQReader in the chain and you want to run embedding")<<endm;
    return kTRUE;
  }
  StDAQReader* daqReader = (StDAQReader*)(dataSet->GetObject());
  if (!daqReader){
    LOG_ERROR<<("BIG TROUBLE: StDAQReader is empty and you want to run embedding")<<endm;
    return kTRUE;
  }
    if (!daqReader->SSDPresent ())
    {
      LOG_INFO<<("NO SSD in DAQ")<<endm;
      return kTRUE; //No SSD in the datastream 
    }
  
  LOG_INFO<<("SSD found in DAQ")<<endm;
  return kFALSE;
}
//______________________________________________________________________
void StSsdEmbeddingMaker::CheckTables(){
 spa_strip_st *strip = mSsdSimuData->GetTable();
  
  Int_t idWaf         = 0;
  Int_t iWaf          = 0;
  Int_t iLad          = 0;
  Int_t nStrip        = 0;
  Int_t iSide         = 0;
  Int_t i=0;
  LOG_DEBUG<<"check simu table :" << endm;
  for (i = 0 ; i < mSsdSimuData->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf    = idWaferToWafer(idWaf);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      LOG_DEBUG<<Form("side=%d idWafer=%d Ladder=%d wafer=%d nstrip=%d signal=%d",iSide,idWaf,iLad,iWaf,nStrip,strip[i].adc_count)<<endm;
      for(Int_t ii=0;ii<5;ii++){printf("id_mchit[%d] =%d\n",ii,strip[i].id_mchit[ii]);}
    }
  if(Debug()){
    LOG_DEBUG << "check real table : " << endm;
    strip  = mSsdrealData->GetTable();
    for (i = 0 ; i < mSsdrealData->GetNRows(); i++)
      {
	nStrip  = (int)(strip[i].id_strip/100000.);
	idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
	iWaf    = idWaferToWafer(idWaf);
	iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
	LOG_DEBUG<<Form("side=%d Ladder=%d wafer=%d nstrip=%d signal=%d",iSide,iLad,iWaf,nStrip,strip[i].adc_count)<<endm;
      }
  }
}
//__________________________________________________________________________
void StSsdEmbeddingMaker::setSsdParameters(ssdDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSsdLayer            = 7; 
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}
//___________________________________________________________________________

