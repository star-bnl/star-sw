// 
// $Id: StBemcRaw.cxx,v 1.1 2004/10/18 18:20:06 suaide Exp $
// $Log: StBemcRaw.cxx,v $
// Revision 1.1  2004/10/18 18:20:06  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
#include "StBemcRaw.h"
#include "Stiostream.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TString.h"
#include "StEventTypes.h"
#include "StEvent.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

ClassImp(StBemcRaw)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StBemcRaw::StBemcRaw():TObject()
{  
  mPrint = kTRUE;
  mDecoder = 0;
  mDate = 0;
  mTables = new StBemcTables();    
  mControlADCtoE = new controlADCtoE_st();
  Int_t   calib[]      = {1, 1, 1, 1, 0, 0, 0, 0};
  Int_t   pedSub[]     = {1, 1, 1, 1, 0, 0, 0, 0};
  Float_t cut[]        = {-1, 1.5, 1.5, 1.5, -1, -1, -1, -1};
  Int_t   cutType[]    = {0, 1, 1, 1, 0, 0, 0, 0};
  Int_t   onlyCal[]    = {0, 0, 0, 0, 0, 0, 0, 0};
  
  for(Int_t i=0; i<MAXDETBARREL; i++)
  {
    mControlADCtoE->Calibration[i]=calib[i];
    mControlADCtoE->DeductPedestal[i]=pedSub[i];  
    mControlADCtoE->CutOff[i]=cut[i];
    mControlADCtoE->CutOffType[i]=cutType[i];
    mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
  }

}
//_____________________________________________________________________________
/*! 
   Default destructor
*/
StBemcRaw::~StBemcRaw()
{
  if(mTables) delete mTables;
  if(mDecoder) delete mDecoder;
  if(mControlADCtoE) delete mControlADCtoE;
}
void StBemcRaw::createDecoder(Int_t date, Int_t time)
{
  if(mDecoder) delete mDecoder;
  mDecoder = new StEmcDecoder(date,time);
}
Bool_t StBemcRaw::make(TDataSet* TheData, StEvent* event)
{
  if(!TheData) return kFALSE;
  if(!event)   return kFALSE;
  StEmcCollection* emc = event->emcCollection();
  if(!emc) return kFALSE;
  StEmcRawData *bemcRaw = emc->bemcRawData();
  if(!bemcRaw) return kFALSE;
  if(!convertFromDaq(TheData,bemcRaw)) return kFALSE;  
  return make(bemcRaw,event);
}
Bool_t StBemcRaw::make(StEmcRawData* bemcRaw, StEvent* event)
{
  if(!bemcRaw) return kFALSE;
  if(!event)   return kFALSE;
  StEmcCollection* emc = event->emcCollection();
  if(!emc) return kFALSE;
  
  checkHeaders(bemcRaw);
  emptyEmcCollection(emc);
  
  Int_t cap,crate;
  Int_t ADC;
  Float_t E;
  
  for(Int_t det = 1; det<=MAXDETBARREL; det++)
  {
    Int_t nch = 4800;
    if(det>2) nch=18000;
    
    clearStats(det);    
    for(Int_t id = 1; id<=nch; id++) 
    {
      ADC = getBemcADCRaw(det,id,bemcRaw,crate,cap);
      Int_t S = makeHit(emc,det,id,ADC,crate,cap,E);
      updateStats(det,S,ADC,E);
    }
    if(mPrint) printStats(det);
    StDetectorId did = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
    StEmcDetector* detector=emc->detector(did);
    if(detector)
    {
      for(Int_t crate = 1;crate<=30;crate++)
        detector->setCrateStatus(crate,(StEmcCrateStatus)mCrateStatus[det-1][crate-1]);
    }
  }
  
  return kTRUE;
}
//_____________________________________________________________________________
/*!
  Read EMC from DAQ structure
*/
Bool_t StBemcRaw::convertFromDaq(TDataSet* DAQ, StEmcRawData* RAW)
{
  if(!DAQ) { if(mPrint) cout <<"Could not find DAQ DataSet "<<endl; return kFALSE; }
  if(!RAW) { if(mPrint) cout <<"Could not find StEmcRawData pointer "<<endl; return kFALSE; }
	
  StDAQReader* TheDataReader=(StDAQReader*)(DAQ->GetObject());
  if(!TheDataReader || !TheDataReader->EMCPresent()) { if(mPrint) cout <<"Data Reader is not present "<<endl; return kFALSE; }

  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) { if(mPrint) cout <<"Could not find EMC Reader "<<endl; return kFALSE; }
	
	EMC_Reader* reader = TheEmcReader->getBemcReader();
	if(!reader) { if(mPrint) cout <<"Could not find Barrel Reader "<<endl; return kFALSE; }
      
  if(reader->isTowerPresent())
	{
		Bank_BTOWERADCR& tower = reader->getBTOWERADCR();
    if(RAW->header(0)) RAW->deleteBank(0);
    RAW->createBank(0,120,4800);
    for(Int_t i = 0; i<120  ;i++) RAW->setHeader(0,i,tower.TDCHeader[i]);
		for(Int_t i = 0; i<4800 ;i++) RAW->setData(0,i,tower.TowerADCArray[i]);
	}		
	// smd data  
	if(reader->isSmdPresent())
	{
		Bank_BSMDADCR& smd =  reader->getSMD_ADCR();
    Int_t NSMD = 8;
    // there is only 4 SMD Crates before that data and some
    // of them are PSD crates. For Y2004 AuAu runs PSD do
    // not have its own data format and it is being read as 
    // SMD
    if(mDate<20040701) NSMD = 4;
    for(Int_t i = 0; i<NSMD; i++)
    {      
      if(smd.HasData[i]==1)
      {
        Int_t bank = i+1;
        if(RAW->header(bank)) RAW->deleteBank(bank);
        RAW->createBank(bank,128,4800);
        for(Int_t j=0; j<128;  j++) RAW->setHeader(bank,j,smd.SmdHeader[i][j]);
        for(Int_t j=0; j<4800; j++) RAW->setData(bank,j,smd.SMDADCArray[i][j]);
      }
    }
    /////////////////////////////////////////////////////////////////////
    // read Pre Shower data for Y2004 AuAu data. This year, the PSD data
    // is read as SMD data for fibers 4 and 5.
    // 
    // This is a temporary solution while the PSD data format is not
    // decided by Tonko. He needs to have a decision on some
    // hardware issues before the data format is decided
    //
    // AAPSUAIDE 20040318
    //
    if(mDate>20040101 && mDate<20040701)
    {
      for(Int_t RDO = 0; RDO<2; RDO++)
      {
        Int_t SMDRDO = RDO+4;
        if(smd.HasData[SMDRDO]==1) 
        {
          mCrateStatus[1][RDO] = crateOK;
          mNCRATESOK[1]++;
          Int_t bank = RDO+9;
          if(RAW->header(bank)) RAW->deleteBank(bank);
          RAW->createBank(bank,128,4800);
          for(Int_t i = 0; i<128;  i++) RAW->setHeader(bank,i,smd.SmdHeader[SMDRDO][i]);
          for(Int_t i = 0; i<4800; i++) RAW->setData(bank,i,smd.SMDADCArray[SMDRDO][i]);
        }
      }
    }
    /////////////////////////////////////////////////////////////////////
	}
  return kTRUE;    
}
void StBemcRaw::checkHeaders(StEmcRawData* RAW)
{
	for(Int_t det=1;det<=MAXDETBARREL; det++)
    for(Int_t crate = 1; crate<=30;crate++) mCrateStatus[det-1][crate-1] = crateUnknown;
  
  checkBtowCrates(RAW);
	
  mNCRATESOK[1]=mNCRATESOK[2]=mNCRATESOK[3]=0;
  // smd data
  for(Int_t i = 0; i<8; i++)
  {
    UShort_t *header = RAW->header(i+1);
    if(header)   
    {      
      mCrateStatus[2][i] = crateOK;
      mCrateStatus[3][i] = crateOK;
      mNCRATESOK[2]++;
      mNCRATESOK[3]++;
    }
  }
  // PSD data
  for(Int_t i = 0; i<4; i++)
  {
    UShort_t *header = RAW->header(i+9);
    if(header)   
    {      
      mCrateStatus[1][i] = crateOK;
      mNCRATESOK[1]++;
    }
  }
}
void StBemcRaw::emptyEmcCollection(StEmcCollection *emc)
{
	if(!emc) return;
	StSPtrVecEmcPoint& pvec = emc->barrelPoints();
  if(pvec.size()>0)  pvec.clear(); 
 
  for(Int_t i=0; i<4; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=emc->detector(id);
    if(detector)
    {
      if(detector->cluster())
			{
      	StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
      	if(cluster.size()>0) cluster.clear();  
      }
      for(UInt_t j=1;j<=detector->numberOfModules() ;j++)
			{
				StEmcModule *module = detector->module(j);
				if(module)
				{
					StSPtrVecEmcRawHit&  hits=module->hits();
					hits.clear();
				}
			}
    }
  }
  return;
}
//_____________________________________________________________________________
/*!
  Check tower crates header
*/
void StBemcRaw::checkBtowCrates(StEmcRawData* RAW)
{
  if(!RAW) return;
  if(!mDecoder) return;
  UShort_t *header = RAW->header(0);
  if(!header) return;
  mNCRATESOK[0] = 0;
  for(Int_t crate = 1;crate<=30;crate++)
  {
    Int_t TDC;
    mDecoder->GetTowerTDCFromCrate(crate,TDC);
    Int_t sum = header[TDC];
    Int_t err = header[TDC+30];
    //Int_t tok = header[TDC+60];
    //Int_t trg = (header[TDC+90] & 0xF00) >> 8;
    //Int_t crt = (header[TDC+90] & 0x0FF);
    mCrateStatus[0][crate-1] = crateUnknown;
    if(sum==164 && err == 0) mCrateStatus[0][crate-1] = crateOK;
    else mCrateStatus[0][crate-1] = crateHeaderCorrupt;
    
    if(sum==4095 && err == 4095) mCrateStatus[0][crate-1] = crateNotPresent;
    
    if(mCrateStatus[0][crate-1]==crateOK) mNCRATESOK[0]++;
  }
  return;
}
void StBemcRaw::clearStats(Int_t det)
{
  mNZ[det-1] = 0;
  mNCRATE[det-1] = 0;
  mNSTATUS[det-1] = 0;
  mNRMS[det-1] = 0;
  mNPED[det-1] = 0;
  mNOK[det-1] = 0;
  mNTOTAL[det-1] = 0;
  mADCSUM[det-1] = 0;
  mTOTALE[det-1] = 0;
}
void StBemcRaw::updateStats(Int_t det,Int_t S,Int_t ADC, Float_t E)
{
  if(S==kZero) mNZ[det-1]++;
  else if(S==kCrate) mNCRATE[det-1]++;
  else if(S==kStatus) mNSTATUS[det-1]++;
  else if(S==kRms) mNRMS[det-1]++;
  else if(S==kPed) mNPED[det-1]++;
  else if(S==kOK) mNOK[det-1]++;
  mNTOTAL[det-1]++;
  if(S==kOK) 
  {
    mADCSUM[det-1]+=ADC;
    mTOTALE[det-1]+=E;
  }
}
void StBemcRaw::printStats(Int_t det)
{
      cout <<"Statistics for detector  "<<detname[det-1].Data()<<endl;
      cout <<"   Total number of crates with header ok = "<<mNCRATESOK[det-1]<<endl;
      cout <<"   Total number of hits                  = "<<mNTOTAL[det-1]<<endl;
      cout <<"   Total hits removed because of crates  = "<<mNCRATE[det-1]<<endl;
      cout <<"   Total hits removed because ADC = 0    = "<<mNZ[det-1]<<endl;
      cout <<"   Total hits removed by Pedestal        = "<<mNPED[det-1]+mNRMS[det-1]<<endl;
      cout <<"   Total hits removed by Status          = "<<mNSTATUS[det-1]<<endl;
      cout <<"   Total number of hits saved            = "<<mNOK[det-1]<<endl;
      cout <<"   Total ADCSUM of valid hits            = "<<mADCSUM[det-1]<<endl;
      cout <<"   Total Energy of valid hits            = "<<mTOTALE[det-1]<<endl;
}    
//_____________________________________________________________________________
/*!
  Get BEMC ADC Value from StEmcRawData
*/
Int_t StBemcRaw::getBemcADCRaw(Int_t det, Int_t softId, StEmcRawData* RAW, Int_t& CRATE, Int_t& CAP)
{
  if(!RAW) { if(mPrint) cout <<"Could not find StEmcRawData pointer "<<endl; return kFALSE; }
  if(!mDecoder) { if(mPrint) cout <<"Could not find StEmcmDecoderoder pointer "<<endl; return kFALSE; }
  CAP = 0;
  if(det==1) // tower
  {
    Int_t daq;
    if(mDecoder->GetDaqIdFromTowerId(softId,daq)==1) 
    {
      Int_t CR,INDEX;
      mDecoder->GetTowerCrateFromDaqId(daq,CR,INDEX);
      CRATE = CR;
      CAP = 0;
      return RAW->data(0,daq);    
    }
    return 0;
  }
  else if(det==2) // PSD
  {
    Int_t RDO,index;
    Int_t S = mDecoder->GetPsdRDO(softId,RDO,index);
    CRATE = RDO+1;
    if(S==1 && RAW->header(RDO+9) && RDO>=0 && RDO<4)     
    {
      CAP = RAW->header(RDO+9,32);
      return RAW->data(RDO+9,index); 
    }
    return 0;
  }
  else if(det==3) // SMDE
  {
    StEmcGeom *geo = StEmcGeom::instance("bsmde");
    Int_t m,e,s;
    if(geo->getBin(softId,m,e,s)==1) return 0;
    Int_t RDO,index;
    Int_t S = mDecoder->GetSmdRDO(3,m,e,s,RDO,index);
    CRATE = RDO+1;
    if(S==1 && RAW->header(RDO+1) && RDO>=0 && RDO<8) 
    {
      CAP = RAW->header(RDO+1,32);
      return RAW->data(RDO+1,index); 
    }
    return 0;
  }
  else if(det==4) // SMDP
  {
    StEmcGeom *geo = StEmcGeom::instance("bsmdp");
    Int_t m,e,s;
    if(geo->getBin(softId,m,e,s)==1) return 0;
    Int_t RDO,index;
    Int_t S = mDecoder->GetSmdRDO(4,m,e,s,RDO,index);
    CRATE = RDO+1;
    if(S==1 && RAW->header(RDO+1) && RDO>=0 && RDO<8)     
    {
       CAP = RAW->header(RDO+1,32);
      return RAW->data(RDO+1,index); 
    }
    return 0;
  }
  return 0;
}
//_____________________________________________________________________________
/*!
  Construct the StEmcRawHit. Checks if the hit is going to be saved, subtract
  pedestal and apply the calibration, if this is the case.
*/
Int_t StBemcRaw::makeHit(StEmcCollection* emc, Int_t det, Int_t id, Int_t ADC, Int_t CRATE, Int_t CAP,Float_t& E)
{    
  E=0;
    
  if(mCrateStatus[det-1][CRATE-1]!=crateOK) return kCrate;
  if(ADC==0) return kZero;
  
  Int_t STATUS;
  mTables->getStatus(det,id,STATUS);
  if(STATUS!=STATUS_OK) return kStatus;
          
  Float_t PEDESTAL = 0,RMS = 0;
  if(mControlADCtoE->DeductPedestal[det-1]>0) 
  {
    mTables->getPedestal(det,id,CAP,PEDESTAL,RMS);    
    // do not consider hits wih capacitor number CAP1 and CAP2 for
    // PSD and SMD as valid hits
    if(det>=2) if(CAP==CAP1 || CAP==CAP2) return kPed;
  }
  
  if(mControlADCtoE->CutOffType[det-1]==1) // pedestal cut
  {
    if(RMS<=0) return kRms;
    Float_t x = (ADC-PEDESTAL)/RMS;
    if(x<=mControlADCtoE->CutOff[det-1]) return kPed;
  }

  if(mControlADCtoE->Calibration[det-1]==1)
  {
    Float_t ADCP = 1;
    Float_t C;
    for(Int_t i=0;i<5;i++)
    {
      mTables->getCalib(det,id,i,C);
      E+=ADCP*C;
      ADCP*=(Float_t)(ADC-PEDESTAL);
    }          
    mTables->getGain(det,id,C);
    E*=C;
  
    if(mControlADCtoE->CutOffType[det-1]==2) // energy cut
    {
      if(E<mControlADCtoE->CutOff[det-1]) return kEn;
    }
  }
  
  if(mControlADCtoE->OnlyCalibrated[det-1]>0 && E==0) return kCalib;
  
  StDetectorId did = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
  StEmcDetector* detector=emc->detector(did);
  StEmcGeom *geo = StEmcGeom::instance(det);
  if(!detector)
  {
    detector = new StEmcDetector(did,120); 
    emc->setDetector(detector);
  }
  Int_t m,e,s;
  geo->getBin(id,m,e,s);
  StEmcRawHit* hit=new StEmcRawHit(did,m,e,s,(UInt_t)ADC);
  hit->setEnergy(E);
  hit->setCalibrationType(CAP);
  detector->addHit(hit);
  return kOK;  
}
