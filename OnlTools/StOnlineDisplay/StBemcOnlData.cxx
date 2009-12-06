#include "StBemcOnlData.h"
//#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "Stiostream.h"
 
ClassImp(StBemcOnlData)
 
//_____________________________________________________________________________
StBemcOnlData::StBemcOnlData(char* name):TDataSet(name)
{
  memset(this,0,sizeof(StBemcOnlData));
  for(int i=0;i<TDCCHANNELS;i++) 
  {
    TDCGhost[i] = 200;
  }
  TowerRemoveGhost = kFALSE;
}
//_____________________________________________________________________________
void StBemcOnlData::zeroAll()
{
  //mDecoder = NULL;
  ///////////////////////////////////////////////////////////
  // NOTE from AAPSUAIDE
  //
  // the status flags (TowerStatus, SmdeStatus, etc) should
  // not be zeroed event by event. This should only clear the
  // detector data, such as ADC, energies and headers
  //
  // Status come from the database and are filled only once
  // per run, so, if it is zeroed it will not be set again
  // until the next run starts
  //
  //////////////////////////////////////////////////////////
  
  NumberGhost = 0;
  NumberBadHeader = 0;
  for(int i=0;i<TDCCHANNELS;i++)
    {
      TDCError[i]    = 0;
      TDCToken[i]    = 0;
      TDCTrigger[i]  = 4;
      TDCCrateId[i]  = 0;
      TDCCount[i]    = 164;
    }
  for(int i=0;i<TOWERCHANNELS;i++)
    {
      TowerADC[i]    = 0;
      TowerEnergy[i] = 0;
      SmdeADC[i]     = 0;
      SmdpADC[i]     = 0;
      SmdeEnergy[i]  = 0;
      SmdpEnergy[i]  = 0;
      PsdADC[i]      = 0;
      PsdEnergy[i]   = 0;
    }
  for(int i=TOWERCHANNELS;i<SMDCHANNELS;i++)
    {
      SmdeADC[i] = 0;
      SmdpADC[i] = 0;
      SmdeEnergy[i] = 0;
      SmdpEnergy[i] = 0;
    }
  for(int i=0;i<EMCTRIGGERPATCH;i++)
    {
      HighTower[i] = 0;
      Patch[i] = 0;
    }
  TowerPresent = kFALSE;
  SMDPresent = kFALSE;
  PSDPresent = kFALSE;
  ValidTowerEvent = kFALSE;
  ValidSMDEvent = kFALSE;
  ValidPSDEvent = kFALSE;
  TDCErrorFlag = 0;
  SMDErrorFlag = 0;
  PSDErrorFlag = 0;
}
//_____________________________________________________________________________
StBemcOnlData::~StBemcOnlData()
{
  if(mDecoder) delete mDecoder;
} 
//_____________________________________________________________________________
Bool_t StBemcOnlData::getTDCStatus(Int_t c)
{
   if(c<0 || c>29) return kFALSE;
   if(c<3  && EventDate<20011105) return kTRUE;
   if(c<5  && EventDate>=20011105 && EventDate<20020301) return kTRUE;
   if(c<15 && EventDate>=20020301 && EventDate<20030701) return kTRUE;
   if(EventDate>=20030701) // returns the status based on the event header
   {
      Bool_t ok = checkTDC(c);
      if (!ok) invalidateTDC(c);
      return ok;
   }
  return kFALSE;
} 
//_____________________________________________________________________________
Bool_t StBemcOnlData::getSMDStatus(Int_t c)
{
  if(c<0 || c>7) return kFALSE;
  if(c<1 && EventDate<20011105) return kTRUE;
  if(c<2 && EventDate>=20011105 && EventDate<20020301) return kTRUE;
  if(c<4 && EventDate>=20020301 && EventDate<20030701) return kTRUE;
  if(EventDate>=20030701) // returns the status based on the event header
    {
      return kTRUE;
    }
  return kFALSE;
} 
//_____________________________________________________________________________
Bool_t StBemcOnlData::checkTDC(Int_t tdc)
{
   Bool_t badHeader = kFALSE;
  // checking for header of the event
  // vf  if(TDCError[tdc]!=0 || TDCCount[tdc]!=164 ) badHeader = kTRUE;
  // if(TDCCrateId[i]!=crate && TDCCrateId[i]!=0) badHeader = kTRUE;
   
  return !badHeader;
}
//_____________________________________________________________________________
void StBemcOnlData::validateData()
{
  //towers first
  Bool_t ok = kTRUE;
  if(!TowerPresent) ok = kFALSE;
  if(TDCErrorFlag==1) ok = kFALSE;
  Int_t nbad = 0;
  for(Int_t i=0;i<TDCCHANNELS;i++) 
  if(!checkTDC(i)) {
      nbad++;
      invalidateTDC(i);
  }
  if(nbad==TDCCHANNELS) ok = kFALSE;
  if(TowerRemoveGhost && NumberGhost>0) ok = kFALSE;
  
  //cout <<"Number Ghost = "<<NumberGhost<<"  ok = "<<(int)ok<<endl;

  ValidTowerEvent = ok;
	
  //SMD
  ok = kTRUE;
  if(!SMDPresent) ok = kFALSE;
  if(SMDErrorFlag ==1) ok = kFALSE;
  ValidSMDEvent = ok;
  	
  //PSD
  ok = kTRUE;
  if(!PSDPresent) ok = kFALSE;
  if(PSDErrorFlag ==1) ok = kFALSE;
  ValidPSDEvent = ok;
}
//_____________________________________________________________________________
void StBemcOnlData::invalidateTDC(int tdc)
{
  if(!mDecoder) mDecoder = new StEmcDecoder(EventDate,EventTime);
  int id; 
  for(int j=0;j<160;j++)
  {
	  mDecoder->GetTowerIdFromTDC(tdc,j,id);
	  TowerADC[id-1] = 0;
  }
}
//_____________________________________________________________________________
void StBemcOnlData::printTower()
{
  cout <<"TOWER DATA -----------------------------------------------------------\n";
  cout <<"RunNumber = "<<RunNumber<<endl;
  cout <<"TriggerWord = "<<TriggerWord<<endl;
  cout <<"EventDate = "<<EventDate<<endl;
  cout <<"EventTime = "<<EventTime<<endl;
  cout <<"ValidTowerEvent = "<<(Int_t)ValidTowerEvent<<endl;
  cout <<"EventNumber = "<<EventNumber<<endl;
  cout <<"TowerByteCount = "<<TowerByteCount<<endl;
  cout <<"NTowerHits = "<<NTowerHits<<endl;
  cout <<"TDCErrorFlag = "<<TDCErrorFlag<<endl;
  cout <<"NTDCChannels = "<<NTDCChannels<<endl;
  for(Int_t i=0;i<TDCCHANNELS;i++)
    cout <<"  TDC = "<<i<<"  Error = "<<TDCError[i]<<"  Token = "<<TDCToken[i]<<"  Trigger = "<<TDCTrigger[i]
	 <<"  Crate = "<<TDCCrateId[i]<<"  Count = "<<TDCCount[i]<<endl;
  for(Int_t k=0;k<TOWERCHANNELS;k++)
    {
      cout <<" ID = "<<k<<" STAT = "<<(Int_t)TowerStatus[k]<<" ADC = "<<TowerADC[k]<<" E = "<<TowerEnergy[k]<<" |";
      if((k+1)%3==0) cout <<endl;
    }
}

void StBemcOnlData::printSMD()
{
}
