// 
// $Id: StBemcTables.cxx,v 1.1 2004/10/18 18:20:07 suaide Exp $
// $Log: StBemcTables.cxx,v $
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
#include "StBemcTables.h"
#include "Stiostream.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "TString.h"

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

ClassImp(StBemcTables)

//_____________________________________________________________________________
/* 
   Default constructor. Set Initial values for some variables
*/
StBemcTables::StBemcTables():TObject()
{  
  mBtowP = NULL;
  mBprsP = NULL;
  mSmdeP = NULL;
  mSmdpP = NULL;
  mBtowS = NULL;
  mBprsS = NULL;
  mSmdeS = NULL;
  mSmdpS = NULL;
  mBtowC = NULL;
  mBprsC = NULL;
  mSmdeC = NULL;
  mSmdpC = NULL;
  mBtowG = NULL;
  mBprsG = NULL;
  mSmdeG = NULL;
  mSmdpG = NULL;
}
//_____________________________________________________________________________
/*! 
   Default destructor
*/
StBemcTables::~StBemcTables()
{
}
//_____________________________________________________________________________
/*!
  Get pedestals and status tables.  
*/
void StBemcTables::loadTables(Int_t det, TDataSet *DB)
{  
  if(det==1)      { mBtowP = NULL; mBtowS = NULL; mBtowC = NULL; mBtowG = NULL; }
  else if(det==2) { mBprsP = NULL; mBprsS = NULL; mBprsC = NULL; mBprsG = NULL; }
  else if(det==2) { mSmdeP = NULL; mSmdeS = NULL; mSmdeC = NULL; mSmdeG = NULL; }
  else if(det==2) { mSmdpP = NULL; mSmdpS = NULL; mSmdpC = NULL; mSmdpG = NULL; }
  
  TString TableName;
  if(DB)
  {
    /////////////////////////////////////////////////
    TableName = detname[det-1]+"Ped";
    if(det<=2)
    {
      St_emcPed* ped=(St_emcPed*)DB->Find(TableName.Data());
      if(ped)
      {
        if(det==1) mBtowP = ped->GetTable();
        else       mBprsP = ped->GetTable();
      } 
    }
    else
    {
      St_smdPed* ped=(St_smdPed*)DB->Find(TableName.Data());
      if(ped)
      {
        if(det==3) mSmdeP = ped->GetTable();
        else       mSmdpP = ped->GetTable();
      }
    }
     
    /////////////////////////////////////////////////
    TableName = detname[det-1]+"Status";
    if(det<=2)
    {
      St_emcStatus* status=(St_emcStatus*)DB->Find(TableName.Data());
      if(status)
      {
        if(det==1) mBtowS = status->GetTable();
        else       mBprsS = status->GetTable();
      }
    }
    else
    {
      St_smdStatus* status=(St_smdStatus*)DB->Find(TableName.Data());
      if(status)
      {
        if(det==3) mSmdeS = status->GetTable();
        else       mSmdpS = status->GetTable();
      }
    }
      
    /////////////////////////////////////////////////
    TableName = detname[det-1]+"Calib";
    if(det<=2)
    {
      St_emcCalib* calib=(St_emcCalib*)DB->Find(TableName.Data());
      if(calib)
      {
        if(det==1) mBtowC = calib->GetTable();
        else       mBprsC = calib->GetTable();
      }
    }
    else
    {
      St_smdCalib* calib=(St_smdCalib*)DB->Find(TableName.Data());
      if(calib)
      {
        if(det==3) mSmdeC = calib->GetTable();
        else       mSmdpC = calib->GetTable();
      }
    }
      
    /////////////////////////////////////////////////
    TableName = detname[det-1]+"Gain";
    if(det<=2)
    {
      St_emcGain* gain=(St_emcGain*)DB->Find(TableName.Data());
      if(gain)
      {
        if(det==1) mBtowG = gain->GetTable();
        else       mBprsG = gain->GetTable();
      }
    }
    else
    {
      St_smdGain* gain=(St_smdGain*)DB->Find(TableName.Data());
      if(gain)
      {
        if(det==3) mSmdeG = gain->GetTable();
        else       mSmdpG = gain->GetTable();
      }
    }
  }
  return;  
}
//_____________________________________________________________________________
/*!
  Get pedestals and status tables.  
*/
void StBemcTables::loadTables(StMaker* maker)
{
  for(Int_t det = 1; det<=MAXDETBARREL;det++)
  {	  
    TString DbName = "Calibrations/emc/y3"+detname[det-1];
    TDataSet *DB   = maker->GetInputDB(DbName.Data());
    if(DB) loadTables(det,DB);
  }
}  
//_____________________________________________________________________________
/*!
  Get pedestal value  
*/
void StBemcTables::getPedestal(Int_t det, Int_t id, Int_t CAP,Float_t& P, Float_t& R)
{
  P = 0;
  R = 0;
  if(det==1 && mBtowP) 
  {
    P = ((Float_t)mBtowP[0].AdcPedestal[id-1])/100;
    R = ((Float_t)mBtowP[0].AdcPedestalRMS[id-1])/100;
    return;
  }
  if(det==2 && mBprsP) 
  {
    P = ((Float_t)mBprsP[0].AdcPedestal[id-1])/100;
    R = ((Float_t)mBprsP[0].AdcPedestalRMS[id-1])/100;
    return;
  }
  if(det==3 && mSmdeP) 
  {
    Int_t C = 0;
    if(CAP==CAP1) C = 1;
    if(CAP==CAP2) C = 2;
    P = ((Float_t)mSmdeP[0].AdcPedestal[id-1][C])/100;
    R = ((Float_t)mSmdeP[0].AdcPedestalRMS[id-1][C])/100;
    return;
  }
  if(det==4 && mSmdpP) 
  {
    Int_t C = 0;
    if(CAP==CAP1) C = 1;
    if(CAP==CAP2) C = 2;
    P = ((Float_t)mSmdpP[0].AdcPedestal[id-1][C])/100;
    R = ((Float_t)mSmdpP[0].AdcPedestalRMS[id-1][C])/100;
    return;
  }
  return;  
}
//_____________________________________________________________________________
/*!
  Get status  
*/
void StBemcTables::getStatus(Int_t det, Int_t id, Int_t& S)
{
  S = STATUS_OK;
  if(det==1 && mBtowS) { S = (Int_t)mBtowS[0].Status[id-1];return;}
  if(det==2 && mBprsS) { S = (Int_t)mBprsS[0].Status[id-1];return;}
  if(det==3 && mSmdeS) { S = (Int_t)mSmdeS[0].Status[id-1];return;}
  if(det==4 && mSmdpS) { S = (Int_t)mSmdpS[0].Status[id-1];return;}
  return;  
}
//_____________________________________________________________________________
/*!
  Get Gain 
*/
void StBemcTables::getGain(Int_t det, Int_t id, Float_t& G)
{
  G = 1;
  if(det==1 && mBtowG) { G = (Float_t)mBtowG[0].Gain[id-1];return;}
  if(det==2 && mBprsG) { G = (Float_t)mBprsG[0].Gain[id-1];return;}
  if(det==3 && mSmdeG) { G = (Float_t)mSmdeG[0].Gain[id-1];return;}
  if(det==4 && mSmdpG) { G = (Float_t)mSmdpG[0].Gain[id-1];return;}
  return;  
}
//_____________________________________________________________________________
/*!
  Get Calib 
*/
void StBemcTables::getCalib(Int_t det, Int_t id, Int_t power, Float_t& C)
{
  C = 0;
  if(det==1 && mBtowC) { C = (Float_t)mBtowC[0].AdcToE[id-1][power];return;}
  if(det==2 && mBprsC) { C = (Float_t)mBprsC[0].AdcToE[id-1][power];return;}
  if(det==3 && mSmdeC) { C = (Float_t)mSmdeC[0].AdcToE[id-1][power];return;}
  if(det==4 && mSmdpC) { C = (Float_t)mSmdpC[0].AdcToE[id-1][power];return;}
  return;  
}
