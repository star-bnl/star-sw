/***************************************************************************
*
* $Id: StIstDbMaker.cxx,v 1.1 2014/01/23 20:11:30 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstDbMaker.cxx,v $
* Revision 1.1  2014/01/23 20:11:30  ypwang
* adding scripts
*
*
****************************************************************************
* StIstDbMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 16:15:30 Yaping
* Initial version
****************************************************************************/
/*
  relation within STAR frame
  IstOnGlobal = Tpc2Magnet * Ids2Tpc *    Ist2Ids     * Ladder2Ist * Sensor2Ladder * PS
  
  with 
  
  Ids2Tpc = IstIdsOnTpc
  Ist2Ids = IstIstOnPst * IstPstOnIds
  
  Naming of roatation matrices in this maker :
  PG          = Tpc2Global *  GL     *  IPOI * ISOP   *    LI      *      WLL      * PS
  
  note to self : 
  LI is the IstLadderOnIst
  WLL is the old IstSensorOnLadder table

  numbering 
  Id  = 1000 + (ladder-1)*6 + sensor 
  1<= ladder <= 24
  1<= sensor <= 6
*/

#include <assert.h>
#include "StRoot/StIstUtil/StIstConsts.h"
#include "StIstDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_Survey_Table.h"
#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "TMath.h"
#include "TVector3.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"
THashList *StIstDbMaker::fRotList = 0;

ClassImp(StIstDbMaker)
//_____________________________________________________________________________
StIstDbMaker::StIstDbMaker(const char *name) : StMaker(name), mPedNoise(NULL), mGain(NULL), mMapping(NULL) {
}
//_____________________________________________________________________________
StIstDbMaker::~StIstDbMaker() {
}
//_____________________________________________________________________________
Int_t StIstDbMaker::Init()
{
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StIstDbMaker::InitRun(Int_t runNumber) {
  CalculateSensorsPosition();  
  LOG_DEBUG <<" StIstDbMaker::InitRun() --> GetIstPedNoise" << endm;
  GetIstPedNoise();
  LOG_DEBUG <<" StIstDbMaker::InitRun() --> GetIstGain" << endm;
  GetIstGain();
  LOG_DEBUG <<" StIstDbMaker::InitRun() --> GetIstMapping" << endm;
  GetIstMapping();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StIstDbMaker::Make()
{
  LOG_DEBUG << "Make" << endm;
  return kStOK;
}
//_____________________________________________________________________________
void StIstDbMaker::Clear(const char*)
{
  LOG_DEBUG << "Clear" << endm;
  StMaker::Clear();
}
//_____________________________________________________________________________
Int_t StIstDbMaker::Finish()
{
  LOG_DEBUG << "Finish" << endm;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StIstDbMaker::CalculateSensorsPosition(){
  SafeDelete(fRotList);
  fRotList = new THashList(kIstNumLadders*kIstNumSensorsPerLadder, 0);
  fRotList->SetOwner(kFALSE);

  TGeoHMatrix GL, LI, SD, WG, IPOI , ISOP, DP;

  //get TPC positionement relative to STAR
  assert(gStTpcDb);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

  St_Survey *idsOnTpc          = (St_Survey *) GetDataBase("Geometry/ist/idsOnTpc");         //GL
  if (! idsOnTpc)          {cout << "idsOnTpc has not been found"  << endl; return 0;}

  St_Survey *pstOnIds          = (St_Survey *) GetDataBase("Geometry/ist/pstOnIds");         //IPOI
  if (! pstOnIds)          {cout << "pstOnIds has not been found"  << endl; return 0;}

  St_Survey *istOnPst          = (St_Survey *) GetDataBase("Geometry/ist/istOnPst");         //ISOP
  if (! istOnPst)          {cout << "istOnPst has not been found"  << endl; return 0;}

  St_Survey *istLadderOnIst    = (St_Survey *) GetDataBase("Geometry/ist/istLadderOnIst");   //LI
  if (! istLadderOnIst)    {cout << "istLadderOnIst has not been found"  << endl; return 0;}

  St_Survey *istSensorOnLadder = (St_Survey *) GetDataBase("Geometry/ist/istSensorOnLadder");//WLL
  if (! istSensorOnLadder) {cout << "istSensorOnLadder has not been found"  << endl; return 0;}

  //get these tables
  Survey_st *IdsOnTpc          = idsOnTpc->GetTable();   
  Survey_st *PstOnIds          = pstOnIds->GetTable();   
  Survey_st *IstOnPst          = istOnPst->GetTable();   
  Survey_st *LaddersOnIst      = istLadderOnIst->GetTable();   
  Survey_st *SensorsOnLadders  = istSensorOnLadder->GetTable();   

  //get the number of rows of each tables
  int NoIds                  = idsOnTpc->GetNRows();
  int NoPst                  = pstOnIds->GetNRows();
  int NoIst                  = istOnPst->GetNRows();
  int NoLadders              = istLadderOnIst->GetNRows();
  int NoSensors              = istSensorOnLadder->GetNRows();
 
  if(Debug()>2) { 
    LOG_DEBUG <<" # of Ids     : " << NoIds << endm;
    LOG_DEBUG <<" # of Pst     : " << NoPst << endm;
    LOG_DEBUG <<" # of Ist     : " << NoIst<< endm;
    LOG_DEBUG <<" # of Ladders : " << NoLadders << endm;
    LOG_DEBUG <<" # of Sensors : " << NoSensors << endm;
  }

  GL.SetRotation(&IdsOnTpc->r00);
  GL.SetTranslation(&IdsOnTpc->t0);

  IPOI.SetRotation(&PstOnIds->r00);
  IPOI.SetTranslation(&PstOnIds->t0);

  ISOP.SetRotation(&IstOnPst->r00);
  ISOP.SetTranslation(&IstOnPst->t0);

  if(Debug()>2) {
    LOG_DEBUG << "IDS on TPC :" << endm;
    GL.Print();
    LOG_DEBUG << "PST on IDS :" << endm;
    IPOI.Print();
    LOG_DEBUG << "IST on PST :" << endm;
    ISOP.Print();
  }

  for (int i = 0; i < NoSensors; i++,SensorsOnLadders++) {
    int Id = SensorsOnLadders->Id;
    TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i",Id));
    if (comb) continue;
    comb = new TGeoHMatrix(Form("R%04i",Id)); 
    int ladder = ((Id - 1000) - 1) / 6 + 1;  // 1 <= ladder <= 24
    TGeoHMatrix WLL;
    WLL.SetRotation(&SensorsOnLadders->r00);
    WLL.SetTranslation(&SensorsOnLadders->t0); 
    TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%04i",Id));
    if (! WL) {
      WL = new  TGeoHMatrix(Form("WL%04i",Id)); 
      WL->SetRotation(WLL.GetRotationMatrix());
      WL->SetTranslation(WLL.GetTranslation());
      fRotList->Add(WL);
    }

    if(ladder<=0 || ladder>24) { cout << "Ladder has not been defined!" << endl; continue; }
    for (int l = 0; l < NoLadders; l++, LaddersOnIst++) {
      if(ladder == LaddersOnIst->Id){
	LI.SetRotation(&LaddersOnIst->r00);
	LI.SetTranslation(&LaddersOnIst->t0);
	break;
      }
    }
    
    WG = Tpc2Global * GL * IPOI * ISOP * LI * WLL;
    if(Debug()>2) {
      cout << "WG\t"; 
      WG.Print();
    }

    comb->SetRotation(WG.GetRotationMatrix());
    comb->SetTranslation(WG.GetTranslation());
    fRotList->Add(comb);
    if(Debug()>2){
      comb->Print();
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstPedNoise() {
  mPedNoise = (St_istPedNoise*)GetDataBase("Calibrations/ist/istPedNoise");
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstGain() {
  mGain = (St_istGain*)GetDataBase("Calibrations/ist/istGain");
}
//_____________________________________________________________________________
void StIstDbMaker::GetIstMapping() {
  mMapping = (St_istMapping*)GetDataBase("Calibrations/ist/istMapping");
}
