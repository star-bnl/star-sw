// $Id: StPxlDbMaker.cxx,v 1.7 2013/09/22 15:02:43 bouchet Exp $
// $Log: StPxlDbMaker.cxx,v $
// Revision 1.7  2013/09/22 15:02:43  bouchet
// sector 10 was missing
//
// Revision 1.6  2013/09/16 16:45:06  bouchet
// clean up
//
// Revision 1.5  2013/09/14 17:53:01  bouchet
// methods to retrieve PXL masks tables
//
// Revision 1.3  2013/06/21 21:12:26  qiuh
// *** empty log message ***
//
// Revision 1.2  2013/06/20 19:19:01  bouchet
// update for pxlSensorRowColumnMask tables
//
// Revision 1.1  2013/05/24 15:59:52  bouchet
// first version
//
/***************************************************************************
 * Authors: J. Bouchet, M. Lomnitz , KSU
 * Description: PXL DB access Maker
 **************************************************************************/

/*
  relation within STAR frame
  PixelOnGlobal = Tpc2Magnet * Ids2Tpc *    Pxl2Ids *      DShell2Ids * Sector2DShell * Ladder2Sector * Sensor2Ladder * PS
  
  with 
  
  Ids2Tpc = PxlIdsOnTpc
  Pxl2Ids = PxlPxlOnPst * PxlPstOnIds
  
  Naming of roatation matrices in this maker :
  PG            = Tpc2Global * GL      *  PSOI * PXOP *    DP              * SD            * LS        *      WLL     *  PS
  
  numbering : 
  Id  = (sector-1)*40 + (ladder-1)*10 + sensor 
  1<= sector <= 10
  1<= ladder <= 4
  1<= sensor <= 10
*/

#include "StPxlDbMaker.h"
#include "StPxlUtil/StPxlConstants.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_Survey_Table.h"
#include "TMath.h"
#include "TVector3.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_pxlSensorStatus_Table.h" 
#include "tables/St_pxlRowColumnStatus_Table.h" 
StPxlDbMaker* gStPxlDbMaker=NULL; 
THashList *StPxlDbMaker::fRotList = 0;

ClassImp(StPxlDbMaker)
//_____________________________________________________________________________
StPxlDbMaker::StPxlDbMaker(const char *name) : 
  StMaker(name){gStPxlDbMaker = this;
}
//_____________________________________________________________________________
StPxlDbMaker::~StPxlDbMaker() {gStPxlDbMaker = 0;}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Init()
{
  LOG_DEBUG << " number of sectors : " << nPxlSectors << endm;
  LOG_DEBUG << " number of ladders per sector : " << nPxlLaddersPerSector << endm;
  LOG_DEBUG << " number of sensors per ladder : " << nPxlSensorsPerLadder << endm;
  LOG_DEBUG << "Init done" << endm;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::InitRun(Int_t runNumber) {
  CalculateSensorsPosition(); 
  LOG_DEBUG <<" StPxlDbMaker::InitRun() --> GetPxlSensorStatus" << endm;
  GetPxlSensorStatus();
  LOG_DEBUG <<" StPxlDbMaker::InitRun() --> GetPxlRowColumnStatus" << endm;
  GetPxlRowColumnStatus(); 
  return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Make()
{
  LOG_DEBUG << "Make" << endm;
  return kStOK;
}
//_____________________________________________________________________________
void StPxlDbMaker::Clear(const char*)
{
  LOG_DEBUG << "Clear" << endm;
  StMaker::Clear();
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::Finish()
{
  LOG_DEBUG << "Finish" << endm;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StPxlDbMaker::CalculateSensorsPosition(){
  SafeDelete(fRotList);
  fRotList = new THashList(nPxlSectors*nPxlLaddersPerSector*nPxlSensorsPerLadder,0);
  fRotList->SetOwner(kFALSE);

  TGeoHMatrix GL, LS, SD, PSOI , PXOP, DP;

  //get TPC positionement relative to STAR
  assert(gStTpcDb);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

  St_Survey *idsOnTpc          = (St_Survey *) GetDataBase("Geometry/pxl/idsOnTpc");         //GL
  if (! idsOnTpc)          {cout << "idsOnTpc has not been found"  << endl; return 0;}

  St_Survey *pstOnIds          = (St_Survey *) GetDataBase("Geometry/pxl/pstOnIds");         //PSOI
  if (! pstOnIds)          {cout << "pstOnIds has not been found"  << endl; return 0;}

  St_Survey *pxlOnPst          = (St_Survey *) GetDataBase("Geometry/pxl/pxlOnPst");         //PXOP
  if (! pxlOnPst)          {cout << "pxlOnPst has not been found"  << endl; return 0;}

  St_Survey *pxlHalfOnPxl         = (St_Survey *) GetDataBase("Geometry/pxl/pxlHalfOnPxl");        //DP
  if (! pxlHalfOnPxl)         {cout << "pxlHalfOnPxl has not been found"  << endl; return 0;}

  St_Survey *pxlSectorOnHalf     = (St_Survey *) GetDataBase("Geometry/pxl/pxlSectorOnHalf");    //SD
  if (! pxlSectorOnHalf)     {cout << "pxlSectorOnHalf has not been found"  << endl; return 0;}

  St_Survey *pxlLadderOnSector  = (St_Survey *) GetDataBase("Geometry/pxl/pxlLadderOnSector"); //LS
  if (! pxlLadderOnSector)  {cout << "pxladderOnSector has not been found"  << endl; return 0;}

  St_Survey *pxlSensorOnLadder  = (St_Survey *) GetDataBase("Geometry/pxl/pxlSensorOnLadder"); //WLL
  if (! pxlSensorOnLadder)  {cout << "pxlSensorOnLadder has not been found"  << endl; return 0;}

  //get these tables
  Survey_st *IdsOnTpc          = idsOnTpc->GetTable();   
  Survey_st *PstOnIds          = pstOnIds->GetTable();   
  Survey_st *PxlOnPst          = pxlOnPst->GetTable();   
  Survey_st *HalfOnPxl         = pxlHalfOnPxl->GetTable();   
  Survey_st *SectorsOnHalf     = pxlSectorOnHalf->GetTable();   
  Survey_st *LaddersOnSectors  = pxlLadderOnSector->GetTable();   
  Survey_st *SensorsOnLadders  = pxlSensorOnLadder->GetTable();   

  //get the number of rows of each tables
  Int_t NoIds                  = idsOnTpc->GetNRows();
  Int_t NoPst                  = pstOnIds->GetNRows();
  Int_t NoPxl                  = pxlOnPst->GetNRows();
  Int_t NoDShell               = pxlHalfOnPxl->GetNRows();
  Int_t NoSectors              = pxlSectorOnHalf->GetNRows();
  Int_t NoLadders              = pxlLadderOnSector->GetNRows();
  Int_t NoSensors              = pxlSensorOnLadder->GetNRows();
  
  LOG_DEBUG <<" # of Ids     : " << NoIds << endm;
  LOG_DEBUG <<" # of Pst     : " << NoPst << endm;
  LOG_DEBUG <<" # of Pxl     : " << NoPxl<< endm;
  LOG_DEBUG <<" # of DShell  : " << NoDShell<< endm;
  LOG_DEBUG <<" # of Sectors : " << NoSectors << endm;
  LOG_DEBUG <<" # of Ladders : " << NoLadders << endm;
  LOG_DEBUG <<" # of Sensors : " << NoSensors << endm;

  GL.SetRotation(&IdsOnTpc->r00);
  GL.SetTranslation(&IdsOnTpc->t0);

  PSOI.SetRotation(&PstOnIds->r00);
  PSOI.SetTranslation(&PstOnIds->t0);

  PXOP.SetRotation(&PxlOnPst->r00);
  PXOP.SetTranslation(&PxlOnPst->t0);

  if(Debug()>2){
    GL.Print();
    PSOI.Print();
    PXOP.Print();
  }

  for (Int_t i = 0; i < NoSensors; i++,SensorsOnLadders++) {
    Int_t Id = SensorsOnLadders->Id;
    TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%03i",Id));
    if (comb) continue;
    comb = new TGeoHMatrix(Form("R%03i",Id));
    // numbering of sensors in the RotMatrices() list is :
    // id = [1-10]*40 + [1-4]*10 + sensor
    Int_t sector = (Id-1)/NoLadders + 1;
    Int_t ladder = (Id-1)%NoLadders/NoSectors + 1;
    TGeoHMatrix WLL;
    WLL.SetRotation(&SensorsOnLadders->r00);
    WLL.SetTranslation(&SensorsOnLadders->t0); 
    TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%03i",Id));
    if (! WL) {
      WL = new  TGeoHMatrix(Form("WL%03i",Id)); 
      Double_t *r = WLL.GetRotationMatrix();   
      Double_t rot[9] = {r[0], r[1], r[2],   
			 r[3], r[4], r[5],   
			 r[6], r[7], r[8]};
      WL->SetRotation(rot);
      WL->SetTranslation(WLL.GetTranslation());
      fRotList->Add(WL);
    }
    LaddersOnSectors = pxlLadderOnSector->GetTable();
    for (Int_t l = 0; l < NoLadders; l++, LaddersOnSectors++) {
      Int_t SECTOR = (LaddersOnSectors->Id-1)/nPxlLaddersPerSector + 1;
      Int_t LADDER = (LaddersOnSectors->Id-1)%nPxlLaddersPerSector + 1;
      if ((ladder == LADDER) && (sector == SECTOR)){
	LS.SetRotation(&LaddersOnSectors->r00);
	LS.SetTranslation(&LaddersOnSectors->t0);
	break;
      }
    }
    if (sector <=0  || sector >NoSectors) {cout << "Sector has not been defined" << endl; continue;}
    SectorsOnHalf = pxlSectorOnHalf->GetTable();
    for (Int_t s = 0; s <NoSectors; s++, SectorsOnHalf++) {
      if (SectorsOnHalf->Id != (sector)) continue; //1<=SectorsOnHalf<=10
      SD.SetRotation(&SectorsOnHalf->r00);
      SD.SetTranslation(&SectorsOnHalf->t0); 
      break;
    }

    // numbering is :
    //sector 1 to 5  --> half 1
    //sector 6 to 10 --> half 2
    
    HalfOnPxl = pxlHalfOnPxl->GetTable();
    Int_t HALF = 0;
    for (Int_t t = 0; t <NoDShell; t++, HalfOnPxl++) {
      (sector>=1 && sector<=NoSectors/2)?HALF=1:HALF=2;
      if (HalfOnPxl->Id != HALF) continue; 
      DP.SetRotation(&HalfOnPxl->r00);
      DP.SetTranslation(&HalfOnPxl->t0); 
      break;
    }   
    
    TGeoHMatrix WG = Tpc2Global * GL * PSOI * PXOP * DP * SD * LS * WLL; 
    Double_t *r    = WG.GetRotationMatrix();
    Double_t *wgtr = WG.GetTranslation();
    Double_t rot[9] = 
      {
	r[0], r[1], r[2],
	r[3], r[4], r[5],
	r[6] ,r[7], r[8]
      };
    
    Double_t tr[3] = {wgtr[0], wgtr[1], wgtr[2]};
    comb->SetRotation(rot);
    comb->SetTranslation(tr);
    fRotList->Add(comb);
    if(Debug()>2){
      comb->Print();
    }
  }
  return kStOk;
}
//_______________________________________________
void StPxlDbMaker::GetPxlSensorStatus(){
  mSensorStatus = (St_pxlSensorStatus*)GetDataBase("Calibrations/pxl/pxlSensorStatus"); 
  if(mSensorStatus){ 
    pxlSensorStatus_st *g = mSensorStatus->GetTable() ;
  }
}
//_______________________________________________
void StPxlDbMaker::GetPxlRowColumnStatus(){
  mRowColumnStatus = (St_pxlRowColumnStatus*)GetDataBase("Calibrations/pxl/pxlRowColumnStatus");
  if(mRowColumnStatus){ 
    pxlRowColumnStatus_st *gg = mRowColumnStatus->GetTable() ;
  }
}
