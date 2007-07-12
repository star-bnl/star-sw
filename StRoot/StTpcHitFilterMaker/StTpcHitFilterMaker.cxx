// $Id: StTpcHitFilterMaker.cxx,v 1.6 2007/07/12 20:21:43 fisyak Exp $
// $Log: StTpcHitFilterMaker.cxx,v $
// Revision 1.6  2007/07/12 20:21:43  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.5  2007/04/28 17:57:21  perev
// Redundant StChain.h removed
//
// Revision 1.4  2003/09/02 17:59:13  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2001/04/13 21:34:46  hardtke
// Add option to disable hit deletion
//
// Revision 1.2  2001/04/12 22:04:56  hardtke
// Add option for setting large hit errors
//
// Revision 1.1  2001/03/22 19:55:30  hardtke
// Initial version of hit filtering maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcHitFilterMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <math.h>
#include "StTpcHitFilterMaker.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "TString.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tpc/St_tfs_filt_Module.h"

ClassImp(StTpcHitFilterMaker)
  
  //_____________________________________________________________________________
  StTpcHitFilterMaker::StTpcHitFilterMaker(const char *name):
    StMaker(name)
{
 for (int i = 0;i<24;i++){
  SectorOn[i] = kTRUE;
 }
 for (int i = 0;i<45;i++){
  RowOn[i] = kTRUE;
 }
 z_min = -999.;
 z_max = 999.;
 minrow = 1;
 maxrow = 45;
 membrane_cut = -10;
 BigErrorsInner = kFALSE;
 BigErrorsOuter = kFALSE;
 DeleteHits = kTRUE;
}
//----------------------------------------------------------------------------
StTpcHitFilterMaker::~StTpcHitFilterMaker(){}
//_____________________________________________________________________________
Int_t StTpcHitFilterMaker::Init(){
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StTpcHitFilterMaker::Make(){
  
  St_DataSet *tpc_data =  GetInputDS("tpc_hits");
  
  if (!tpc_data) return 0;
  
// 		Clusters exist -> do tracking
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  if (! tphit) return kStWarn;
  gMessMgr->QAInfo() << Form(" Input hit table size is %d\n\n",(int)tphit->GetNRows()) << endm;

  float xhit;
  float yhit;
  float zhit;
  float delx;
  float dely;
  float delz;
  int sectorhit;
  int rowhit;
  int iflaghit;
  tcl_tphit_st *spc = tphit -> GetTable() ;
    for ( Int_t i = 0 ; i < tphit->GetNRows() ; i++ , spc++ )
      {
        sectorhit = (spc->row)/100;
        rowhit    = (spc->row)%100;
        iflaghit  = spc->flag;
	xhit = spc -> x;    
	yhit = spc -> y;    
	zhit = spc -> z;
        delx = spc -> dx;
        dely = spc -> dy;
        delz = spc -> dz;
        if (rowhit<=13&&BigErrorsInner) {
	  spc->dx = 100*delx;
          spc->dy = 100*dely;
          spc->dz = 100*delz;
        }
        if (rowhit>13&&BigErrorsOuter) {
	  spc->dx = 100*delx;
          spc->dy = 100*dely;
          spc->dz = 100*delz;
        }
	if (DeleteHits) {
	 if(zhit>z_max||zhit<z_min||rowhit<minrow||rowhit>maxrow||(!RowOn[rowhit-1])||(!SectorOn[sectorhit-1]) ){
	  if (iflaghit>=0) iflaghit = -123;
	   spc->flag = iflaghit;  //remove hit for tracking
         }
	   if ((sectorhit<13&&zhit<membrane_cut)||(sectorhit>12&&zhit>-membrane_cut)){
	   if (iflaghit>=0) iflaghit = -123;
	   spc->flag = iflaghit;  //remove hit for tracking
           }
        }
      }  

    //now discard the hits:

    if (DeleteHits){
     Int_t Res_tfs_filt = tfs_filt(tphit);
      if ( Res_tfs_filt !=  kSTAFCV_OK){ 
	 gMessMgr->Info() << " Problem running tfs_filt..." << endm;
      }
    }
    gMessMgr->QAInfo() << Form(" Output hit table size is %d\n\n",(int)tphit->GetNRows()) << endm;


  return kStOK;
}
//_____________________________________________________________________________
Int_t StTpcHitFilterMaker::Finish(){
  return kStOK;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetZrange(float min, float max){
  SetZmin(min);
  SetZmax(max);
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetZmax(float max){
  gMessMgr->Info() << "StTpcHitFilterMaker::Maximum Z for hits = " << max << endm;
  z_max = max;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetZmin(float min){
  gMessMgr->Info() << "StTpcHitFilterMaker::Minimum Z for hits = " << min << endm;
  z_min = min;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetMembraneCut(float cut){
  gMessMgr->Info() << "StTpcHitFilterMaker::Membrane Cut abs(z)> " << cut << endm;
  membrane_cut = cut;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::DisableInner(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Disabling inner rows 1-13" << endm;
  for (int i=0;i<13;i++){
    RowOn[i] = kFALSE;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EnableInner(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Enabling inner rows 1-13" << endm;
  for (int i=0;i<13;i++){
    RowOn[i] = kTRUE;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::DisableOuter(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Disabling outer rows 14-45" << endm;
  for (int i=13;i<45;i++){
    RowOn[i] = kFALSE;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EnableOuter(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Enabling outer rows 14-45" << endm;
  for (int i=13;i<45;i++){
    RowOn[i] = kTRUE;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetMinRow(int row){
  if (row>=1&&row<=45){
    gMessMgr->Info() << "StTpcHitFilterMaker::Minimum Row = " << row << endm;
    minrow = row;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Minimum Row = " << row << endm;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetMaxRow(int row){
  if (row>=1&&row<=45){
    gMessMgr->Info() << "StTpcHitFilterMaker::Maximum Row = " << row << endm;
    maxrow = row;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Maximum Row = " << row << endm;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::SetRowRange(int min_row, int max_row){
  SetMinRow(min_row);
  SetMaxRow(max_row);
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::DisableRow(int row){
  if (row>=1&&row<=45){
    gMessMgr->Info() << "StTpcHitFilterMaker::Disabling Row = " << row << endm;
    RowOn[row-1] = kFALSE;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Row = " << row << endm;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EnableRow(int row){
  if (row>=1&&row<=45){
    gMessMgr->Info() << "StTpcHitFilterMaker::Enabling Row = " << row << endm;
    RowOn[row-1] = kTRUE;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Row = " << row << endm;
  }
  
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::DisableSector(int sec){
  if (sec>=1&&sec<=24){
    gMessMgr->Info() << "StTpcHitFilterMaker::Disabling Sector = " << sec << endm;
    SectorOn[sec-1] = kFALSE;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Sector = " << sec << endm;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EnableSector(int sec){
  if (sec>=1&&sec<=24){
    gMessMgr->Info() << "StTpcHitFilterMaker::Enabling Sector = " << sec << endm;
    SectorOn[sec-1] = kTRUE;
  }
  else{
    gMessMgr->Info() << "StTpcHitFilterMaker::Invalid Sector = " << sec << endm;
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EastOn(){
    gMessMgr->Info() << "StTpcHitFilterMaker::Enabling East TPC " <<  endm;
  for (int i=13;i<=24;i++){
    EnableSector(i);
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::EastOff(){
    gMessMgr->Info() << "StTpcHitFilterMaker::Disabling East TPC " <<  endm;
  for (int i=13;i<=24;i++){
    DisableSector(i);
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::WestOn(){
    gMessMgr->Info() << "StTpcHitFilterMaker::Enabling West TPC " <<  endm;
  for (int i=1;i<=12;i++){
    EnableSector(i);
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::WestOff(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Disabling West TPC " <<  endm;
  for (int i=1;i<=12;i++){
    DisableSector(i);
  }
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::RidiculousErrorsInner(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Setting Huge Errors in Inner section " <<  endm;
  BigErrorsInner = kTRUE;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::RidiculousErrorsOuter(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Setting Huge Errors in Outer section " <<  endm;
  BigErrorsOuter = kTRUE;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::DoNotDeleteHits(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Disabling Hit Deletion " <<  endm;
  DeleteHits = kFALSE;
}
//-----------------------------------------------------------------------------
void StTpcHitFilterMaker::CanDeleteHits(){
  gMessMgr->Info() << "StTpcHitFilterMaker::Enabling Hit Deletion " <<  endm;
  DeleteHits = kTRUE;
}


