/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.7 2000/02/10 00:29:09 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *This make initializes and controls the TPC interface to the database
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.cxx,v $
 * Revision 1.7  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.6  2000/01/11 15:49:53  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/

#include "StTpcDbMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
ClassImp(StTpcDbMaker)

//
//C and Fortran routines:
//________________________________________
int type_of_call numberOfPadsAtRow_(float *row) {
    return gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow((int)*row);
}
int type_of_call tpc_row_to_y_(float *row,float *y) {
  StTpcPadCoordinate raw(12,(int)*row,1,1); //sector 12, row row, pad 1, bucket 1
  StTpcLocalSectorCoordinate localSector;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(raw,localSector);
  *y = localSector.position().y();
  return 0;
}
int type_of_call tpc_pad_to_x_(float *pad,float *row, float* x) {
  StTpcPadCoordinate raw(12,(int)*row,(int)*pad,1); //sector 12, row row, pad pad, bucket 1
  StTpcLocalSectorCoordinate localSector;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(raw,localSector);
  float offset = *pad - (float)(int)*pad;
  *x = localSector.position().x() + offset*gStTpcDb->PadPlaneGeometry()->PadPitchAtRow((int)*row);
  return 0;
}
int type_of_call tpc_global_to_local_(int *isect,float *xglobal, float* xlocal){
  StGlobalCoordinate global(xglobal[0],xglobal[1],xglobal[2]);
  StTpcLocalSectorCoordinate localSector;
  StTpcPadCoordinate pad;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(global,localSector); 
  transform(global,pad);
  *isect = pad.sector();
  xlocal[0] = localSector.position().x(); 
  xlocal[1] = localSector.position().y(); 
  xlocal[2] = localSector.position().z();
  return 0; 
}
int type_of_call tpc_local_to_global_(int *isect,float *xlocal, float* xglobal){
  StGlobalCoordinate global;
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect);
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(localSector,global); 
  xglobal[0] = global.position().x(); 
  xglobal[1] = global.position().y(); 
  xglobal[2] = global.position().z(); 
  return 0; 
}
int type_of_call tpc_time_to_z_(int *time,int *padin, int* row, int* sector,float *z){
StTpcPadCoordinate pad(*sector,*row,*padin,*time);
StTpcLocalSectorCoordinate localSector;
StTpcCoordinateTransform transform(gStTpcDb);
transform(pad,localSector);
*z = localSector.position().z();
//tph_fit_isolated_cluster wants return=1:
return 1;
}
int type_of_call tpc_drift_velocity_(float *dvel){
*dvel = gStTpcDb->DriftVelocity();
return 0;
}

//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name):StMaker(name){
 m_TpcDb = 0;
}
//_____________________________________________________________________________
StTpcDbMaker::~StTpcDbMaker(){
  //delete m_TpcDb;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Init(){

   m_TpcDb = 0;
// Create Needed Tables:    
   m_tpg_pad_plane = new St_tpg_pad_plane("tpg_pad_plane",1);
   m_tpg_pad_plane->SetNRows(1);
   AddConst(m_tpg_pad_plane);
   m_tpg_detector = new St_tpg_detector("tpg_detector",1);
   m_tpg_detector->SetNRows(1);
   AddConst(m_tpg_detector);
   if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
//
   return StMaker::Init();
}
//_____________________________________________________________________________

Int_t StTpcDbMaker::Make(){

  if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
   Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
   Update_tpg_detector();
  return kStOK;
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_pad_plane(){
  if (m_tpg_pad_plane) {
    St_tpg_pad_plane &pp = *m_tpg_pad_plane;
    pp[0].nrow_in = tpcDbInterface()->PadPlaneGeometry()->numberOfInnerRows();
    pp[0].nrow_out = tpcDbInterface()->PadPlaneGeometry()->numberOfOuterRows();
    pp[0].pad_len_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadLength();
    pp[0].pad_len_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadLength();
    pp[0].pad_sep_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPitch();
    pp[0].pad_sep_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPitch();
    pp[0].pad_wid_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadWidth();
    pp[0].pad_wid_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadWidth();
    pp[0].nsect = tpcDbInterface()->Dimensions()->numberOfSectors();
    for (int i=1;i<=tpcDbInterface()->PadPlaneGeometry()->numberOfRows();i++){
      pp[0].npads[i-1] = tpcDbInterface()->PadPlaneGeometry()->numberOfPadsAtRow(i);
      pp[0].rad[i-1] = tpcDbInterface()->PadPlaneGeometry()->radialDistanceAtRow(i);
    }
  }
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_detector(){
 if (m_tpg_detector) {
     St_tpg_detector &pp = *m_tpg_detector;
     pp[0].nsectors = 2*tpcDbInterface()->Dimensions()->numberOfSectors();
           // note tpg table define number of sectors as 48
     pp[0].drift_length = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPlaneZ();
     pp[0].clock_frequency = 1e6*tpcDbInterface()->Electronics()->samplingFrequency();
     pp[0].z_inner_offset = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPlaneZ()-tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPlaneZ();
     pp[0].vdrift = tpcDbInterface()->DriftVelocity();
 }
}

