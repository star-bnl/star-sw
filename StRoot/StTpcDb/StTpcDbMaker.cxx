/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.13 2000/02/24 18:21:51 hardtke Exp $
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
 * Revision 1.13  2000/02/24 18:21:51  hardtke
 * re-define drift distance as central membrane to gating grid
 *
 * Revision 1.12  2000/02/23 22:21:09  hardtke
 * add tpc_global_to_local_p
 *
 * Revision 1.11  2000/02/23 21:03:17  hardtke
 * fix tpc_row_par -- causing tpt problems
 *
 * Revision 1.10  2000/02/23 15:09:57  hardtke
 * move tpg_detector and tpg_pad_plane from .const to .data
 *
 * Revision 1.9  2000/02/22 19:40:30  hardtke
 * fix tpc_row_par to give expected results
 *
 * Revision 1.8  2000/02/17 19:43:20  hardtke
 * fixes to tpc functions
 *
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
#include "math_constants.h"
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
  return 1;
}
int type_of_call tpc_pad_to_x_(float *pad,float *row, float* x) {
  StTpcPadCoordinate raw(12,(int)*row,(int)floor(*pad),1); //sector 12, row row, pad pad, bucket 1
  StTpcLocalSectorCoordinate localSector;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(raw,localSector);
  float offset = fmod(*pad,floor(*pad));
  *x = localSector.position().x() - offset*gStTpcDb->PadPlaneGeometry()->PadPitchAtRow((int)*row);
  return 1;
}
int type_of_call tpc_x_to_pad_(float *row,float *x, float* pad) {
  //copy code from tgc_x_to_pad.F
  int irow = (int)(*row+0.5);
  *pad = -(*x)/gStTpcDb->PadPlaneGeometry()->PadPitchAtRow(irow) + 0.5 + 0.5*gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(irow);
  return 1;
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
  return 1; 
}
int type_of_call tpc_global_to_local_p_(int *isect,float *xglobal, float* xlocal){
  float b_rot, ff;
  if (*isect<=12){
    b_rot = (float)(*isect)*(C_PI_2/3.);
    ff = -1.0;
  }
  else{
    b_rot = (float)(24. - *isect)*(C_PI_2/3.);
    ff = 1.0;
  }
  xlocal[0] = ff*(xglobal[0]*cos(b_rot)-xglobal[1]*sin(b_rot));  
  xlocal[1] = xglobal[0]*sin(b_rot)+xglobal[1]*cos(b_rot)  ;
  if (*isect<=12) xlocal[2] = -xlocal[2];
  return 1; 
}
int type_of_call tpc_local_to_global_(int *isect,float *xlocal, float* xglobal){
  StGlobalCoordinate global;
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect);
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(localSector,global); 
  xglobal[0] = global.position().x(); 
  xglobal[1] = global.position().y(); 
  xglobal[2] = global.position().z(); 
  return 1; 
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
int type_of_call tpc_z_to_time_(float* z, int* padin, int* padrow, int* sector, int* time){
  int temp[1];
  *temp = 100;
  StTpcPadCoordinate pad(*sector,*padrow,*padin,*temp);
  StTpcLocalSectorCoordinate localSector;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(pad,localSector);
  StTpcLocalSectorCoordinate localSector2(localSector.position().x(),localSector.position().y(),(double)*z,localSector.fromSector());
  transform(localSector2,pad);
  *time = pad.timeBucket();
  return 1;
}
int type_of_call tpc_drift_velocity_(float *dvel){
*dvel = gStTpcDb->DriftVelocity();
return 1;
}
int type_of_call tpc_drift_volume_length_(float *length){
  *length = gStTpcDb->Dimensions()->gatingGridZ();
  return 1;
}
int type_of_call tpc_row_par_(int *isector, float *row, float *a, float *b){
  //return ax+by = 1 parameterization of a TPC row
  int time[1] = {10};
  int ipad[2] = {20,40};
  StTpcPadCoordinate pad1(*isector, (int)*row, ipad[0], *time);
  StTpcPadCoordinate pad2(*isector, (int)*row, ipad[1], *time);
  StGlobalCoordinate gc1,gc2;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(pad1,gc1);
  transform(pad2,gc2);
  float x1,y1,x2,y2;
  float m,bb; // y = mx + bb
  x1 = gc1.position().x();
  y1 = gc1.position().y();
  x2 = gc2.position().x();
  y2 = gc2.position().y();
  if (abs(x2-x1)<0.000001) {
     *a = 1/x1;
     *b = 0.;
     return 1;
  }
  m = (y2 - y1)/(x2 - x1);
  bb = y1 - m*x1;
  if (bb == 0) return 0;
  *a = -m/bb;
  *b = 1/bb;
  return 1;
}
int type_of_call tpc_global_to_sector_(int *isector, float *xglobal){
  StGlobalCoordinate gc(xglobal[0],xglobal[1],xglobal[2]);
  StTpcPadCoordinate pad;
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(gc,pad);
  *isector = pad.sector();
  return 1;
}
int type_of_call tpc_sec24_to_sec12_(int *isecin, int *isecout){
  int isec24[24] = {1,2,3,4,5,6,7,8,9,10,11,12,11,10,9,8,7,6,5,4,3,2,1,12};
  if (*isecin>0&&*isecin<25) *isecout = isec24[*isecin-1];
  return 1;
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
   if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
  m_tpg_pad_plane = new St_tpg_pad_plane("tpg_pad_plane",1);
  m_tpg_pad_plane->SetNRows(1);
  m_tpg_detector = new St_tpg_detector("tpg_detector",1);
  m_tpg_detector->SetNRows(1);
  AddConst(m_tpg_pad_plane);
  AddConst(m_tpg_detector);
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
   Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
   Update_tpg_detector();
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
     pp[0].drift_length = tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
     pp[0].clock_frequency = 1e6*tpcDbInterface()->Electronics()->samplingFrequency();
     pp[0].z_inner_offset = tpcDbInterface()->Dimensions()->innerEffectiveDriftDistance() - tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
     pp[0].vdrift = tpcDbInterface()->DriftVelocity();
 }
}


