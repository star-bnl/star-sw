/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.28 2002/02/12 22:50:35 hardtke Exp $
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
 * Revision 1.28  2002/02/12 22:50:35  hardtke
 * separate geometrical tpc rotation from field twist
 *
 * Revision 1.27  2002/02/05 22:21:08  hardtke
 * Move Init code to InitRun
 *
 * Revision 1.26  2002/01/03 00:01:09  hardtke
 * Add switches for type of drift velocity data (i.e. laser vs. t0 analysis).  Default to use either.
 *
 * Revision 1.25  2001/10/25 22:59:36  hardtke
 * Add function tpc_localsector_to_local
 *
 * Revision 1.24  2001/10/24 21:36:20  hardtke
 * Add sim flavor option
 *
 * Revision 1.23  2001/07/27 23:52:33  hardtke
 * use Global (magnet) coordinates
 *
 * Revision 1.22  2001/06/21 16:27:52  perev
 * two error matrix transformation methods added
 *
 * Revision 1.21  2001/06/19 23:07:13  hardtke
 * Restore to old functionality using Tpc Local Coordinates
 *
 * Revision 1.20  2001/04/19 19:52:48  hardtke
 * add tpc_pad_time_offset function and add ifdef for static arrays
 *
 * Revision 1.19  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.18  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.17  2000/08/08 19:15:23  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.16  2000/07/06 21:37:34  hardtke
 * speed up tpc_pad_to_x function
 *
 * Revision 1.15  2000/05/31 19:50:16  hardtke
 * speed up tpc_time_to_z and tpc_z_to_time by factor of 5
 *
 * Revision 1.14  2000/04/11 16:06:26  hardtke
 * improve speed of tpc_row_par and tpc_global_to_sector
 *
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

#define StTpc_STATIC_ARRAYS
#include "TCL.h"
#include "StTpcDbMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
#include "math_constants.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/StDetectorDbMagnet.h"
#ifndef gufld
#define gufld gufld_
extern "C" void gufld(float *,float *);
#endif
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
  int irow = (int)(*row+0.5);
  float pitch = gStTpcDb->PadPlaneGeometry()->PadPitchAtRow(irow);
  float pad_int = floor(*pad+0.5);
  float pad_half = gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(irow)/2.0;
  float pad_diff = *pad - pad_int;
  *x = -(pad_int-pad_half-0.5+pad_diff)*pitch;
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
  //  StTpcLocalCoordinate global(xglobal[0],xglobal[1],xglobal[2]);
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
int type_of_call tpc_local_to_global_(int *isect,const float *xlocal, float* xglobal){
  StGlobalCoordinate global;
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect);
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(localSector,global); 
  xglobal[0] = global.position().x(); 
  xglobal[1] = global.position().y(); 
  xglobal[2] = global.position().z(); 
  return 1; 
}
int type_of_call tpc_localsector_to_local_(int *isect,const float *xlocal, float* xtpc){
  //translates from sector 12 coordinates to TPC local coordinates
  StTpcLocalCoordinate tpc;
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect);
  StTpcCoordinateTransform transform(gStTpcDb);
  transform(localSector,tpc); 
  xtpc[0] = tpc.position().x(); 
  xtpc[1] = tpc.position().y(); 
  xtpc[2] = tpc.position().z(); 
  return 1; 
}
int tpc_local_to_global_emx_(int &isect,const float *glocal, float* gglobal)
{
//////////////////////////////////////////////////////////////////////////////////////////
//                                                              			//
// The routine of tpc_local_to_global_emx_ computes the error matrix of space point  	//
// Arguments:		        							//
// isect	sector number       							//
// glocal       error matrix (3X3)      in local  coordinate system  (INPUT)    	//
// gglobal      error matrix (3X3)      in global coordinate system  (OUTPUT)		//
//                                                                                   	//
//////////////////////////////////////////////////////////////////////////////////////////

  static int iSECT=-1;
  static const float req[4][3] = {{0,0,0},{1,0,0},{0,1,0},{0,0,1}};
  static       float ans[4][3];

  if (iSECT != isect) {
    iSECT = isect;
    for (int i=0;i<4;i++) {
      tpc_local_to_global_(&isect,req[i], ans[i]);
      if (i) TCL::vsub(ans[i],ans[0],ans[i],3);
    }  
  }

  TCL::mxmlrt(ans[1], glocal, gglobal, 3, 3);
  return 1;
}
int tpc_local_to_global_err_(int &isect,const float *elocal, float* gglobal)
//////////////////////////////////////////////////////////////////////////////////////////
//                                                              			//
// The routine of tpc_local_to_global_err_ computes the error matrix of space point  	//
// Arguments:		        							//
// isect	sector number       							//
// elocal       array of errx,erry,errz in local  coordinate system  (INPUT)    	//
// gglobal      error matrix (3X3)      in global coordinate system  (OUTPUT)		//
//                                                                                   	//
//////////////////////////////////////////////////////////////////////////////////////////
{
   float glocal[3][3];
   TCL::vzero(glocal[0],9);
   for (int i=0;i<3;i++) glocal[i][i]=elocal[i]*elocal[i];
   return tpc_local_to_global_emx_(isect,glocal[0],gglobal);
}


int type_of_call tpc_time_to_z_(int *time,int *padin, int* row, int* sector,float *z){
  static StTpcCoordinateTransform* trans = new StTpcCoordinateTransform(gStTpcDb); 
  double zoff;
//    double tbwidth = gStTpcDb->Electronics()->samplingFrequency();
//         double timeBin = (double)*time;
//         double zztop = 
//           gStTpcDb->DriftVelocity()*1e-6*         //cm/s->cm/us
//  	 (gStTpcDb->triggerTimeOffset()*1e6 +  // units are s
//  	  gStTpcDb->Electronics()->tZero() +   // units are us 
//  	  //	  gStTpcDb->Electronics()->shapingTime()*1e-3 + //units are ns
//            (
//            gStTpcDb->T0(*sector)->getT0(*row,*padin)+
//            timeBin)/
//            (tbwidth)
//           ); 
  double zztop = trans->zFromTB(*time);
       if (*row<14){ 
	 zoff = gStTpcDb->Dimensions()->zInnerOffset(); 
       }
       else { 
	 zoff = gStTpcDb->Dimensions()->zOuterOffset(); 
       }
       *z = zztop - zoff;

//    StTpcPadCoordinate pad(*sector,*row,*padin,*time);
//    StTpcLocalSectorCoordinate localSector;
//    StTpcCoordinateTransform transform(gStTpcDb);
//    transform(pad,localSector);
//    *z = localSector.position().z();
//tph_fit_isolated_cluster wants return=1:
return 1;
}
int type_of_call tpc_z_to_time_(float* z, int* padin, int* padrow, int* sector, int* time){
  static StTpcCoordinateTransform* trans = new StTpcCoordinateTransform(gStTpcDb); 
  double zoff;
  double zin;
       if (*padrow<14){ 
	 zoff = gStTpcDb->Dimensions()->zInnerOffset(); 
       }
       else { 
	 zoff = gStTpcDb->Dimensions()->zOuterOffset(); 
       }
       zin = *z + zoff;
       *time = trans->tBFromZ(zin);

//    int temp[1];
//    *temp = 100;
//    StTpcPadCoordinate pad(*sector,*padrow,*padin,*temp);
//    StTpcLocalSectorCoordinate localSector;
//    StTpcCoordinateTransform transform(gStTpcDb);
//    transform(pad,localSector);
//    StTpcLocalSectorCoordinate localSector2(localSector.position().x(),localSector.position().y(),(double)*z,localSector.fromSector());
//    transform(localSector2,pad);
//    *time = pad.timeBucket();
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
  *a = aline[*isector-1][(int)*row-1];
  *b = bline[*isector-1][(int)*row-1];
  return 1;
}
int type_of_call tpc_global_to_sector_(int *isector, float *xglobal){
//    StGlobalCoordinate gc(xglobal[0],xglobal[1],xglobal[2]);
//    StTpcPadCoordinate pad;
//    StTpcCoordinateTransform transform(gStTpcDb);
//    transform(gc,pad);
//    *isector = pad.sector();
  float angle = atan2(xglobal[1],xglobal[0]);
  //  cout << angle << endl;
  if(angle<0)angle=angle+C_2PI;
  int isect=int((angle+C_PI_4/3.)/(C_PI_2/3.));
  if(isect==12)isect=0;
  if(xglobal[2]>0) {
    isect=15-isect;
    if(isect>12) isect=isect-12;
  }
  else{
    isect=isect+9;
    if(isect<=12) isect=isect+12;
  }
  *isector = isect;
  return 1;
}
int type_of_call tpc_sec24_to_sec12_(int *isecin, int *isecout){
  int isec24[24] = {1,2,3,4,5,6,7,8,9,10,11,12,11,10,9,8,7,6,5,4,3,2,1,12};
  if (*isecin>0&&*isecin<25) *isecout = isec24[*isecin-1];
  return 1;
}
int type_of_call tpc_pad_time_offset_(int *isec, int *irow, int *ipad, float *t0value){
  if (gStTpcDb->T0(*isec)) {
   *t0value = gStTpcDb->T0(*isec)->getT0(*irow, *ipad);
  }
  else {
   *t0value = 0;
  }
  return 1;
}
int type_of_call tpc_rdo_mask_(int *isect, int* irow){
  StDetectorDbTpcRDOMasks* mask = StDetectorDbTpcRDOMasks::instance ();
  int RDO;
  if (*irow>=1&&*irow<=8){
    RDO = 1;
  }
  else if (*irow>8&&*irow<=13){
    RDO = 2;
  }
  else if (*irow>13&&*irow<=21){
    RDO = 3;
  }
  else if (*irow>21&&*irow<=29){
    RDO = 4;
  }
  else if (*irow>29&&*irow<=37){
    RDO = 5;
  }
  else if (*irow>37&&*irow<=45){
    RDO = 6;
  }
  return (int)mask->isOn(*isect,RDO);
} 
  
//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name):StMaker(name){
 m_TpcDb = 0;
 m_dvtype = 0;
}
//_____________________________________________________________________________
StTpcDbMaker::~StTpcDbMaker(){
  //delete m_TpcDb;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Init(){


   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::InitRun(int runnumber){
   m_TpcDb = 0;
  // Set Table Flavors
   if (m_Mode==1){
     gMessMgr->Info()  << "StTpcDbMaker::Setting Sim Flavor tag for database " << endm;
     SetFlavor("sim","tpcGlobalPosition");
     SetFlavor("sim","tpcSectorPosition");
   }
   if (m_dvtype==0) {
   SetFlavor("ofl+laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using any drift velocity" << endm;
   }
   else if (m_dvtype==1) {
   SetFlavor("ofl","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from T0 analysis" << endm;
   }
   else if (m_dvtype==2) {
   SetFlavor("laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from laser analysis" << endm;
   }
   else {
     gMessMgr->Info() << "StTpcDbMaker::Undefined drift velocity flavor requested" << endm;
   }

 
//
  if (m_Mode!=1){
    float x[3] = {0,0,0};
    float b[3];
    gufld(x,b);
    float gFactor = b[2]/4.980;
    cout << "Magnetic Field = " << b[2] << endl;
   if (gFactor<-0.8) {
     gMessMgr->Info() << "StTpcDbMaker::Full Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("FullMagFNegative","tpcGlobalPosition");
   }
   else if (gFactor<-0.2) {
     gMessMgr->Info() << "StTpcDbMaker::Half Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("HalfMagFNegative","tpcGlobalPosition");
   }
   else if (gFactor<0.2) {
     gMessMgr->Info() << "StTpcDbMaker::Zero Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("ZeroMagF","tpcGlobalPosition");
   }
   else if (gFactor<0.8) {
     gMessMgr->Info() << "StTpcDbMaker::Half Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("HalfMagFPositive","tpcGlobalPosition");
   }
   else if (gFactor<1.2) {
     gMessMgr->Info() << "StTpcDbMaker::Full Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("FullMagFPositive","tpcGlobalPosition");
   }
  }

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
  //Here I fill in the arrays for the row parameterization ax+by=1
  for (int i=0;i<24;i++){
    for (int j=0;j<45;j++){
     int time[1] = {10}; 
     int ipad[2] = {20,40};
     StTpcPadCoordinate pad1(i+1, j+1, ipad[0], *time);
     StTpcPadCoordinate pad2(i+1, j+1, ipad[1], *time);
     StGlobalCoordinate gc1,gc2;
     StTpcCoordinateTransform transform(gStTpcDb);
     transform(pad1,gc1);
     transform(pad2,gc2);
     double x1,y1,x2,y2;
     double m,bb; // y = mx + bb
     x1 = gc1.position().x();
     y1 = gc1.position().y();
     x2 = gc2.position().x();
     y2 = gc2.position().y();
     if (abs(x2-x1)<0.000001) {
        aline[i][j] = 1/x1;
        bline[i][j] = 0.;
        continue;
     }
     m = (y2 - y1)/(x2 - x1);
     bb = y1 - m*x1;
     if (bb == 0) {
      gMessMgr->Warning() << "StTpcDbMaker::Init() Row intersects 0,0" << endm;
      continue;
     }
     aline[i][j] = (float) -m/bb;
     bline[i][j] = (float) 1.0/bb;
    }
  }
  return 0;
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

//---------------------------------------------------------------------------
void StTpcDbMaker::Clear(const char *opt){
  if (m_TpcDb) m_TpcDb->Clear();
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





