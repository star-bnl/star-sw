/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.46 2009/08/11 20:38:04 genevb Exp $
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
 * Revision 1.46  2009/08/11 20:38:04  genevb
 * slightly more detailed message
 *
 * Revision 1.45  2009/05/01 19:09:23  fisyak
 * StTpcDbMaker::Make is aware about TPC trips and generagte EoF when this happenss
 *
 * Revision 1.44  2008/09/10 15:46:37  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.43  2008/08/01 14:28:34  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.42  2008/07/10 20:25:31  fisyak
 * Warn of
 *
 * Revision 1.41  2007/12/28 00:30:06  fine
 * Add a function to calculate the tpc coord transfoirmation in one step
 *
 * Revision 1.40  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.39  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.38  2007/04/28 17:57:19  perev
 * Redundant StChain.h removed
 *
 * Revision 1.37  2007/03/21 17:27:02  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.36  2006/02/27 19:20:53  fisyak
 * Set simu flag for tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.35  2005/03/30 17:56:59  fisyak
 * Fix a bug with flavor handling, StTpcDb has to be instantiated after setting flavor
 *
 * Revision 1.34  2004/10/27 21:45:13  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.33  2004/06/05 23:38:22  fisyak
 * Add more chairs for TPC Db parameters
 *
 * Revision 1.32  2004/05/03 23:29:28  perev
 * WarnOff
 *
 * Revision 1.31  2003/04/10 21:30:59  hardtke
 * Allow multiple InitRun calls
 *
 * Revision 1.30  2003/01/12 20:38:23  jeromel
 * fabs() not abs() for doube
 *
 * Revision 1.29  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
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
#include <assert.h>
#include "StTpcDbMaker.h"
#include "StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
#include "tables/St_MagFactor_Table.h"
#include "math_constants.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/StDetectorDbMagnet.h"
#include "StDetectorDbMaker/St_tpcAnodeHVC.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
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
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect,0);
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
  StTpcLocalSectorCoordinate localSector(xlocal[0],xlocal[1],xlocal[2],*isect,0);
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
       *time = (Int_t )trans->tBFromZ(zin);

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
  if (gStTpcDb->tpcT0()) {
    *t0value = gStTpcDb->tpcT0()->T0(*isec,*irow, *ipad);
  }
  else {
   *t0value = 0;
  }
  return 1;
}
int type_of_call tpc_rdo_mask_(int *isect, int* irow){
  StDetectorDbTpcRDOMasks* mask = StDetectorDbTpcRDOMasks::instance ();
  int RDO=0;
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
  assert(RDO);
  return (int)mask->isOn(*isect,RDO);
} 
  
int type_of_call tpc_hit_error_table_(int *i, int*j, int *k,float *val){
  //The first index is inner(1), outer (2)
  //The second index is x(1), z(2)
  //The third index is the parameter type: intrinsic(1), drift(2), tan(3)
  //To switch to a C-array, I subtract 1
  float table[2][2][3];
  table[0][0][0] = gStTpcDb->HitErrors()->Sig2IntrinsicInnerX();
  table[0][1][0] = gStTpcDb->HitErrors()->Sig2IntrinsicInnerZ();
  table[0][0][1] = gStTpcDb->HitErrors()->Sig2DriftInnerX();
  table[0][1][1] = gStTpcDb->HitErrors()->Sig2DriftInnerZ();
  table[0][0][2] = gStTpcDb->HitErrors()->Sig2TanInnerX();
  table[0][1][2] = gStTpcDb->HitErrors()->Sig2TanInnerZ();

  table[1][0][0] = gStTpcDb->HitErrors()->Sig2IntrinsicOuterX();
  table[1][1][0] = gStTpcDb->HitErrors()->Sig2IntrinsicOuterZ();
  table[1][0][1] = gStTpcDb->HitErrors()->Sig2DriftOuterX();
  table[1][1][1] = gStTpcDb->HitErrors()->Sig2DriftOuterZ();
  table[1][0][2] = gStTpcDb->HitErrors()->Sig2TanOuterX();
  table[1][1][2] = gStTpcDb->HitErrors()->Sig2TanOuterZ();
  *val = table[*i-1][*j-1][*k-1];
  return 1;

}

int type_of_call tpc_hit_postion(int sector, int row, int pad, int timebucket, float &x,float &y,float &z)
{
  StTpcCoordinateTransform transform(gStTpcDb);
  // (const int sector, const int row, const int pad, const int tb) 
  StTpcPadCoordinate padcoord(sector, row, pad, timebucket);
  StGlobalCoordinate global;
  transform(padcoord,global);
  StThreeVector<double> p = global.position();
  x = p.x();
  y = p.y();
  z = p.z();
  return 0;
}

//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name): StMaker(name), m_TpcDb(0), m_tpg_pad_plane(0), m_tpg_detector(0) {}
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
    if (m_TpcDb) return 0;
    // Create Needed Tables:    
    TDataSet *RunLog = GetDataBase("RunLog");                              assert(RunLog);
    St_MagFactor *fMagFactor = (St_MagFactor *) RunLog->Find("MagFactor"); assert(fMagFactor);
    Float_t gFactor = (*fMagFactor)[0].ScaleFactor;
    gMessMgr->Info() << "StTpcDbMaker::Magnetic Field gFactor = " << gFactor << endm;
    if (fabs(gFactor)>0.8){
      gMessMgr->Info() << "StTpcDbMaker::Using full field TPC hit errors" << endm;
      SetFlavor("FullMagF","tpcHitErrors");
    }
    else {
      gMessMgr->Info() << "StTpcDbMaker::Using half field TPC hit errors" << endm;
      SetFlavor("HalfMagF","tpcHitErrors");
    }

  // Set Table Flavors
   if (m_Mode%1000000==1){
     const Char_t *tabNames[5] = {"tpcGlobalPosition","tpcSectorPosition", "tpcISTimeOffsets", 
				  "tpcOSTimeOffsets","starClockOnl"};
     for (Int_t i = 0; i < 5; i++) {
       SetFlavor("sim",tabNames[i]); 
       gMessMgr->Info()  << "StTpcDbMaker::Setting Sim Flavor tag for table " << "\t" << tabNames[i] << endm;
     }
   }
   if ((m_Mode/1000000)%10==0) {
   SetFlavor("ofl+laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using any drift velocity" << endm;
   }
   else if ((m_Mode/1000000)%10==1) {
   SetFlavor("ofl","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from T0 analysis" << endm;
   }
   else if ((m_Mode/1000000)%10==2) {
   SetFlavor("laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from laser analysis" << endm;
   }
   else if ((m_Mode/1000000)%10==3) {
   SetFlavor("NewlaserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from New laser analysis" << endm;
   }
   else {
     gMessMgr->Info() << "StTpcDbMaker::Undefined drift velocity flavor requested" << endm;
   }
   
 
//
  if (m_Mode%1000000 != 1){
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

  m_TpcDb = new StTpcDb(this); if (Debug()) m_TpcDb->SetDebug(Debug());
  if (m_Mode%1000000 & 2) {
    Int_t option = (m_Mode & 0xfffc) >> 2;
    m_TpcDb->SetExB(new StMagUtilities(gStTpcDb, RunLog, option));
  }
  m_TpcDb->SetDriftVelocity();
  m_tpg_pad_plane = new St_tpg_pad_plane("tpg_pad_plane",1);
  m_tpg_detector = new St_tpg_detector("tpg_detector",1);
  AddConst(m_tpg_pad_plane);
  AddConst(m_tpg_detector);
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
   Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
   Update_tpg_detector();
  //Here I fill in the arrays for the row parameterization ax+by=1
  if (m_TpcDb->GlobalPosition()) {
    SetTpc2Global();
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
	if (fabs(x2-x1)<0.000001) {
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
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Make(){
  // check that TPC is tripped 
  if (St_tpcAnodeHVC::instance()->tripped()) {
    gMessMgr->Info() << "StTpcDbMaker::TPC has tripped - declaring EOF to avoid possibly bad data" << endm;
    return kStEOF;
  }
  if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
  m_TpcDb->SetDriftVelocity();
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
    Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
    Update_tpg_detector();
  SetTpc2Global();
  return kStOK;
}

//---------------------------------------------------------------------------
void StTpcDbMaker::Clear(const char *opt){
  if (m_TpcDb) m_TpcDb->Clear();
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_pad_plane(){
  if (m_tpg_pad_plane) {
    tpg_pad_plane_st pp;
    memset(&pp, 0, sizeof(tpg_pad_plane_st));
    pp.nrow_in = tpcDbInterface()->PadPlaneGeometry()->numberOfInnerRows();
    pp.nrow_out = tpcDbInterface()->PadPlaneGeometry()->numberOfOuterRows();
    pp.pad_len_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadLength();
    pp.pad_len_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadLength();
    pp.pad_sep_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPitch();
    pp.pad_sep_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPitch();
    pp.pad_wid_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadWidth();
    pp.pad_wid_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadWidth();
    pp.nsect = tpcDbInterface()->Dimensions()->numberOfSectors();
    for (int i=1;i<=tpcDbInterface()->PadPlaneGeometry()->numberOfRows();i++){
      pp.npads[i-1] = tpcDbInterface()->PadPlaneGeometry()->numberOfPadsAtRow(i);
      pp.rad[i-1] = tpcDbInterface()->PadPlaneGeometry()->radialDistanceAtRow(i);
    }
    m_tpg_pad_plane->AddAt(&pp,0);
  }
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_detector(){
 if (m_tpg_detector) {
   tpg_detector_st pp;
   memset(&pp, 0, sizeof(tpg_detector_st));
   pp.nsectors = 2*tpcDbInterface()->Dimensions()->numberOfSectors();
   // note tpg table define number of sectors as 48
   pp.drift_length = tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
   pp.clock_frequency = 1e6*tpcDbInterface()->Electronics()->samplingFrequency();
   pp.z_inner_offset = 
     tpcDbInterface()->Dimensions()->innerEffectiveDriftDistance() - 
     tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
   pp.vdrift = tpcDbInterface()->DriftVelocity();
   m_tpg_detector->AddAt(&pp,0);
 }
}
//_____________________________________________________________________________
void StTpcDbMaker::SetTpc2Global() {
  Double_t phi   = 0.0;  //large uncertainty, so set to 0
  Double_t theta = 0.0;
  Double_t psi   = 0.0;
  Double_t xyz[3] = {0,0,0};
  if (m_TpcDb->GlobalPosition()) {
    theta = m_TpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisY()*180./TMath::Pi();
    psi   = m_TpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisX()*180./TMath::Pi();
    xyz[0] =  m_TpcDb->GlobalPosition()->TpcCenterPositionX();
    xyz[1] =  m_TpcDb->GlobalPosition()->TpcCenterPositionY();
    xyz[2] =  m_TpcDb->GlobalPosition()->TpcCenterPositionZ();
  };
  TGeoHMatrix Tpc2Global("Tpc2Global");
  Tpc2Global.RotateX(-psi);
  Tpc2Global.RotateY(-theta);
  Tpc2Global.RotateZ(-phi);
  Tpc2Global.SetTranslation(xyz);
  m_TpcDb->SetTpc2GlobalMatrix(&Tpc2Global);
}


StTpcDb* StTpcDbMaker::tpcDbInterface() const {return m_TpcDb;}


