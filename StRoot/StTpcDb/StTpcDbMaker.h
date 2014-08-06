/***************************************************************************
 *
 * $Id: StTpcDbMaker.h,v 1.14 2002/02/05 22:21:08 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  This maker creates StTpcDb.  
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.h,v $
 * Revision 1.14  2002/02/05 22:21:08  hardtke
 * Move Init code to InitRun
 *
 * Revision 1.13  2002/01/03 00:01:09  hardtke
 * Add switches for type of drift velocity data (i.e. laser vs. t0 analysis).  Default to use either.
 *
 * Revision 1.12  2001/10/25 22:59:36  hardtke
 * Add function tpc_localsector_to_local
 *
 * Revision 1.11  2001/06/21 16:27:52  perev
 * two error matrix transformation methods added
 *
 * Revision 1.10  2001/04/19 19:52:48  hardtke
 * add tpc_pad_time_offset function and add ifdef for static arrays
 *
 * Revision 1.9  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.8  2000/04/11 16:06:26  hardtke
 * improve speed of tpc_row_par and tpc_global_to_sector
 *
 * Revision 1.7  2000/02/23 22:21:09  hardtke
 * add tpc_global_to_local_p
 *
 * Revision 1.6  2000/02/23 15:09:58  hardtke
 * move tpg_detector and tpg_pad_plane from .const to .data
 *
 * Revision 1.5  2000/02/17 19:43:21  hardtke
 * fixes to tpc functions
 *
 * Revision 1.4  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.3  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef STAR_StTpcDbMaker
#define STAR_StTpcDbMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDbMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef __CINT__
#include "StarCallf77.h"
#define numberOfPadsAtRow_ F77_NAME(numberofpadsatrow,NUMBEROFPADSATROW)
#define tpc_row_to_y_ F77_NAME(tpc_row_to_y,TPC_ROW_TO_Y)
#define tpc_pad_to_x_ F77_NAME(tpc_pad_to_x,TPC_PAD_TO_X)
#define tpc_x_to_pad_ F77_NAME(tpc_x_to_pad,TPC_X_TO_PAD)
#define tpc_local_to_global_ F77_NAME(tpc_local_to_global,TPC_LOCAL_TO_GLOBAL)
#define tpc_localsector_to_local_ F77_NAME(tpc_localsector_to_local,TPC_LOCALSECTOR_TO_LOCAL)
#define tpc_local_to_global_err_ F77_NAME(tpc_local_to_global_err,TPC_LOCAL_TO_GLOBAL_ERR)
#define tpc_local_to_global_emx_ F77_NAME(tpc_local_to_global_emx,TPC_LOCAL_TO_GLOBAL_EMX)
#define tpc_global_to_local_ F77_NAME(tpc_global_to_local,TPC_GLOBAL_TO_LOCAL)
#define tpc_global_to_local_p_ F77_NAME(tpc_global_to_local_p,TPC_GLOBAL_TO_LOCAL_P)
#define tpc_drift_velocity_ F77_NAME(tpc_drift_velocity,TPC_DRIFT_VELOCITY)
#define tpc_time_to_z_ F77_NAME(tpc_time_to_z,TPC_TIME_TO_Z)
#define tpc_z_to_time_ F77_NAME(tpc_z_to_time,TPC_Z_TO_TIME)
#define tpc_drift_velocity_ F77_NAME(tpc_drift_velocity,TPC_DRIFT_VELOCITY)
#define tpc_drift_volume_length_ F77_NAME(tpc_drift_volume_length,TPC_DRIFT_VOLUME_LENGTH)
#define tpc_row_par_ F77_NAME(tpc_row_par,TPC_ROW_PAR)
#define tpc_global_to_sector_ F77_NAME(tpc_global_to_sector,TPC_GLOBAL_TO_SECTOR)
#define tpc_sec24_to_sec12_ F77_NAME(tpc_sec24_to_sec12,TPC_SEC24_TO_SEC12)
#define tpc_pad_time_offset_ F77_NAME(tpc_pad_time_offset,TPC_PAD_TIME_OFFSET)
#define tpc_rdo_mask_ F77_NAME(tpc_rdo_mask,TPC_RDO_MASK)
extern "C" {
R__EXTERN int type_of_call numberOfPadsAtRow_(int *);
}
extern "C" {
R__EXTERN int type_of_call tpc_row_to_y_(float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_pad_to_x_(float *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_x_to_pad_(float *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_local_to_global_err_(int &,const float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_local_to_global_emx_(int &,const float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_local_to_global_(int *,const float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_localsector_to_local_(int *,const float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_global_to_local_(int *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_global_to_local_p_(int *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_time_to_z_(int *,int *,int *,int *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_z_to_time_(float *,int *,int *,int *,int *);
}
extern "C" {
R__EXTERN int type_of_call tpc_drift_velocity_(float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_drift_volume_length_(float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_row_par_(int *,float *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_global_to_sector_(int*, float*);
}
extern "C" {
  R__EXTERN int type_of_call tpc_sec24_to_sec12_(int*, int*);
}
extern "C" {
  R__EXTERN int type_of_call tpc_pad_time_offset_(int*, int*, int*, float*);
}
extern "C" {
  R__EXTERN int type_of_call tpc_rdo_mask_(int*, int*);
}
#endif
class StTpcDb;
class St_tpg_pad_plane;
class St_tpg_detector;
//class StTpcCoordinateTransform;

#ifdef StTpc_STATIC_ARRAYS
static float aline[24][45];  //hold parameterization
static float bline[24][45];  //ax+by=0
#endif

class StTpcDbMaker : public StMaker { 
 private:
  StTpcDb* m_TpcDb;               //! tpc database class
  St_tpg_pad_plane* m_tpg_pad_plane; //!
  St_tpg_detector* m_tpg_detector; //! 
  Int_t m_dvtype;  //! 0= use all, 1=only StTpcT0Maker, 2=only laser;
 protected:
 public: 
                  StTpcDbMaker(const char *name="TLA");
   virtual       ~StTpcDbMaker();
   virtual Int_t Init();
   virtual Int_t InitRun(int runnumber);
   virtual Int_t  Make();
   virtual void Clear(const char *opt);
   virtual void Update_tpg_pad_plane();
   virtual void Update_tpg_detector();
   virtual void UseOnlyLaserDriftVelocity();
   virtual void UseOnlyCathodeDriftVelocity();
   virtual void UseAnyDriftVelocity();
   virtual StTpcDb* tpcDbInterface() const;    //! return m_TpcDb
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcDbMaker.h,v 1.14 2002/02/05 22:21:08 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcDbMaker, 1)   //StAF chain virtual base class for Makers
};

inline StTpcDb* StTpcDbMaker::tpcDbInterface() const {return m_TpcDb;}
inline void StTpcDbMaker::UseOnlyLaserDriftVelocity() {m_dvtype=2;}
inline void StTpcDbMaker::UseOnlyCathodeDriftVelocity() {m_dvtype=1;}
inline void StTpcDbMaker::UseAnyDriftVelocity() {m_dvtype=0;}

#endif




