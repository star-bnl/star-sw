/***************************************************************************
 *
 * $Id: StTpcDbMaker.h,v 1.4 2000/02/10 00:29:09 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  This maker creates StTpcDb.  
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.h,v $
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
#define tpc_local_to_global_ F77_NAME(tpc_local_to_global,TPC_LOCAL_TO_GLOBAL)
#define tpc_global_to_local_ F77_NAME(tpc_global_to_local,TPC_GLOBAL_TO_LOCAL)
#define tpc_drift_velocity_ F77_NAME(tpc_drift_velocity,TPC_DRIFT_VELOCITY)
#define tpc_time_to_z_ F77_NAME(tpc_time_to_z,TPC_TIME_TO_Z)
#define tpc_time_to_z_ F77_NAME(tpc_time_to_z,TPC_TIME_TO_Z)
#define tpc_drift_velocity_ F77_NAME(tpc_drift_velocity,TPC_DRIFT_VELOCITY)
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
R__EXTERN int type_of_call tpc_local_to_global_(int *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_global_to_local_(int *,float *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_time_to_z_(int *,int *,int *,int *,float *);
}
extern "C" {
R__EXTERN int type_of_call tpc_drift_velocity_(float *);
}
#endif
class StTpcDb;
class St_tpg_pad_plane;
class St_tpg_detector;
//class StTpcCoordinateTransform;
class StTpcDbMaker : public StMaker {
 private:
  StTpcDb* m_TpcDb;               //! tpc database class
  St_tpg_pad_plane* m_tpg_pad_plane; //!
  St_tpg_detector* m_tpg_detector; //! 
 protected:
 public: 
                  StTpcDbMaker(const char *name="TLA");
   virtual       ~StTpcDbMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void Update_tpg_pad_plane();
   virtual void Update_tpg_detector();
   virtual StTpcDb* tpcDbInterface() const;    //! return m_TpcDb
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcDbMaker.h,v 1.4 2000/02/10 00:29:09 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcDbMaker, 1)   //StAF chain virtual base class for Makers
};

inline StTpcDb* StTpcDbMaker::tpcDbInterface() const {return m_TpcDb;}

#endif
