/***************************************************************************
 *
 * $Id: StTpcDbMaker.h,v 1.21 2009/12/07 23:44:58 fisyak Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  This maker creates StTpcDb.  
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.h,v $
 * Revision 1.21  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.20  2007/12/25 17:39:31  fine
 * Add the TPC coordinate transformation global function to the Root/Cint dictionary
 *
 * Revision 1.19  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.18  2007/03/21 17:27:02  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.17  2004/01/14 22:54:31  fisyak
 * Add hooks for Pedestal and tpcGain
 *
 * Revision 1.16  2003/09/10 19:47:39  perev
 * ansi corrs
 *
 * Revision 1.15  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
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
 protected:
 public: 
                  StTpcDbMaker(const char *name="TpcDb");
   virtual       ~StTpcDbMaker();
   virtual Int_t Init();
   virtual Int_t InitRun(int runnumber);
   virtual Int_t  Make();
   virtual void Clear(const char *opt);
   virtual void Update_tpg_pad_plane();
   virtual void Update_tpg_detector();
   virtual void UseOnlyLaserDriftVelocity()   {m_Mode = m_Mode%1000000 + 2000000;}
   virtual void UseOnlyCathodeDriftVelocity() {m_Mode = m_Mode%1000000 + 1000000;}
   virtual void UseAnyDriftVelocity()         {m_Mode = m_Mode%1000000;}
   virtual StTpcDb* tpcDbInterface() const;    //! return m_TpcDb
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   virtual void SetTpc2Global();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcDbMaker.h,v 1.21 2009/12/07 23:44:58 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StTpcDbMaker,0)   //StAF chain virtual base class for Makers
};


#endif




