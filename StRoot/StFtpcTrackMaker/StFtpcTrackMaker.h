// $Id: StFtpcTrackMaker.h,v 1.6 2002/04/05 16:51:07 oldi Exp $
// $Log: StFtpcTrackMaker.h,v $
// Revision 1.6  2002/04/05 16:51:07  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.5  2002/03/01 14:21:21  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.4  2001/07/12 13:05:01  oldi
// QA histogram of FTPC vertex estimation is generated.
// FTPC vertex estimation is stored as pre vertex (id = 301) in any case, now.
//
// Revision 1.3  2001/02/21 13:14:09  jcs
// Add CVS Id strings in correct place
//
// Revision 1.2  2000/07/03 12:45:23  jcs
// get (pre)Vertex coordinates directly from (pre)Vertex table instead of from
// fptpars
//
// Revision 1.1  2000/05/10 13:39:30  oldi
// Initial version of StFtpcTrackMaker
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcTrackMaker virtual base class for Maker                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcTrackMaker
#define STAR_StFtpcTrackMaker

#include "StMaker.h"
#include "StFtpcTracker.hh"

class TH1F;
class TH2F;
class TProfile;
class St_fde_fdepar;
 
class StFtpcTrackMaker : public StMaker {

 private:

  protected:
       TH1F          *m_vtx_pos;    //! vertex position
       TH1F          *m_q;          //! charge
       TH1F          *m_theta;      //! theta
       TH1F          *m_ndedx;      //! # points used in de/dx calulation
       TH1F          *m_found;      //! # points found per track
       TH1F          *m_track;      //! # tracks found   
       TH2F          *m_nrec_track; //! # points found per track vs. # tracks found
       TH2F          *m_padvstime_West; //! padlength vs. timelength
       TH2F          *m_padvstime_East; //! padlength vs. timelength
       TH1F          *m_maxadc_West;
       TH1F          *m_maxadc_East;
       TH1F          *m_charge_West;
       TH1F          *m_charge_East;
       TH1F          *m_xres;       //! x residuals
       TH1F          *m_yres;       //! y residuals
       TH1F          *m_rres;       //! r residuals
       TH1F          *m_phires;     //! phi residuals
       TH2F          *m_rres_vs_r_east;   //! r residuals vs. r east
       TH2F          *m_phires_vs_r_east; //! phi residuals vs. r east
       TH2F          *m_rres_vs_r_west;   //! r residuals vs. r west
       TH2F          *m_phires_vs_r_west; //! phi residuals vs. r west
       TH2F          *m_vertex_east_xy;   //! vertex xy estimation east
       TH1F          *m_vertex_east_z;    //! vertex z estimation east
       TH2F          *m_vertex_west_xy;   //! vertex xy estimation west
       TH1F          *m_vertex_west_z;    //! vertex z estimation west

   St_fde_fdepar   *m_fdepar;   //!

 public: 
                  StFtpcTrackMaker(const char *name="ftpc_tracks"); // constructor
   virtual       ~StFtpcTrackMaker();                               // destructor
   virtual Int_t  Init();                                           // Initialisation 
   virtual Int_t  Make();                                           // actual program
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcTrackMaker.h,v 1.6 2002/04/05 16:51:07 oldi Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   virtual void   PrintInfo();                                      // prints information
           void   MakeHistograms();                                 // makes histograms
	   void   MakeHistograms(StFtpcTracker *tracker);           // makes histograms


   ClassDef(StFtpcTrackMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
