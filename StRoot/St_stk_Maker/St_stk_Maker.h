// $Id: St_stk_Maker.h,v 1.9 1999/07/15 00:19:59 caines Exp $
// $Log: St_stk_Maker.h,v $
// Revision 1.9  1999/07/15 00:19:59  caines
// Switch to turn tracking on/off
//
// Revision 1.8  1999/03/13 00:26:48  perev
// New maker schema
//
// Revision 1.7  1999/02/16 21:17:54  caines
// Added QA histograms
//
// Revision 1.6  1999/01/02 19:08:22  fisyak
// Add ctf
//
// Revision 1.5  1998/10/31 00:26:21  fisyak
// Makers take care about branches
//
// Revision 1.4  1998/10/06 18:00:46  perev
// cleanup
//
// Revision 1.3  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.2  1998/08/18 14:05:03  fisyak
// Add to bfc dst
//
// Revision 1.1  1998/08/12 13:09:05  fisyak
// Add stk_Maker
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_stk_Maker
#define STAR_St_stk_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_stk_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_stk_stkpar;
class St_sgr_pixmap;
class St_stk_vtx;
class St_stk_vtx_direct;
class St_stk_filler;
class St_svg_config;
class St_svg_geom;
class St_spr_sprpar;
class TH1F;
class TH2F;

class St_stk_Maker : public StMaker {
protected:
// static Char_t m_VersionCVS = "$Id: St_stk_Maker.h,v 1.9 1999/07/15 00:19:59 caines Exp $";
   Int_t  m_mode;      // mode 1 = primaries;
	               // mode 2 = secondaries;
	               // mode 3 = primaries to secondaries 
   Int_t  m_method;    // method = 1 use main vertex in fits
	               // method = 2 do not use main vertex in fits
   Int_t  m_fill;      // fill = 0 do not do standard tracking
	               // fill = 1 do standard tracking
   Int_t  m_direct;    // direct = 0 do not do perfect tracking
	               // direct = 1 do perfect tracking with fit
	               // direct = 2 do perfect trackinf without fit, mtm from mc info
   Int_t  m_nlayers;   // nlayers = 3 three layer svt
	               // nlayers = 4 four layer svt
   Float_t m_c1norm[3];// normalization factor for chi:
	               // chi --> chi/[norm(1)+norm(2)*exp(norm(3)*pt)]
   Float_t m_c2norm[3];// The idea here is to take the transverse
	               // momentum dependence out of the chisqr.
	               // (if resolution is changed from 20 mic then
	               // norm(1) will need to be adjusted)
	               // c1norm is for the x-y fit, c2 norm is for the phi z fit
   Int_t m_vertex;     // vertex = 0 do not smear main vertex
	               // vertex = 1 smear main vertex
   Float_t m_chicut[2];// chicut(1) is the maximum chi for the xy fit
	               // chicut(2) is the maximum chi for the z-phi fit
   Float_t m_chi_filter;//chi_filter = 0 do not apply chi**2 cuts
	               // chi_filter = 1 do apply chi**2 cuts
   Int_t m_spt_mark;   // spt_mark=0 use space points more than once
	               // spt_mark=1 use space points only  once
   Int_t m_long_tracks;// =0 do not remove short tracks which are subsets of longer tracks
	               // =1 do remove short tracks which are subsets of longer tracks
   Float_t m_th_init;  // initial cone angle
   Float_t m_th_max;   // maximum cone angle
   Int_t   m_nitermax; // maximum number of iterations
   Int_t   m_niternull;// maximum number of iterations allowed to find no tracks
   Float_t m_sec_factor;//factor for cone angle for secondaries
   Bool_t  m_ifstk;    // flag to switch on stk and sgr

// 		parameter tables

   St_stk_stkpar     *m_stk_stkpar;	//!
   St_sgr_pixmap     *m_pix_info;  	//!
   St_stk_vtx        *m_stk_vtx;  	//!
   St_stk_vtx_direct *m_stk_vtx_direct; //!
   St_stk_filler     *m_stk_filler; 	//!
   St_svg_config     *m_config; 	//!
   St_svg_geom       *m_geom;		//!
   St_spr_sprpar     *m_sprpar; 	//!

   void   MakeHistograms(); // Tracking histograms

protected:

   TH1F *m_q_pt; //!number of hits assigned to a reconstructed track
   TH1F *m_frac_used;   //!Frac. of hits used
   TH1F *m_azimuth;       //!azimuthal angle
   TH1F *m_tan_dip;       //!tangent of the dip angle
   TH2F *m_dedx;       //! dedx plot
   TH2F *m_gededx;  //! geant dedx plot

public: 
                  St_stk_Maker(const char *name="svt_tracks");
   virtual       ~St_stk_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   TrackSwitch(Bool_t flag=kFALSE){m_ifstk=flag;} // *MENU*
   virtual void   TrackSwitchon() {TrackSwitch(kTRUE);} 
   virtual void   TrackSwitchoff(){TrackSwitch();}
   ClassDef(St_stk_Maker, 1)   //STAR chain virtual base class for Makers
};

#endif






