// $Id: St_stk_Maker.h,v 1.1 1998/08/12 13:09:05 fisyak Exp $
// $Log: St_stk_Maker.h,v $
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

class St_stk_Maker : public StMaker {
 private:
               Bool_t drawinit;
// static Char_t m_VersionCVS = "$Id: St_stk_Maker.h,v 1.1 1998/08/12 13:09:05 fisyak Exp $";
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
               Bool_t  m_ifstk;    // flag to swit between stk and sgr
	       // parameter tables
               St_stk_stkpar     *m_stk_stkpar;//!
	       St_sgr_pixmap     *m_pix_info;  //!
               St_stk_vtx        *m_stk_vtx;  //!
               St_stk_vtx_direct *m_stk_vtx_direct; //!
               St_stk_filler     *m_stk_filler; //!
               St_svg_config     *m_config; //!
               St_svg_geom       *m_geom;//!
               
 protected:
 public: 
                  St_stk_Maker();
                  St_stk_Maker(const char *name, const char *title);
   virtual       ~St_stk_Maker();
   virtual void   Clear(Option_t *option="");
   virtual void   Finish();
   virtual void   Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   virtual void Set_method     (Int_t   m =      2){m_method     = m;} // *MENU*
   virtual void Set_fill       (Int_t   m =      1){m_fill       = m;} // *MENU*
   virtual void Set_direct     (Int_t   m =      1){m_direct     = m;} // *MENU*
   virtual void Set_nlayers    (Int_t   m =      3){m_nlayers    = m;} // *MENU*
   virtual void Set_c1norm0    (Float_t m =   3.47){m_c1norm[0]  = m;} // *MENU*
   virtual void Set_c1norm1    (Float_t m =   280.){m_c1norm[1]  = m;} // *MENU*
   virtual void Set_c1norm2    (Float_t m =  -13.7){m_c1norm[2]  = m;} // *MENU*
   virtual void Set_c2norm0    (Float_t m =   45.5){m_c2norm[0]  = m;} // *MENU*
   virtual void Set_c2norm1    (Float_t m = 14200.){m_c2norm[1]  = m;} // *MENU*
   virtual void Set_c2norm2    (Float_t m =  -17.5){m_c2norm[2]  = m;} // *MENU*
   virtual void Set_vertex     (Int_t   m =      0){m_vertex 	 = m;} // *MENU*
   virtual void Set_chicut0    (Float_t m =    1e9){m_chicut[0]  = m;} // *MENU*
   virtual void Set_chicut1    (Float_t m =    1e9){m_chicut[1]  = m;} // *MENU*
   virtual void Set_chi_filter (Int_t   m =      1){m_chi_filter = m;} // *MENU*
   virtual void Set_spt_mark   (Int_t   m =      1){m_spt_mark   = m;} // *MENU*
   virtual void Set_long_tracks(Int_t   m =      1){m_long_tracks= m;} // *MENU*
   virtual void Set_th_init    (Float_t m =     .5){m_th_init 	 = m;} // *MENU*
   virtual void Set_th_max     (Float_t m =   10.0){m_th_max	 = m;} // *MENU*
   virtual void Set_nitermax   (Float_t m =    7.0){m_nitermax	 = m;} // *MENU*
   virtual void Set_niternull  (Float_t m =   1000){m_niternull  = m;} // *MENU*
   virtual void Set_sec_factor (Float_t m =    6.0){m_sec_factor = m;} // *MENU*
   virtual void Set_ifstk      (Bool_t  m =  kTRUE){m_ifstk 	 = m;} // *MENU*
   ClassDef(St_stk_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
