// $Id: St_tpt_Maker.h,v 1.2 1998/08/18 14:05:04 fisyak Exp $
// $Log: St_tpt_Maker.h,v $
// Revision 1.2  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.1  1998/07/21 00:36:47  fisyak
// tcl and tpt
//
#ifndef STAR_St_tpt_Maker
#define STAR_St_tpt_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tpt_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_tpg_pad_plane;
class St_tcl_tpc_index_type;
class St_tpt_pars;
class St_tpt_spars;
class St_tte_control;
class St_tdeparm;
class St_tpipar;

class St_tpt_Maker : public StMaker {
 private:
               Bool_t drawinit;
	       Bool_t m_iftte;
//static Char_t m_VersionCVS = "$Id: St_tpt_Maker.h,v 1.2 1998/08/18 14:05:04 fisyak Exp $";
               St_tpg_pad_plane *m_tpg_pad_plane; //! Constants that describe TPC pad plane
               St_tcl_tpc_index_type *m_type;   //!  Table of many-to-many index 
	                                        // correlations for tpc evaluations
               St_tpt_pars  *m_tpt_pars;  //! Parameters for the track finding
               St_tpt_spars *m_tpt_spars; //! Parameters for the track finding
               St_tte_control *m_tte_control;//! Control switches for the evaluation 
               St_tdeparm   *m_tdeparm;   //! Parameters for the tde dedx module
               St_tpipar    *m_tpipar;    //! parameter file for tpi package
 protected:
 public: 
                  St_tpt_Maker();
                  St_tpt_Maker(const char *name, const char *title);
   virtual       ~St_tpt_Maker();
   virtual void   Clear(Option_t *option="");
   virtual void   Finish();
   virtual void   Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   Set_tte(Bool_t m=kFALSE){m_iftte = m;}
   ClassDef(St_tpt_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
