// $Id: St_tcl_Maker.h,v 1.2 1998/08/26 12:15:10 fisyak Exp $
// $Log: St_tcl_Maker.h,v $
// Revision 1.2  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
#ifndef STAR_St_tcl_Maker
#define STAR_St_tcl_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tcl_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_tpg_pad_plane;
class St_tpg_detector;
class St_tpg_pad;

class St_tss_tsspar;

class St_tcl_sector_index;
class St_tcl_tclpar;
class St_tcl_tpc_index_type;

class St_tfs_fspar;
class St_tfs_bmpar;
class St_tfs_fsctrl;

class St_tcl_Maker : public StMaker {
 private:
               Bool_t drawinit;
               St_tpg_detector  *m_tpg_detector;  //! TPC geometry parameters 
               St_tpg_pad       *m_tpg_pad;       //! characteristics unique to a given pad
	                                          // (not used)
               St_tpg_pad_plane *m_tpg_pad_plane; //! Constants that describe TPC pad plane
               St_tss_tsspar    *m_tsspar;        //! parameters for slow simulator running.
               St_tcl_sector_index   *m_tcl_sector_index; //! Current sector
	                                                  //  for processing
               St_tcl_tclpar         *m_tclpar; //! Table of parameters controlling
	                                        // how tcl works
               St_tcl_tpc_index_type *m_type;   //!  Table of many-to-many index 
	                                        // correlations for tpc evaluations
               St_tfs_fspar     *m_tfs_fspar;   //! TFS parameter table 
               St_tfs_bmpar     *m_tfs_bmpar;   //! TFS beam parameter table
               St_tfs_fsctrl    *m_tfs_fsctrl;  //! TFS control switches
 protected:
 public: 
                  St_tcl_Maker();
                  St_tcl_Maker(const char *name, const char *title);
   virtual       ~St_tcl_Maker();
   virtual void   Clear(Option_t *option="");
   virtual void   Finish();
   virtual void   Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_tcl_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
