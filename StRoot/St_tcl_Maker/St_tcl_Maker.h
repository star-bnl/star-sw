// $Id: St_tcl_Maker.h,v 1.6 1999/02/10 20:57:40 kathy Exp $
// $Log: St_tcl_Maker.h,v $
// Revision 1.6  1999/02/10 20:57:40  kathy
// added histograms to Maker
//
// Revision 1.5  1998/12/16 22:19:20  fisyak
// New tfs
//
// Revision 1.4  1998/10/31 00:26:22  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:48  perev
// cleanup
//
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
class St_tfs_fsctrl;

class TH1F;

class St_tcl_Maker : public StMaker {
 private:
               void                  MakeHistograms();// Histograms for tpc clustering
               Bool_t                drawinit;
               St_tpg_detector       *m_tpg_detector;  //! TPC geometry parameters 
               St_tpg_pad            *m_tpg_pad;       //! characteristics unique to a given pad
	                                          // (not used)
               St_tpg_pad_plane      *m_tpg_pad_plane; //! Constants that describe TPC pad plane
               St_tss_tsspar         *m_tsspar;        //! parameters for slow simulator running.
               St_tcl_sector_index   *m_tcl_sector_index; //! Current sector
	                                                  //  for processing
               St_tcl_tclpar         *m_tclpar; //! Table of parameters controlling
	                                        // how tcl works
               St_tcl_tpc_index_type *m_type;   //!  Table of many-to-many index 
	                                        // correlations for tpc evaluations
               St_tfs_fspar          *m_tfs_fspar;   //! TFS parameter table 
               St_tfs_fsctrl         *m_tfs_fsctrl;  //! TFS control switches

protected:

// for tcl
  TH1F *m_nseq_cluster; //! number sequences in cluster
  TH1F *m_nhits;        //! estimated # of overlapping hits in cluster

// for tph
  TH1F *m_nseq_hit;     //! number sequences contributing to hit
  TH1F *m_tpc_row;      //! tpc row number
  TH1F *m_x_of_hit;     //! x distribution of hits
  TH1F *m_y_of_hit;     //! y distribution of hits
  TH1F *m_z_of_hit;     //! z distribution of hits
  TH1F *m_charge_hit;   //! total charge assigned to point
  TH1F *m_alpha;        //! reconstructed crossing angle in xy
  TH1F *m_phi;          //! orientation of the hit w.r.t padplane 
  TH1F *m_lambda;       //! dip angle (radians)


 public: 
                  St_tcl_Maker(const char *name="tpc_hits", const char *title="event/data/tpc/hits");
   virtual       ~St_tcl_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_tcl_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif



