// $Id: St_tcl_Maker.h,v 1.13 1999/09/24 01:23:43 fisyak Exp $
// $Log: St_tcl_Maker.h,v $
// Revision 1.13  1999/09/24 01:23:43  fisyak
// Reduced Include Path
//
// Revision 1.12  1999/07/15 13:58:24  perev
// cleanup
//
// Revision 1.11  1999/03/17 19:23:51  sakrejda
// unpacking of raw data into adcxyz table with an on/off switch added
//
// Revision 1.11  1999/03/17 00:10:43  snellings
// switch for the pixel translation stuff added
//
// Revision 1.10  1999/03/16 00:20:43  sakrejda
// switch for the cluster morphology stuff added
//
// Revision 1.9  1999/03/13 23:34:03  perev
// New makers
//
// Revision 1.8  1999/03/11 20:40:20  ward
// Add code for cluster morphology.
//
// Revision 1.7  1999/03/01 18:53:33  sakrejda
// hit eveluation switchable
//
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

enum eTcl {
 TCC_PAD = 182, 
 TCC_BIN = 512
};
const float CENTIMETERS_PER_TIME_BIN 	= 0.4082;
const float LINEARIZATION 		= 1.2;

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

#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tcl_tpcluster_Table.h"
#include "tables/St_tcl_tp_seq_Table.h"
#include "tables/St_tcc_morphology_Table.h"

class St_tcl_Maker : public StMaker {

private:
  Bool_t                m_tclPixTransOn;        // switch for pixel translation evaluation
  Bool_t                m_tclEvalOn;            // switch for the cluster finder evaluation
  Bool_t                m_tclMorphOn;           // switch for the cluster morphology study
  St_tpg_detector       *m_tpg_detector;  	//! TPC geometry parameters 
  St_tpg_pad            *m_tpg_pad;       	//! characteristics unique to a given pad
	                                  	// (not used)
  St_tpg_pad_plane      *m_tpg_pad_plane; 	//! Constants that describe TPC pad plane
  St_tss_tsspar         *m_tsspar;        	//! parameters for slow simulator running.
  St_tcl_sector_index   *m_tcl_sector_index; 	//! Current sector
	                                     	//  for processing
  St_tcl_tclpar         *m_tclpar; 		//! Table of parameters controlling
	                           		// how tcl works
  St_tcl_tpc_index_type *m_type;   		//!  Table of many-to-many index 
	                           		// correlations for tpc evaluations
  St_tfs_fspar          *m_tfs_fspar;   	//! TFS parameter table 
  St_tfs_fsctrl         *m_tfs_fsctrl;  	//! TFS control switches

Int_t cluster_morphology( 
     	Int_t 		   sectorNumber,
        St_type_shortdata *pixel_data_in, 
        St_type_shortdata *pixel_data_out,
        St_tcl_tpcluster  *tpcluster, 
        St_tcl_tp_seq     *tpseq,
        St_tcc_morphology *morph);

Int_t FillOneRowOfMorphTable(
	int iClusterTbl,
        St_tcc_morphology 	*morph,
        int padrow,
        int sector,
        int nseq,
        int npix,
        int npad,
        unsigned int 	totalChargeEq1,
        int 	maxCharge,
        float 	averageCharge,
        float 	meanPadPos,
        float 	meanTimePos,
        float 	padSigma1Eq5,
        float 	bucSigma1Eq6,
        float 	padTimeSigma1Eq7,
        float 	padSigma2Eq12,
        float 	bucSigma2Eq13,
        float 	padTimeSigma2Eq14,
        float 	ecc1Eq15,
        float 	ecc2Eq16,
        float 	linEcc1Eq8,
        float 	linEcc2Eq9);


Int_t CalculateQuadrupoleMoms(
        int padrow,
        int npad,
        int pads[TCC_PAD],
        unsigned short charge[TCC_PAD][TCC_BIN],
        unsigned int  &totChargeEq1,
        float &meanPadEq3,
        float &meanTimeEq4,
        float &padSigma1Eq5,
        float &timeSigma1Eq6,
        float &padTimeSigma1Eq7,
        float &padSigma2Eq12,
        float &timeSigma2Eq13,
        float &padTimeSigma2Eq14,
        float &ecc1Eq15,
        float &ecc2Eq16,
        float &linEcc1Eq8,
        float &linEcc2Eq9);

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
                  St_tcl_Maker(const char *name="tpc_hits");
   virtual       ~St_tcl_Maker(); 
   virtual void   tclEval(Bool_t flag=kFALSE){m_tclEvalOn=flag;}
   virtual void   tclEvalOn() {tclEval(kTRUE);}                       // *MENU*
   virtual void   tclEvalOff(){tclEval();} 
   virtual void   tclMorph(Bool_t flag=kFALSE){m_tclMorphOn=flag;}
   virtual void   tclMorphOn() {tclMorph(kTRUE);}                       // *MENU*
   virtual void   tclMorphOff(){tclMorph();} 
   virtual void   tclPixTrans(Bool_t flag=kFALSE){m_tclPixTransOn=flag;}
   virtual void   tclPixTransOn() {tclPixTrans(kTRUE);}                       // *MENU*
   virtual void   tclPixTransOff(){tclPixTrans();} 

   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   MakeHistograms();// Histograms for tpc clustering
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_tcl_Maker.h,v 1.13 1999/09/24 01:23:43 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_tcl_Maker, 1)       //StAF chain virtual base class for Makers
};

#endif



