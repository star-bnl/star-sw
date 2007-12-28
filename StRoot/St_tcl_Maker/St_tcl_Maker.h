// $Id: St_tcl_Maker.h,v 1.24 2007/12/28 13:47:40 fisyak Exp $
// $Log: St_tcl_Maker.h,v $
// Revision 1.24  2007/12/28 13:47:40  fisyak
// Split tcl and tfs Makers
//
// Revision 1.23  2003/09/10 19:47:51  perev
// ansi corrs
//
// Revision 1.22  2002/02/05 22:21:56  hardtke
// Move Init code to InitRun
//
// Revision 1.21  2001/05/22 22:32:50  hardtke
// Add option for returning hits in global coordinates
//
// Revision 1.20  2000/08/22 00:17:55  hardtke
// Add ability to turn off either half of TPC:  new functions EastOff(), WestOff(), AllOn()
//
// Revision 1.19  1999/12/05 00:07:05  snelling
// Modifications made for eval option: added Histograms and NTuple support
//
// Revision 1.18  1999/11/22 23:18:46  snelling
// added Li Qun's changes to tfs
//
// Revision 1.17  1999/11/20 20:53:51  snelling
// Removed hitclus table and added entries to tphit table
//
// Revision 1.16  1999/10/07 03:24:51  snelling
// created tables dynamically, correct for TFS - TRS/DATA ipix/10
//
// Revision 1.15  1999/10/05 00:46:07  snelling
// added some histogram protections
//
// Revision 1.14  1999/10/01 22:22:25  snelling
// updated histograms
//
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
#ifndef St_tcl_Maker_H
#define St_tcl_Maker_H

enum eTcl {
 TCC_PAD = 182, 
 TCC_BIN = 512
};

const float CENTIMETERS_PER_TIME_BIN 	= 0.4082;
const float LINEARIZATION 		= 1.2;

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tcl_Maker: Wrapper for fortran tph and tcl code                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
class St_type_shortdata;

class St_tpg_pad_plane;
class St_tpg_detector;
class St_tpg_pad;

class St_tcl_sector_index;
class St_tcl_tclpar;
class St_tcl_tpc_index_type;
class St_tcl_tpcluster;
class St_tcl_tp_seq;

class St_tcc_morphology;

class St_tcl_tp_seq;
class St_tcl_tphit;
class St_tcl_tpcluster;
class St_tcc_morphology;
class St_tcl_tpc_index;
class St_tss_tsspar;
class TH1F;
class TNtuple;

class St_tcl_Maker : public StMaker {

 public: 

  St_tcl_Maker(const char *name="tpc_hits");
  virtual       ~St_tcl_Maker(); 
  virtual void   tclEvalOn() {tclEval(kTRUE);}
  virtual void   tclEvalOff(){tclEval();} 
  virtual void   tclMorphOn() {tclMorph(kTRUE);}
  virtual void   tclMorphOff(){tclMorph();} 
  virtual void   tclPixTransOn() {tclPixTrans(kTRUE);}
  virtual void   tclPixTransOff(){tclPixTrans();} 
  virtual void   WriteTNtupleOn() {WriteTNtuple(kTRUE);}
  virtual void   WriteTNtupleOff(){WriteTNtuple();}
  virtual void   HitsInGlobalCoordinates(){UseGlobal(kTRUE);}
  virtual void   HitsInTpcLocalCoordinates(){UseGlobal();}
  void   EastOff();  // turn off east half of tpc
  void   WestOff();  // turn off west half of tpc
  void   AllOn();    // turn on all of tpc
 
  virtual Int_t  Init();
  virtual Int_t  InitRun(int runnumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: St_tcl_Maker.h,v 1.24 2007/12/28 13:47:40 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
 private:

  Bool_t                 m_tclEvalOn;           // switch for the cluster finder evaluation
  Bool_t                 m_tclPixTransOn;       // switch for pixel translation evaluation
  Bool_t                 m_tclMorphOn;          // switch for the cluster morphology study
  Bool_t                 bWriteTNtupleOn;       // switch for writing ntuple to disk

  Bool_t                 m_raw_data_tpc;        // bool used to check if there is pixel data
  Bool_t                 m_EastOff;              //no East half
  Bool_t                 m_WestOff;              //no West half
  Bool_t                 m_GlobalHits;           //use global coordinates for hits

  // define the tables used
  St_tpg_detector*       m_tpg_detector;  	//! TPC geometry parameters 
  St_tpg_pad*            m_tpg_pad;       	//! characteristics unique to a given pad
	                                  	// (not used)
  St_tpg_pad_plane*      m_tpg_pad_plane; 	//! Constants that describe TPC pad plane
  St_tss_tsspar*         m_tsspar;        	//! parameters for slow simulator running.
  St_tcl_sector_index*   m_tcl_sector_index; 	//! Current sector
	                                     	//  for processing
  St_tcl_tclpar*         m_tclpar; 		//! Table of parameters controlling
	                           		// how tcl works
  St_tcl_tpc_index_type* m_type;   		//!  Table of many-to-many index 
	                           		// correlations for tpc evaluations
  St_tcl_tp_seq*         tpseq;                 //! TPC sequence table
  St_tcl_tphit*          tphit;                 //! TPC hit table
  St_tcl_tpcluster*      tpcluster;             //! TPC cluster table
  St_tcc_morphology*     morph;                 //! TPC cluster morphology table
  St_tcl_tpc_index*      index;                 //! TPC evaluation table


  void   tclEval(Bool_t flag=kFALSE){m_tclEvalOn=flag;}
  void   tclPixTrans(Bool_t flag=kFALSE){m_tclPixTransOn=flag;}
  void   tclMorph(Bool_t flag=kFALSE){m_tclMorphOn=flag;}
  void   WriteTNtuple(Bool_t flag=kFALSE){bWriteTNtupleOn=flag;}
  void   UseGlobal(Bool_t flag=kFALSE){m_GlobalHits=flag;}
  void   MakeHistograms(); 
  void   InitHistograms(); 

  Int_t cluster_morphology( 
			   Int_t sectorNumber,
			   St_type_shortdata *pixel_data_in, 
			   St_type_shortdata *pixel_data_out);
  
  Int_t FillOneRowOfMorphTable(
			       int iClusterTbl,
			       int padrow,
			       int sector,
			       int nseq,
			       int npix,
			       int npad,
			       unsigned int totalChargeEq1,
			       int maxCharge,
			       float averageCharge,
			       float meanPadPos,
			       float meanTimePos,
			       float padSigma1Eq5,
			       float bucSigma1Eq6,
			       float padTimeSigma1Eq7,
			       float padSigma2Eq12,
			       float bucSigma2Eq13,
			       float padTimeSigma2Eq14,
			       float ecc1Eq15,
			       float ecc2Eq16,
			       float linEcc1Eq8,
			       float linEcc2Eq9);
  
  
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

 protected:

  TNtuple *mNtupleTcl; //!
  
  ClassDef(St_tcl_Maker,0)       //Cint definition
};
#endif
