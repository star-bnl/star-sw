#ifndef STAR_StMinidaqMaker
#define STAR_StMinidaqMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMinidaqMaker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class St_tpg_pad_plane;
class St_tpg_detector;
class St_tpg_pad;
class St_type_structtbl;
class St_tss_tsspar;
class St_tcl_sector_index;

class StMinidaqMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StMinidaqMaker.h,v 1.6 1999/03/31 20:29:42 liq Exp $";
// St_stk_stkpar *m_stk_stkpar;  	//! pointer to stk parameters
   St_tpg_pad_plane *m_tpg_pad_plane; 	//! Constants that describe TPC pad plane
   St_tpg_detector  *m_tpg_detector;  	//! TPC geometry parameters 
   St_tpg_pad       *m_tpg_pad;       	//! characteristics unique to a given pad
	                 		// (not used)
   St_type_structtbl *m_tpc_gain;   	//! pointer to the gain table
   Int_t m_first_sector; 		// The first sector
   Int_t m_last_sector;  		// The last sector
   static const Int_t no_of_sectors;  	// Total no. of sector
   St_DataSet     *m_Params;     	//! Params
   St_tss_tsspar *m_tsspar; 		//! parameters for slow simulator running
   St_tcl_sector_index *m_tfc_sector_index; //! current sector for processing

   virtual void TransferData();
   Float_t      m_clock_frequency;	//Different clock
   Float_t      m_drift_velocity;       //Different by run
   Float_t      m_z_inner_offset;       //varies? (in cm)
   Float_t      m_trigger_offset;       //Differs by run (in seconds)
 
protected:

 public: 
                  StMinidaqMaker(const char *name="tpc_raw");
   virtual       ~StMinidaqMaker();
   virtual Int_t Init();
   virtual Int_t  Finish();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   Set_first_sector(Int_t m=1){m_first_sector = m;} // *MENU*
   virtual void   Set_last_sector(Int_t m=24){m_last_sector = m;}  // *MENU*
   ClassDef(StMinidaqMaker, 1)   	// StAF chain virtual base class for Makers
};

#endif




