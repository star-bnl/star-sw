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

#include "St_DataSet.h"

class St_type_structtbl;
class St_tss_tsspar;
class St_tcl_sector_index;

class StMinidaqMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StMinidaqMaker.h,v 1.10 2000/03/28 18:24:20 fine Exp $";
// St_stk_stkpar *m_stk_stkpar;  	//! pointer to stk parameters
   Int_t m_init;			//! init counter
   St_type_structtbl *m_tpc_gain;   	//! pointer to the gain table
   Int_t m_first_sector; 		// The first sector
   Int_t m_last_sector;  		// The last sector
   static const Int_t no_of_sectors;  	// Total no. of sector
   St_DataSet     *m_Params;     	//! Params
   St_tss_tsspar *m_tsspar; 		//! parameters for slow simulator running
   St_tcl_sector_index *m_tfc_sector_index; //! current sector for processing

   virtual void TransferData();
protected:

 public: 
                  StMinidaqMaker(const char *name="tpc_raw");
   virtual       ~StMinidaqMaker();
   virtual Int_t Init();
   virtual Int_t  Finish();
   virtual Int_t  Make();
   virtual void   Set_first_sector(Int_t m=1){m_first_sector = m;} // *MENU*
   virtual void   Set_last_sector(Int_t m=24){m_last_sector = m;}  // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMinidaqMaker.h,v 1.10 2000/03/28 18:24:20 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StMinidaqMaker, 1)   	// StAF chain virtual base class for Makers
};

#endif




