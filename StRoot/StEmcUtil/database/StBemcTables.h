// $Id: StBemcTables.h,v 1.1 2004/12/21 12:54:30 suaide Exp $
// $Log: StBemcTables.h,v $
// Revision 1.1  2004/12/21 12:54:30  suaide
// moved StBemcTables to StEmcUtil/database
// added interface to trigger tables in offline DB
//
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
//
/*!\class StBemcTables
\author Alexandre A. P. Suaide

This class handles all the BEMC database requests
*/

#ifndef STAR_StBemcTables
#define STAR_StBemcTables

#include "TObject.h"
#include "TDataSet.h"

#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"
#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "tables/St_emcCalib_Table.h"
#include "tables/St_smdCalib_Table.h"
#include "tables/St_emcGain_Table.h"
#include "tables/St_smdGain_Table.h"
#include "tables/St_emcTriggerStatus_Table.h"
#include "tables/St_emcTriggerPed_Table.h"

#include "StMaker.h"
#include "StEmcRawMaker/defines.h"


class StBemcTables : public TObject 
{
 private: 
 
   emcPed_st*              mBtowP;
   emcPed_st*              mBprsP;
   smdPed_st*              mSmdeP;
   smdPed_st*              mSmdpP;
   emcStatus_st*           mBtowS;
   emcStatus_st*           mBprsS;
   smdStatus_st*           mSmdeS;
   smdStatus_st*           mSmdpS;
   emcCalib_st*            mBtowC;
   emcCalib_st*            mBprsC;
   smdCalib_st*            mSmdeC;
   smdCalib_st*            mSmdpC;
   emcGain_st*             mBtowG;
   emcGain_st*             mBprsG;
   smdGain_st*             mSmdeG;
   smdGain_st*             mSmdpG;
   emcTriggerStatus_st*    mTrigS;
   emcTriggerPed_st*       mTrigP;
   
   
   void                    loadTables(Int_t,TDataSet*);
          
 public: 
                 
                            StBemcTables(); ///< StBemcTables constructor
  virtual                   ~StBemcTables(); ///< StBemcTables destructor

   void                    loadTables(StMaker*); ///< load tables.
   
   // These are methods to get offline database values, like pedestals, gains, status
   
   void                    getPedestal(Int_t,Int_t,Int_t,Float_t&,Float_t&); ///<Return pedestal mean and rms
   void                    getStatus(Int_t,Int_t,Int_t&); ///<Return status
   void                    getGain(Int_t,Int_t,Float_t&); ///<Return gain correction factor
   void                    getCalib(Int_t,Int_t,Int_t,Float_t&); ///<Return calibration constant
   
   // Thease are methods to get trigger information, like masks, trigger pedestals and bit conversion mode
   void                    getTriggerPatchStatus(Int_t,Int_t&); ///< Return trigger patch status
   void                    getTriggerHighTowerStatus(Int_t,Int_t&); ///< Return trigger highTower status
   void                    getTriggerTowerStatus(Int_t,Int_t,Int_t&); ///< Return trigger single tower status
   void                    getTriggerPedestal(Int_t,Int_t,Float_t&); ///< Return tower pedestal loaded in trigger
   void                    getTriggerBitConv(Int_t,Int_t,Int_t&); ///< Return 6 bits conversion mode
  ClassDef(StBemcTables, 1)  
};

#endif
