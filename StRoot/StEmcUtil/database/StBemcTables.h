/***************************************************************************
 *
 * $Id: StBemcTables.h,v 1.9 2015/03/02 20:26:09 jkadkins Exp $
 * Author:      Alexandre A. P. Suaide
 * Maintainer:  Adam Kocoloski, MIT, kocolosk@mit.edu
 *
 ***************************************************************************/

#ifndef STAR_StBemcTables
#define STAR_StBemcTables

#include <string>
using std::string;

#include <map>
using std::map;

#include <utility>
using std::pair;

#include "TObject.h"

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
#include "tables/St_bemcTriggerPed4_Table.h"
#include "tables/St_emcTriggerLUT_Table.h"

class StMaker;
class StEmcDecoder;

/*!\class StBemcTables
\author Alexandre A. P. Suaide

This class handles all the BEMC database requests
*/

class StBemcTables : public TObject 
{
protected: 
    emcPed_st*                mBtowP;
    emcPed_st*                mBprsP;
    smdPed_st*                mSmdeP;
    smdPed_st*                mSmdpP;
    emcStatus_st*             mBtowS;
    emcStatus_st*             mBprsS;
    smdStatus_st*             mSmdeS;
    smdStatus_st*             mSmdpS;
    emcCalib_st*              mBtowC;
    emcCalib_st*              mBprsC;
    smdCalib_st*              mSmdeC;
    smdCalib_st*              mSmdpC;
    emcGain_st*               mBtowG;
    emcGain_st*               mBprsG;
    smdGain_st*               mSmdeG;
    smdGain_st*               mSmdpG;
    emcTriggerStatus_st*      mTrigS;
    emcTriggerPed_st*         mTrigP;
    emcTriggerLUT_st*         mTrigL;
    bemcTriggerPed4_st*       mTrigP4;
    
    StEmcDecoder*           mDecoder;
    Bool_t                  mBtowMapFix;
    Bool_t                  mBprsMapFix;
    Int_t                   getOldId(int det, Int_t newId) const;
    
    map<string, pair<string, string> > mValidRanges;
    void updateValidity(StMaker* dbMaker, TTable* table);
   
public:
    StBemcTables(Bool_t btowMapFix = kFALSE, Bool_t bprsMapFix = kFALSE); ///< StBemcTables constructor
    virtual  ~StBemcTables(); ///< StBemcTables destructor
    
    void    loadTables(StMaker* anyMaker); ///< load tables.
    
    const char* beginTime(const char * tableName) const;
    const char* endTime(const char * tableName) const;
    
    //the following methods are simple wrappers around the original "get" methods for those who 
    //prefer a return value instead of passing a parameter
    //detector numbering uses StEmcRawMaker/defines
    //BTOW == 1, BPRS == 2, BSMDE == 3, BSMDP == 4
    float   calib(int det, int softId, int power=1) const;
    float   pedestal(int det, int softId, int cap=0) const;
    float   pedestalRMS(int det, int softId, int cap=0) const;
    float   gain(int det, int softId) const;
    
    /// option can be one of pedestal/calib/gain to get the detailed status for that table,
    /// otherwise it returns the general status code for the channel
    int     status(int det, int softId, const char *option = "") const;
    
    int     triggerPatchStatus(int triggerPatch) const;
    int     triggerHighTowerStatus(int triggerPatch) const;
    int     triggerTowerStatus(int crate, int sequence) const;
    float   triggerPedestal(int crate, int sequence) const;
    int     triggerPedestalShift() const;
    int     triggerBitConversion(int crate, int patchSequence) const;
    short   triggerPed4(int softId) const;
    int     triggerFormulaTag(int crate, int patchSequence) const;
    int*    triggerFormulaParameters(int crate, int patchSequence) const;
    
    //these methods allow the user to access the same trigger info via softId.
    int     triggerPatchStatusByID(int softId) const;
    int     triggerHighTowerStatusByID(int softId) const;
    int     triggerTowerStatusByID(int softId) const;
    float   triggerPedestalByID(int softId) const;
    int     triggerBitConversionByID(int softId) const;
    int     triggerFormulaTagByID(int softId) const;
    int*    triggerFormulaParametersByID(int softId) const;
    
    //-----------------------------------------------------------------------------
    
    // These are methods to get offline database values, like pedestals, gains, status
    void    getPedestal(Int_t det, Int_t softId, Int_t cap, Float_t& ped, Float_t& rms) const; ///<Return pedestal mean and rms
    void    getStatus(Int_t det, Int_t softId, Int_t& status, const char *option = "") const; ///<Return status
    void    getGain(Int_t det, Int_t softId, Float_t& gain) const; ///<Return gain correction factor
    void    getCalib(Int_t det, Int_t softId, Int_t power, Float_t& calib) const; ///<Return calibration constant
             
    // These get trigger information, like masks, trigger pedestals and bit conversion mode
    void    getTriggerPatchStatus(Int_t patch, Int_t& status) const; ///< Return trigger patch status
    void    getTriggerHighTowerStatus(Int_t hightower, Int_t& status) const; ///< Return trigger highTower status
    void    getTriggerTowerStatus(Int_t crate, Int_t index, Int_t& status) const; ///< Return trigger single tower status
    void    getTriggerPedestal(Int_t crate, Int_t index, Float_t& pedestal) const; ///< Return tower pedestal loaded in trigger
    void    getTriggerBitConv(Int_t crate, Int_t patch, Int_t& bit) const; ///< Return 6 bits conversion mode
    void    getTriggerPedestalShift(Int_t& pedestalShift) const; ///< Return target pedestal shift
    void    getTriggerPed4(Int_t softId, Short_t& PED4) const; // Return Ped4 value
    void    getTriggerFormulaTag(Int_t crate, Int_t index, Int_t& formula) const; ///< Return LUT formula
    void    getTriggerFormulaParameters(Int_t crate, Int_t index, Int_t* parameters) const; ///< Return LUT formula parameters
    
    StEmcDecoder* getDecoder() {return mDecoder;}///< Return pointer to decoder

    ClassDef(StBemcTables, 1)  
};

#endif

/***************************************************************************
 *
 * $Log: StBemcTables.h,v $
 * Revision 1.9  2015/03/02 20:26:09  jkadkins
 * Updates to hold values for bemcTriggerPed4
 *
 * Revision 1.8  2009/09/25 15:50:29  mattheww
 * added getter to StBemcTable for StEmcDecoder used
 *
 * Revision 1.7  2007/09/18 19:41:47  kocolosk
 * added an optional argument to status methods to get status for calib, pedestal, and gain tables
 *
 * Revision 1.6  2007/09/10 21:23:23  kocolosk
 * specify kTRUE as 2nd ctor argument to implement swapping corrections for 06/07 BPRS DB
 *
 * Revision 1.5  2007/08/21 18:39:23  kocolosk
 * added methods to get beginTime / endTime of a given DB table
 *
 *
 **************************************************************************/
