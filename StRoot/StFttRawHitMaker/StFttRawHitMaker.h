/***************************************************************************
 *
 * StFttRawHitMaker.h
 *
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: StFttRawHitMaker - class to fill the StEvent from DAQ reader
 *
 ***************************************************************************/
#ifndef STFTTRAWHITMAKER_H
#define STFTTRAWHITMAKER_H

#include "StRTSBaseMaker.h"

#include <vector>

#ifndef __CINT__
// copied from RTS ... trying to include directly fails
struct stgc_vmm_t {
    u_char feb_vmm ;    
    u_char ch ;
    u_short adc ;
    u_short bcid ;
    short tb ;  // from the trigger
} ;
#endif
#ifdef __CINT__
struct stgc_vmm_t;
#endif

class StEvent;
class StFttCollection;

class StFttRawHitMaker: public StRTSBaseMaker {

public:
    StFttRawHitMaker( const char* name = "stgc" );
    ~StFttRawHitMaker();

    Int_t  Init();
    Int_t  InitRun( Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    void PrintTheVMM( stgc_vmm_t * the_vmm ){
        u_char feb = the_vmm[0].feb_vmm >> 2 ;  // feb [0..5]
        u_char vm = the_vmm[0].feb_vmm & 3 ;    // VMM [0..3]
        
        printf("  FEB %d:%d, ch %02d: ADC %d, BCID %d, TB %d\n",feb,vm,the_vmm[0].ch,the_vmm[0].adc,the_vmm[0].bcid, the_vmm[0].tb) ;
    }

private:
    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    Int_t                mRunYear;
    Bool_t               mDebug;

    ClassDef( StFttRawHitMaker, 1 )
};

#endif // STFTTRAWHITMAKER_H