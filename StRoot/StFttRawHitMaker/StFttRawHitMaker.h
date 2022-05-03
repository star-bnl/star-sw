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

#ifdef __CINT__
struct stgc_vmm_t;
#else
#include <RTS/src/DAQ_STGC/daq_stgc.h>
#endif

class StEvent;
class StFttCollection;

class StFttRawHitMaker: public StRTSBaseMaker {

public:
    StFttRawHitMaker( const char* name = "stgc" );

    Int_t  Init();
    Int_t  InitRun( Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

	void setReadMuDst( int r = 0 ) { mReadMuDst = r; }

    void PrintTheVMM( stgc_vmm_t * the_vmm ){
        u_char feb = the_vmm[0].feb_vmm >> 2 ;  // feb [0..5]
        u_char vm = the_vmm[0].feb_vmm & 3 ;    // VMM [0..3]
        
        printf("  FEB %d:%d, ch %02d: ADC %d, BCID %d, TB %d, BCID_Delta %d\n",feb,vm,the_vmm[0].ch,the_vmm[0].adc,the_vmm[0].bcid, the_vmm[0].tb, the_vmm[0].bcid_delta) ;
    }

private:
    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    Int_t                mRunYear;
    Bool_t               mDebug;
    Int_t                mReadMuDst;

	int readMuDst();

    ClassDef( StFttRawHitMaker, 1 )
};

#endif // STFTTRAWHITMAKER_H
