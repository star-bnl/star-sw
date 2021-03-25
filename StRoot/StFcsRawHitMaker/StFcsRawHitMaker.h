/*
 *
 * \class StFcsRawHitMaker
 *
 */

#ifndef STAR_StFcsRawHitMaker_HH
#define STAR_StFcsRawHitMaker_HH

#include "StRoot/StChain/StRTSBaseMaker.h"

class StEvent;
class StFcsCollection;
class StTriggerData;
class StFcsDbMaker;

class StFcsRawHitMaker : public StRTSBaseMaker {
public: 
    StFcsRawHitMaker( const char* name = "fcsHit");
    ~StFcsRawHitMaker();
    
    int InitRun(int runNumber);
    int Make();
    void Clear( Option_t *opts = "" );

    void setReadMode(int v) {mReadMode=v;}
    void setDebug(int v=1) {SetDebug(v);}  //!backward compatubility     
    
    // Get CVS
    virtual const char *GetCVS() const;
    
private:
    int prepareEnvironment();   
    StEvent *mEvent;
    StFcsCollection *mFcsCollectionPtr;
    unsigned int mRun=0;
    StFcsDbMaker* mFcsDbMkr=0;
    unsigned int mReadMode=0;

    ClassDef(StFcsRawHitMaker,1);
};

// inline functions
inline const char *StFcsRawHitMaker::GetCVS() const {
    static const char cvs[] = "Tag $Name:  $ $Id: StFcsRawHitMaker.h,v 1.4 2021/02/25 21:55:32 akio Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;
};

#endif

/*
 * $Id: StFcsRawHitMaker.h,v 1.4 2021/02/25 21:55:32 akio Exp $
 * $Log: StFcsRawHitMaker.h,v $
 * Revision 1.4  2021/02/25 21:55:32  akio
 * Int_t -> int
 *
 * Revision 1.3  2021/02/25 19:27:10  akio
 * Modified for STAR code review (Hongwei)
 *
 * Revision 1.2  2019/07/05 15:00:52  akio
 * small corrections
 *
 * Revision 1.1  2019/07/03 17:02:45  akio
 * Initial version of FCS offline daq file reader
 *
 */
