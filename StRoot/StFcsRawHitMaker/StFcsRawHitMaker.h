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
class StFcsDb;

class StFcsRawHitMaker : public StRTSBaseMaker {
public: 
    StFcsRawHitMaker( const char* name = "fcsHit");
    ~StFcsRawHitMaker();
    
    int InitRun(int runNumber);
    int Make();
    void Clear( Option_t *opts = "" );

    void setReadMode(int v) {mReadMode=v;}
    void setDebug(int v=1) {SetDebug(v);}  //!backward compatubility     
    void setReadMuDst(int v=1) {mReadMuDst=v;} //!reading Mudst/StMuFcsHit into StEvent/StFcsHit

    // Get CVS
    virtual const char *GetCVS() const;
    
private:
    StEvent *mEvent;
    StFcsCollection *mFcsCollectionPtr;
    unsigned int mRun=0;
    StFcsDb* mFcsDb=0;
    int mReadMode=1;
    int mReadMuDst=0; 

    int readMuDst();

    ClassDef(StFcsRawHitMaker,1);
};

// inline functions
inline const char *StFcsRawHitMaker::GetCVS() const {
    static const char cvs[] = "Tag $Name:  $ $Id: StFcsRawHitMaker.h,v 1.2 2021/04/26 20:05:35 akio Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;
};

#endif

/*
 * $Id: StFcsRawHitMaker.h,v 1.2 2021/04/26 20:05:35 akio Exp $
 * $Log: StFcsRawHitMaker.h,v $
 * Revision 1.2  2021/04/26 20:05:35  akio
 * change default readmode = 1 (ZS bank)
 *
 * Revision 1.1  2021/03/30 13:40:12  akio
 * FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
 *
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
