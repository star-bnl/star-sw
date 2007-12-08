/***************************************************************************
 *
 * $Id: StFmsTriggerDetector.cxx,v 2.4 2007/12/08 21:43:33 jeromel Exp $
 *
 * Author: Akio Ogawa, Apr 2007
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsTriggerDetector.cxx,v $
 * Revision 2.4  2007/12/08 21:43:33  jeromel
 * Wraped with LOG_ERROR, remove assert()
 *
 * Revision 2.3  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.2  2007/07/10 17:04:48  perev
 * Check for zero pointer added
 *
 * Revision 2.1  2007/07/02 20:21:54  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFmsTriggerDetector.h"
#include "StTriggerData.h"
#include "Stiostream.h"
#include <stdio.h>

static const char rcsid[] = "$Id: StFmsTriggerDetector.cxx,v 2.4 2007/12/08 21:43:33 jeromel Exp $";

ClassImp(StFmsTriggerDetector)
    
StFmsTriggerDetector::StFmsTriggerDetector()
{
    memset(mBeg,0,mEnd-mBeg);
    mNumHeader=-1;
    mNumQTdata=0;
}

StFmsTriggerDetector::StFmsTriggerDetector(const StTriggerData& t)
{
    const unsigned char  *c=0;
    const unsigned short *s=0;
    const unsigned int   *i=0;

    memset(mBeg,0,mEnd-mBeg);

    mNumHeader=-1;
    mNumQTdata = (int)t.nQTdata();
    if( !(mMaxLine>=mNumQTdata)){
      {LOG_ERROR << "StFmsTriggerDetector::StFmsTriggerDetector() mMaxLine < mNumQTdata" << endm;}
    } else {
      i = t.QTdata();       if (i) memcpy(mQTdata,i, mNumQTdata*sizeof(int ));  
      c = t.getDsm_FMS()  ; if (c) memcpy(mDSM   ,c, sizeof(mDSM  ));
      c = t.getDsm02_FMS(); if (c) memcpy(mDSM01 ,c, sizeof(mDSM01));
      c = t.getDsm02_FMS(); if (c) memcpy(mDSM02 ,c, sizeof(mDSM02));
      s = t.getDsm1_FMS() ; if (s) memcpy(mDSM1  ,s, sizeof(mDSM1 ));
      s = t.getDsm2_FMS() ; if (s) memcpy(mDSM2  ,s, sizeof(mDSM2 ));
      decode();
    }
}

StFmsTriggerDetector::~StFmsTriggerDetector() {/* noop */}

void
StFmsTriggerDetector::decode()
{
    mNumHeader=0;
    if(mNumQTdata==0) return;
    memset(mADC,0,sizeof(mADC));
    memset(mTDC,0,sizeof(mTDC));
    int header=1, nline=0;
    int crate=0, addr=0;
    for (int i=0; i<static_cast<int>(mNumQTdata-1); i++){    
        unsigned int d = mQTdata[i];    
        if (header==1){
            crate = getCRT(d);
            addr  = getADR(d);
            nline = getNHT(d);
            if(nline>0) {header=0; nline=0;}
            mNumHeader++;
        }
        else {
            unsigned short dcard = getQT8(d);
            unsigned short dch   = getCHA(d);
            int tst=(char*)&mADC[crate-mOffsetCrate][addr-mOffsetAddr][dcard][dch]
                   -(char*)&mADC[0][0][0][0];
	    
	    if ( ! (tst>=0 && tst<(int)sizeof(mADC)) ){
	      {LOG_ERROR << "StFmsTriggerDetector::decode() Sanity check failed in" << endm;}
	    } else {
	      mADC[crate-mOffsetCrate][addr-mOffsetAddr][dcard][dch]=getADC(d);
	      mTDC[crate-mOffsetCrate][addr-mOffsetAddr][dcard][dch]=getTDC(d);
	      nline++;
	      if(mNumHeader==nline) header=1;
	    }
        }
    }
}

unsigned int
StFmsTriggerDetector::hit(int line) const
{
    if(line>=0 && line<mMaxLine && line<(int)mNumQTdata) return mQTdata[line];
    return 0;
}

unsigned short
StFmsTriggerDetector::adc(int crate,  int addr,  int dcard,  int dch)
{
    if(mNumHeader==-1) decode();
    crate -= mOffsetCrate;
    addr  -= mOffsetAddr;
    if(crate >= 0 && crate < mMaxCrate && 
       addr  >= 0 && addr  < mMaxAddr && 
       dcard >= 0 && dcard < mMaxDCard && 
       dch   >= 0 && dch   < mMaxChan ){
      return mADC[crate][addr][dcard][dch];
    } else{
      {LOG_WARN << "StFmsTriggerDetector::adc() arguments out of range" << endm;}
      return 0;
    }
}

unsigned short
StFmsTriggerDetector::tdc(int crate,  int addr,  int dcard,  int dch)
{
    if(mNumHeader==-1) decode();
    crate -= mOffsetCrate;
    addr  -= mOffsetAddr;
    if(crate >= 0 && crate < mMaxCrate && 
       addr  >= 0 && addr  < mMaxAddr && 
       dcard >= 0 && dcard < mMaxDCard && 
       dch   >= 0 && dch   < mMaxChan ){
      return mTDC[crate][addr][dcard][dch];
    }else{
      {LOG_WARN << "StFmsTriggerDetector::tdc() arguments out of range" << endm;}
      return 0;
    }
}

void
StFmsTriggerDetector::dump()
{
    if(mNumHeader==-1) decode();
    cout << "FMS data dump" << endl;
    cout << "Number of data lines = " << mNumQTdata << endl;
    if(mNumQTdata>0){
        printf("Number of header lines = %d\n", mNumHeader);
        printf("Last check line (should be 0xAC10) = %x\n",mQTdata[mNumQTdata-1]);
        printf("ADC\n");
        for(int crate=0; crate<mMaxCrate; crate++){
            for(int addr=0; addr<mMaxAddr; addr++){
                for(int dcard=0; dcard<mMaxDCard; dcard++){
                    printf("Crate=%5x Addr=%5x DCard=%5x   %4d %4d %4d %4d %4d %4d %4d %4d\n",
                           crate+mOffsetCrate, addr+mOffsetAddr, dcard,
                           mADC[crate][addr][dcard][0], mADC[crate][addr][dcard][1],
                           mADC[crate][addr][dcard][2], mADC[crate][addr][dcard][3],
                           mADC[crate][addr][dcard][4], mADC[crate][addr][dcard][5],
                           mADC[crate][addr][dcard][6], mADC[crate][addr][dcard][7]);
                }
            }
        }
        printf("TDC\n");
        for(int crate=0; crate<mMaxCrate; crate++){
            for(int addr=0; addr<mMaxAddr; addr++){
                for(int dcard=0; dcard<mMaxDCard; dcard++){
                    printf("Crate=%5x Addr=%5x DCard=%5x   %4d %4d %4d %4d %4d %4d %4d %4d\n",
                           crate+mOffsetCrate, addr+mOffsetAddr, dcard,
                           mTDC[crate][addr][dcard][0], mTDC[crate][addr][dcard][1],
                           mTDC[crate][addr][dcard][2], mTDC[crate][addr][dcard][3],
                           mTDC[crate][addr][dcard][4], mTDC[crate][addr][dcard][5],
                           mTDC[crate][addr][dcard][6], mTDC[crate][addr][dcard][7]);
                }
            }
        }    
    }
}
