/***************************************************************************
 *
 * $Id: StFmsTriggerDetector.cxx,v 2.1 2007/07/02 20:21:54 ullrich Exp $
 *
 * Author: Akio Ogawa, Apr 2007
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsTriggerDetector.cxx,v $
 * Revision 2.1  2007/07/02 20:21:54  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFmsTriggerDetector.h"
#include "StTriggerData.h"
#include "Stiostream.h"
#include <stdio.h>

static const char rcsid[] = "$Id: StFmsTriggerDetector.cxx,v 2.1 2007/07/02 20:21:54 ullrich Exp $";

ClassImp(StFmsTriggerDetector)
    
StFmsTriggerDetector::StFmsTriggerDetector()
{
    mNumHeader=-1;
    mNumQTdata=0;
    memset(mQTdata, 0, mMaxLine *sizeof(int));
    memset(mDSM   , 0, mMaxDSM  *sizeof(char));
    memset(mDSM01 , 0, mMaxDSM01*sizeof(char));
    memset(mDSM02 , 0, mMaxDSM02*sizeof(char));
    memset(mDSM1  , 0, mMaxDSM1 *sizeof(short));
    memset(mDSM2  , 0, mMaxDSM2 *sizeof(short));
}

StFmsTriggerDetector::StFmsTriggerDetector(const StTriggerData& t)
{
    mNumHeader=-1;
    mNumQTdata = (int)t.nQTdata();
    memcpy(mQTdata, t.QTdata(),     mNumQTdata*sizeof(int));  
    memset(mQTdata+mNumQTdata, 0, (mMaxLine-mNumQTdata)*sizeof(int));
    memcpy(mDSM   ,t.getDsm_FMS(),   mMaxDSM  *sizeof(char));
    memcpy(mDSM01 ,t.getDsm01_FMS(), mMaxDSM01*sizeof(char));
    memcpy(mDSM02 ,t.getDsm02_FMS(), mMaxDSM02*sizeof(char));
    memcpy(mDSM1  ,t.getDsm1_FMS(),  mMaxDSM1 *sizeof(short));
    memcpy(mDSM2  ,t.getDsm2_FMS(),  mMaxDSM2 *sizeof(short));
    decode();
}

StFmsTriggerDetector::~StFmsTriggerDetector() {/* noop */}

void
StFmsTriggerDetector::decode()
{
    mNumHeader=0;
    if(mNumQTdata==0) return;
    int l=mMaxCrate*mMaxAddr*mMaxDCard*mMaxChan;
    memset(mADC,0,l*sizeof(int));
    memset(mTDC,0,l*sizeof(int));
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
            mADC[crate-mOffsetCrate][addr-mOffsetAddr][dcard][dch]=getADC(d);
            mTDC[crate-mOffsetCrate][addr-mOffsetAddr][dcard][dch]=getTDC(d);
            nline++;
            if(mNumHeader==nline) header=1;
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
    }else{
        printf("StFmsTriggerDetector::adc() arguments out of range\n");
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
        printf("StFmsTriggerDetector::tdc() arguments out of range\n");
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
