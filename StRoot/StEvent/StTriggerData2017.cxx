/***************************************************************************
 *
 * $Id: StTriggerData2017.cxx,v 2.2 2016/12/15 16:30:07 ullrich Exp $
 *
 * Author: Akio Ogawa, Dec 2016
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData for 2017.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2017.cxx,v $
 * Revision 2.2  2016/12/15 16:30:07  ullrich
 * Updates from Jeff.
 *
 * Revision 2.1  2016/12/08 18:58:28  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include <string.h>
#include <assert.h>
#include <iostream>
#include "StTriggerData2017.h"

ClassImp(StTriggerData2017)

StTriggerData2017::StTriggerData2017():mData()
{
    mDebug = 0;
    //    printf("StTriggerData2017 Default Constructor\n");
}

StTriggerData2017::StTriggerData2017(const TriggerDataBlk2017* data, int run):mData()
{
    //printf("StTriggerData2017 Constructor with trigger data block\n");
    mYear=2017; mRun = run; mDebug = 0;
    mData = new TriggerDataBlk2017;
    readData(data,1);
}

StTriggerData2017::StTriggerData2017(const TriggerDataBlk2017* data, int run, int bs, int dbg):mData()
{
    mYear=2017; mRun = run; mDebug = dbg;
    if(mDebug==1) printf("StTriggerData2017 Constructor with trigger data block and byteswap option=%d\n",bs);
    mData = new TriggerDataBlk2017;
    readData(data,bs);
}

void StTriggerData2017::readData(const TriggerDataBlk2017* data, int bs) {
    int copyflag=1;
    if (data==0) {copyflag=0;}
    if(mDebug==1) printf("StTriggerData2017::readData copyflag=%d byteswap=%d data=%p mData=%p\n",copyflag,bs,data,mData);
    
    if (copyflag==1){
        unsigned int ver = data->FormatVersion;
        if (bs) swapI(&ver);
        
        if (ver == y17FORMAT_VERSION ) {
            if (mDebug==1) printf("StTriggerData2017: version = 0x%x (0x%x or 0x08121140)\n",ver,y17FORMAT_VERSION);
        }
        else {
            mErrorFlag = mErrorFlag | 0x1;
            printf("StTriggerData2017: version = 0x%x != (0x%x)\n",ver,y17FORMAT_VERSION);
            assert(0);
        }
        
        unsigned int size = data->totalTriggerLength;
        if (bs) swapI(&size);
        if (size > y17MAX_TRG_BLK_SIZE) {
            gMessMgr->Warning() << "StTriggerData2017: Data length = " << size
            << " is bigger than max = " << y17MAX_TRG_BLK_SIZE
            << endm;
            assert(0);
        }
        if (mDebug==1) printf("StTriggerData2017: size = %d, maxsize = %d\n",size,y17MAX_TRG_BLK_SIZE);
        memcpy(mData,data,size);
        memset((char*)mData+size,0,sizeof(TriggerDataBlk2017)-size);
    }
    
    if (bs) swapDataBlk(mData);
    if (mDebug==1){
        printf("StTriggerData2017: version = 0x%x (0x%x)\n",mData->FormatVersion,y17FORMAT_VERSION);
        printf("StTriggerData2017: size = %d, maxsize = %d\n",mData->totalTriggerLength,y17MAX_TRG_BLK_SIZE);
        printf("EventDesc  length=%10d   offset=%10d\n",mData->EventDesc_ofl.length,mData->EventDesc_ofl.offset);
        printf("L1_DSM     length=%10d   offset=%10d\n",mData->L1_DSM_ofl.length,mData->L1_DSM_ofl.offset);
        printf("Summary    length=%10d   offset=%10d\n",mData->Summary_ofl.length,mData->Summary_ofl.offset);
    }
    
    EvtDesc=0; L1_DSM=0; TrgSum=0;
    if (mData->EventDesc_ofl.length > 0) EvtDesc = (EvtDescData2017*)((char*)mData + mData->EventDesc_ofl.offset);
    if (mData->L1_DSM_ofl.length > 0)    L1_DSM  = (L1_DSM_Data2017*)((char*)mData + mData->L1_DSM_ofl.offset);
    if (mData->Summary_ofl.length   > 0) TrgSum  = (TrgSumData2017* )((char*)mData + mData->Summary_ofl.offset);
    if (bs){
        if (EvtDesc) swapEvtDesc(EvtDesc);
        if (L1_DSM) swapL1_DSM(L1_DSM);
        if (TrgSum) swapTrgSum(TrgSum);
    }
    if (EvtDesc==0 || L1_DSM==0 || TrgSum==0){
        mErrorFlag = mErrorFlag | 0x1;
        gMessMgr->Warning() << "StTriggerData2017: EvtDesc, L1_DSM or TrgSum is missing"
        <<" mErrorFlag="<<mErrorFlag<<endm;
    }
    
    int npre  = numberOfPreXing();
    int npost = numberOfPostXing();
    if (npre<0 || npre>10 || npost<0 || npost>10){
        mErrorFlag = mErrorFlag | 0x2;
        gMessMgr->Warning() << "StTriggerData2017: Invalid npre/post  = "<< npre << " / " << npost
        <<" mErrorFlag="<<mErrorFlag<<endm;
    }
    if (mDebug==1) printf("StTriggerData2017: pre=%d post=%d\n",npre,npost);
    
    memset(mBC1,0,sizeof(mBC1));
    memset(mMXQ,0,sizeof(mMXQ));
    memset(mMIX,0,sizeof(mMIX));
    memset(mBCW,0,sizeof(mBCW));
    memset(mBCE,0,sizeof(mBCE));
    memset(mEPQ,0,sizeof(mEPQ));
    memset(mBBC,0,sizeof(mBBC));
    memset(mBBQ,0,sizeof(mBBQ));
    memset(mFMS,0,sizeof(mFMS));
    memset(mQT1,0,sizeof(mQT1));
    memset(mQT2,0,sizeof(mQT2));
    memset(mQT3,0,sizeof(mQT3));
    memset(mQT4,0,sizeof(mQT4));
    memset(mFQ1,0,sizeof(mFQ1));
    memset(mFQ2,0,sizeof(mFQ2));
    memset(mxq,0,sizeof(mxq)); memset(tmxq,0,sizeof(tmxq));
    memset(epq,0,sizeof(epq)); memset(tepq,0,sizeof(tepq));
    memset(bbq,0,sizeof(bbq)); memset(tbbq,0,sizeof(tbbq));
    memset(qt1,0,sizeof(qt1)); memset(tqt1,0,sizeof(tqt1));
    memset(qt2,0,sizeof(qt2)); memset(tqt2,0,sizeof(tqt2));
    memset(qt3,0,sizeof(qt3)); memset(tqt3,0,sizeof(tqt3));
    memset(qt4,0,sizeof(qt4)); memset(tqt4,0,sizeof(tqt4));
    memset(fq1,0,sizeof(fq1)); memset(tfq1,0,sizeof(tfq1));
    memset(fq2,0,sizeof(fq2)); memset(tfq2,0,sizeof(tfq2));
    TrgOfflen2017* offlen;
    
    for (int i=0; i<1+npre+npost; i++){
        //printf("Doing prepost = %d\n",i);
        if (i==0) {
            offlen = mData->MainX;
        }
        else {
            //printf("Prepost list offset = %d\n",mData->PrePostList[i-1]);
            if (mData->PrePostList[i-1]==0) continue;
            offlen = (TrgOfflen2017*) ((char*)mData + mData->PrePostList[i-1]);
        }
        if (bs) swapRawDetOfflen(offlen);
        for(int k=0; k<y17MAX_OFFLEN; k++){
            if(static_cast<unsigned int>(offlen[k].length + offlen[k].offset) > static_cast<unsigned int>(mData->totalTriggerLength)) {
                mErrorFlag = mErrorFlag | (1 << k);
                gMessMgr->Warning() << "StTriggerData2017: offset ("<<offlen[k].offset<<") + length ("<<offlen[k].length
                <<") exceeds total size("<<mData->totalTriggerLength<<") for data block id="<<k
                <<" mErrorFlag="<<mErrorFlag<<endm;
            }
        }
        int j;
        j=offlen[y17BC1_CONF_NUM].length; if (j>0){mBC1[i] = (BELayerBlock2017*)((char*)mData + offlen[y17BC1_CONF_NUM].offset); swapRawDet((DataBlock2017*)mBC1[i],y17BC1_CONF_NUM,j,bs);}
        j=offlen[y17MXQ_CONF_NUM].length; if (j>0){mMXQ[i] = (QTBlock2017*     )((char*)mData + offlen[y17MXQ_CONF_NUM].offset); swapRawDet((DataBlock2017*)mMXQ[i],y17MXQ_CONF_NUM,j,bs);}
        j=offlen[y17MIX_CONF_NUM].length; if (j>0){mMIX[i] = (MIXBlock2017*    )((char*)mData + offlen[y17MIX_CONF_NUM].offset); swapRawDet((DataBlock2017*)mMIX[i],y17MIX_CONF_NUM,j,bs);}
        j=offlen[y17BCW_CONF_NUM].length; if (j>0){mBCW[i] = (BWestBlock2017*  )((char*)mData + offlen[y17BCW_CONF_NUM].offset); swapRawDet((DataBlock2017*)mBCW[i],y17BCW_CONF_NUM,j,bs);}
        j=offlen[y17BCE_CONF_NUM].length; if (j>0){mBCE[i] = (BEastBlock2017*  )((char*)mData + offlen[y17BCE_CONF_NUM].offset); swapRawDet((DataBlock2017*)mBCE[i],y17BCE_CONF_NUM,j,bs);}
        j=offlen[y17EPQ_CONF_NUM].length; if (j>0){mEPQ[i] = (QTBlock2017*     )((char*)mData + offlen[y17EPQ_CONF_NUM].offset); swapRawDet((DataBlock2017*)mEPQ[i],y17EPQ_CONF_NUM,j,bs);}
        j=offlen[y17BBC_CONF_NUM].length; if (j>0){mBBC[i] = (BBCBlock2017*    )((char*)mData + offlen[y17BBC_CONF_NUM].offset); swapRawDet((DataBlock2017*)mBBC[i],y17BBC_CONF_NUM,j,bs);}
        j=offlen[y17BBQ_CONF_NUM].length; if (j>0){mBBQ[i] = (QTBlock2017*     )((char*)mData + offlen[y17BBQ_CONF_NUM].offset); swapRawDet((DataBlock2017*)mBBQ[i],y17BBQ_CONF_NUM,j,bs);}
        j=offlen[y17FMS_CONF_NUM].length; if (j>0){mFMS[i] = (FMSBlock2017*    )((char*)mData + offlen[y17FMS_CONF_NUM].offset); swapRawDet((DataBlock2017*)mFMS[i],y17FMS_CONF_NUM,j,bs);}
        j=offlen[y17QT1_CONF_NUM].length; if (j>0){mQT1[i] = (QTBlock2017*     )((char*)mData + offlen[y17QT1_CONF_NUM].offset); swapRawDet((DataBlock2017*)mQT1[i],y17QT1_CONF_NUM,j,bs);}
        j=offlen[y17QT2_CONF_NUM].length; if (j>0){mQT2[i] = (QTBlock2017*     )((char*)mData + offlen[y17QT2_CONF_NUM].offset); swapRawDet((DataBlock2017*)mQT2[i],y17QT2_CONF_NUM,j,bs);}
        j=offlen[y17QT3_CONF_NUM].length; if (j>0){mQT3[i] = (QTBlock2017*     )((char*)mData + offlen[y17QT3_CONF_NUM].offset); swapRawDet((DataBlock2017*)mQT3[i],y17QT3_CONF_NUM,j,bs);}
        j=offlen[y17QT4_CONF_NUM].length; if (j>0){mQT4[i] = (QTBlock2017*     )((char*)mData + offlen[y17QT4_CONF_NUM].offset); swapRawDet((DataBlock2017*)mQT4[i],y17QT4_CONF_NUM,j,bs);}
        j=offlen[y17FQ1_CONF_NUM].length; if (j>0){mFQ1[i] = (QTBlock2017*     )((char*)mData + offlen[y17FQ1_CONF_NUM].offset); swapRawDet((DataBlock2017*)mFQ1[i],y17FQ1_CONF_NUM,j,bs);}
        j=offlen[y17FQ2_CONF_NUM].length; if (j>0){mFQ2[i] = (QTBlock2017*     )((char*)mData + offlen[y17FQ2_CONF_NUM].offset); swapRawDet((DataBlock2017*)mFQ2[i],y17FQ2_CONF_NUM,j,bs);}
        if (mMXQ[i]) decodeQT(mMXQ[i]->length/4, mMXQ[i]->data, mxq[i], tmxq[i]);
        if (mEPQ[i]) decodeQT(mEPQ[i]->length/4, mEPQ[i]->data, epq[i], tepq[i]);
        if (mBBQ[i]) decodeQT(mBBQ[i]->length/4, mBBQ[i]->data, bbq[i], tbbq[i]);
        if (mQT1[i]) decodeQT(mQT1[i]->length/4, mQT1[i]->data, qt1[i], tqt1[i]);
        if (mQT2[i]) decodeQT(mQT2[i]->length/4, mQT2[i]->data, qt2[i], tqt2[i]);
        if (mQT3[i]) decodeQT(mQT3[i]->length/4, mQT3[i]->data, qt3[i], tqt3[i]);
        if (mQT4[i]) decodeQT(mQT4[i]->length/4, mQT4[i]->data, qt4[i], tqt4[i]);
        if (mFQ1[i]) decodeQT(mFQ1[i]->length/4, mFQ1[i]->data, fq1[i], tfq1[i]);
        if (mFQ2[i]) decodeQT(mFQ2[i]->length/4, mFQ2[i]->data, fq2[i], tfq2[i]);
    }
    if (mDebug==1) dump();
}

StTriggerData2017::~StTriggerData2017() {delete mData;}

unsigned int StTriggerData2017::version() const
{
    return EvtDesc->TrgDataFmtVer;
}

unsigned int StTriggerData2017::eventNumber() const
{
    return mData->eventNumber;
}

unsigned int StTriggerData2017::token() const
{
    return EvtDesc->TrgToken;
}

unsigned int StTriggerData2017::triggerWord() const
{
    return 0;
}

unsigned int StTriggerData2017::actionWord() const
{
    return
    ( (unsigned short)(EvtDesc->actionWdTrgCommand) * 16 * 16 * 16 ) +
    ( (unsigned short)(EvtDesc->actionWdDaqCommand) * 16 * 16      ) +
    (                  EvtDesc->actionWdDetectorBitMask & 0x00ff   );
}

unsigned int StTriggerData2017::numberOfPreXing() const
{
    return EvtDesc->npre & 0xf;
}

unsigned int StTriggerData2017::numberOfPostXing() const
{
    return EvtDesc->npost & 0xf;
}

unsigned short StTriggerData2017::busyStatus() const
{
    return EvtDesc->internalBusy;
}

unsigned short StTriggerData2017::dsmInput() const
{
    return EvtDesc->DSMInput;
}

unsigned short StTriggerData2017::trgToken() const
{
    return EvtDesc->TrgToken;
}

unsigned short StTriggerData2017::dsmAddress() const
{
    return EvtDesc->DSMAddress;
}

unsigned short StTriggerData2017::mAddBits() const
{
    return EvtDesc->addBits;
}

unsigned short StTriggerData2017::bcData(int channel) const
{
    return L1_DSM->BCdata[channel];
}

unsigned short StTriggerData2017::getTrgDetMask() const
{
    return EvtDesc->trgDetMask;
}

unsigned int StTriggerData2017::getTrgCrateMask() const
{
    unsigned int p = EvtDesc->npost & 0xfff0;
    unsigned int r = EvtDesc->res1  & 0x0ff0;
    return 
	(   ((EvtDesc->npre  & 0xfff0) >>  4) 
	  + (p <<  8)
	  + (r << 20) );
}

unsigned short StTriggerData2017::lastDSM(int channel) const
{
    return L1_DSM->lastDSM[channel];
}

unsigned short StTriggerData2017::tcuBits() const
{
    return EvtDesc->DSMInput;
}


unsigned int StTriggerData2017::tcuCounter() const
{
    unsigned int hi = EvtDesc->tcuCtrBunch_hi;
    return (hi << 16) + EvtDesc->DSMAddress;
}

unsigned int StTriggerData2017::rccCounter(int crate) const
{
    if(crate >= y17L1_CONF_NUM && crate <= y17QT4_CONF_NUM){
        return TrgSum->LocalClocks[crate];
    }
    return 0;
}

unsigned long long StTriggerData2017::bunchCounter() const
{
    unsigned long long bxinghi,bxing1,bxinglo, bx;
    bxinghi = L1_DSM->BCdata[3];
    bxing1 =  L1_DSM->BCdata[10];
    bxinglo = (bxing1 << 16) + L1_DSM->BCdata[11];
    bx = (bxinghi << 32) + bxinglo;
    return bx;
}

unsigned int StTriggerData2017::bunchCounterHigh() const
{
    return EvtDesc->bunchXing_hi;
}

unsigned int StTriggerData2017::bunchCounterLow() const
{
    return EvtDesc->bunchXing_lo;
}

unsigned int StTriggerData2017::bunchId48Bit() const
{
    return (int)(bunchCounter() % 120);
}

unsigned int StTriggerData2017::bunchId7Bit() const
{
    int b7=0, b7dat;
    b7dat = L1_DSM->BCdata[2];
    b7 = b7dat & 0x7f;
    return b7;
}

unsigned int StTriggerData2017::spinBit() const
{
    if(mRun<12000000){
        return (L1_DSM->lastDSM[7]/16)%256;
    }else{
        return (L1_DSM->lastDSM[4]/16)%256;
    }
}

unsigned int StTriggerData2017::spinBitYellowFilled() const
{
    unsigned int sb = spinBit();
    return sb%2;
}

unsigned int StTriggerData2017::spinBitYellowUp() const
{
    unsigned int sb = spinBit();
    return (sb/2)%2;
}

unsigned int StTriggerData2017::spinBitYellowDown() const
{
    unsigned int sb = spinBit();
    return (sb/4)%2;
}

unsigned int StTriggerData2017::spinBitYellowUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/8)%2;
}

unsigned int StTriggerData2017::spinBitBlueFilled() const
{
    unsigned int sb = spinBit();
    return (sb/16)%2;
}

unsigned int StTriggerData2017::spinBitBlueUp() const
{
    unsigned int sb = spinBit();
    return (sb/32)%2;
}

unsigned int StTriggerData2017::spinBitBlueDown() const
{
    unsigned int sb = spinBit();
    return (sb/64)%2;
}

unsigned int StTriggerData2017::spinBitBlueUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/128)%2;
}

unsigned short StTriggerData2017::bbcADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    const int addrmap[2][24] = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        4, 4, 4, 4, 4, 4, 4, 4},
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            4, 4, 4, 4, 4, 4, 4, 4} };
    const int chmap[2][24]   = { { 0, 3, 8,16,19,24, 1, 2, 9,10,11,17,18,25,26,27,
        0, 1, 2, 3, 8, 9,10,11},
        { 0, 3, 8,16,19,24, 1, 2, 9,10,11,17,18,25,26,27,
            16,17,18,19,24,25,26,27} };
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt>=1 && pmt<=24) return bbq[buffer][addrmap[eastwest][pmt-1]][chmap[eastwest][pmt-1]];
    return 0;
}

unsigned short StTriggerData2017::bbcTDC(StBeamDirection eastwest, int pmt, int prepost) const
{
    const int addrmap[2][24] = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        4, 4, 4, 4, 4, 4, 4, 4},
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            4, 4, 4, 4, 4, 4, 4, 4} };
    const int chmap[2][24]   = { { 0, 3, 8,16,19,24, 1, 2, 9,10,11,17,18,25,26,27,
        0, 1, 2, 3, 8, 9,10,11},
        { 0, 3, 8,16,19,24, 1, 2, 9,10,11,17,18,25,26,27,
            16,17,18,19,24,25,26,27} };
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt>=1 && pmt<=24) return bbq[buffer][addrmap[eastwest][pmt-1]][chmap[eastwest][pmt-1]+4];
    return 0;
}

unsigned short StTriggerData2017::bbcADCSum(StBeamDirection eastwest, int prepost) const
{
    unsigned short sum=0;
    int buffer = prepostAddress(prepost);
    //if (buffer >= 0) for(int i=1; i<=16; i++) {sum+=bbcADC(eastwest,i,prepost);}
    if(buffer>=0){
        if (mBBC[buffer]){
            if(eastwest==east) { sum = mBBC[buffer]->BBClayer1[3]; }
            else               { sum = mBBC[buffer]->BBClayer1[1]; }
        }
    }
    return sum;
}

unsigned short StTriggerData2017::bbcADCSumLargeTile(StBeamDirection eastwest, int prepost) const
{
    unsigned short sum=0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) for(int i=17; i<=24; i++) {sum+=bbcADC(eastwest,i,prepost);}
    return sum;
}

unsigned short StTriggerData2017::bbcEarliestTDC(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if (eastwest==east) {return mBBC[buffer]->BBClayer1[2]%4096;}
            else               {return mBBC[buffer]->BBClayer1[0]%4096;}
        }
    }
    return 0;
}

unsigned short StTriggerData2017::bbcTimeDifference() const
{
    return L1_DSM->VTX[3]%8192;
}

unsigned short StTriggerData2017::bbcTacSum() const
{
    return (((L1_DSM->VTX[3]) >> 13) & 0x1);
}

unsigned short StTriggerData2017::bbcEarliestTDCLarge(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if (eastwest==east) {return  mBBC[buffer]->BBClayer1[11] & 0x0fff;}
            else {return ((mBBC[buffer]->BBClayer1[11] & 0xf000) >> 12)
                +((mBBC[buffer]->BBClayer1[10] & 0x00ff) << 4 );}
        }
    }
    return 0;
}

unsigned short StTriggerData2017::bbcTimeDifferenceLarge() const
{
    return L1_DSM->VTX[2]%8192;
}


unsigned short StTriggerData2017::fpd(StBeamDirection eastwest, int module, int pmt, int prepost) const
{
    return 0;
}

unsigned short StTriggerData2017::fpdSum(StBeamDirection eastwest, int module) const
{
    return 0;
}

unsigned short StTriggerData2017::fpdLayer1DSMRaw(StBeamDirection eastwest, int channel, int prepost) const{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0){
        if (eastwest==east) { if (mMIX[buffer]) return mMIX[buffer]->FPDEastNSLayer1[channel]; }
        else                { if (mFMS[buffer]) return mFMS[buffer]->FMS[channel]; }
    }
    return 0;
}

unsigned short StTriggerData2017::fpdLayer2DSMRaw(int channel) const{
    if (channel<8) return L1_DSM->FPD[channel];
    return 0;
}

bool StTriggerData2017::zdcPresent(int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) return mBBQ[buffer];
    return false;
}

unsigned short StTriggerData2017::zdcAtChannel(int channel, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && channel>=0 && channel<32) return bbq[buffer][14][channel];
    return 0;
}

unsigned short StTriggerData2017::zdcAtAddress(int address, int prepost) const
{
    return zdcAtChannel(address,prepost);
}

unsigned short StTriggerData2017::zdcUnAttenuated(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (eastwest == east) return bbq[buffer][14][2];
        else                 return bbq[buffer][14][18];
    }
    return 0;
}

unsigned short StTriggerData2017::zdcAttenuated(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (eastwest == east) return bbq[buffer][14][3];
        else                 return bbq[buffer][14][19];
    }
    return 0;
}

unsigned short StTriggerData2017::zdcADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt>=1 && pmt<=3) {
        if (eastwest == east) {
            if (pmt == 1) return bbq[buffer][14][0];
            if (pmt == 2) return bbq[buffer][14][8];
            if (pmt == 3) return bbq[buffer][14][9];
        }
        else {
            if (pmt == 1) return bbq[buffer][14][16];
            if (pmt == 2) return bbq[buffer][14][24];
            if (pmt == 3) return bbq[buffer][14][25];
        }
    }
    return 0;
}

unsigned short StTriggerData2017::zdcTDC(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (eastwest == east) return bbq[buffer][14][6];
        else                 return bbq[buffer][14][22];
    }
    return 0;
}

unsigned short StTriggerData2017::zdcPmtTDC(StBeamDirection eastwest, int pmt, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt>=1 && pmt<=3) {
        if (eastwest == east) {
            if (pmt == 1) return bbq[buffer][14][4];
            if (pmt == 2) return bbq[buffer][14][12];
            if (pmt == 3) return bbq[buffer][14][13];
        }
        else {
            if (pmt == 1) return bbq[buffer][14][20];
            if (pmt == 2) return bbq[buffer][14][28];
            if (pmt == 3) return bbq[buffer][14][29];
        }
    }
    return 0;
}

unsigned short StTriggerData2017::zdcHardwareSum(int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) return bbq[buffer][14][11];
    return 0;
}

bool StTriggerData2017::zdcSMDPresent(int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) return mMXQ[buffer];
    return false;
}

unsigned short StTriggerData2017::zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost) const
{
    static const int zdcsmd_map[2][2][8] ={
        { { 31, 30, 29, 28, 27, 26, 25, 19} ,
            { 24, 23, 22, 21, 20, 16, 18, 17} } ,
        { { 15, 14, 13, 12, 11, 10,  9,  2} ,
            {  8,  7,  6,  5,  4,  3,  0,  1} }
    };
    static const int zdcsmd_map2011[2][2][8] ={
        { {24, 25, 26, 27, 28, 29, 30, 31} ,
            {16, 17, 18, 19, 20, 21, 22, 23} } ,
        { {8,  9,  10, 11, 12, 13, 14, 15} ,
            {0,  1,  2,  3,  4,  5,  6,  7} }
    };
    if (verthori<0 || verthori>1) return 0;
    if (strip<1 || strip>8) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if(mRun<12034085){
            return mxq[buffer][4][zdcsmd_map[eastwest][verthori][strip-1]];
        }else{
            return mxq[buffer][4][zdcsmd_map2011[eastwest][verthori][strip-1]];
        }
    }
    return 0;
}

unsigned short StTriggerData2017::zdcEarliestTDC(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if(mRun<12000000){
                if (eastwest==east) {return ((mBBC[buffer]->ZDClayer1[3] >> 12) % 16) | ((mBBC[buffer]->ZDClayer1[2] % 256) << 4);}
                else                {return (mBBC[buffer]->ZDClayer1[3]) % 4096;}
            }else{
                if (eastwest==east) {return ((mBBC[buffer]->ZDClayer1[3] >> 10) % 64) | ((mBBC[buffer]->ZDClayer1[2] % 16) << 6);}
                else                {return (mBBC[buffer]->ZDClayer1[3]) % 1024;}
            }
        }
    }
    return 0;
}

bool StTriggerData2017::zdcSumADCaboveThreshold(StBeamDirection eastwest, int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if(mRun<12000000){
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (27-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (24-16));}
            }else{
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (25-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (22-16));}
            }
        }
    }
    return 0;
}

bool StTriggerData2017::zdcFrontADCaboveThreshold(StBeamDirection eastwest, int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if(mRun<12000000){
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (29-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (26-16));}
            }else{
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (23-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (20-16));}
            }
        }
    }
    return 0;
}

bool StTriggerData2017::zdcBackADCaboveThreshold(StBeamDirection eastwest, int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if(mRun<12000000){
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (28-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (25-16));}
            }else{
                if (eastwest==east) {return mBBC[buffer]->ZDClayer1[2] & (1 << (24-16));}
                else                {return mBBC[buffer]->ZDClayer1[2] & (1 << (21-16));}
            }
        }
    }
    return 0;
}

unsigned short StTriggerData2017::zdcTimeDifference() const
{
    return L1_DSM->VTX[1]%1024;
}

bool StTriggerData2017::zdcSumADCaboveThresholdL2(StBeamDirection eastwest) const {
    return L1_DSM->VTX[1] & (1 << ((eastwest==east) ? 10 : 11));
}

bool StTriggerData2017::zdcFrontADCaboveThresholdL2(StBeamDirection eastwest) const {
    return L1_DSM->VTX[1] & (1 << ((eastwest==east) ? 12 : 14));
}

bool StTriggerData2017::zdcBackADCaboveThresholdL2(StBeamDirection eastwest) const {
    return L1_DSM->VTX[1] & (1 << ((eastwest==east) ? 13 : 15));
}

bool StTriggerData2017::zdcSumADCaboveThresholdL3(StBeamDirection eastwest) const {
    if(mRun<12000000){ return lastDSM(2) & (1 << ((eastwest==east) ? 7 : 8)); }
    else             { return lastDSM(1) & (1 << ((eastwest==east) ? 7 : 8)); }
}

bool StTriggerData2017::zdcFrontADCaboveThresholdL3(StBeamDirection eastwest) const {
    if(mRun<12000000){ return lastDSM(2) & (1 << ((eastwest==east) ? 9 : 11)); }
    else             { return lastDSM(1) & (1 << ((eastwest==east) ? 9 : 11)); }
}

bool StTriggerData2017::zdcBackADCaboveThresholdL3(StBeamDirection eastwest) const {
    if(mRun<12000000){ return lastDSM(2) & (1 << ((eastwest==east) ? 10 : 12)); }
    else             { return lastDSM(1) & (1 << ((eastwest==east) ? 10 : 12)); }
}

bool StTriggerData2017::zdcTimeDifferenceInWindow() const
{
    if(mRun<12000000){ return lastDSM(2) & (1 << 6); }
    else             { return lastDSM(1) & (1 << 6); }
}

unsigned short StTriggerData2017::zdcSMDHighestStrip(StBeamDirection eastwest, int verthori, int prepost) const
{
    if(mRun<12000000) return 0;
    // copy of the scaler output from ZDC SMD QT is sent to ZD101 J2
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if (eastwest==east) {return (mBBC[buffer]->ZDClayer1[1] >> (verthori ? 6 : 9)) % 8;}
            else {return (mBBC[buffer]->ZDClayer1[1] >> (verthori ? 0 : 3)) % 8;}
        }
    }
    return 0;
}

unsigned short StTriggerData2017::zdcTruncatedSum(StBeamDirection eastwest, int prepost) const
{
    if(mRun<12000000) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >=0){
        if (mBBC[buffer]){
            if (eastwest==east) {return (mBBC[buffer]->ZDClayer1[2] >> (26-16)) % 8;}
            else {return (mBBC[buffer]->ZDClayer1[2] >> (29-16)) % 8;}
        }
    }
    return 0;
}

unsigned short StTriggerData2017::pp2ppADC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost) const
{
    static const int map[2][2][2][2] ={ { { { 0, 1}, { 2, 3} } , { {16,17}, {18,19} } , } ,
        { { { 8, 9}, {10,11} } , { {24,25}, {26,27} } , } };
    if (vh<0   || vh>1)   return 0;
    if (udio<0 || udio>1) return 0;
    if (ch<0   || ch>1)   return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) return mxq[buffer][2][map[eastwest][vh][udio][ch]];
    return 0;
}

unsigned short StTriggerData2017::pp2ppTAC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost) const
{
    static const int map[2][2][2][2] ={ { { { 0, 1}, { 2, 3} } , { {16,17}, {18,19} } , } ,
        { { { 8, 9}, {10,11} } , { {24,25}, {26,27} } , } };
    if (vh<0   || vh>1)   return 0;
    if (udio<0 || udio>1) return 0;
    if (ch<0   || ch>1)   return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) return mxq[buffer][2][map[eastwest][vh][udio][ch]+4];
    return 0;
}

unsigned long StTriggerData2017::pp2ppDSM(int prepost) const {
    if (prepost!=0) return 0;
    return L1_DSM->TOF[7];
}

unsigned short StTriggerData2017::bemcLayer1DSM(int channel, int prepost) const {
    const int n_bemc_layer1=48;
    if (channel<0 || channel >=n_bemc_layer1) {
        gMessMgr->Warning() << "Barrel DSM layer 1 out of range (" << channel << ")" << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBC1[buffer]) return mBC1[buffer]->BEMClayer1[channel];
    return 0;
}

unsigned short StTriggerData2017::eemcLayer1DSM(int channel, int prepost) const {
    const int n_eemc_layer1=16;
    if (channel<0 || channel >=n_eemc_layer1) {
        gMessMgr->Warning() << "Endap DSM layer 1 out of range (" << channel << ")" << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBC1[buffer]) return mBC1[buffer]->EEMClayer1[channel];
    return 0;
}

unsigned short StTriggerData2017::emcLayer2DSM(int channel) const {
    const int n_emc_layer2=8;
    if (channel<0 || channel >=n_emc_layer2) {
        gMessMgr->Warning() << "EMC DSM layer 2 out of range (" << channel << ")" << endm;
        return 0;
    }
    return L1_DSM->EMC[channel];
}

unsigned short StTriggerData2017::tpcMaskDSM(int channel) const {
    const int n_tpcMask=8;
    if (channel<0 || channel >=n_tpcMask) {
        gMessMgr->Warning() << "TPCMask DSM out of range (" << channel << ")" << endm;
        return 0;
    }
    return L1_DSM->TPCMask[channel];
}

unsigned char StTriggerData2017::bemcHighTower(int patch_id, int prepost) const {
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Barrel patch id: " << patch_id << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        int dsm=patch_id/10;
        int channel=patch_id%10;
        unsigned short trg_word;
        if (dsm>=15) {
            if (mBCE[buffer])
                trg_word=decodeEmc12bit(dsm-15,channel,mBCE[buffer]->BEMCEast);
            else
                return 0;
        }
        else {
            if (mBCW[buffer])
                trg_word=decodeEmc12bit(dsm,channel,mBCW[buffer]->BEMCWest);
            else
                return 0;
        }
        return trg_word & 0x3F;
    }
    return 0;
}

unsigned char StTriggerData2017::bemcJetPatch (int patch_id, int prepost) const
{
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Barrel patch id: " << patch_id << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        int dsm=patch_id/10;
        int channel=patch_id%10;
        unsigned short trg_word;
        if (dsm>=15) {
            if (mBCE[buffer])
                trg_word=decodeEmc12bit(dsm-15,channel,mBCE[buffer]->BEMCEast);
            else
                return 0;
        }
        else {
            if (mBCW[buffer])
                trg_word=decodeEmc12bit(dsm,channel,mBCW[buffer]->BEMCWest);
            else
                return 0;
        }
        return trg_word >> 6;
    }
    return 0;
}


unsigned char StTriggerData2017::eemcHighTower(int patch_id, int prepost) const
{
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90;
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Endcap patch id" << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && mBC1[buffer]) {
        int dsm=patch_id/10;
        int channel=patch_id%10;
        unsigned short trg_word = decodeEmc12bit(dsm,channel,mBC1[buffer]->EEMC);
        return trg_word & 0x3F;
    }
    return 0;
}

unsigned char StTriggerData2017::eemcJetPatch (int patch_id, int prepost) const
{
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90;
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Endcap patch id" << endm;
        return 0;
    }
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && mBC1[buffer]) {
        int dsm=patch_id/10;
        int channel=patch_id%10;
        unsigned short trg_word = decodeEmc12bit(dsm,channel,mBC1[buffer]->EEMC);
        return trg_word >> 6;
    }
    return 0;
}

unsigned char StTriggerData2017::bemcHighestTowerADC(int prepost) const {
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    unsigned char h=0;
    for (int i=1; i<m_max_patch; i++){
        unsigned char hh=bemcHighTower(i,prepost);
        if (h>hh) h=hh;
    }
    return h;
}

unsigned char StTriggerData2017::eemcHighestTowerADC(int prepost) const {
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90;
    unsigned char h=0;
    for (int i=1; i<m_max_patch; i++){
        unsigned char hh=eemcHighTower(i,prepost);
        if (h>hh) h=hh;
    }
    return h;
}

char* StTriggerData2017::getTriggerStructure()
{
    return (char*) mData;
}

TriggerDataBlk2017* StTriggerData2017::getTriggerStructure2017()
{
    return mData;
}

int StTriggerData2017::getRawSize() const
{
    return  mData->totalTriggerLength;
}

unsigned char* StTriggerData2017::getDsm0_BEMCE(int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBCE[buffer]) return mBCE[buffer]->BEMCEast;
    return 0;
}

unsigned char* StTriggerData2017::getDsm0_BEMCW(int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBCW[buffer]) return mBCW[buffer]->BEMCWest;
    return 0;
}

unsigned short* StTriggerData2017::getDsm1_BEMC(int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBC1[buffer]) return mBC1[buffer]->BEMClayer1;
    return 0;
}

unsigned char* StTriggerData2017::getDsm0_EEMC(int prepost) const {
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBC1[buffer]) return mBC1[buffer]->EEMC;
    return 0;
}

unsigned short* StTriggerData2017::getDsm1_EEMC(int prepost) const{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mBC1[buffer]) return mBC1[buffer]->EEMClayer1;
    return 0;
}

unsigned short* StTriggerData2017::getDsm2_EMC() const{
    return   L1_DSM->EMC;
}

unsigned short* StTriggerData2017::getDsm3() const{
    return   L1_DSM->lastDSM;
}

int StTriggerData2017::L2ResultsOffset(StL2AlgorithmId id) const
{
    switch(id) {
        default: return -999999999;
    }
}

bool StTriggerData2017::isL2Triggered(StL2TriggerResultType id) const
{
    return false;
}

unsigned int StTriggerData2017::l2ResultLength() const
{
    return sizeof(TrgSum->L2Result)/sizeof(unsigned int);
}

const unsigned int* StTriggerData2017::l2Result() const
{
    return TrgSum->L2Result;
}

unsigned long long StTriggerData2017::l2sum() const
{
    //printf("L2sum0=%08o\n",TrgSum->L2Sum[0]);
    //printf("L2sum1=%08o\n",TrgSum->L2Sum[1]);
    unsigned long long hi = TrgSum->L2Sum[1];
    unsigned long long lo = TrgSum->L2Sum[0];
    unsigned long long mask=(hi<<32) | lo;
    return mask;
}

unsigned short StTriggerData2017::vpdADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[16] = {0, 1, 2, 3, 8, 9,10,11,16,17,18,19,24,25,26,27};
    if (pmt<1 || pmt>16) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (mRun<=12003001) {return bbq[buffer][10+(int)eastwest*2][map[pmt-1]];}
        else {return bbq[buffer][6+(int)eastwest*2][map[pmt-1]];}
    }
    return 0;
}

unsigned short StTriggerData2017::vpdTDC(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[16] = {0, 1, 2, 3, 8, 9,10,11,16,17,18,19,24,25,26,27};
    if (pmt<1 || pmt>16) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (mRun<=12003001) {return bbq[buffer][10+(int)eastwest*2][map[pmt-1]+4];}
        else {return bbq[buffer][6+(int)eastwest*2][map[pmt-1]+4];}
    }
    return 0;
}

unsigned short StTriggerData2017::vpdADCHighThr(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[16] = {0, 1, 2, 3, 8, 9,10,11,16,17,18,19,24,25,26,27};
    if (pmt<1 || pmt>16) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (mRun<=12003001) {return bbq[buffer][6+(int)eastwest*2][map[pmt-1]];}
        else {return mxq[buffer][6+(int)eastwest*2][map[pmt-1]];}
    }
    return 0;
}

unsigned short StTriggerData2017::vpdTDCHighThr(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[16] = {0, 1, 2, 3, 8, 9,10,11,16,17,18,19,24,25,26,27};
    if (pmt<1 || pmt>16) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) {
        if (mRun<=12003001) {return bbq[buffer][6+(int)eastwest*2][map[pmt-1]+4];}
        else {return mxq[buffer][6+(int)eastwest*2][map[pmt-1]+4];};
    }
    return 0;
}

unsigned short StTriggerData2017::vpdEarliestTDC(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0){
        if (mBBC[buffer]){
            if (mRun<=10096084){
                if (eastwest==east) {return mBBC[buffer]->VPD[6]%4096;}
                else               {return mBBC[buffer]->VPD[4]%4096;}
            }
            else if(mRun<=12003001) {
                if (eastwest==east) {return mBBC[buffer]->VPD[2]%4096;}
                else               {return mBBC[buffer]->VPD[0]%4096;}
            }
            else {
                if (eastwest==east) {return mBBC[buffer]->VPD[6]%4096;}
                else               {return mBBC[buffer]->VPD[4]%4096;}
            }
            
        }
    }
    return 0;
}

unsigned short StTriggerData2017::vpdEarliestTDCHighThr(StBeamDirection eastwest, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0){
        if (mRun<=10365999){
            return 0;
        }
        else if(mRun<=12003001) {
            if (mBBC[buffer]){
                if (eastwest==east) {return mBBC[buffer]->VPD[6]%4096;}
                else                {return mBBC[buffer]->VPD[4]%4096;}
            }
        }else if(mRun<=14001001){
            if(mMIX[buffer]){
                if (eastwest==east) {return mMIX[buffer]->MTD_P2PLayer1[13] + ((mMIX[buffer]->MTD_P2PLayer1[12]&0x0f)<<8);}
                else                {return mMIX[buffer]->MTD_P2PLayer1[9]  + ((mMIX[buffer]->MTD_P2PLayer1[8]&0x0f)<<8);}
            }
        }else {
            if(mMIX[buffer]){
                if (eastwest==east) {return mMIX[buffer]->MTD_P2PLayer1[11] + ((mMIX[buffer]->MTD_P2PLayer1[10]&0xf)<<8);}
                else                {return (mMIX[buffer]->MTD_P2PLayer1[10]>>4) + ((mMIX[buffer]->MTD_P2PLayer1[9]&0xff)<<4);}
            }
        }
    }
    return 0;
}

unsigned short StTriggerData2017::vpdTimeDifference() const
{
    return L1_DSM->VTX[7]%8192;
}


unsigned short StTriggerData2017::bbcVP101(int ch, int prepost) const
{
    int map[8]={3, 2, 1, 0, 7, 6, 5, 4};
    if(ch<0 || ch>7) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && mBBC[buffer]){
        return mBBC[buffer]->VPD[map[ch]];
    }
    return 0;
}


unsigned short StTriggerData2017::dsmTF201Ch(int ch) const   // read TF201 data
{
    int map[8]={3, 2, 1, 0, 7, 6, 5, 4};
    return L1_DSM->TOF[map[ch]];  //ch4-7 currently unused
}

unsigned short StTriggerData2017::mtd4AtAddress(int address, int prepost) const  // read QT4 data
{
    if (mRun<=15001001) return 0;           // Run-14 onwards...
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && address>=0 && address<32) return mxq[buffer][14][address];
    return 0;
}

unsigned short StTriggerData2017::nQTdata(int prepost) const
{
    return 0;
}

unsigned int* StTriggerData2017::QTdata(int prepost) const
{
    return 0;
}

unsigned short StTriggerData2017::fmsADC(int crt, int adr, int ch, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && crt>=1 && crt<=5 && adr>=0 && adr<16 && ch>=0 && ch<=31){
        switch(crt){
            case 1: return qt1[buffer][adr][ch];
            case 2: return qt2[buffer][adr][ch];
            case 3: return qt3[buffer][adr][ch];
            case 4: return qt4[buffer][adr][ch];
            case 5: return epq[buffer][adr][ch];
            case 6: return fq1[buffer][adr][ch];
            case 7: return fq2[buffer][adr][ch];
        }
    }
    return 0;
}

unsigned short StTriggerData2017::fmsTDC(int crt, int adr, int ch, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && crt>=1 && crt<=5 && adr>=0 && adr<16 && ch>=0 && ch<=31){
        switch(crt){
            case 1: return tqt1[buffer][adr][ch];
            case 2: return tqt2[buffer][adr][ch];
            case 3: return tqt3[buffer][adr][ch];
            case 4: return tqt4[buffer][adr][ch];
            case 5: return tepq[buffer][adr][ch];
            case 6: return tfq1[buffer][adr][ch];
            case 7: return tfq2[buffer][adr][ch];
        }
    }
    return 0;
}

unsigned char* StTriggerData2017::getDsm_FMS(int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mFMS[buffer]) return mFMS[buffer]->FMS;
    return 0;
}

unsigned short* StTriggerData2017::getDsm1_FMS(int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0) if (mMIX[buffer]) return mMIX[buffer]->FPDEastNSLayer1;
    return 0;
}

unsigned short* StTriggerData2017::getDsm2_FMS() const {return L1_DSM->FPD;}

unsigned short StTriggerData2017::mxqAtSlotAddress(int address, int prepost, int slot) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && address>=0 && address<32){
        if (slot >= 0 && slot<16){
            return mxq[buffer][slot][address];
        }
    }
    return 0;
}

unsigned short StTriggerData2017::mtdQtAtCh(int qtid, int address, int prepost) const
{ // read all the MTD QTs data, and the range of qtid is 1-8
    int map1[4] = {0, 10, 12, 14};
    int map2[8] = {0,  9, 10, 11, 12, 13, 14, 15};
    
    int buffer = prepostAddress(prepost);
    if(buffer>=0 && qtid>0 && address>=0 && address<32){
        if(mRun<=12003001){
            if(qtid>1) return 0;
            else return mxq[buffer][map1[qtid-1]][address];
        }
        else if(mRun<=14001001){
            if(qtid>2) return 0;
            else return mxq[buffer][map1[qtid-1]][address];
        }
        else if(mRun<=15001001){
            if(qtid>3) return 0;
            else return mxq[buffer][map1[qtid-1]][address];
        }
        else if(mRun<=17001001){
            if(qtid>4) return 0;
            else return mxq[buffer][map1[qtid-1]][address];
        }
        else{
            if(qtid>8) return 0;
            else return mxq[buffer][map2[qtid-1]][address];
        }
    }
    
    return 0;
}

unsigned short StTriggerData2017::mtdAtAddress(int address, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && address>=0 && address<32) return mxq[buffer][0][address];
    return 0;
}

unsigned short StTriggerData2017::mtdgemAtAddress(int address, int prepost) const
{
    if (mRun<=12003001) return 0;
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && address>=0 && address<32) return mxq[buffer][10][address];
    return 0;
}

unsigned short StTriggerData2017::mtd3AtAddress(int address, int prepost) const
{
    if (mRun<=14001001) return 0;			// Run-13 onwards...
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && address>=0 && address<32) return mxq[buffer][12][address];
    return 0;
}


unsigned short StTriggerData2017::mtdAdc(StBeamDirection eastwest, int pmt, int prepost) const
{
    //pmt in not used for 2017, it is place holder for next year
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt==0){
        if (eastwest==east) {
            if (mRun<=10133008) return mxq[buffer][0][0];
            else               return mxq[buffer][0][24];
        }
        if (eastwest==west) return mxq[buffer][0][8];
    }
    return 0;
}

unsigned short StTriggerData2017::mtdTdc(StBeamDirection eastwest, int pmt, int prepost) const
{
    //pmt in not used for 2017, it is place holder for next year
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && pmt==0){
        if (eastwest==east) {
            if (mRun<=10133008) return mxq[buffer][0][4];
            else               return mxq[buffer][0][28];
        }
        if (eastwest==west) return mxq[buffer][0][12];
    }
    return 0;
}

unsigned char StTriggerData2017::mtdDsmAtCh(int ch, int prepost) const
{
    int map[16] = {7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8};
    int map2[8] = {3, 2, 1, 0, 7, 6, 5, 4};
    int buffer = prepostAddress(prepost);
    if (buffer >= 0 && ch>=0){
        if(mMIX[buffer]){
            if(ch<16){
                return mMIX[buffer]->MTD_P2PLayer1[map[ch]];
            }else if(ch<32){
                int add= 8 + map2[(ch-16)/2];
                unsigned char v=0;
                if(ch%2==0){ v=(unsigned char)((mMIX[buffer]->TPCpreMask[add] & 0xff00)>>8);}
                else       { v=(unsigned char)((mMIX[buffer]->TPCpreMask[add] & 0x00ff)   );}
                return v;
            }
        }
    }
    return 0;
}

bool StTriggerData2017::mtdDsmHit(int pmt, int prepost) const
{
    //pmt in not used for 2017, it is place holder for next year
    int buffer = prepostAddress(prepost);
    if (buffer >= 0){
        if(mMIX[buffer]){
            if(mRun<10133008 && mRun<11000000){
                if( (mMIX[buffer]->MTD_P2PLayer1[5] & 0x1) && (mMIX[buffer]->MTD_P2PLayer1[5] & 0x10) ) return true;
            }
            else{
                if(prepost!=0) return false;
                return (L1_DSM->TOF[3] & 0x1);
            }
        }
    }
    return false;
}

unsigned short StTriggerData2017::mtdVpdTacDiff() const
{
    return (L1_DSM->TOF[3] & 0x3fff);
}

unsigned short StTriggerData2017::tofAtAddress(int address, int prepost) const
{
    int buffer = prepostAddress(prepost);
    if (buffer>=0 && address>=0 && address<48) {
        if (mMIX[buffer]) return mMIX[buffer]->TOF[address];
    }
    return 0;
}

unsigned short StTriggerData2017::tofTrayMultiplicity(int tray, int prepost) const
{
    int dsmmap[8] = {3,2,1,0,7,6,5,4};
    int traydsm[120] = { 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1};
    int traych[120]  = { 2, 1, 0, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 9, 8, 7, 6, 5, 4, 3,
        2, 1, 0, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 9, 8, 7, 6, 5, 4, 3,
        2, 1, 0, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 9, 8, 7, 6, 5, 4, 3,
        18,19,10,11,12,13,14,15,16,17,18,19,10,11,12,13,14,15,16,17,
        18,19,10,11,12,13,14,15,16,17,18,19,10,11,12,13,14,15,16,17,
        18,19,10,11,12,13,14,15,16,17,18,19,10,11,12,13,14,15,16,17};
    int buffer = prepostAddress(prepost);
    if (buffer>=0 && tray>=1 && tray<=120) {
        if (mMIX[buffer]) {
            int address = traydsm[tray-1]*8 + dsmmap[traych[tray-1]/3];
            int ch = traych[tray-1]%3;
            return (mMIX[buffer]->TOF[address] >> (5*ch)) & 0x1f;
        }
    }
    return 0;
}

unsigned short StTriggerData2017::tofMultiplicity(int prepost) const
{
    if (prepost==0) return L1_DSM->TOF[1]%8192;
    return 0;
}

void StTriggerData2017::dump() const
{
    printf("***** StTriggerData Dump *****\n");
    printf(" mDebug=%d mData=%p\n",mDebug,mData);
    printf(" Year=%d  Version=%x\n",year(),version());
    printf(" Run#=%d Event#=%d\n",mRun,eventNumber());
    printf(" %d pre and %d post crossing data available\n",numberOfPreXing(),numberOfPostXing());
    printf(" Token=%d  TriggerWord=%x  ActionWord=%x  BusyStatus=%x\n",
           token(), triggerWord(), actionWord(), busyStatus());
    printf(" TUC Bits=%d  : ",tcuBits());
    for (int i=0; i<16; i++) {printf(" %d",(tcuBits()>>(15-i))%2);}; printf("\n");
    printf(" BunchId 7bit=%d  48bit=%d\n",bunchId7Bit(), bunchId48Bit());
    printf(" Spin Bits=%d  : ",spinBit());
    for (int i=0; i<8; i++) {printf(" %d",(spinBit()>>(7-i))%2);}; printf("\n");
    //    printf(" CTB ADC : ");       for (int i=0; i<240;i++){ printf("%d ",ctb(i,0));      }; printf("\n");
    printf(" BBC East ADC : ");  for (int i=1; i<=24;i++){ printf("%d ",bbcADC(east,i,0)); }; printf("\n");
    printf(" BBC West ADC : ");  for (int i=1; i<=24;i++){ printf("%d ",bbcADC(west,i,0)); }; printf("\n");
    printf(" BBC East TAC : ");  for (int i=1; i<=16;i++){ printf("%d ",bbcTDC(east,i,0)); }; printf("\n");
    printf(" BBC West TAC : ");  for (int i=1; i<=16;i++){ printf("%d ",bbcTDC(west,i,0)); }; printf("\n");
    for (int i=-numberOfPreXing(); i<=static_cast<int>(numberOfPostXing()); i++){
        printf(" BBC Sums %d xing : ",i);
        printf("East=%d  West=%d   Large tile East=%d  West=%d\n",
               bbcADCSum(east,i),bbcADCSum(west,i),
               bbcADCSumLargeTile(east,i),bbcADCSumLargeTile(west,i));
    }
    printf(" BBC Earilest : ");  printf("East=%d  West=%d  Difference+256=%d\n",
                                        bbcEarliestTDC(east,0),bbcEarliestTDC(west,0),bbcTimeDifference());
    printf(" ZDC Earilest : ");  printf("East=%d  West=%d  Difference=%d\n",
                                        zdcEarliestTDC(east,0),zdcEarliestTDC(west,0),zdcTimeDifference());
    //printf(" FPD East North   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(east,0,i,0));  }; printf("\n");
    //printf(" FPD East South   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(east,1,i,0));  }; printf("\n");
    //printf(" FPD East Top     : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(east,2,i,0));  }; printf("\n");
    //printf(" FPD East Bottom  : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(east,3,i,0));  }; printf("\n");
    //printf(" FPD East North PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(east,4,i,0));  }; printf("\n");
    //printf(" FPD East South PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(east,5,i,0));  }; printf("\n");
    //printf(" FPD West South   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(west,1,i,0));  }; printf("\n");
    //printf(" FPD West Bottom  : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(west,3,i,0));  }; printf("\n");
    //printf(" FPD West South PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(west,5,i,0));  }; printf("\n");
    //printf(" FPD Sums East    : ");for (int j=0; j<4 ;j++) printf("%d ",fpdSum(east,j));        printf("\n");
    //printf(" FPD Sums West    : ");for (int j=0; j<4 ;j++) printf("%d ",fpdSum(west,j));        printf("\n");
    printf(" ZDC Sum(A) East  : ");printf("%d ",zdcAttenuated(east));        printf("\n");
    printf(" ZDC Sum(A) West  : ");printf("%d ",zdcAttenuated(west));        printf("\n");
    printf(" ZDC Sum(UA) East : ");printf("%d ",zdcUnAttenuated(east));      printf("\n");
    printf(" ZDC Sum(UA) West : ");printf("%d ",zdcUnAttenuated(west));      printf("\n");
    printf(" VPD E Earliest TAC : %d\n", vpdEarliestTDC(east));
    printf(" VPD W Earliest TAC : %d\n", vpdEarliestTDC(west));
    printf(" VPD TimeDifference : %d\n", vpdTimeDifference());
    printf(" L2 result : \n");
    for (int j=0; j<4 ;j++) { for (int k=0; k<16; k++) {printf("%u ",*(l2Result()+j*16+k)); } printf("\n");}
    printf("BBClayer1:");
    int buffer = prepostAddress(0);
    if (buffer >=0){
        if (mBBC[buffer]){
            for (int i = 0;i < 16;i++) printf(" %1x %04X", i, mBBC[buffer]->BBClayer1[i]);
        }
    }
    printf("\n");
    printf("ZDClayer1:");
    if (buffer >=0){
        if (mBBC[buffer]){
            for (int i = 0;i < 8;i++) printf(" %1x %04X", i, mBBC[buffer]->ZDClayer1[i]);
        }
    }
    printf("\n");
    printf("VPDlayer1:");
    if (buffer >=0){
        if (mBBC[buffer]){
            for (int i = 0;i < 8;i++) printf(" %1x %04X", i, mBBC[buffer]->VPD[i]);
        }
    }
    printf("\n");
    printf("VTX:");
    if (L1_DSM){
        for (int i = 0;i < 8;i++) printf(" %1x %04X", i, L1_DSM->VTX[i]);
    }
    printf("\n");
    printf("Last DSM:");
    if (L1_DSM){
        for (int i = 0;i < 8;i++) printf(" %1x %04X", i, L1_DSM->lastDSM[i]);
    }
    printf("\n");
    printf("***** End StTriggerData Dump *****\n");
}

void StTriggerData2017::killFMS(){
    TrgOfflen2017* offlen;
    int npre  = numberOfPreXing();
    int npost = numberOfPostXing();
    for (int i=0; i<1+npre+npost; i++){
        if (i==0)
        {offlen = mData->MainX;}
        else {
            if (mData->PrePostList[i-1]==0) continue;
            offlen = (TrgOfflen2017*) ((char*)mData + mData->PrePostList[i-1]);
        }
        int j;
        j=offlen[y17QT1_CONF_NUM].length; if (j>0){memset((char*)mData + offlen[y17QT1_CONF_NUM].offset, 0, j); offlen[y17QT1_CONF_NUM].length=0;};
        j=offlen[y17QT2_CONF_NUM].length; if (j>0){memset((char*)mData + offlen[y17QT2_CONF_NUM].offset, 0, j); offlen[y17QT2_CONF_NUM].length=0;};
        j=offlen[y17QT3_CONF_NUM].length; if (j>0){memset((char*)mData + offlen[y17QT3_CONF_NUM].offset, 0, j); offlen[y17QT3_CONF_NUM].length=0;};
        j=offlen[y17QT4_CONF_NUM].length; if (j>0){memset((char*)mData + offlen[y17QT4_CONF_NUM].offset, 0, j); offlen[y17QT4_CONF_NUM].length=0;};
    }
}

void StTriggerData2017::swapRawDet(DataBlock2017* data, int name, int hlength,int bs)
{
    BELayerBlock2017* bc1;
    MIXBlock2017* mix;
    BBCBlock2017 *bbc;
    QTBlock2017* qtdata;
    int header_length = 8;
    if(bs) swapI((unsigned int*)&data->length);
    switch(name){
        case y17MXQ_CONF_NUM : case y17EPQ_CONF_NUM : case y17BBQ_CONF_NUM : 
        case y17QT1_CONF_NUM : case y17QT2_CONF_NUM : case y17QT3_CONF_NUM : case y17QT4_CONF_NUM :
        case y17FQ1_CONF_NUM : case y17FQ2_CONF_NUM :
            header_length = 12; break;
    }
    if (hlength != data->length + header_length){
        mErrorFlag = mErrorFlag | (1 << name);
        printf("StTriggerData2017: Error reading Block=%2d [%1c%1c%1c%1c] length %d != %d + %d\n",
               name,data->name[0],data->name[1],data->name[2],data->name[3],
               hlength,data->length,header_length);      
        printf("StTriggerData2017: Droping the data block =%2d [%1c%1c%1c%1c] with ErrorFlag=0x%x\n",
               name,data->name[0],data->name[1],data->name[2],data->name[3],mErrorFlag);
        data=0;
        return;
    }
    if (bs){
        switch(name){
            case y17BC1_CONF_NUM :
                bc1 = (BELayerBlock2017*) data;
                swapSSn((unsigned int*)bc1->BEMClayer1,48);
                swapSSn((unsigned int*)bc1->EEMClayer1,16);
                break;
            case y17MIX_CONF_NUM :
                mix = (MIXBlock2017*) data;
                swapSSn((unsigned int*)mix->FPDEastNSLayer1,8);
                swapSSn((unsigned int*)mix->TOFLayer1,8+48);
                break;
            case y17BCW_CONF_NUM :
                //only char
                break;
            case y17BCE_CONF_NUM :
                //only char
                break;
            case y17BBC_CONF_NUM :
                bbc = (BBCBlock2017*) data;
                swapSSn((unsigned int*)bbc->BBClayer1,16+8+8);
                break;
            case y17FMS_CONF_NUM :
                //only char
                break;
            case y17MXQ_CONF_NUM :
            case y17EPQ_CONF_NUM :
            case y17BBQ_CONF_NUM :
            case y17QT1_CONF_NUM :
            case y17QT2_CONF_NUM :
            case y17QT3_CONF_NUM :
            case y17QT4_CONF_NUM :
            case y17FQ1_CONF_NUM :
            case y17FQ2_CONF_NUM :
                qtdata = (QTBlock2017*) data;
                swapI((unsigned int*)&qtdata->dataLoss);
                swapIn(qtdata->data, qtdata->length/4);
                break;
        }
    }
    if(mDebug>0) 
        printf("Read id=%2d name=%1c%1c%1c%1c length=%d\n",
               name,data->name[0],data->name[1],data->name[2],data->name[3],data->length);
}

void StTriggerData2017::Streamer(TBuffer &R__b)
{
	// Stream an object of class StTriggerData2017.

	if (R__b.IsReading()) {
		R__b.ReadClassBuffer(StTriggerData2017::Class(),this);
		//     cout << "StTriggerData2017::Streamer read trigger data!!!"<<endl;
		if(mData) readData();
	}
	else {
		R__b.WriteClassBuffer(StTriggerData2017::Class(),this);
	}
}
