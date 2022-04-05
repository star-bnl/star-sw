 /***************************************************************************
 *
 * $Id: StTriggerData2008.cxx,v 2.6 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Akio Ogawa, Nov 2007
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData for 2008.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2008.cxx,v $
 * Revision 2.6  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.5  2009/03/19 02:46:01  ullrich
 * Add 2nd argument (pre/post) to vpdEarliestTDC().
 *
 * Revision 2.4  2008/03/13 16:58:28  ullrich
 * Move include file from .cxx to .h file
 *
 * Revision 2.3  2008/03/10 19:35:31  ullrich
 * New methods: tofAtAddress() and tofMultiplicity().
 *
 * Revision 2.2  2007/12/11 18:11:13  ullrich
 * Fix bugs in QT decoding (Akio).
 *
 * Revision 2.1  2007/11/19 19:32:17  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include <string.h>
#include <assert.h>
#include <iostream>
#include "StTriggerData2008.h"

ClassImp(StTriggerData2008)

StTriggerData2008::StTriggerData2008()
{
    mYear=2008;
    mData=0;
}

StTriggerData2008::StTriggerData2008(const TrgDataType2008* data, int run)
{
    mYear=2008;
    mRun = run;
    mData= new TrgDataType2008;
    int npre  = data->EvtDesc.npre;
    int npost = data->EvtDesc.npost;
    assert(npre >=0);    
    assert(npre <=5);    
    assert(npost>=0);    
    assert(npost<=5);    
    int size = sizeof(EvtDescData2008)+sizeof(TrgSumData2008)
        + sizeof(RawTrgDet2008)*(npre+npost+1);    
    memcpy(mData,data,size); 
    memset((char*)mData+size,0,sizeof(TrgDataType2008)-size);
    //dump();
}

unsigned int StTriggerData2008::version() const
{
    return mData->EvtDesc.TrgDataFmtVer;
}

StTriggerData2008::~StTriggerData2008() {delete mData;}

unsigned int StTriggerData2008::token() const
{
    return mData->EvtDesc.TrgToken;
}

unsigned int StTriggerData2008::triggerWord() const
{
    return mData->EvtDesc.TriggerWord;
}

unsigned int StTriggerData2008::actionWord() const
{
    return
        ( (unsigned short)(mData->EvtDesc.actionWdTrgCommand) * 16 * 16 * 16 ) +
        ( (unsigned short)(mData->EvtDesc.actionWdDaqCommand) * 16 * 16      ) +
        (                  mData->EvtDesc.actionWdDetectorBitMask & 0x00ff   ) ;
}

unsigned int StTriggerData2008::numberOfPreXing() const
{
    return mData->EvtDesc.npre;
}

unsigned int StTriggerData2008::numberOfPostXing() const
{
    return mData->EvtDesc.npost;
}

unsigned short StTriggerData2008::busyStatus() const{
    return mData->EvtDesc.modifiedBusyStatus;
}

unsigned short StTriggerData2008::dsmInput() const{
    return mData->EvtDesc.DSMInput;
}

unsigned short StTriggerData2008::trgToken() const{
    return mData->EvtDesc.TrgToken;
}

unsigned short StTriggerData2008::dsmAddress() const{
    return mData->EvtDesc.DSMAddress;
}

unsigned short StTriggerData2008::mAddBits() const{
    return mData->EvtDesc.addBits;
}

unsigned short StTriggerData2008::bcData(int channel) const{
    return mData->TrgSum.DSMdata.BCdata[channel];
}

unsigned short StTriggerData2008::lastDSM(int channel) const{
    return mData->TrgSum.DSMdata.lastDSM[channel];
}

unsigned short StTriggerData2008::tcuBits() const
{
    return mData->EvtDesc.DSMInput;
}

unsigned int StTriggerData2008::bunchCounterHigh() const
{
    return mData->EvtDesc.bunchXing_hi;
}

unsigned int StTriggerData2008::bunchCounterLow() const
{
    return mData->EvtDesc.bunchXing_lo;
}

unsigned int StTriggerData2008::bunchId48Bit() const
{
    unsigned long long bxinghi,bxing1,bxinglo, bx;
    bxinghi = mData->TrgSum.DSMdata.BCdata[3];
    bxing1 =  mData->TrgSum.DSMdata.BCdata[10];
    bxinglo = (bxing1 << 16) + mData->TrgSum.DSMdata.BCdata[11];
    bx = (bxinghi << 32) + bxinglo;
    return (int)(bx % 120);
} 

unsigned int StTriggerData2008::bunchId7Bit() const
{
    int b7=0, b7dat; 
    b7dat = mData->TrgSum.DSMdata.BCdata[2];
    b7 = b7dat & 0x7f;
    return b7;
}

unsigned int StTriggerData2008::spinBit() const
{
    return (mData->TrgSum.DSMdata.lastDSM[7]/16)%256;
}

unsigned int StTriggerData2008::spinBitYellowFilled() const
{
    unsigned int sb = spinBit();
    return sb%2;
}

unsigned int StTriggerData2008::spinBitYellowUp() const
{
    unsigned int sb = spinBit();
    return (sb/2)%2;
}

unsigned int StTriggerData2008::spinBitYellowDown() const
{
    unsigned int sb = spinBit();
    return (sb/4)%2;
}

unsigned int StTriggerData2008::spinBitYellowUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/8)%2;
}

unsigned int StTriggerData2008::spinBitBlueFilled() const
{
    unsigned int sb = spinBit();
    return (sb/16)%2;
}

unsigned int StTriggerData2008::spinBitBlueUp() const
{
    unsigned int sb = spinBit();
    return (sb/32)%2;
}

unsigned int StTriggerData2008::spinBitBlueDown() const
{
    unsigned int sb = spinBit();
    return (sb/64)%2;
}

unsigned int StTriggerData2008::spinBitBlueUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/128)%2;
}

unsigned short  StTriggerData2008::ctbRaw(int address, int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].CTB[address];
}

unsigned short  StTriggerData2008::ctb(int pmt, int prepost) const
{
    static const unsigned char ctbMap[240] = {
        7,  6,  5,  4,  3, 23, 22, 21, 20, 19,
        2,  1,  0, 15, 14, 18, 17, 16, 31, 30,
        13, 12, 11, 10,  9, 29, 28, 27, 26, 25,
        39, 38, 37, 36, 35, 55, 54, 53, 52, 51,
        34, 33, 32, 47, 46, 50, 49, 48, 63, 62,
        45, 44, 43, 42, 41, 61, 60, 59, 58, 57,
        71, 70, 69, 68, 67, 87, 86, 85, 84, 83,
        66, 65, 64, 79, 78, 82, 81, 80, 95, 94,
        77, 76, 75, 74, 73, 93, 92, 91, 90, 89,
        103, 102, 101, 100, 99, 119, 118, 117, 116, 115,
        98, 97, 96, 111, 110, 114, 113, 112, 127, 126,
        109, 108, 107, 106, 105, 125, 124, 123, 122, 121,
        135, 134, 133, 132, 131, 151, 150, 149, 148, 147,
        130, 129, 128, 143, 142, 146, 145, 144, 159, 158,
        141, 140, 139, 138, 137, 157, 156, 155, 154, 153,
        167, 166, 165, 164, 163, 183, 182, 181, 180, 179,
        162, 161, 160, 175, 174, 178, 177, 176, 191, 190,
        173, 172, 171, 170, 169, 189, 188, 187, 186, 185,
        199, 198, 197, 196, 195, 215, 214, 213, 212, 211,
        194, 193, 192, 207, 206, 210, 209, 208, 223, 222,
        205, 204, 203, 202, 201, 221, 220, 219, 218, 217,
        231, 230, 229, 228, 227, 247, 246, 245, 244, 243,
        226, 225, 224, 239, 238, 242, 241, 240, 255, 254,
        237, 236, 235, 234, 233, 253, 252, 251, 250, 249,
    } ;
    int v=0;
    int add=prepostAddress(prepost);
    if(add>=0) v=mData->rawTriggerDet[add].CTB[ctbMap[pmt]];
    return v;
}

unsigned short StTriggerData2008::ctbTraySlat(int tray, int slat, int prepost) const{
    static const unsigned char ctbMap[2][120] = {
        { 109, 108, 107, 106, 105,   7,   6,   5,   4,   3,
	2,   1,   0,  15,  14,  13,  12,  11,  10,   9,
	39,  38,  37,  36,  35,  34,  33,  32,  47,  46,
	45,  44,  43,  42,  41,  71,  70,  69,  68,  67,
	66,  65,  64,  79,  78,  77,  76,  75,  74,  73,
	103, 102, 101, 100,  99,  98,  97,  96, 111, 110,
	141, 140, 139, 138, 137, 167, 166, 165, 164, 163, 
	162, 161, 160, 175, 174, 173, 172, 171, 170, 169,
	199, 198, 197, 196, 195, 194, 193, 192, 207, 206,
	205, 204, 203, 202, 201, 231, 230, 229, 228, 227,
	226, 225, 224, 239, 238, 237, 236, 235, 234, 233,
	135, 134, 133, 132, 131, 130, 129, 128, 143, 142},
        { 125, 124, 123, 122, 121,  23,  22,  21,  20,  19,
	18,  17,  16,  31,  30,  29,  28,  27,  26,  25,
	55,  54,  53,  52,  51,  50,  49,  48,  63,  62,
	61,  60,  59,  58,  57,  87,  86,  85,  84,  83,
	82,  81,  80,  95,  94,  93,  92,  91,  90,  89,
	119, 118, 117, 116, 115, 114, 113, 112, 127, 126,
	157, 156, 155, 154, 153, 183, 182, 181, 180, 179,
	178, 177, 176, 191, 190, 189, 188, 187, 186, 185,
	215, 214, 213, 212, 211, 210, 209, 208, 223, 222,
	221, 220, 219, 218, 217, 247, 246, 245, 244, 243,
	242, 241, 240, 255, 254, 253, 252, 251, 250, 249,
	151, 150, 149, 148, 147, 146, 145, 144, 159, 158}
    };
    int v=0;
    int add=prepostAddress(prepost);    
    if(add>=0) v=mData->rawTriggerDet[add].CTB[ctbMap[slat][tray]];
    return v;
}

unsigned short StTriggerData2008::ctbSum(int prepost) const{
    unsigned short sum=0;
    for (int i=1; i<240; i++){sum+=ctb(i,prepost);}
    return sum;
}

unsigned short StTriggerData2008::bbcADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int q_map[2][24] = { 
        { 8  , 5  , 4  , 40 , 37 , 36 , 7  , 6  ,
	3  , 2  , 1  , 39 , 38 , 35 , 34 , 33 ,
	72 , 71 , 70 , 69 , 68 , 67 , 66 , 65 },
        { 24 , 21 , 20 , 56 , 53 , 52 , 23 , 22 ,
	19 , 18 , 17 , 55 , 54 , 51 , 50 , 49 ,
	88 , 87 , 86 , 85 , 84 , 83 , 82 , 81 } 
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].BBC[q_map[eastwest][pmt-1]-1];
}

unsigned short StTriggerData2008::bbcTDC(StBeamDirection eastwest, int pmt, int prepost) const
{      
    static const int t_map[2][24] ={ 
        { 16 , 13 , 12 , 48 , 45 , 44 , 15 , 14 ,
	11 , 10 , 9  , 47 , 46 , 43 , 42 , 41 ,
	80 , 79 , 78 , 77 , 76 , 75 , 74 , 73 },    
        { 32 , 29 , 28 , 64 , 61 , 60 , 31 , 30 ,
	27 , 26 , 25 , 63 , 62 , 59 , 58 , 57 ,
	96 , 95 , 94 , 93 , 92 , 91 , 90 , 89 }
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].BBC[t_map[eastwest][pmt-1]-1];
}

unsigned short StTriggerData2008::bbcADCSum(StBeamDirection eastwest, int prepost) const
{
    int address = prepostAddress(prepost);
    if (eastwest==east){
        return 
	  mData->rawTriggerDet[address].BBClayer1[7]%2048+
	  mData->rawTriggerDet[address].BBClayer1[3]%2048;
    }
    else {
        return 
	  mData->rawTriggerDet[address].BBClayer1[5]%2048+
	  mData->rawTriggerDet[address].BBClayer1[1]%2048;
    }
}

unsigned short StTriggerData2008::bbcADCSumLargeTile(StBeamDirection eastwest, int prepost) const
{
    int address = prepostAddress(prepost);
    if (eastwest==east) { return mData->rawTriggerDet[address].BBClayer1[11]%2048; }
    else               { return mData->rawTriggerDet[address].BBClayer1[10]%2048; }
}

unsigned short StTriggerData2008::bbcEarliestTDC(StBeamDirection eastwest, int prepost) const
{
    int address = prepostAddress(prepost), t1, t2;
    if (eastwest==east){
        t1 = mData->rawTriggerDet[address].BBClayer1[6]%256;
        t2 = mData->rawTriggerDet[address].BBClayer1[2]%256;
    }
    else {
        t1 = mData->rawTriggerDet[address].BBClayer1[4]%256;
        t2 = mData->rawTriggerDet[address].BBClayer1[0]%256;
    }
    return (t1>t2) ? t1 : t2;
}

unsigned short StTriggerData2008::bbcTimeDifference() const
{
    return mData->TrgSum.DSMdata.VTX[3]%512;
}

unsigned short StTriggerData2008::fpd(StBeamDirection eastwest, int module, int pmt, int prepost) const
{
    static const short fpdmap[2][6][49] = {
        //East
        {
	  //East North
	  {39, 38, 37, 36, 35, 34, 33,
	   7,  6,  5, 23, 22, 21, 55,
	   4,  3,  2, 20, 19, 18, 54,
	   1,  0, 15, 17, 16, 31, 53,
	   14, 13, 12, 30, 29, 28, 52,
	   11, 10,  9, 27, 26, 25, 51,
	   32, 47, 46, 45, 44, 43, 42},
	  //East South
	  { 103,101,100, 99,  98,  97, 96,
	    71, 70, 69, 87,  86,  85, 48,
	    68, 67, 66, 84,  83,  82, 63,
	    65, 64, 79, 81,  80,  95, 61,
	    78, 77 ,76, 94,  93,  92, 60,
	    75, 74, 73, 91,  90,  89, 59,
	    111,110,109,108, 107, 106,105},
	  //East Top
	  {135, 134, 133, 132, 131, 130, 129, 128, 143, 142,
	   119, 118, 117, 116, 115, 114, 113, 112,
	   127, 126, 125, 124, 123, 122, 121,
	   -1,  -1,  -1,
	   -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	   -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	   -1,  -1,  -1,  -1,  -1,  -1,  -1},
	  //East Bottom
	  {151, 150, 149, 148, 147, 146, 145, 144,
	   159, 158, 157, 156, 155, 154, 153,
	   167, 166, 165, 164, 163, 162, 161, 160, 175, 174,
	   -1,  -1,  -1,
	   -1,  -1,  -1,  -1,  -1,  -1,  -1,
	   -1,  -1,  -1,  -1,  -1,  -1,  -1,
	   -1,  -1,  -1,  -1,  -1,  -1,  -1},
	  //East North PreShower
	  {  50, 49, 141, 140, 139, 138, 137,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	  //East South PreShower
	  { 58, 57, 173, 172, 171, 170, 169,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1,
	    -1,  -1,  -1,  -1,  -1,  -1,  -1},
        },
        //West
        {
	  //West North
	  {  -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1,
	     -1, -1, -1, -1,  -1,  -1, -1},
	  //West South
	  {  71, 70, 87, 86,  96,  97, 48, 
	     69, 68, 85, 84,  98,  99, 63, 
	     67, 66, 83, 82, 100, 101, 62,
	     65, 64, 81, 80, 102, 103, 61, 
	     79, 78, 90, 91, 106, 107, 60, 
	     77, 76, 92, 93, 108, 109, 59,
	     75, 74, 94, 95, 110, 111, 58},
	  //West Top
	  {  -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1,
               -1, -1, -1, -1,  -1,  -1, -1},
	  //West Bottom
	  {  77, 70, 69, 68, 67,
	     66, 65, 64, 79, 78,
	     87, 86, 85, 84, 83,
	     82, 81, 80, 95, 94,
	     93, 76, 91, 90, 89,
	     -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	  //West North PreShower
	  {  -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	  //West South PreShower
	  {  7,   6,   5,   4,   3,   2,   1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
        }
    };
    int address = fpdmap[eastwest][module][pmt-1];
    if (address>=0){
        if (eastwest==east){
	  if (address<112) return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer0[address];
	  else             return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer0[address-112];
        }
        else {
	  return 0;  
        }
    }
    else {
        return 0;
    }
}

unsigned short StTriggerData2008::fpdSum(StBeamDirection eastwest, int module) const
{
    static const short map[2][4]={{3,2,1,0},{7,6,5,4}};
    static const short nbit[2][4]={{16384,16384,8192,8192},{16384,16384,8192,8192}};
    return mData->TrgSum.DSMdata.FPD[map[eastwest][module]] % nbit[eastwest][module];
}

unsigned short StTriggerData2008::fpdLayer1DSMRaw(StBeamDirection eastwest, int channel, int prepost) const{
    if (eastwest==east){
        if (channel<8)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer1[channel];
    }
    return 0;
}

unsigned short StTriggerData2008::fpdLayer1DSM(StBeamDirection eastwest, int module, int board, int prepost) const{
    static const short map[4][4]={{3,2,1,0},{7,6,5,4},{3,2,6,7},{1,0,4,5}};
    if (board<4){
        if (eastwest==east){
	  if (module<2)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer1[map[module][board]];
	  else           return 0;
        }
        else{ 
	  return 0;
        }
    }
    return 0;
}

unsigned short StTriggerData2008::fpdLayer2DSMRaw(int channel) const{
    if (channel<8) return mData->TrgSum.DSMdata.FPD[channel];
    return 0;
}

unsigned short StTriggerData2008::fpdLayer2DSM(StBeamDirection eastwest, int module) const{
    static const int dsmmap[2][4]={{3,2,1,0},{7,6,5,4}};
    if (module<4) return mData->TrgSum.DSMdata.FPD[dsmmap[eastwest][module]];
    return 0;
}

unsigned short StTriggerData2008::zdcAtChannel(int channel, int prepost) const
{
    static const int dsmmap[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
    if (channel>=0 && channel<16){ return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[dsmmap[channel]]; }
    return 0;
}

unsigned short StTriggerData2008::zdcAtAddress(int address, int prepost) const
{
    if (address>=0 && address<16){ return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[address]; }
    return 0;
}

unsigned short StTriggerData2008::zdcUnAttenuated(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[4];} // fixed bug: was 3
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[0];}
    return 0;
}

unsigned short StTriggerData2008::zdcAttenuated(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[13];}
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[10];}
    return 0;
}

unsigned short StTriggerData2008::zdcADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    if (eastwest==east && pmt==1) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[7];}
    if (eastwest==east && pmt==2) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[6];}
    if (eastwest==east && pmt==3) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[5];}
    if (eastwest==west && pmt==1) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[3];}
    if (eastwest==west && pmt==2) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[2];}
    if (eastwest==west && pmt==3) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[1];}
    return 0;
    
}

unsigned short StTriggerData2008::zdcTDC(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[8];}
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[9];}
    return 0;
}

unsigned short StTriggerData2008::zdcHardwareSum(int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[14];
}

unsigned short StTriggerData2008::zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost) const
{
    // new map from Gang Wang (gwang@physics.ucla.edu) 2008 March 26
    static const int zdcsmd_map[2][2][8] ={
        { { 7, 6, 5, 4, 3, 2, 1,11} ,  
	{ 0,15,14,13,12,8, 10, 9} } ,
        { {23,22,21,20,19,18,17,26} ,
	{16,31,30,29,28,27,24,25} }
    };
    if (verthori<0 || verthori>1) return 0;
    if (strip<1 || strip>8) return 0;  //the last one in vertical strips is for LED. Could be used for Forward counter later. T.A.H.
    int add=-1;    
    add=zdcsmd_map[eastwest][verthori][strip-1];    
    if(add>=0) return mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[add];
    else return 0;
};

unsigned short StTriggerData2008::bemcLayer1DSM(int channel, int prepost) const {
    const int n_bemc_layer1=48;
    if (channel<0 || channel >=n_bemc_layer1) {
        gMessMgr->Warning() << "Barrel DSM layer 1 out of range (" << channel << ")" << endm;
        return 0;
    }
    return mData->rawTriggerDet[prepostAddress(prepost)].BEMClayer1[channel];
} 

unsigned short StTriggerData2008::eemcLayer1DSM(int channel, int prepost) const {
    const int n_eemc_layer1=48;
    if (channel<0 || channel >=n_eemc_layer1) {
        gMessMgr->Warning() << "Endap DSM layer 1 out of range (" << channel << ")" << endm;
        return 0;
    }
    return mData->rawTriggerDet[prepostAddress(prepost)].EEMClayer1[channel];
} 

unsigned short StTriggerData2008::emcLayer2DSM(int channel) const {
    const int n_emc_layer2=8;
    if (channel<0 || channel >=n_emc_layer2) {
        gMessMgr->Warning() << "EMC DSM layer 2 out of range (" << channel << ")" << endm;
        return 0;
    }
    return mData->TrgSum.DSMdata.EMC[channel];
} 

unsigned char StTriggerData2008::bemcHighTower(int patch_id, int prepost) const
{
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Barrel patch id: " << patch_id << endm;
        return 0;
    }
    
    int dsm=patch_id/10;
    int channel=patch_id%10;
    unsigned short trg_word;
    if (dsm>=15) 
        trg_word=decodeEmc12bit(dsm-15,channel,mData->rawTriggerDet[prepostAddress(prepost)].BEMCEast);
    else 
        trg_word=decodeEmc12bit(dsm,channel,mData->rawTriggerDet[prepostAddress(prepost)].BEMCWest);
    return trg_word & 0x3F;
}

unsigned char StTriggerData2008::bemcJetPatch (int patch_id, int prepost) const
{
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Barrel patch id: " << patch_id << endm;
        return 0;
    }
    
    int dsm=patch_id/10;
    int channel=patch_id%10;
    unsigned short trg_word;
    if (dsm>=15) 
        trg_word=decodeEmc12bit(dsm-15,channel,mData->rawTriggerDet[prepostAddress(prepost)].BEMCEast);
    else 
        trg_word=decodeEmc12bit(dsm,channel,mData->rawTriggerDet[prepostAddress(prepost)].BEMCWest);
    return trg_word >> 6;
}

unsigned char StTriggerData2008::eemcHighTower(int patch_id, int prepost) const
{
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90; 
    
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Endcap patch id" << endm;
        return 0;
    }
    
    int dsm=patch_id/10;
    int channel=patch_id%10;
    unsigned short trg_word;
    trg_word=decodeEmc12bit(dsm,channel,mData->rawTriggerDet[prepostAddress(prepost)].EEMC);
    return trg_word & 0x3F;
}

unsigned char StTriggerData2008::eemcJetPatch (int patch_id, int prepost) const
{
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90; 
    
    if ( patch_id < 0 || patch_id >= m_max_patch) {
        gMessMgr->Warning() << "Invalid Endcap patch id" << endm;
        return 0;
    }
    
    int dsm=patch_id/10;
    int channel=patch_id%10;
    unsigned short trg_word;
    trg_word=decodeEmc12bit(dsm,channel,mData->rawTriggerDet[prepostAddress(prepost)].EEMC);
    return trg_word >> 6;
}

unsigned char StTriggerData2008::bemcHighestTowerADC(int prepost) const
{
    // Unpacking of Bemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=300; // Full barrel
    unsigned char h=0;
    for (int i=1; i<m_max_patch; i++){
        unsigned char hh=bemcHighTower(i,prepost);
        if (h>hh) h=hh;
    }
    return h;
}

unsigned char StTriggerData2008::eemcHighestTowerADC(int prepost) const
{
    // Unpacking of Eemc trigger data (level 0 DSM input, trigger patches)
    const int m_max_patch=90;
    unsigned char h=0;
    for (int i=1; i<m_max_patch; i++){
        unsigned char hh=eemcHighTower(i,prepost);
        if (h>hh) h=hh;
    }
    return h;
}

void StTriggerData2008::dump() const
{
    printf("***** StTriggerData Dump *****\n");
    printf(" Year=%d  Version=%x\n",year(),version());
    printf(" %d pre and %d post crossing data available\n",numberOfPreXing(),numberOfPostXing());
    printf(" Token=%d  TriggerWord=%x  ActionWord=%x  BusyStatus=%x\n",
	 token(), triggerWord(), actionWord(), busyStatus());    
    printf(" TUC Bits=%d  : ",tcuBits());
    for (int i=0; i<16; i++) {printf(" %d",(tcuBits()>>(15-i))%2);}; printf("\n");
    printf(" BunchId 7bit=%d  48bit=%d\n",bunchId7Bit(), bunchId48Bit());  
    printf(" Spin Bits=%d  : ",spinBit());
    for (int i=0; i<8; i++) {printf(" %d",(spinBit()>>(7-i))%2);}; printf("\n");
    printf(" CTB ADC : ");       for (int i=0; i<240;i++){ printf("%d ",ctb(i,0));      }; printf("\n");
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
    printf(" FPD East North   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(east,0,i,0));  }; printf("\n");
    printf(" FPD East South   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(east,1,i,0));  }; printf("\n");
    printf(" FPD East Top     : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(east,2,i,0));  }; printf("\n");
    printf(" FPD East Bottom  : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(east,3,i,0));  }; printf("\n");
    printf(" FPD East North PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(east,4,i,0));  }; printf("\n");
    printf(" FPD East South PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(east,5,i,0));  }; printf("\n");
    printf(" FPD West South   : ");for (int i=1; i<=49;i++){ printf("%d ",fpd(west,1,i,0));  }; printf("\n");
    printf(" FPD West Bottom  : ");for (int i=1; i<=25;i++){ printf("%d ",fpd(west,3,i,0));  }; printf("\n");
    printf(" FPD West South PS: ");for (int i=1; i<= 7;i++){ printf("%d ",fpd(west,5,i,0));  }; printf("\n");
    printf(" FPD Sums East    : ");for (int j=0; j<4 ;j++) printf("%d ",fpdSum(east,j));        printf("\n");
    printf(" FPD Sums West    : ");for (int j=0; j<4 ;j++) printf("%d ",fpdSum(west,j));        printf("\n");
    printf(" ZDC Sum(A) East  : ");printf("%d ",zdcAttenuated(east));        printf("\n");
    printf(" ZDC Sum(A) West  : ");printf("%d ",zdcAttenuated(west));        printf("\n");
    printf(" ZDC Sum(UA) East : ");printf("%d ",zdcUnAttenuated(east));      printf("\n");
    printf(" ZDC Sum(UA) West : ");printf("%d ",zdcUnAttenuated(west));      printf("\n");
    printf(" VPD E Earliest TAC : %d\n", vpdEarliestTDC(east));
    printf(" VPD W Earliest TAC : %d\n", vpdEarliestTDC(west));
    printf(" VPD TimeDifference : %d\n", vpdTimeDifference());
    printf(" TOF : "); for (int j=0; j<16 ;j++) {printf("%d ",tofAtAddress(j));} printf("\n");
    printf(" TOF Multiplicity : %d\n",tofMultiplicity());
    printf(" NQTData=%d\n",nQTdata());
    printf(" QTLastWord=0x%x\n",(QTdata())[nQTdata()-1]);
    printf(" L2 result : \n"); 
    for (int j=0; j<4 ;j++) { for (int k=0; k<16; k++) {printf("%u ",*(l2Result()+j*16+k)); } printf("\n");}
    printf("\n");
    printf("***** StTriggerData Dump *****\n");
}

char* StTriggerData2008::getTriggerStructure()
{
    return (char*) mData;
}

TrgDataType2008* StTriggerData2008::getTriggerStructure2008()
{
    return mData;
}

int StTriggerData2008::getRawSize() const
{
    int npre = numberOfPreXing();
    int npost = numberOfPostXing();
    int rawSize=sizeof(EvtDescData2008)+sizeof(TrgSumData2008)
        + sizeof(RawTrgDet2008)*(npre+npost+1);
    return  rawSize;
}

unsigned char * StTriggerData2008::getDsm0_EEMC(int prepost) const {
    return   mData->rawTriggerDet[prepostAddress(prepost)].EEMC;
}

unsigned short int  * StTriggerData2008::getDsm1_EEMC(int prepost) const{
    return   mData->rawTriggerDet[prepostAddress(prepost)].EEMClayer1;
}

unsigned short int  * StTriggerData2008::getDsm2_EMC() const{
    return   mData->TrgSum.DSMdata.EMC;
}

unsigned short int  * StTriggerData2008::getDsm3() const{
    return   mData->TrgSum.DSMdata.lastDSM;
}

int StTriggerData2008::L2ResultsOffset(StL2AlgorithmId id) const
{
    switch(id) {
    case l2Diagnostic:      return L2RESULTS_2008_OFFSET_TRG;
    case l2EmcCheck:        return L2RESULTS_2008_OFFSET_EMC_CHECK;
    case l2Jpsi:            return L2RESULTS_2008_OFFSET_JPSI;
    case l2Upsilon:         return L2RESULTS_2008_OFFSET_UPS;
    case l2EmcPedestal:     return L2RESULTS_2008_OFFSET_EMC_PED;
    case l2Pi0Gamma:        return L2RESULTS_2008_OFFSET_GAMMA;
    case l2Dijet:           return L2RESULTS_2008_OFFSET_DIJET;
    default: return -999999999;
    }
}

bool StTriggerData2008::isL2Triggered(StL2TriggerResultType id) const
{
    return false;
    
    /*
      if(mRun<7000000) return false;
      if(mRun>7270000) return false;

      int offset;
      offset=L2ResultsOffset(l2Dijet);
      L2jetResults2006 *jet = (L2jetResults2006 *) &(mData->TrgSum.L2Result[offset]);
      offset=L2ResultsOffset(l2Pi0Gamma);
      L2gammaResult *gam_bemc = (L2gammaResult *) &(mData->TrgSum.L2Result[offset]);
      L2gammaResult *gam_eemc = (L2gammaResult *) &(mData->TrgSum.L2Result[offset+2]);
      
      switch(id) {
      case l2Trg2006BEMCGammaPi:    
      return (gam_bemc->trigger & 0x3)==0x3;
      break;
      case l2Trg2006BEMCGammaPiRandom:    
      return gam_bemc->trigger & 0x4;
      break;
      case l2Trg2006EEMCGammaPi:    
      return (gam_eemc->trigger & 0x3)==0x3;
      break;
      case l2Trg2006EEMCGammaPiRandom:    
      return gam_eemc->trigger & 0x4;
      break;
      case l2Trg2006MonoJet:
      return jet->int0.decision & 0x40;
      break;
      case l2Trg2006DiJet:
      return jet->int0.decision & 0x80;
      break;
      case l2Trg2006RandomJet:    
      return jet->int0.decision & 0x20;
      break;
      default: 
      return false;
      }
    */
}    

unsigned int StTriggerData2008::l2ResultLength() const
{
    return sizeof(mData->TrgSum.L2Result)/sizeof(unsigned int);
}

const unsigned int* StTriggerData2008::l2Result() const
{
    return mData->TrgSum.L2Result;
}

unsigned short StTriggerData2008::vpdADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[2][16] = {
        {  7,   6,  5,  4,  3,  2,  1,  0, 
	 23, 22, 21, 20, 19, 18, 17, 16} ,  
        {  39, 38, 37, 36, 35, 34, 33, 32,
	 55, 54, 53, 52, 51, 50, 49, 48} 
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].VPD[map[eastwest][pmt-1]];
}

unsigned short StTriggerData2008::vpdTDC(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[2][16] = {
        {  15, 14, 13, 12, 11, 10,  9,  8,
	 31, 30, 29, 28, 27, 26, 25, 24} ,
        {  47, 46, 45, 44, 43, 42, 41, 40,
	 63, 62, 61, 60, 59, 58, 57, 56}
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].VPD[map[eastwest][pmt-1]];
}

unsigned short StTriggerData2008::vpdEarliestTDC(StBeamDirection eastwest, int prepost) const
{
    if(prepost!=0) return 0;
    int map[2][2] = {{2, 0},{6, 4}};
    int i1 = map[eastwest][0];
    int i2 = map[eastwest][1];
    bool b1 = (mData->TrgSum.DSMdata.VPD[i1] >> 8) & 0x1;
    bool b2 = (mData->TrgSum.DSMdata.VPD[i2] >> 8) & 0x1;
    int  t1 = mData->TrgSum.DSMdata.VPD[i1] & 0xFF;
    int  t2 = mData->TrgSum.DSMdata.VPD[i2] & 0xFF;
    if(b1 && b2)  {return (t1>t2) ? t1 : t2;}
    else if(b1)   {return t1;}
    else if(b2)   {return t2;}
    else          {return 0;}
}

unsigned short StTriggerData2008::vpdTimeDifference() const
{
    return mData->TrgSum.DSMdata.CTB[4] & 0x1FF;
}

unsigned short StTriggerData2008::nQTdata(int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].QQTdataBytes/4;
}

unsigned int* StTriggerData2008::QTdata(int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].QQTdata;
}

unsigned char* StTriggerData2008::getDsm_FMS(int prepost) const {return mData->rawTriggerDet[prepostAddress(prepost)].FPDW;}
unsigned char* StTriggerData2008::getDsm01_FMS(int prepost) const {return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer0;}
unsigned char* StTriggerData2008::getDsm02_FMS(int prepost) const {return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer0;}
unsigned short int* StTriggerData2008::getDsm1_FMS(int prepost) const {return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer1;}
unsigned short int* StTriggerData2008::getDsm2_FMS() const {return mData->TrgSum.DSMdata.FPD;}

unsigned short StTriggerData2008::mtdAtAddress(int address, int prepost) const
{
    if (address>=0 && address<32){ return mData->rawTriggerDet[prepostAddress(prepost)].MTD[address]; }
    return 0;
}

unsigned short StTriggerData2008::mtdAdc(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[2][8] = {
        { 6, 5, 11, 12, 13, 10,  9,  8},
        { 7, 4,  3,  2,  1,  0, 15, 14}
    };
    if(pmt>=0 && pmt<8) { return mData->rawTriggerDet[prepostAddress(prepost)].MTD[map[eastwest][pmt]]; }
    return 0;
}

unsigned short StTriggerData2008::mtdTdc(StBeamDirection eastwest, int pmt, int prepost) const
{
    static const int map[2][8] = {
        {22,21,27,28,29,26,25,24},
        {23,20,19,18,17,16,31,30}
    };
    if(pmt>=0 && pmt<8) { return mData->rawTriggerDet[prepostAddress(prepost)].MTD[map[eastwest][pmt]]; }
    return 0;
}

unsigned short StTriggerData2008::tofAtAddress(int address, int prepost) const 
{
    if (address>=0 && address<16){ return mData->rawTriggerDet[prepostAddress(prepost)].TOF[address]; }
    return 0;
}

unsigned short StTriggerData2008::tofMultiplicity(int prepost) const 
{
    return mData->rawTriggerDet[prepostAddress(prepost)].TOF[8]; 
}
