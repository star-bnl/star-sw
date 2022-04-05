/***************************************************************************
 *
 * $Id: StTriggerData2005.cxx,v 2.10 2006/09/20 00:44:56 ullrich Exp $
 *
 * Author: Akio Ogawa, Oct 2004
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData.
 *               For year 2005 only.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2005.cxx,v $
 * Revision 2.10  2006/09/20 00:44:56  ullrich
 * Modified method to return length of L2 results.
 *
 * Revision 2.9  2006/09/19 22:53:55  ullrich
 * Added access method to L2 results.
 *
 * Revision 2.8  2006/09/13 23:59:55  ullrich
 * Added new data member mRun. Removed arg run from ctb(), ctbTraySlat(), zdcSMD()
 *
 * Revision 2.7  2006/08/21 19:41:51  ullrich
 * Add run number as argument to ctb(), ctbTray(), and zdcSMD(). Used 2005 only. (Akio)
 *
 * Revision 2.6  2006/05/04 19:05:51  ullrich
 * Added stuff to handle L2 results data.
 *
 * Revision 2.5  2006/03/22 20:58:21  ullrich
 * Added interface to L2 results (offsets).
 *
 * Revision 2.4  2006/02/08 16:29:46  ullrich
 * Fixed bug in zdcUnAttenuated.
 *
 * Revision 2.3  2004/11/30 19:19:12  ullrich
 * Added new access function for EEMC data (Akio).
 *
 * Revision 2.2  2004/11/16 15:58:23  ullrich
 * Checks for valid pre/post samples added (Akio).
 *
 * Revision 2.1  2004/11/02 21:18:39  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <string.h>
#include <assert.h>
#include <iostream>
#include "StTriggerData2005.h"
#include "StDaqLib/TRG/trgStructures2005.h"
#include "StDaqLib/TRG/L2jetResults2006.h"
#include "StDaqLib/TRG/L2pedResults2006.h"
#include "StDaqLib/TRG/L2gammaResult2006.h"

ClassImp(StTriggerData2005)

StTriggerData2005::StTriggerData2005()
{
    mYear=2005;
    mData=0;
}

StTriggerData2005::StTriggerData2005(const TrgDataType2005* data, int run)
{
    mYear=2005;
    mRun = run;
    mData= new TrgDataType2005;
    int npre  = data->EvtDesc.npre;
    int npost = data->EvtDesc.npost;
    assert(npre >=0);    
    assert(npre <=5);    
    assert(npost>=0);    
    assert(npost<=5);    
    int size = sizeof(EvtDescData2005)+sizeof(TrgSumData2005)
        + sizeof(RawTrgDet2005)*(npre+npost+1);
    
    memcpy(mData,data,size); 
    memset((char*)mData+size,0,sizeof(TrgDataType2005)-size);
}

unsigned int StTriggerData2005::version() const
{
    return mData->EvtDesc.TrgDataFmtVer;
}

StTriggerData2005::~StTriggerData2005() {delete mData;}

unsigned int StTriggerData2005::token() const
{
    return mData->EvtDesc.TrgToken;
}

unsigned int StTriggerData2005::triggerWord() const
{
    return mData->EvtDesc.TriggerWord;
}

unsigned int StTriggerData2005::actionWord() const
{
    return
	( (unsigned short)(mData->EvtDesc.actionWdTrgCommand) * 16 * 16 * 16 ) +
	( (unsigned short)(mData->EvtDesc.actionWdDaqCommand) * 16 * 16      ) +
	(                  mData->EvtDesc.actionWdDetectorBitMask & 0x00ff   ) ;
}

unsigned int StTriggerData2005::numberOfPreXing() const
{
    return mData->EvtDesc.npre;
}

unsigned int StTriggerData2005::numberOfPostXing() const
{
    return mData->EvtDesc.npost;
}

unsigned short StTriggerData2005::busyStatus() const{
    return mData->EvtDesc.modifiedBusyStatus;
}

unsigned short StTriggerData2005::dsmInput() const{
    return mData->EvtDesc.DSMInput;
}

unsigned short StTriggerData2005::trgToken() const{
    return mData->EvtDesc.TrgToken;
}

unsigned short StTriggerData2005::dsmAddress() const{
    return mData->EvtDesc.DSMAddress;
}

unsigned short StTriggerData2005::mAddBits() const{
    return mData->EvtDesc.addBits;
}

unsigned short StTriggerData2005::bcData(int channel) const{
    return mData->TrgSum.DSMdata.BCdata[channel];
}

unsigned short StTriggerData2005::lastDSM(int channel) const{
    return mData->TrgSum.DSMdata.lastDSM[channel];
}

unsigned short StTriggerData2005::tcuBits() const
{
    return mData->EvtDesc.DSMInput;
}

unsigned int StTriggerData2005::bunchCounterHigh() const
{
    return mData->EvtDesc.bunchXing_hi;
}

unsigned int StTriggerData2005::bunchCounterLow() const
{
    return mData->EvtDesc.bunchXing_lo;
}

unsigned int StTriggerData2005::bunchId48Bit() const
{
    unsigned long long bxinghi,bxing1,bxinglo, bx;
    bxinghi = mData->TrgSum.DSMdata.BCdata[3];
    bxing1 =  mData->TrgSum.DSMdata.BCdata[10];
    bxinglo = (bxing1 << 16) + mData->TrgSum.DSMdata.BCdata[11];
    bx = (bxinghi << 32) + bxinglo;
    return (int)(bx % 120);
} 

unsigned int StTriggerData2005::bunchId7Bit() const
{
    int b7=0, b7dat; 
    b7dat = mData->TrgSum.DSMdata.BCdata[2];
    b7 = b7dat & 0x7f;
    return b7;
}

unsigned int StTriggerData2005::spinBit() const
{
    return (mData->TrgSum.DSMdata.lastDSM[7]/16)%256;
}

unsigned int StTriggerData2005::spinBitYellowFilled() const
{
    unsigned int sb = spinBit();
    return sb%2;
}

unsigned int StTriggerData2005::spinBitYellowUp() const
{
    unsigned int sb = spinBit();
    return (sb/2)%2;
}

unsigned int StTriggerData2005::spinBitYellowDown() const
{
    unsigned int sb = spinBit();
    return (sb/4)%2;
}

unsigned int StTriggerData2005::spinBitYellowUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/8)%2;
}

unsigned int StTriggerData2005::spinBitBlueFilled() const
{
    unsigned int sb = spinBit();
    return (sb/16)%2;
}

unsigned int StTriggerData2005::spinBitBlueUp() const
{
    unsigned int sb = spinBit();
    return (sb/32)%2;
}

unsigned int StTriggerData2005::spinBitBlueDown() const
{
    unsigned int sb = spinBit();
    return (sb/64)%2;
}

unsigned int StTriggerData2005::spinBitBlueUnpol() const
{
    unsigned int sb = spinBit();
    return (sb/128)%2;
}

unsigned short  StTriggerData2005::ctbRaw(int address, int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].CTB[address];
}

unsigned short  StTriggerData2005::ctb(int pmt, int prepost) const
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
    if(mRun>7000000 && mRun<7151026){
      if     (add== 76){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[7];} 
      else if(add== 75){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[6];} 
      else if(add== 74){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[5];} 
      else if(add==166){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[4];} 
      else if(add==165){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[3];} 
      else if(add==164){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[2];} 
    }
    else if(mRun>=7151026){
      if     (add==  4){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[7];} 
      else if(add==  3){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[6];} 
      else if(add==  2){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[5];} 
      else if(add==238){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[4];} 
      else if(add==237){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[3];} 
      else if(add==236){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[2];} 
    }
    return v;
}

unsigned short StTriggerData2005::ctbTraySlat(int tray, int slat, int prepost) const{
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
    if(mRun>7000000 && mRun<7151026){
      if     (tray== 46 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[7];} 
      else if(tray== 47 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[6];} 
      else if(tray== 48 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[5];} 
      else if(tray== 66 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[4];} 
      else if(tray== 67 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[3];} 
      else if(tray== 68 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[2];} 
    }
    else if(mRun>=7151026){
      if     (tray==  8 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[7];} 
      else if(tray==  9 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[6];} 
      else if(tray== 10 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[5];} 
      else if(tray==104 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[4];} 
      else if(tray==105 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[3];} 
      else if(tray==106 && slat==0){v=mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[2];} 
    }
    return v;
}

unsigned short StTriggerData2005::ctbSum(int prepost) const{
    unsigned short sum=0;
    for (int i=1; i<240; i++){sum+=ctb(i,prepost);}
    return sum;
}

unsigned short StTriggerData2005::mwc(int pmt, int prepost) const
{
    static const unsigned char mwcMap[96] = {
	71, 70, 69, 68, 67, 66, 65, 64, 79, 78, 77, 76,
	95, 94, 93, 92, 87, 86, 85, 84, 83, 82, 81, 80,
	99, 98, 97, 96, 111, 110, 109, 108, 103, 102, 101, 100,
	119, 118, 117, 116, 115, 114, 113, 112, 127, 126, 125, 124,
	7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12,
	31, 30, 29, 28, 23, 22, 21, 20, 19, 18, 17, 16,
	35, 34, 33, 32, 47, 46, 45, 44, 39, 38, 37, 36,
	55, 54, 53, 52, 51, 50, 49, 48, 63, 62, 61, 60,
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].MWC[mwcMap[pmt]];
}

unsigned short StTriggerData2005::bbcADC(StBeamDirection eastwest, int pmt, int prepost) const
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

unsigned short StTriggerData2005::bbcTDC(StBeamDirection eastwest, int pmt, int prepost) const
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

unsigned short StTriggerData2005::bbcADCSum(StBeamDirection eastwest, int prepost) const
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

unsigned short StTriggerData2005::bbcADCSumLargeTile(StBeamDirection eastwest, int prepost) const
{
    int address = prepostAddress(prepost);
    if (eastwest==east) { return mData->rawTriggerDet[address].BBClayer1[11]%2048; }
    else               { return mData->rawTriggerDet[address].BBClayer1[10]%2048; }
}

unsigned short StTriggerData2005::bbcEarliestTDC(StBeamDirection eastwest, int prepost) const
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

unsigned short StTriggerData2005::bbcTimeDifference() const
{
    return mData->TrgSum.DSMdata.VTX[3]%512;
}

unsigned short StTriggerData2005::fpd(StBeamDirection eastwest, int module, int pmt, int prepost) const
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
	    else            return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer0[address-112];
	}else{
	    if (address<112) return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestNSLayer0[address];
	    else            return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestTBLayer0[address-112];
	}
    }else{
	return 0;
    }
}

unsigned short StTriggerData2005::fpdSum(StBeamDirection eastwest, int module) const
{
    static const short map[2][4]={{3,2,1,0},{7,6,5,4}};
    static const short nbit[2][4]={{16384,16384,8192,8192},{16384,16384,8192,8192}};
    return mData->TrgSum.DSMdata.FPD[map[eastwest][module]] % nbit[eastwest][module];
}

unsigned short StTriggerData2005::fpdLayer1DSMRaw(StBeamDirection eastwest, int channel, int prepost) const{
    if (eastwest==east){
	if (channel<8)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer1[channel];
	if (channel<16) return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer1[channel-8];
    }
    else{
	if (channel<8)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestNSLayer1[channel];
	if (channel<16) return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestTBLayer1[channel-8];
    }
    return 0;
}

unsigned short StTriggerData2005::fpdLayer1DSM(StBeamDirection eastwest, int module, int board, int prepost) const{
    static const short map[4][4]={{3,2,1,0},{7,6,5,4},{3,2,6,7},{1,0,4,5}};
    if (board<4){
	if (eastwest==east){
	    if (module<2)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer1[map[module][board]];
	    else          return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer1[map[module][board]];
	}
	else{
	    if (module<2)  return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestNSLayer1[map[module][board]];
	    else          return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestTBLayer1[map[module][board]];
	}
    }
    return 0;
}

unsigned short StTriggerData2005::fpdLayer2DSMRaw(int channel) const{
    if (channel<8) return mData->TrgSum.DSMdata.FPD[channel];
    return 0;
}

unsigned short StTriggerData2005::fpdLayer2DSM(StBeamDirection eastwest, int module) const{
    static const int dsmmap[2][4]={{3,2,1,0},{7,6,5,4}};
    if (module<4) return mData->TrgSum.DSMdata.FPD[dsmmap[eastwest][module]];
    return 0;
}

unsigned short StTriggerData2005::zdcAtChannel(int channel, int prepost) const
{
    static const int dsmmap[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
    if (channel>=0 && channel<16){ return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[dsmmap[channel]]; }
    return 0;
}

unsigned short StTriggerData2005::zdcAtAddress(int address, int prepost) const
{
    if (address>=0 && address<16){ return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[address]; }
    return 0;
}

unsigned short StTriggerData2005::zdcUnAttenuated(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[4];} // fixed bug: was 3
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[0];}
    return 0;
}

unsigned short StTriggerData2005::zdcAttenuated(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[13];}
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[10];}
    return 0;
}

unsigned short StTriggerData2005::zdcADC(StBeamDirection eastwest, int pmt, int prepost) const
{
    if (eastwest==east && pmt==1) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[7];}
    if (eastwest==east && pmt==2) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[6];}
    if (eastwest==east && pmt==3) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[5];}
    if (eastwest==west && pmt==1) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[3];}
    if (eastwest==west && pmt==2) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[2];}
    if (eastwest==west && pmt==3) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[1];}
    return 0;
    
}

unsigned short StTriggerData2005::zdcTDC(StBeamDirection eastwest, int prepost) const
{
    if (eastwest==east) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[8];}
    if (eastwest==west) {return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[9];}
    return 0;
}

unsigned short StTriggerData2005::zdcHardwareSum(int prepost) const
{
    return mData->rawTriggerDet[prepostAddress(prepost)].ZDC[14];
}

unsigned short StTriggerData2005::zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost) const
{
    static const int zdcsmd_map[2][2][8] ={
	// wrong mapping
	//{ { 6, 5, 4, 3, 2, 1, 0, 7} ,  
	//  {15,14,13,12,11,10, 9, 8} } ,
	//{ {22,21,20,19,18,17,16,23} ,
	//  {31,30,29,28,27,26,25,24} }
	// correct mapping from Xzb
	{ { 7, 6, 5, 4, 3, 2, 1, 11} ,  
	  { 0,15,14,13,12,8,10, 9} } ,
	{ {23,22,21,20,19,18,17,24} ,
	  {16,31,30,29,28,27,26,25} }
    };
    static const int zdcsmd_map2006[2][2][8] ={
	{ {23,22,21,20,-1,-1,1,14} ,  
	  {19,18,17,16,-1,-1,13,8} } ,
	{ {31,30,29,28,-1,-1,0,15} ,
	  {27,26,25,24,-1,-1,12,9} }
    };
    if (verthori<0 || verthori>1) return 0;
    if (strip<1 || strip>8) return 0;  //the last one in vertical strips is for LED. Could be used for Forward counter later. T.A.H.
    int add=-1;    
    if(mRun<7000000) {add=zdcsmd_map[eastwest][verthori][strip-1];}
    else            {add=zdcsmd_map2006[eastwest][verthori][strip-1];}
    if(add>=0) return mData->rawTriggerDet[prepostAddress(prepost)].ZDCSMD[add];
    else return 0;
};

unsigned short StTriggerData2005::bemcLayer1DSM(int channel, int prepost) const {
    const int n_bemc_layer1=48;
    if (channel<0 || channel >=n_bemc_layer1) {
	gMessMgr->Warning() << "Barrel DSM layer 1 out of range (" << channel << ")" << endm;
	return 0;
    }
    return mData->rawTriggerDet[prepostAddress(prepost)].BEMClayer1[channel];
} 

unsigned short StTriggerData2005::eemcLayer1DSM(int channel, int prepost) const {
    const int n_eemc_layer1=48;
    if (channel<0 || channel >=n_eemc_layer1) {
	gMessMgr->Warning() << "Endap DSM layer 1 out of range (" << channel << ")" << endm;
	return 0;
    }
    return mData->rawTriggerDet[prepostAddress(prepost)].EEMClayer1[channel];
} 

unsigned short StTriggerData2005::emcLayer2DSM(int channel) const {
    const int n_emc_layer2=8;
    if (channel<0 || channel >=n_emc_layer2) {
	gMessMgr->Warning() << "EMC DSM layer 2 out of range (" << channel << ")" << endm;
	return 0;
    }
    return mData->TrgSum.DSMdata.EMC[channel];
} 

unsigned char StTriggerData2005::bemcHighTower(int patch_id, int prepost) const
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

unsigned char StTriggerData2005::bemcJetPatch (int patch_id, int prepost) const
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

unsigned char StTriggerData2005::eemcHighTower(int patch_id, int prepost) const
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

unsigned char StTriggerData2005::eemcJetPatch (int patch_id, int prepost) const
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

unsigned char StTriggerData2005::bemcHighestTowerADC(int prepost) const
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

unsigned char StTriggerData2005::eemcHighestTowerADC(int prepost) const
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

void StTriggerData2005::dump() const
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
    printf(" MWC ADC : ");       for (int i=0; i<96; i++){ printf("%d ",mwc(i,0));      }; printf("\n");
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
    printf("\n");
    printf("***** StTriggerData Dump *****\n");
}

char* StTriggerData2005::getTriggerStructure()
{
    return (char*) mData;
}

TrgDataType2005* StTriggerData2005::getTriggerStructure2005()
{
    return mData;
}

int StTriggerData2005::getRawSize() const
{
    int npre = numberOfPreXing();
    int npost = numberOfPostXing();
    int rawSize=sizeof(EvtDescData2005)+sizeof(TrgSumData2005)
        + sizeof(RawTrgDet2005)*(npre+npost+1);
    return  rawSize;
}

unsigned char * StTriggerData2005::getDsm0_EEMC(int prepost) const {
  return   mData->rawTriggerDet[prepostAddress(prepost)].EEMC;
}

unsigned short int  * StTriggerData2005::getDsm1_EEMC(int prepost) const{
  return   mData->rawTriggerDet[prepostAddress(prepost)].EEMClayer1;
}

unsigned short int  * StTriggerData2005::getDsm2_EMC() const{
  return   mData->TrgSum.DSMdata.EMC;
}

unsigned short int  * StTriggerData2005::getDsm3() const{
  return   mData->TrgSum.DSMdata.lastDSM;
}

int StTriggerData2005::L2ResultsOffset(StL2AlgorithmId id) const
{
    if(mRun<7000000) return -1;
    
    switch(id) {
    case l2Diagnostic:  return L2RESULTS_OFFSET_TRG;
    case l2EmcCheck:    return L2RESULTS_OFFSET_EMC_CHECK;
    case l2Jpsi:        return L2RESULTS_OFFSET_JPSI;
    case l2Upsilon:     return L2RESULTS_OFFSET_UPS;
    case l2Dijet:       return L2RESULTS_OFFSET_DIJET;
    case l2EmcPedestal: return L2RESULTS_OFFSET_EMC_PED;
    case l2Pi0Gamma:    return L2RESULTS_OFFSET_PIG;
    default: return -1;
    }
}

bool StTriggerData2005::isL2Triggered(StL2TriggerResultType id) const
{
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
}    

unsigned int StTriggerData2005::l2ResultLength() const
{
    return sizeof(mData->TrgSum.L2Result)/sizeof(unsigned int);
}

const unsigned int* StTriggerData2005::l2Result() const
{
    return mData->TrgSum.L2Result;
}
