/***************************************************************************
 *
 * $Id: StTriggerData2003.cxx,v 2.1 2003/04/16 17:47:41 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2003.cxx,v $
 * Revision 2.1  2003/04/16 17:47:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerData2003.h"
#include "StTriggerDataMaker/trgStructures2003.h"

ClassImp(StTriggerData2003)
  
StTriggerData2003::StTriggerData2003(char* data)
{
    mYear=2003;
    mData=(TrgDataType2003*)data;
}

StTriggerData2003::StTriggerData2003()
{
    mYear=2003;
    mData=0;
}

StTriggerData2003::StTriggerData2003(TrgDataType2003* data)
{
    mYear=2003;
    mData=data;
}

unsigned int StTriggerData2003::version() const
{
    return mData->EvtDesc.TrgDataFmtVer;
}

unsigned int StTriggerData2003::token() const
{
    return mData->EvtDesc.TrgToken;
}

unsigned int StTriggerData2003::triggerWord() const
{
    return mData->EvtDesc.TriggerWord;
}

unsigned int StTriggerData2003::actionWord() const
{
    return
	( (unsigned short)(mData->EvtDesc.actionWdTrgCommand) * 16 * 16 * 16 ) +
	( (unsigned short)(mData->EvtDesc.actionWdDaqCommand) * 16 * 16      ) +
	(                  mData->EvtDesc.actionWdDetectorBitMask & 0x00ff   ) ;
}

unsigned int StTriggerData2003::numberOfPreXing() const
{
    return mData->EvtDesc.npre;
}

unsigned int StTriggerData2003::numberOfPostXing() const
{
    return mData->EvtDesc.npost;
}

unsigned int StTriggerData2003::bunchId48Bit() const
{
    unsigned long long bxinghi,bxing1,bxinglo, bx;
    bxinghi = mData->TrgSum.DSMdata.BCdata[3];
    bxing1 =  mData->TrgSum.DSMdata.BCdata[10];
    bxinglo = (bxing1 << 16) + mData->TrgSum.DSMdata.BCdata[11];
    bx = (bxinghi << 32) + bxinglo;
    return (int)(bx % 120);
} 

unsigned int StTriggerData2003::bunchId7Bit() const
{
    int i, b7=0, b7dat, ibits=6; 
    b7dat = mData->TrgSum.DSMdata.BCdata[2];
    for (i=0; i<7; i++) {
	b7 += (!((b7dat>>(ibits-i)) & 0x1) << (i)) & 0x7f;
    }
    return b7;
}

unsigned int StTriggerData2003::spinBit() const
{
    int ldsm0,spin1,spin2,spin3,spin4;
    ldsm0 = mData->TrgSum.DSMdata.lastDSM[0];
    spin1 = (ldsm0>>8) & 0x1;
    spin2 = (ldsm0>>9) & 0x1;
    spin3 = (ldsm0>>10) & 0x1;
    spin4 = (ldsm0>>11) & 0x1;
    return spin1+spin2*2+spin3*4+spin4*8;
}

unsigned short  StTriggerData2003::ctb(int pmt, int prepost=0) const
{
    static const unsigned char ctbMap[240] = {
	7, 6, 5, 4, 3, 23, 22, 21, 20, 19,
	2, 1, 0, 15, 14, 18, 17, 16, 31, 30,
	13, 12, 11, 10, 9, 29, 28, 27, 26, 25,
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
    return mData->rawTriggerDet[prepostAddress(prepost)].CTB[ctbMap[pmt]];
}

unsigned short StTriggerData2003::mwc(int pmt, int prepost=0) const
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

unsigned short StTriggerData2003::bbcADC(int eastwest, int pmt, int prepost=0) const
{
    static const int q_map[2][24] = { 
	{ 8  , 5  , 4  , 40 , 37 , 36 , 7  , 6  ,
	  3  , 2  , 1  , 39 , 38 , 35 , 34 , 33 ,
	  72 , 71 , 70 , 69 , 68 , 67 , 66 , 65 },
	{ 24 , 21 , 20 , 56 , 53 , 52 , 23 , 22 ,
	  19 , 18 , 17 , 55 , 54 , 51 , 50 , 49 ,
	  80 , 79 , 78 , 77 , 76 , 75 , 74 , 73 } 
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].BBC[q_map[eastwest][pmt-1]-1];
}

unsigned short StTriggerData2003::bbcTDC(int eastwest, int pmt, int prepost=0) const
{      
    static const int t_map[2][16] ={ 
	{ 16 , 13 , 12 , 48 , 45 , 44 , 15 , 14 ,
	  11 , 10 , 9  , 47 , 46 , 43 , 42 , 41 },    
	{ 32 , 29 , 28 , 64 , 61 , 60 , 31 , 30 ,
	  27 , 26 , 25 , 63 , 62 , 59 , 58 , 57 }
    };
    return mData->rawTriggerDet[prepostAddress(prepost)].BBC[t_map[eastwest][pmt-1]-1];
}

unsigned short StTriggerData2003::bbcADCSum(int eastwest, int prepost=0) const
{
    int address = prepostAddress(prepost);
    if(eastwest==0){
	return 
	    mData->rawTriggerDet[address].BBClayer1[7]%2048+
	    mData->rawTriggerDet[address].BBClayer1[3]%2048;
    }else{
	return 
	    mData->rawTriggerDet[address].BBClayer1[5]%2048+
	    mData->rawTriggerDet[address].BBClayer1[1]%2048;
    }
}

unsigned short StTriggerData2003::bbcADCSumLargeTile(int eastwest, int prepost=0) const
{
    int address = prepostAddress(prepost);
    if(eastwest==0){ return mData->rawTriggerDet[address].BBClayer1[11]%2048; }
    else           { return mData->rawTriggerDet[address].BBClayer1[10]%2048; }
}

unsigned short StTriggerData2003::bbcEarliestTDC(int eastwest, int prepost=0) const
{
    int address = prepostAddress(prepost), t1, t2;
    if(eastwest==0){
	t1 = mData->rawTriggerDet[address].BBClayer1[6]%256;
	t2 = mData->rawTriggerDet[address].BBClayer1[2]%256;
    }else{
	t1 = mData->rawTriggerDet[address].BBClayer1[4]%256;
	t2 = mData->rawTriggerDet[address].BBClayer1[0]%256;
    }
    return (t1>t2) ? t1 : t2;
}

unsigned short StTriggerData2003::bbcTimeDifference() const
{
    return mData->TrgSum.DSMdata.VTX[3]%512;
}

unsigned short StTriggerData2003::fpd(int eastwest, int module, int pmt, int prepost=0) const
{
    static const short fpdmap[2][6][49] = {
	//East
	{
	    //East North
	    {  7,   6,  23,  22,  39,  38, 55, 
	       5,   4,  21,  20,  37,  36, 54,
	       3,   2,  19,  18,  35,  34, 53,
	       1,   0,  17,  16,  33,  32, 52,
	       15,  14,  31,  30,  47,  46, 51, 
	       13,  12,  29,  28,  45,  44, 50, 
	       11,  10,  27,  26,  43,  42, 49},
	    //East South
	    { 71,  70,  87,  86, 103, 102, 48, 
	      69,  68,  85,  84, 101, 100, 63,
	      67,  66,  83,  82,  99,  98, 62,
	      65,  64,  81,  80,  97,  96, 61,
	      79,  78 , 95,  94, 111, 110, 60,
	      77,  76,  93,  92, 109, 108, 59,
	      75,  74,  91,  90, 107, 106, 58},
	    //East Top
	    {135, 134, 133, 132, 131, 130, 129, 
	     128, 143, 142, 119, 118, 117, 116, 
	     115, 114, 113, 112, 127, 126, 125, 
	     124, 123, 122, 121,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //East Bottom
	    {167, 166, 165, 164, 163, 162, 161, 
	     160, 175, 174, 151, 150, 149, 148, 
	     147, 146, 145, 144, 159, 158, 157, 
	     156, 155, 154, 153,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //East North PreShower
	    {  9,   8,  25,  24,  41,  40,  57,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //East South PreShower
	    { 73,  72,  89,  88, 105, 104,  56,
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
	    {  7,   6,  23,  22,  39,  38, 55, 
	       5,   4,  21,  20,  37,  36, 54,
	       3,   2,  19,  18,  35,  34, 53,
	       1,   0,  17,  16,  33,  32, 52,
	       15,  14,  31,  30,  47,  46, 51, 
	       13,  12,  29,  28,  45,  44, 50, 
	       11,  10,  27,  26,  43,  42, 49},
	    //West South
	    { 71,  70,  87,  86, 103, 102, 48, 
	      69,  68,  85,  84, 101, 100, 63,
	      67,  66,  83,  82,  99,  98, 62,
	      65,  64,  81,  80,  97,  96, 61,
	      79,  78 , 95,  94, 111, 110, 60,
	      77,  76,  93,  92, 109, 108, 59,
	      75,  74,  91,  90, 107, 106, 58},
	    //West Top
	    {135, 134, 133, 132, 131, 130, 129, 
	     128, 143, 142, 119, 118, 117, 116, 
	     115, 114, 113, 112, 127, 126, 125, 
	     124, 123, 122, 121,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,  
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //West Bottom
	    {167, 166, 165, 164, 163, 162, 161, 
	     160, 175, 174, 151, 150, 149, 148, 
	     147, 146, 145, 144, 159, 158, 157, 
	     156, 155, 154, 153,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1,
	     -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //West North PreShower
	    {  9,   8,  25,  24,  41,  40,  57,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1,
	       -1,  -1,  -1,  -1,  -1,  -1,  -1},
	    //West South PreShower
	    { 73,  72,  89,  88, 105, 104,  56,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1,
	      -1,  -1,  -1,  -1,  -1,  -1,  -1},
	}
    };
    int address = fpdmap[eastwest][module][pmt-1];
    if(eastwest==0){
	if(address<112) return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastNSLayer0[address];
	else            return mData->rawTriggerDet[prepostAddress(prepost)].FPDEastTBLayer0[address-112];
    }else{
	if(address<112) return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestNSLayer0[address];
	else            return mData->rawTriggerDet[prepostAddress(prepost)].FPDWestTBLayer0[address-112];
    }
}      

unsigned short StTriggerData2003::fpdSum(int eastwest, int module) const
{
    static const short map[2][4]={{3,2,1,0},{7,6,5,4}};
    static const short nbit[2][4]={{16384,16384,8192,8192},{16384,16384,8192,8192}};
    return mData->TrgSum.DSMdata.FPD[map[eastwest][module]] % nbit[eastwest][module];
}

void StTriggerData2003::dump() const
{
    printf("***** StTriggerData Dump *****\n");
    printf(" Year=%d  Version=%x\n",year(),version());
    printf(" %d pre and %d post crossing data available\n",numberOfPreXing(),numberOfPostXing());
    printf(" Token=%d  TriggerWord=%x  ActionWord=%x  BusyStatus=%x\n",
	   token(), triggerWord(), actionWord(), busyStatus());
    printf(" BunchId 7bit=%d  48bit=%d  SpinBits=%d\n",bunchId7Bit(), bunchId48Bit(), spinBit());  
    printf(" CTB ADC : ");       for(int i=0; i<240;i++){ printf("%d ",ctb(i,0));      }; printf("\n");
    printf(" MWC ADC : ");       for(int i=0; i<96; i++){ printf("%d ",mwc(i,0));      }; printf("\n");
    printf(" BBC East ADC : ");  for(int i=1; i<=24;i++){ printf("%d ",bbcADC(0,i,0)); }; printf("\n");
    printf(" BBC West ADC : ");  for(int i=1; i<=24;i++){ printf("%d ",bbcADC(1,i,0)); }; printf("\n");
    printf(" BBC East TAC : ");  for(int i=1; i<=16;i++){ printf("%d ",bbcTDC(0,i,0)); }; printf("\n");
    printf(" BBC West TAC : ");  for(int i=1; i<=16;i++){ printf("%d ",bbcTDC(1,i,0)); }; printf("\n");
    for(int i=-numberOfPreXing(); i<=static_cast<int>(numberOfPostXing()); i++){
	printf(" BBC Sums %d xing : ",i);  
	printf("East=%d  West=%d   Large tile East=%d  West=%d\n",
	       bbcADCSum(0,i),bbcADCSum(1,i),
	       bbcADCSumLargeTile(0,i),bbcADCSumLargeTile(1,i));
    }
    printf(" BBC Earilest : ");  printf("East=%d  West=%d  Difference+256=%d\n",
					bbcEarliestTDC(0,0),bbcEarliestTDC(1,0),bbcTimeDifference());
    printf(" FPD East North   : ");for(int i=1; i<=49;i++){ printf("%d ",fpd(0,0,i,0));  }; printf("\n");
    printf(" FPD East South   : ");for(int i=1; i<=49;i++){ printf("%d ",fpd(0,1,i,0));  }; printf("\n");
    printf(" FPD East Top     : ");for(int i=1; i<=25;i++){ printf("%d ",fpd(0,2,i,0));  }; printf("\n");
    printf(" FPD East Bottom  : ");for(int i=1; i<=25;i++){ printf("%d ",fpd(0,3,i,0));  }; printf("\n");
    printf(" FPD East North PS: ");for(int i=1; i<= 7;i++){ printf("%d ",fpd(0,4,i,0));  }; printf("\n");
    printf(" FPD East South PS: ");for(int i=1; i<= 7;i++){ printf("%d ",fpd(0,5,i,0));  }; printf("\n");
    printf(" FPD West South   : ");for(int i=1; i<=49;i++){ printf("%d ",fpd(1,1,i,0));  }; printf("\n");
    printf(" FPD West Bottom  : ");for(int i=1; i<=25;i++){ printf("%d ",fpd(1,3,i,0));  }; printf("\n");
    printf(" FPD West South PS: ");for(int i=1; i<= 7;i++){ printf("%d ",fpd(1,5,i,0));  }; printf("\n");
    printf(" FPD Sums         : ");for(int i=0; i<2;i++){ for(int j=0; j<4 ;j++) printf("%d ",fpdSum(i,j));};
    printf("\n");
    printf("***** StTriggerData Dump *****\n");
}

char* StTriggerData2003::getTriggerStructure()
{
    return (char*) mData;
}

TrgDataType2003* StTriggerData2003::getTriggerStructure2003()
{
    return mData;
}





