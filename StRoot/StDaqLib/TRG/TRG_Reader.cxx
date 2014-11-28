/*
 ***************************************************************************
 * Author: Herb Ward
 ***************************************************************************
 * Description: TRG unpacking code
 *      
 *
 *   change log
 *  14-Jan-00 MJL added TRGD::PrintDataCompact()
 *  04-Feb-00 hjw adaptations for "ushort ZDCDSM[8]" to "BYTE ZDC[16]" in 
 *            trgStructures.h (BYTE = uchar)
 *                
 *************************************************************************** 
*/

#include "TRG_Reader.hh"
#include <assert.h>
#include "trgStructures2007.h"
#include "trgStructures2008.h"
#define PP printf(

using namespace OLDEVP;

////////////////////////////////////////////////////////////////////////////////
#define PREPOST 11
#include "trgStructures.h"
typedef struct {
  EvtDescData    EvtDesc;  /* L1 Event Descriptor Data */
  TrgSumData     TrgSum;   /* summary data */
  RawTrgDet      RAW[PREPOST];      /* pre and post history determines actual dimension. */
} MarilynMonroe;
MarilynMonroe *gs;

#include "trgStructures2000.h"
typedef struct {
  EvtDescData2000    EvtDesc;  /* L1 Event Descriptor Data */
  TrgSumData2000     TrgSum;   /* summary data */
  RawTrgDet2000      RAW[PREPOST];      /* For simplicity, I assume that you don't want pre and post history. */
} MarilynMonroe2000;
MarilynMonroe2000 *gs2000; // CAUTION: this is used in duplicated.code, even 
                           // though grep doesn't find it (see macro SWITCH).
////////////////////////////////////////////////////////////////////////////////
// This section of code causes the functions in the file duplicated.code to appear twice
// in this code file.  The second time is for trigger data in the year 2000 format.
// The first time is for year 2001 format.
#define SWITCH(x) x
#include "duplicated.code"
#undef SWITCH
#define SWITCH(x) x ## 2000
#include "duplicated.code"
////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////  functions  /////////////////////

/// This routine also exists in St_trg_Maker
int TRG_Reader::YearOfData(char *data) {
  data+=sizeof(unsigned short)+sizeof(char); // Skip the first two data.

  if(*data==0x12) return 2000; // These years (2000, 2001, 2003, ...) correspond
  if(*data==0x13) return 2001; // roughly to the file names for the various
  if(*data==0x20) return 2003; // trgStructures.h versions (eg, trgStructures2003.h).
  if(*data==0x21) return 2004; // trgStructures.h versions (eg, trgStructures2004.h).
  if(*data==0x22) return 2005; // trgStructures.h versions (eg, trgStructures2005.h).
  //This version 30 had problem and fixed 20070404 by version 31.
  //Data before 20070404 have version 30, but will read correctly when casted to version 31
  if(*data==0x30) return 2007; // trgStructures.h versions (eg, trgStructures2007.h).
  //Fixed version of 2007 trigger structure
  if(*data==0x31) return 2007; // trgStructures.h versions (eg, trgStructures2007.h).
  //From run8 this will be TrgTowerTrnfer version#
  if(*data==0x10 || *data==0x32) return 2008; // trgStructures.h versions (eg, trgStructures2008.h).
  
  (void) printf("TRG_Reader::YearOfData : value %d=0x%x not treated\n",*data,*data);
  //assert(0);  // Should not be here.  My ne dolzhny byt6 zdec6.

  return 0;
}


TRG_Reader::TRG_Reader(EventReader *er, Bank_TRGP *pTRGP) :
    mErr(0),pBankUnp(0),sizeUnp(0),pTRGD(0)
{
  if (!pTRGD) pTRGD = new Bank_TRGD();

  pBankTRGP=pTRGP; //copy into class data member for use by other methods
  ercpy=er;        // squirrel away pointer eventreader for our friends

  if ( !pTRGP) return;

  if(!pBankTRGP->test_CRC()) { 
    (void) printf("TRG_Reader::TRG_Reader: CRC error: %s %d\n",__FILE__,__LINE__); 
  }

  if(pBankTRGP->swap()<0) { 
    // Use default swap.
    mErr = 1;
    (void) printf("TRG_Reader::TRG_Reader: swap error: %s %d\n",__FILE__,__LINE__); 
  }
  pBankTRGP->header.CRC=0;
  pBankTRGD=(Bank_TRGD*) ((char *)pBankTRGP + 4*pBankTRGP->theData.offset);
  assert(pBankTRGD);

  if(!pBankTRGD->test_CRC()) {
    mErr=2;
    (void) printf("TRG_Reader::TRG_Reader: CRC error: %s %d\n",__FILE__,__LINE__); 
  }
  char *ptr=(char*)pBankTRGD; ptr+=40; /* Skip the 10-word TRGD bank header. */ 

  switch(YearOfData(ptr)) {
    case 0:
      // An error occured with decoding the version / year number
      (void) printf("TRG_Reader::TRG_Reader: YearOfData() returned a failure status\n");
      mErr = 9999;
      return;
      break;

    case 2000:
      gs2000=(MarilynMonroe2000*)ptr;
      // SanityCheck2000(1);
      if(pBankTRGD->HerbSwap2000()   <0) { 
	mErr = 2000; 
	(void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__); 
      }
      break;

    case 2001:
      gs=(MarilynMonroe*)ptr;
      // SanityCheck(1);
      if(pBankTRGD->HerbSwap()       <0) { 
	mErr = 2001; 
	(void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__); 
      }
      break;

    case 2003:
      S_mode = 0;
      // SanityCheck2003(ptr,S_mode);
      if(pBankTRGD->HerbSwap2003(ptr)<0) { 
	mErr = 2003; 
	(void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__); 
      }
      break;

    case 2004:
      S_mode = 0;
      // SanityCheck2004(ptr,S_mode);
      if(pBankTRGD->HerbSwap2004(ptr)<0) { 
	mErr = 2004; 
	(void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__); 
      }
      break;

    case 2005:
      S_mode = 0;
      // SanityCheck2004(ptr,S_mode);
      if(pBankTRGD->HerbSwap2005(ptr)<0) { 
	mErr = 2005; 
	(void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__); 
      }
      break;

    case 2007:
      S_mode = 0;
      // Since 2007 run, trigger data has variable length. 
      // Also byte swap accordingly
      if(UnpackTrg2007(pBankTRGP) < 0){
        mErr = 2007;
        (void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__);
      }
      pBankTRGD=(Bank_TRGD*) ((char *)pBankTRGP + 4*pBankTRGP->theData.offset);
      break;

    case 2008:
      S_mode = 0;      
      if(UnpackTrg2008(pBankTRGP) < 0){
        mErr = 2008;
        (void) printf("TRG_Reader::TRG_Reader: Swap error %s %d.\n",__FILE__,__LINE__);
      }
      pBankTRGD=(Bank_TRGD*) ((char *)pBankTRGP + 4*pBankTRGP->theData.offset);
      break;

    default: 
      assert(0);
  }
  (void) printf("TRG_Reader::TRG_Reader: Trigger reader instantiated, distance to data = %d bytes.\n",
		pBankTRGP->theData.offset);
}

void TRG_Reader::dumpWordsToScreenInHexAndExit(int nwords) {
  int i;
  unsigned int *pp;
  pp=(unsigned int*)pBankTRGD;
  if(sizeof(unsigned int)!=4) { PP"Error 77b in %s.\n",__FILE__); assert(0); }
  for(i=0;i<nwords;i++) {
    PP"word number %3d: 0x%08x\n",i+1,pp[i]);
  }
  PP"Exiting from diagnostic function.\n"); 
  assert(0); // Do not remove this exit.  If you don't want to exit, don't call this function.
}
char *Bank_TRGD::PrintHelp(char *msg,int nn) {
  static char rv[100];
  sprintf(rv,"%s[%3d]",msg,nn);
  return rv;
}
#define Funsigned_short "%32s     0x%04x %15d\n"
#define Fchar           "%32s          %c %15d\n"
#define Funsigned_int   "%32s 0x%08x %15d\n"
#define Funsigned_char  "%32s       0x%02x %15d\n"
#define Funsigned_long  Funsigned_int
#define FF fprintf(ff,
/*--------------------- Commented Sep 7 2001.  St_trg_Maker has functions for printing the data.
void Bank_TRGD::PrintAllTheData(FILE *ff) {
  int i;
  assert(ff);
  FF Funsigned_short  ,"EvtDesc.TCUdataBytes",GS->EvtDesc.TCUdataBytes,GS->EvtDesc.TCUdataBytes);
  FF Fchar            ,"EvtDesc.TCUEvtDesc",GS->EvtDesc.TCUEvtDesc,GS->EvtDesc.TCUEvtDesc);
  FF Funsigned_char   ,"EvtDesc.TrgDataFmtVer",GS->EvtDesc.TrgDataFmtVer,GS->EvtDesc.TrgDataFmtVer);
  FF Funsigned_int    ,"EvtDesc.bunchXing_hi",GS->EvtDesc.bunchXing_hi,GS->EvtDesc.bunchXing_hi);
  FF Funsigned_int    ,"EvtDesc.bunchXing_lo",GS->EvtDesc.bunchXing_lo,GS->EvtDesc.bunchXing_lo);
  FF Funsigned_short  ,"EvtDesc.TCU1.FIFO1.TrgToken",GS->EvtDesc.TCU1.FIFO1.TrgToken,GS->EvtDesc.TCU1.FIFO1.TrgToken);
  FF Funsigned_short  ,"EvtDesc.TCU1.FIFO1.TrgActionWd",GS->EvtDesc.TCU1.FIFO1.TrgActionWd,GS->EvtDesc.TCU1.FIFO1.TrgActionWd);
  FF Funsigned_long   ,"EvtDesc.TCU1.fifo1",GS->EvtDesc.TCU1.fifo1,GS->EvtDesc.TCU1.fifo1);
  FF Funsigned_short  ,"EvtDesc.TCU2.FIFO2.DSMInput",GS->EvtDesc.TCU2.FIFO2.DSMInput,GS->EvtDesc.TCU2.FIFO2.DSMInput);
  FF Funsigned_short  ,"EvtDesc.TCU2.FIFO2.DSMAddress",GS->EvtDesc.TCU2.FIFO2.DSMAddress,GS->EvtDesc.TCU2.FIFO2.DSMAddress);
  FF Funsigned_long   ,"EvtDesc.TCU2.fifo2",GS->EvtDesc.TCU2.fifo2,GS->EvtDesc.TCU2.fifo2);
  FF Funsigned_char   ,"EvtDesc.TCU3.FIFO3.DetectorBusy",GS->EvtDesc.TCU3.FIFO3.DetectorBusy,GS->EvtDesc.TCU3.FIFO3.DetectorBusy);
  FF Funsigned_char   ,"EvtDesc.TCU3.FIFO3.addBits",GS->EvtDesc.TCU3.FIFO3.addBits,GS->EvtDesc.TCU3.FIFO3.addBits);
  FF Funsigned_char   ,"EvtDesc.TCU3.FIFO3.TriggerWd",GS->EvtDesc.TCU3.FIFO3.TriggerWd,GS->EvtDesc.TCU3.FIFO3.TriggerWd);
  FF Funsigned_long   ,"EvtDesc.TCU3.fifo3",GS->EvtDesc.TCU3.fifo3,GS->EvtDesc.TCU3.fifo3);
  FF Funsigned_short  ,"EvtDesc.npre",GS->EvtDesc.npre,GS->EvtDesc.npre);
  FF Funsigned_short  ,"EvtDesc.npost",GS->EvtDesc.npost,GS->EvtDesc.npost);
  FF Funsigned_short  ,"TrgSum.TrgSumBytes",GS->TrgSum.TrgSumBytes,GS->TrgSum.TrgSumBytes);
  char *TrgSumHeader=(char*)&(GS->TrgSum.TrgSumHeader);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("TrgSum.TrgSumHeader",i),
      TrgSumHeader[i],
      TrgSumHeader[i]);
  for(i=0;i<  2;i++) FF Funsigned_int    ,PrintHelp("TrgSum.L1Sum",i),GS->TrgSum.L1Sum[i],GS->TrgSum.L1Sum[i]);
  for(i=0;i<  2;i++) FF Funsigned_int    ,PrintHelp("TrgSum.L2Sum",i),GS->TrgSum.L2Sum[i],GS->TrgSum.L2Sum[i]);
  FF Funsigned_short  ,"TrgSum.L0SumBytes",GS->TrgSum.L0SumBytes,GS->TrgSum.L0SumBytes);
  char *L0SumHeader=(char*)&(GS->TrgSum.L0SumHeader);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("TrgSum.L0SumHeader",i),
      L0SumHeader[i],
      L0SumHeader[i]);
  for(i=0;i< 32;i++) FF Funsigned_short  ,PrintHelp("TrgSum.DSM.CPA",i),GS->TrgSum.DSM.CPA[i],GS->TrgSum.DSM.CPA[i]);
  for(i=0;i<  8;i++) FF Funsigned_short  ,PrintHelp("TrgSum.DSM.quadDSM",i),GS->TrgSum.DSM.quadDSM[i],GS->TrgSum.DSM.quadDSM[i]);
  for(i=0;i<  8;i++) FF Funsigned_short  ,PrintHelp("TrgSum.DSM.lastDSM",i),GS->TrgSum.DSM.lastDSM[i],GS->TrgSum.DSM.lastDSM[i]);
  for(i=0;i< 16;i++) FF Funsigned_char  ,PrintHelp("TrgSum.DSM.ZDC",i),GS->TrgSum.DSM.ZDC[i],GS->TrgSum.DSM.ZDC[i]);
  for(i=0;i< 16;i++) FF Funsigned_short  ,PrintHelp("TrgSum.DSM.BCdata",i),GS->TrgSum.DSM.BCdata[i],GS->TrgSum.DSM.BCdata[i]);
  FF Funsigned_short  ,"TrgSum.L1SumBytes",GS->TrgSum.L1SumBytes,GS->TrgSum.L1SumBytes);
  char *L1SumHeader=(char*)&(GS->TrgSum.L1SumHeader);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("TrgSum.L1SumHeader",i),
      L1SumHeader[i],
      L1SumHeader[i]);
  for(i=0;i< 32;i++) FF Funsigned_int    ,PrintHelp("TrgSum.L1Result",i),GS->TrgSum.L1Result[i],GS->TrgSum.L1Result[i]);
  FF Funsigned_short  ,"TrgSum.L2SumBytes",GS->TrgSum.L2SumBytes,GS->TrgSum.L2SumBytes);
  char *L2SumHeader=(char*)&(GS->TrgSum.L2SumHeader);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("TrgSum.L2SumHeader",i),
      L2SumHeader[i],
      L2SumHeader[i]);
  for(i=0;i< 32;i++) FF Funsigned_int    ,PrintHelp("TrgSum.L2Result",i),GS->TrgSum.L2Result[i],GS->TrgSum.L2Result[i]);
  FF Funsigned_short  ,"RAW.RawDetBytes",GS->RAW[0].RawDetBytes,GS->RAW[0].RawDetBytes);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("RAW.RawDetHeader",i),GS->RAW[0].RawDetHeader[i],GS->RAW[0].RawDetHeader[i]);
  FF Funsigned_short  ,"RAW.CTBdataBytes",GS->RAW[0].CTBdataBytes,GS->RAW[0].CTBdataBytes);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("RAW.CTBdataHeader",i),GS->RAW[0].CTBdataHeader[i],GS->RAW[0].CTBdataHeader[i]);
  for(i=0;i<256;i++) FF Funsigned_char   ,PrintHelp("RAW.CTB",i),GS->RAW[0].CTB[i],GS->RAW[0].CTB[i]);
  FF Funsigned_short  ,"RAW.MWCdataBytes",GS->RAW[0].MWCdataBytes,GS->RAW[0].MWCdataBytes);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("RAW.MWCdataHeader",i),GS->RAW[0].MWCdataHeader[i],GS->RAW[0].MWCdataHeader[i]);
  FF Funsigned_long   ,"RAW.MWCfiller",GS->RAW[0].MWCfiller,GS->RAW[0].MWCfiller);
  for(i=0;i<128;i++) FF Funsigned_char   ,PrintHelp("RAW.MWC",i),GS->RAW[0].MWC[i],GS->RAW[0].MWC[i]);
  FF Funsigned_short  ,"RAW.EMCdataBytes",GS->RAW[0].EMCdataBytes,GS->RAW[0].EMCdataBytes);
  for(i=0;i<  2;i++) FF Fchar            ,PrintHelp("RAW.EMCdataHeader",i),GS->RAW[0].EMCdataHeader[i],GS->RAW[0].EMCdataHeader[i]);
  FF Funsigned_long   ,"RAW.EMCfiller",GS->RAW[0].EMCfiller,GS->RAW[0].EMCfiller);
}
#define Munsigned_short "%-32s     0x%04x %15d\n"
#define Mchar           "%-32s          %c %15d\n"
#define Munsigned_int   "%-32s 0x%08x %15d\n"
#define Munsigned_char  "%-32s       0x%02x %15d\n"
#define Munsigned_long  Munsigned_int
#define M1unsigned_short "%-32s     0x%04x\n"
#define M1char           "%-32s          %c\n"
#define M1unsigned_int   "%-32s 0x%08x\n"
#define M1unsigned_char  "%-32s       0x%02x\n"
#define M1unsigned_long  M1unsigned_int
#define PLINE fprintf(ff,"%s",line);
void Bank_TRGD::PrintDataCompact(FILE *ff) {
  int i;
  char *L0SumHeader,*TrgSumHeader;
  TrgSumHeader=(char*)&(GS->TrgSum.TrgSumHeader);
  L0SumHeader=(char*)&(GS->TrgSum.L0SumHeader);
  char line[] = "-------------------------------------------------------------";
  assert(ff);
  FF Munsigned_short  ,"EvtDesc.TCUdataBytes",GS->EvtDesc.TCUdataBytes,GS->EvtDesc.TCUdataBytes);
  FF M1char            ,"EvtDesc.TCUEvtDesc",GS->EvtDesc.TCUEvtDesc);
  FF Munsigned_char   ,"EvtDesc.TrgDataFmtVer",GS->EvtDesc.TrgDataFmtVer,GS->EvtDesc.TrgDataFmtVer);
  FF M1unsigned_int    ,"EvtDesc.bunchXing_hi",GS->EvtDesc.bunchXing_hi);
  FF M1unsigned_int    ,"EvtDesc.bunchXing_lo",GS->EvtDesc.bunchXing_lo);
  FF Munsigned_short  ,"EvtDesc.TCU1.FIFO1.TrgToken",GS->EvtDesc.TCU1.FIFO1.TrgToken,GS->EvtDesc.TCU1.FIFO1.TrgToken);
  FF M1unsigned_short  ,"EvtDesc.TCU1.FIFO1.TrgActionWd",GS->EvtDesc.TCU1.FIFO1.TrgActionWd);
  FF M1unsigned_long   ,"EvtDesc.TCU1.fifo1",GS->EvtDesc.TCU1.fifo1);
  FF M1unsigned_short  ,"EvtDesc.TCU2.FIFO2.DSMInput",GS->EvtDesc.TCU2.FIFO2.DSMInput);
  FF M1unsigned_short  ,"EvtDesc.TCU2.FIFO2.DSMAddress",GS->EvtDesc.TCU2.FIFO2.DSMAddress);
  FF M1unsigned_long   ,"EvtDesc.TCU2.fifo2",GS->EvtDesc.TCU2.fifo2);
  FF M1unsigned_char   ,"EvtDesc.TCU3.FIFO3.DetectorBusy",GS->EvtDesc.TCU3.FIFO3.DetectorBusy);
  FF M1unsigned_char   ,"EvtDesc.TCU3.FIFO3.addBits",GS->EvtDesc.TCU3.FIFO3.addBits);
  FF M1unsigned_char   ,"EvtDesc.TCU3.FIFO3.TriggerWd",GS->EvtDesc.TCU3.FIFO3.TriggerWd);
  FF M1unsigned_long   ,"EvtDesc.TCU3.fifo3",GS->EvtDesc.TCU3.fifo3);
  FF Munsigned_short  ,"EvtDesc.npre",GS->EvtDesc.npre,GS->EvtDesc.npre);
  FF Munsigned_short  ,"EvtDesc.npost",GS->EvtDesc.npost,GS->EvtDesc.npost);
  FF Munsigned_short  ,"TrgSum.TrgSumBytes",GS->TrgSum.TrgSumBytes,GS->TrgSum.TrgSumBytes);
  FF "%-32s    '%c%c'\n","TrgSum.TrgSumHeader", TrgSumHeader[0],TrgSumHeader[1]);
  FF "%-32s    0x%08x   0x%08x\n", "TrgSum.L1Sum",GS->TrgSum.L1Sum[0],GS->TrgSum.L1Sum[1]);
  FF "%-32s    0x%08x   0x%08x\n", "TrgSum.L2Sum",GS->TrgSum.L2Sum[0],GS->TrgSum.L2Sum[1]);
  FF Munsigned_short  ,"TrgSum.L0SumBytes",GS->TrgSum.L0SumBytes,GS->TrgSum.L0SumBytes);
  FF "%-32s    '%c%c'\n", "TrgSum.L0SumHeader",L0SumHeader[0],L0SumHeader[1]);


  FF "\n%s   ","TrgSum.DSM.CPA:");
  for(i=0;i< 32;i++) {
    if (i%8) FF "%4x ",GS->TrgSum.DSM.CPA[i]);
    else     FF "\n%2d| %4x ",i,GS->TrgSum.DSM.CPA[i]);
  }
  FF "\n\n");

  FF "\n%s   ","TrgSum.DSM.quadDSM:");
  for(i=0;i< 8;i++) {
    if (i%8) FF "%4x ",GS->TrgSum.DSM.quadDSM[i]);
    else     FF "\n%2d| %4x ",i,GS->TrgSum.DSM.quadDSM[i]);
  }
  FF "\n\n");

  FF "\n%s   ","TrgSum.DSM.lastDSM:");
  for(i=0;i< 8;i++) {
    if (i%8) FF "%4x ",GS->TrgSum.DSM.lastDSM[i]);
    else     FF "\n%2d| %4x ",i,GS->TrgSum.DSM.lastDSM[i]);
  }
  FF "\n\n");


  FF "\n%s   ","TrgSum.DSM.ZDC:");
  for(i=0;i<16;i++) {
    if (i%8) FF "%2x ",GS->TrgSum.DSM.ZDC[i]);
    else     FF "\n%2d| %2x ",i,GS->TrgSum.DSM.ZDC[i]);
  }
  FF "\n\n");


  FF "\n%s   ","TrgSum.DSM.BCdata:");
  for(i=0;i< 16;i++) {
    if (i%8) FF "%4x ",GS->TrgSum.DSM.BCdata[i]);
    else     FF "\n%2d| %4x ",i,GS->TrgSum.DSM.BCdata[i]);
  }
  FF "\n\n");

  PLINE
  char *L1SumHeader=(char*)&(GS->TrgSum.L1SumHeader);
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","TrgSum.L1Result:",GS->TrgSum.L1SumBytes,GS->TrgSum.L1SumBytes,
      L1SumHeader[0],
      L1SumHeader[1]);
  PLINE
  for(i=0;i< 32;i++) {
    if (i%8) FF "%8x ",GS->TrgSum.L1Result[i]);
    else     FF "\n%2d| %8x ",i,GS->TrgSum.L1Result[i]);
  }
  FF "\n\n");


  PLINE
  char *L2SumHeader=(char*)&(GS->TrgSum.L2SumHeader);
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","TrgSum.L2Result:",GS->TrgSum.L2SumBytes,GS->TrgSum.L2SumBytes,
      L2SumHeader[0],
      L2SumHeader[1]);
  PLINE
  for(i=0;i< 32;i++) {
    if (i%8) FF "%8x ",GS->TrgSum.L2Result[i]);
    else     FF "\n%2d| %8x ",i,GS->TrgSum.L2Result[i]);
  }
  FF "\n\n");

  PLINE
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","RAW Detector:",GS->RAW[0].RawDetBytes,GS->RAW[0].RawDetBytes,GS->RAW[0].RawDetHeader[0],GS->RAW[0].RawDetHeader[1]);
  PLINE
  FF "\n");

  PLINE
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","RAW.CTB:",GS->RAW[0].CTBdataBytes,GS->RAW[0].CTBdataBytes,GS->RAW[0].CTBdataHeader[0],GS->RAW[0].CTBdataHeader[1]);
  PLINE

  for(i=0;i< 256;i++) {
    if (i%24) FF "%2x ",GS->RAW[0].CTB[i]);
    else     FF "\n%3d| %2x ",i,GS->RAW[0].CTB[i]);
  }
  FF "\n\n");

  PLINE
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","RAW.MWCdataBytes:",GS->RAW[0].MWCdataBytes,GS->RAW[0].MWCdataBytes,GS->RAW[0].MWCdataHeader[0],GS->RAW[0].MWCdataHeader[1]);
  PLINE

  for(i=0;i< 128;i++) {
    if (i%24) FF "%2x ",GS->RAW[0].MWC[i]);
    else     FF "\n%3d| %2x ",i,GS->RAW[0].MWC[i]);
  }
  FF "\n\n");

  PLINE
  FF "\n%-20s0x%x (%d) bytes\tHeader:  '%c%c'\n","RAW.MWCdata:",GS->RAW[0].EMCdataBytes,GS->RAW[0].EMCdataBytes,GS->RAW[0].EMCdataHeader[0],GS->RAW[0].EMCdataHeader[1]);
  PLINE

  FF "\n");

  FF M1unsigned_long   ,"RAW.EMCfiller",GS->RAW[0].EMCfiller);
  FF "\n\n\n\n");
}
----------------------------------------------------------------------*/

int Bank_TRGD::swapHerb4bytes(unsigned long *data,int number) {
  return swapHerb4bytes((unsigned int*)data,number);
}
int Bank_TRGD::swapHerb4bytes(unsigned int  *data,int number) {
  int i;
  char *hh,temp[4];
  for(i=0;i<number;i++) {
    hh=(char*)(&(data[i]));
    temp[0]=hh[3]; temp[1]=hh[2]; temp[2]=hh[1]; temp[3]=hh[0];
    data[i]=*((INT32*)temp);
  }
  return 3;
}
int Bank_TRGD::swapHerb2bytes(short unsigned int *data,int number) {
  int i;
  char *hh,temp[2];
  for(i=0;i<number;i++) {
    hh=(char*)(&(data[i]));
    temp[0]=hh[1]; temp[1]=hh[0];
    data[i]=*((INT16*)temp);
  }
  return 3;
}





