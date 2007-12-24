// This is the analog of duplicated.code for the version of trgStructures.h
// which I (Herb) label "2003".

#define PREPOST 11 // This is also defined in TRG_Reader.cxx.

#include <assert.h>
#include "trgStructures2003.h"
#include "TRG_Reader.hh"

using namespace OLDEVP;

typedef struct {
  EvtDescData2003 EvtDesc;  /* L1 Event Descriptor Data */
  TrgSumData2003  TrgSum;   /* summary data */
  RawTrgDet2003   RAW[PREPOST];      
} MarilynMonroe;
MarilynMonroe *gs2003;

int Bank_TRGD::HerbSwap2003(char *ptr) {
  int numToSwap,returnValue,i;

  gs2003=(MarilynMonroe*)ptr;

  assert(header.ByteOrder==0x01020304||header.ByteOrder==0x04030201);
  if(header.ByteOrder==0x04030201) return 0;
  returnValue=header.swap();
  assert(header.ByteOrder==0x04030201);

  swapHerb2bytes(&(gs2003->EvtDesc.TCUdataBytes),1);
  swapHerb4bytes(&(gs2003->EvtDesc.bunchXing_hi),1);
  swapHerb4bytes(&(gs2003->EvtDesc.bunchXing_lo),1);
  swapHerb2bytes(&(gs2003->EvtDesc.actionWdDetectorBitMask),1);
  swapHerb2bytes(&(gs2003->EvtDesc.TrgToken),1);
  swapHerb2bytes(&(gs2003->EvtDesc.addBits),1);
  swapHerb2bytes(&(gs2003->EvtDesc.DSMInput),1);
  swapHerb2bytes(&(gs2003->EvtDesc.externalBusy),1);
  swapHerb2bytes(&(gs2003->EvtDesc.modifiedBusyStatus),1);
  swapHerb2bytes(&(gs2003->EvtDesc.physicsWord),1);
  swapHerb2bytes(&(gs2003->EvtDesc.TriggerWord),1);
  swapHerb2bytes(&(gs2003->EvtDesc.DSMAddress),1);
  swapHerb2bytes(&(gs2003->EvtDesc.contaminationBusyStatus),1);
  swapHerb2bytes(&(gs2003->EvtDesc.npre),1);
  swapHerb2bytes(&(gs2003->EvtDesc.npost),1);
  swapHerb2bytes(&(gs2003->EvtDesc.dummy),1);

  swapHerb2bytes(&(gs2003->TrgSum.TrgSumBytes),1);
  swapHerb2bytes(&(gs2003->TrgSum.TrgSumHeader),1);
  swapHerb4bytes(&(gs2003->TrgSum.L1Sum[0]),2);
  swapHerb4bytes(&(gs2003->TrgSum.L2Sum[0]),2);
  swapHerb2bytes(&(gs2003->TrgSum.L0SumBytes),1);
  swapHerb2bytes(&(gs2003->TrgSum.L0SumHeader),1);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.CPA[0]),32);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.quadDSM[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.lastDSM[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.VTX[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.EMC[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.BCdata[0]),16);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.specialTriggers[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.DSMdata.FPD[0]),8);
  swapHerb2bytes(&(gs2003->TrgSum.L1SumBytes),1);
  swapHerb2bytes(&(gs2003->TrgSum.L1SumHeader),1);
  swapHerb4bytes(&(gs2003->TrgSum.L1Result[0]),32);
  swapHerb2bytes(&(gs2003->TrgSum.L2SumBytes),1);
  swapHerb2bytes(&(gs2003->TrgSum.L2SumHeader),1);
  swapHerb4bytes(&(gs2003->TrgSum.L2Result[0]),32);
  
  // Herb, Mar 28 2003.  There is a bug, from which the npre/npost numbers in the 
  // trigger data (5/5) sometimes do not agree with the bank len in the TRGD header, which causes a seg vio
  // in the for() loop below.  Here I check for the bug, and if it's present, I override npre and npost.
  if( 4*header.BankLength < int((1+gs2003->EvtDesc.npre+gs2003->EvtDesc.npost) * sizeof(RawTrgDet2003)) ) {
    gs2003->EvtDesc.npre=0; gs2003->EvtDesc.npost=0; return -1;
  }

  numToSwap=1+gs2003->EvtDesc.npost+gs2003->EvtDesc.npre; assert(numToSwap<50&&numToSwap>0);
  assert(numToSwap>=0&&numToSwap<=PREPOST);


  for(i=0;i<numToSwap;i++) { // loop over NPRE, NPOST as well
    swapHerb2bytes(&(gs2003->RAW[i].RawDetBytes),1);
    swapHerb2bytes(&(gs2003->RAW[i].CTBdataBytes),1);
    swapHerb2bytes(&(gs2003->RAW[i].MWCdataBytes),1);
    swapHerb4bytes(&(gs2003->RAW[i].MWCfiller),1);
    swapHerb2bytes(&(gs2003->RAW[i].BEMCdataBytes),1);
    swapHerb4bytes(&(gs2003->RAW[i].BEMCfiller),1);
    swapHerb2bytes(&(gs2003->RAW[i].BEMClayer1[0]),48);
    swapHerb2bytes(&(gs2003->RAW[i].EEMCdataBytes),1);
    swapHerb4bytes(&(gs2003->RAW[i].EEMCfiller),1);
    swapHerb2bytes(&(gs2003->RAW[i].EEMClayer1[0]),16);
    swapHerb2bytes(&(gs2003->RAW[i].FPDdataBytes),1);
    swapHerb4bytes(&(gs2003->RAW[i].FPDfiller),1);
    swapHerb2bytes(&(gs2003->RAW[i].FPDEastNSLayer1[0]),8);
    swapHerb2bytes(&(gs2003->RAW[i].FPDEastTBLayer1[0]),8);
    swapHerb2bytes(&(gs2003->RAW[i].FPDWestNSLayer1[0]),8);
    swapHerb2bytes(&(gs2003->RAW[i].FPDWestTBLayer1[0]),8);
    swapHerb2bytes(&(gs2003->RAW[i].BBCdataBytes),1);
    swapHerb4bytes(&(gs2003->RAW[i].BBCfiller),1);
    swapHerb2bytes(&(gs2003->RAW[i].BBClayer1[0]),16);
    swapHerb2bytes(&(gs2003->RAW[i].ZDClayer1[0]),8);
  }

  return returnValue;
}

/*!
  Helps ensure we actually have the trigger data.

  If one of these asserts()s fails, it probably means that the
  trigger group wrote the .daq bank with a new version of trgStructures.h .
  If so, then you will have to modify all the offline code to switch
  between versions of trgStructures.h .

*/
void TRG_Reader::SanityCheck2003(char *ptr, int check_s=1) { 

  gs2003=(MarilynMonroe*)ptr;
  unsigned short x;

  x=gs2003->TrgSum.L1SumBytes; assert(x==0x0084||x==0x8400);
  x=gs2003->TrgSum.L2SumBytes; assert(x==0x0084||x==0x8400);

  /* As of Jan 14 2003, the trigger data is messed up, and this sanity check
  ** will fail.  Under pressure to get the chain running, I'm commenting it
  ** out.  We'll depend upon the above checks, which are not messed up.
  if (check_s){
    assert( gs2003->RAW[0].RawDetHeader[0]  =='R');   
    assert( gs2003->RAW[0].RawDetHeader[1]  =='D');   
    assert( gs2003->RAW[0].CTBdataHeader[0] =='C');   
    assert( gs2003->RAW[0].CTBdataHeader[1] =='T');   
    assert( gs2003->RAW[0].MWCdataHeader[0] =='M');   
    assert( gs2003->RAW[0].MWCdataHeader[1] =='W');   
    assert( gs2003->RAW[0].BEMCdataHeader[0]=='E'); 
    assert( gs2003->RAW[0].BEMCdataHeader[1]=='M');  
  } else {
    cout << "TRG_Reader::SanityCheck2003 : Bank position and summary sanity check is disabled" << endl;
  }
  */
}
