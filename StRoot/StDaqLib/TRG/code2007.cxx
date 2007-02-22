// This is 2007 specific code for restoring packed(zero suppressed) trigger 
// data from daq file to full size trigger structure, and byte swap if needed.

#include <assert.h>
#include "trgStructures2007.h"
#include "TRG_Reader.hh"

int TRG_Reader::UnpackTrg2007(Bank_TRGP *pTRGP){
  int returnValue,npre,npost,swap,res;
  
  assert(pTRGP->header.ByteOrder==0x01020304 ||
	 pTRGP->header.ByteOrder==0x04030201);
  if(pTRGP->header.ByteOrder==0x04030201) {
    swap=0;
    returnValue=0;
  }else{
    swap=1;
    returnValue=pTRGP->header.swap();
  }
  if(pTRGP->header.ByteOrder!=0x04030201){
    printf("TRG_Reader::UnpackTrg2007: Swap Header error %s %d.\n",__FILE__,__LINE__);    
    assert(0);
  }

  //Get lengthes
  int size_head = 40;
  int size_trg  = sizeof(TrgDataType2007);
  int size_desc = sizeof(EvtDescData2007);
  int size_sum  = sizeof(TrgSumData2007);
  int size_raw  = sizeof(RawTrgDet2007);
  
  //Create memory space for unpacked trigger bank
  if(pBankUnp!=0) delete[] pBankUnp;
  int sizeUnp = size_head + size_trg;
  pBankUnp = new char[sizeUnp];  
  char *trgd = pBankUnp + size_head; //pointer for trgdata
  
  //Copy Header, EvtDesc and TrgSum and byte swap
  memcpy(pBankUnp, pTRGP, size_head+size_desc+size_sum);
  
  //Swap  EvtDesc and TrgSum
  if(swap){
    Swap2007_DescSum(trgd,npre,npost);
    if(res<0) {
      printf("TRG_Reader::UnpackTrg2007: Swap DescSum error %s %d.\n",__FILE__,__LINE__);
      return -1;
    }
  }

  //Set pointers for daq data and new unpacked data
  char* p_daq = (char *)pTRGP    + size_head + size_desc + size_sum;
  char* p_unp = (char *)pBankUnp + size_head + size_desc + size_sum;
  
  //Zero out all raw data memory before copying partial data
  memset(p_unp, 0, 11*size_raw);
  
  //Loop over # of crossings available and copy
  for(int i=0; i<1+npre+npost; i++){
    unsigned short *nbytes = (unsigned short *)p_daq;
    if(swap) pTRGD->swapHerb2bytes(nbytes,1); //byte swap for # of bytes
    memcpy(p_unp, p_daq, *nbytes);
    if(swap) pTRGD->swapHerb2bytes(nbytes,1); //swap back so that later whole things will swap correctly
    p_daq += *nbytes;
    p_unp += size_raw;
  }
    
  //Byte Swap (if needed) for all raw data section
  if(swap){
    res = Swap2007_Raw(trgd);
    if(res<0){
      printf("TRG_Reader::UnpackTrg2007: Swap RawData error %s %d.\n",__FILE__,__LINE__);
      return -1;
    }
  }
  
  //Switch bank pointer to fully restored data
  pBankTRGP = (Bank_TRGP *)pBankUnp; 
  
  return 0;
};

int TRG_Reader::Swap2007_DescSum(char *ptr, int npre, int npost){
  TrgDataType2007* p=(TrgDataType2007*)ptr;

  pTRGD->swapHerb2bytes(&(p->EvtDesc.TCUdataBytes),1);
  pTRGD->swapHerb4bytes(&(p->EvtDesc.bunchXing_hi),1);
  pTRGD->swapHerb4bytes(&(p->EvtDesc.bunchXing_lo),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.actionWdDetectorBitMask),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.TrgToken),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.addBits),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.DSMInput),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.externalBusy),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.modifiedBusyStatus),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.physicsWord),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.TriggerWord),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.DSMAddress),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.contaminationBusyStatus),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.npre),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.npost),1);
  pTRGD->swapHerb2bytes(&(p->EvtDesc.dummy),1);

  pTRGD->swapHerb2bytes(&(p->TrgSum.TrgSumBytes),1);
  pTRGD->swapHerb2bytes(&(p->TrgSum.TrgSumHeader),1);
  pTRGD->swapHerb4bytes(&(p->TrgSum.L1Sum[0]),2);
  pTRGD->swapHerb4bytes(&(p->TrgSum.L2Sum[0]),2);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L0SumBytes),1);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L0SumHeader),1);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.MTD[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.VPD[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.CPA[0]),16);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.CTB[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.lastDSM[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.VTX[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.EMC[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.BCdata[0]),16);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.specialTriggers[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.DSMdata.FPD[0]),8);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L1SumBytes),1);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L1SumHeader),1);
  pTRGD->swapHerb4bytes(&(p->TrgSum.L1Result[0]),32);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L2SumBytes),1);
  pTRGD->swapHerb2bytes(&(p->TrgSum.L2SumHeader),1);
  pTRGD->swapHerb4bytes(&(p->TrgSum.L2Result[0]),64);

  npre  = p->EvtDesc.npre;
  npost = p->EvtDesc.npost;
  if(npre<0 || npre>5 || npost<0 || npost>5) return -1;

  return 0;
};
  
int TRG_Reader::Swap2007_Raw(char *ptr) {
  int numToSwap,i;
  TrgDataType2007* p=(TrgDataType2007*)ptr;

  numToSwap=1+p->EvtDesc.npost+p->EvtDesc.npre;

  for(i=0;i<numToSwap;i++) { // loop over NPRE, NPOST as well
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].RawDetBytes),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].CTBdataBytes),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].MIXdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].MIXfiller),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].BEMCdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].BEMCfiller),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].BEMClayer1[0]),48);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].EEMCdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].EEMCfiller),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].EEMClayer1[0]),16);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].FPDdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].FPDfiller),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].FPDEastNSLayer1[0]),8);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].BBCdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].BBCfiller),1);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].BBClayer1[0]),16);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].ZDClayer1[0]),8);
    pTRGD->swapHerb2bytes(&(p->rawTriggerDet[i].QQTdataBytes),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].QQTfiller),1);
    pTRGD->swapHerb4bytes(&(p->rawTriggerDet[i].QQTdata[0]),1600);
  }
  return 0;
};

/*!
  Helps ensure we actually have the trigger data.

  If one of these asserts()s fails, it probably means that the
  trigger group wrote the .daq bank with a new version of trgStructures.h .
  If so, then you will have to modify all the offline code to switch
  between versions of trgStructures.h .

*/
void TRG_Reader::SanityCheck2007(char *ptr, int check_s=1) { 
  unsigned short x;
  TrgDataType2007* p=(TrgDataType2007*)ptr;

  x=p->TrgSum.L1SumBytes; assert(x==0x0084||x==0x8400);
  x=p->TrgSum.L2SumBytes; assert(x==0x0084||x==0x8400);

  if (check_s){
    assert( p->rawTriggerDet[0].RawDetHeader[0]  =='R');   
    assert( p->rawTriggerDet[0].RawDetHeader[1]  =='D');   
    assert( p->rawTriggerDet[0].CTBdataHeader[0] =='C');   
    assert( p->rawTriggerDet[0].CTBdataHeader[1] =='T');   
    assert( p->rawTriggerDet[0].BEMCdataHeader[0]=='E'); 
    assert( p->rawTriggerDet[0].BEMCdataHeader[1]=='M');  
  } else {
    cout << "TRG_Reader::SanityCheck2007 : Data position sanity check is disabled" << endl;
  }
};
