// This is 2008 specific code for restoring packed(zero suppressed) trigger
// data from daq file to full size trigger structure, and byte swap if needed.

#include <assert.h>
#include "trgStructures2008.h"
#include "TRG_Reader.hh"
using namespace OLDEVP;

int TRG_Reader::UnpackTrg2008(Bank_TRGP *pTRGP){
  int returnValue,npre,npost,swap,res;

  if ( ! pTRGP){
    printf("TRG_Reader::UnpackTrg2008 - not pTRGP\n");
    return -1;
  }

  assert(pTRGP->header.ByteOrder==0x01020304 ||
	 pTRGP->header.ByteOrder==0x04030201);
  if(pTRGP->header.ByteOrder==0x04030201) {
    swap=1;
    returnValue=0;
  }else{
    swap=1;
    returnValue=pTRGP->header.swap();
  }
  if(pTRGP->header.ByteOrder!=0x04030201){
    printf("TRG_Reader::UnpackTrg2008: Swap Header error %s %d.\n", __FILE__ , __LINE__ );
    assert(0);
  }
  printf("TRG_Reader::UnpackTrg2008: Token from Bank=%d\n",pTRGP->header.Token);

  //Get lengthes
  // int size_bank = pTRGP->header.BankLength * 4;
  int size_off  = pTRGP->theData.offset * 4;
  int size_head = sizeof(pTRGP->header);
  int size_trg  = sizeof(TrgDataType2008);
  int size_desc = sizeof(EvtDescData2008);
  int size_sum  = sizeof(TrgSumData2008);
  int size_raw  = sizeof(RawTrgDet2008);
  //  printf("TRG_Reader::UnpackTrg2008: offset, header, trg, desc, sum, raw = %d %d %d %d %d %d\n",
  //	 ,size_off,size_head,size_trg,size_desc,size_sum,size_raw);

  // New TrgTowerTrnfer structure and find trigger data itself
  char* cttt = (char*)pTRGP + size_off + size_head;
  TrgTowerTrnfer2008* ttt=(TrgTowerTrnfer2008*)cttt;
  if(swap){
    res=Swap2008_TrgTowerTrnfer(cttt);
    if(res<0) {
      printf("TRG_Reader::UnpackTrg2008: Swap TrgTowerTrnfer error %s %d.\n", __FILE__ , __LINE__ );
      return -1;
    }
  }
  int offset = ttt->OffsetBlock[y8TRG_INDEX].offset;
  int length = ttt->OffsetBlock[y8TRG_INDEX].length;
  printf("TRG_Reader::UnpackTrg2008: TrgTowerTrnfer byet_version=0x%x offset=%d length=%d\n",
	 ttt->byteCount_Version,offset,length);
  if(length<=0) {
    printf("TRG_Reader::UnpackTrg2008: No Trigger Data %s %d.\n", __FILE__ , __LINE__ );
    return 0;
  }
  char* trg_version = (char*)pTRGP + size_off + size_head + offset + 3;
  printf("TRG_Reader::UnpackTrg2008: trg_version = 0x%x\n",*trg_version);
  if(*trg_version != 0x32) {
    printf("TRG_Reader::UnpackTrg2008: Trigger version error %s %d.\n",*trg_version, __FILE__ , __LINE__ );
    return -1;
  }

  //Create memory space for unpacked trigger bank
  if(pBankUnp!=0) delete[] pBankUnp;
  int sizeUnp = size_off + size_head + size_trg;
  pBankUnp = new char[sizeUnp];
  char *trgd = pBankUnp + size_off + size_head; //pointer for trgdata
  TrgDataType2008* p=(TrgDataType2008*)trgd;

  //Copy Header, EvtDesc and TrgSum and byte swap
  memcpy(pBankUnp, pTRGP, size_off+size_head);
  memcpy(pBankUnp+size_off+size_head, (char*)pTRGP+size_off+size_head+offset, size_desc+size_sum);
  if(swap){
    res=Swap2008_DescSum(trgd);
  }
  npre  = p->EvtDesc.npre;
  npost = p->EvtDesc.npost;
  printf("TRG_Reader::UnpackTrg2008: TCUdataBytes = %d Token = %d Npre/Npost=%d/%d\n",
	 p->EvtDesc.TCUdataBytes, p->EvtDesc.TrgToken, npre,npost);
  if(swap && res<0) {
    printf("TRG_Reader::UnpackTrg2008: Swap DescSum error %s %d.\n", __FILE__ , __LINE__ );
    return -1;
  }

  if(p->EvtDesc.TrgToken>4096) {
    printf("TRG_Reader::UnpackTrg2008: Found Token beyond 4096\n");
    assert(0);
  }
  if(npre>5 || npre<0 || npost>5 || npost<0) {
    printf("TRG_Reader::UnpackTrg2008: Trigger data has more than 5 pre/post\n");
    assert(0);
  }

  //Set pointers for daq data and new unpacked data
  char* p_daq = (char *)pTRGP    + size_off + size_head + size_desc + size_sum + offset;
  char* p_unp = (char *)pBankUnp + size_off + size_head + size_desc + size_sum;

  //Zero out all raw data memory before copying partial data
  memset(p_unp, 0, 11*size_raw);

  //Loop over # of crossings available and copy
  for(int i=0; i<1+npre+npost; i++){
    unsigned short *nbytes = (unsigned short *)p_daq;
    if(swap) pTRGD->swapHerb2bytes(nbytes,1); //byte swap for # of bytes
    int n = *nbytes;
    //printf("TRG_Reader::UnpackTrg2008: Nprepost=%d   RawDat Size=%d (byte) [%d]\n",i,n,size_raw);
    if(swap){
      //printf("-> Swapping on %p\n",pTRGD);
      pTRGD->swapHerb2bytes(nbytes,1); //swap back so that later whole things will swap correctly
    }

    if (n>size_raw){
      printf("**********************************************************************************\n");
      printf("TRG_Reader::UnpackTrg2008\n");
      printf("\tWARNING! TrgRaw bank has improper reported size %d > %d\n",n,size_raw);
      printf("\tSkipping event\n");
      printf("**********************************************************************************\n");
      return -1;
    }

    //printf("-> Doing memcpy to %p from %p\n",p_unp,p_daq);
    memcpy(p_unp, p_daq, n);

    p_daq += n;
    p_unp += size_raw;
  }

  //Byte Swap (if needed) for all raw data section
  //printf("-> Checking if swap is necessary\n");
  if(swap){
    //printf("-> Swap2008_Raw on %p\n",trgd);
    res = Swap2008_Raw(trgd);
    if(res<0){
      printf("TRG_Reader::UnpackTrg2008: Swap RawData error %s %d.\n", __FILE__ , __LINE__ );
      return -1;
    }
  }

  //Switch bank pointer to fully restored data
  //printf("-> pBankTRGP set to (Bank_TRGP *) %p\n",pBankUnp);
  pBankTRGP = (Bank_TRGP *)pBankUnp;

  return 0;
};

int TRG_Reader::Swap2008_TrgTowerTrnfer(char *ptr){
  TrgTowerTrnfer2008* p = (TrgTowerTrnfer2008 *)ptr;
  pTRGD->swapHerb4bytes(&(p->byteCount_Version),1);
  pTRGD->swapHerb4bytes(&(p->OffsetBlock[0].offset),2 * y8MAX_OFFSET);
  return 0;
};

int TRG_Reader::Swap2008_DescSum(char *ptr){
  TrgDataType2008* p=(TrgDataType2008*)ptr;

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

  int npre  = p->EvtDesc.npre;
  int npost = p->EvtDesc.npost;
  if(npre<0 || npre>5 || npost<0 || npost>5) return -1;

  return 0;
};

int TRG_Reader::Swap2008_Raw(char *ptr) {
  int numToSwap,i;
  TrgDataType2008* p=(TrgDataType2008*)ptr;

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
    int nqt = p->rawTriggerDet[i].QQTdataBytes/4;
    int ac10 = p->rawTriggerDet[i].QQTdata[nqt-1];
    printf("NQTdata = %d, Last word check = 0x%x (should be ac10)\n",nqt,ac10);
    if(nqt>0 && ac10 != 0xAC10){
      printf("Last word of QT data is not 0xAC10 but 0x%x\n ",ac10);
      return -1;
    }
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
void TRG_Reader::SanityCheck2008(char *ptr, int check_s=1) {
  unsigned short x;
  TrgDataType2008* p=(TrgDataType2008*)ptr;

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
    cout << "TRG_Reader::SanityCheck2008 : Data position sanity check is disabled" << endl;
  }
};
