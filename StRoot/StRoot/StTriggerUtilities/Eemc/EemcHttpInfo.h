#ifndef EemcHttpInfo_h
#define  EemcHttpInfo_h

class EemcHttpInfo{ // depends on HTthrX, TPthrX,HTTPthrSelc=X+1
 public:
  int tpId,tpSumDsmAdc; // TP information
  int htwCh,htwCr,htwDsmAdc; // HT info,  give the first  tower above HtThres
  void clear(){  tpId=tpSumDsmAdc=htwCh=htwCr=htwDsmAdc=-1;}
  void print() { printf("TPinfo: id=%d sumDsm=%d  HT: cr=%d ch=%d dsmVal=%d\n",tpId,tpSumDsmAdc,htwCr,htwCh,htwDsmAdc);}
};
 

#endif
// $Log: EemcHttpInfo.h,v $
// Revision 1.1  2007/07/25 01:25:38  balewski
// start
//
