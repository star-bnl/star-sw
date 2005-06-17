#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <strings.h>
#include "trgStructuresVer0x22.h"
#include "trgUtil.h"

#include "StTriggerDataReader.h"

ClassImp(StTriggerDataReader)

typedef struct {
  unsigned short TrgDataBytes;
  unsigned short TrgFiller;
} TrgEvtHeader;
TrgEvtHeader trgHead;

TrgDataType TrgData;

unsigned int BunchCrossingId(unsigned short b3, unsigned short b10, unsigned short b11){
  unsigned long long bxinghi,bxing1,bxinglo,bxing,bx0,bx1,bx2;
  int b120;
  bxinghi = b3;
  bxing1 =  b10;
  bxinglo = (bxing1 << 16) + b11;
  bxing = (bxinghi << 32) + bxinglo;
  bx0 = bxing;
  bx1 = bx0/120;
  bx2 = bx1 * 120;
  b120 = bx0 - bx2;
  return (unsigned int)(b120);
} 

unsigned int BunchCrossingId7bit(unsigned short b2){
  int b7=0, b7dat; 
  b7dat = b2;
  b7 = b7dat & 0x7f;
  return b7;
}

StTriggerDataReader::StTriggerDataReader(const char *name):StMaker(name)
{
  mFile=0;
  cout << "Constructing StTriggerDataReader with name=" << name << endl;
}

Int_t StTriggerDataReader::OpenFile(char* file){
  if ( (mFile= fopen (file, "r") )==NULL ) {
    printf("Error opening data file %s\n",file);
    return kStErr;
  }
  printf("Opening data file %s\n",file);
  return kStOK;
}

Int_t StTriggerDataReader::CloseFile(){
  if (mFile){
    printf("Closing trigger data file\n");
    fclose(mFile);
    return kStOK;
  }else{
    printf("No file to close\n");
    return kStOK;
  }
}

Int_t StTriggerDataReader::Make(){
  //  cout << "StTriggerDataReader Make() starting..................................."  << endl;
  volatile unsigned int *x;
  int err;

  if((err=fread((char *)&trgHead.TrgDataBytes, sizeof(char), sizeof(TrgEvtHeader) , mFile)) <=0) {
    if(err==0) {
      printf("End of file\n");
      return kStEOF;
    }
    printf("Error in reading TrgHead\n");
    fclose(mFile);
    return kStErr;
  }
  // printf("DataBytes = 0x%4.4x TrgFilter=0x%4.4x \n",trgHead.TrgDataBytes, trgHead.TrgFiller); 
  // printf("Swapping Header\n");
  x  = (volatile unsigned int*)&trgHead;
  *x = swapSCC(*x);  
  // printf("DataBytes = 0x%4.4x TrgFilter=0x%4.4x \n",trgHead.TrgDataBytes, trgHead.TrgFiller);

  if((err=fread((char *)&(TrgData.EvtDesc.TCUdataBytes), sizeof(char),trgHead.TrgDataBytes,mFile))<=0){
    printf("Error in reading TrgData\n");
    fclose(mFile);
    return kStErr;
  }  

  //printf("NPre=%d NPost=%d\n",TrgData.EvtDesc.npre,TrgData.EvtDesc.npost);
  //printf("Swapping Data\n");
  swapTrg(&TrgData, TrgData.EvtDesc.npre, TrgData.EvtDesc.npost);
  //printf("NPre=%d NPost=%d\n",TrgData.EvtDesc.npre,TrgData.EvtDesc.npost);
  //printf("Token=%d\n",TrgData.EvtDesc.TrgToken);
  //printf("trgword=%d  tucbit=%x\n",TrgData.EvtDesc.TriggerWord,TrgData.EvtDesc.DSMInput);
  //printf("Bunch=%d\n",BunchCrossingId(TrgData.TrgSum.DSMdata.BCdata[3],
  //				      TrgData.TrgSum.DSMdata.BCdata[10],
  //				      TrgData.TrgSum.DSMdata.BCdata[11]));
  //printf("Bunch7bit=%d\n",BunchCrossingId7bit(TrgData.TrgSum.DSMdata.BCdata[2]));
  //for (int j=0; j<96; j++)  printf("BBC(%d)=%d\n",j,TrgData.rawTriggerDet[0].BBC[j]); 

  mTrg=(void *)(& TrgData);
  return kStOK;
}
