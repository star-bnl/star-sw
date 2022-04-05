#include "EEstarTrig.h"
 
//#include <Stiostream.h>

ClassImp(EEstarTrig)


//--------------------------------------------------
//
//--------------------------------------------------
EEstarTrig ::  EEstarTrig() {
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEstarTrig ::  ~EEstarTrig() { }



//--------------------------------------------------
//
//--------------------------------------------------
void EEstarTrig :: clear() {
#define clearVec(x)  memset( x, 0, sizeof(x))

  bX48hi=bX48lo=bX7bit=0;
  daqbits=0;
  clearVec( offline_id );
  clearVec( EEMC       );
  clearVec( EEMC_l1    );
  clearVec( BEMC       );
  clearVec( BEMC_l1    );
  clearVec( lastDSM    );
  clearVec( VTX        );
  clearVec( EMC        );
  npre=npost=0;

#undef clearVec
}



//--------------------------------------------------
//
//--------------------------------------------------
int  EEstarTrig :: get48bXing() const{
  // printf("EEstarTrig:: print0() %d %d %d \n",bX48hi,bX48lo, bX7bit);
  unsigned long long int hi=bX48hi;
  unsigned long long int lo=bX48lo;
  unsigned long long int bx48=hi<<32;
  bx48+= lo;
  int bx=bx48%120;
  return bx;
}

//
//--------------------------------------------------
void EEstarTrig :: print(int k, FILE *fd) const{
  // printf("EEstarTrig:: print0() %d %d %d \n",bX48hi,bX48lo, bX7bit);
  int bx=get48bXing();
  int off=bx-bX7bit;
  if(off<0) off+=120;  
  fprintf(fd,"EEstarTrig:: print() bX=%d bX7=%d off=%d\n",bx,bX7bit,off);
  if(k<=0) return;

  int ii;
  fprintf(fd,"  daqBits=0x%x  npre=%d npost=%d\n",daqbits,npre,npost);
  
  fprintf(fd,"  trigID[] -->  ");
  for(ii=0;ii<32;ii++)  {
    if(offline_id[ii]==0) break;
    fprintf(fd,"[%2d]=0x%02X  [%u dec], ",ii,offline_id[ii],offline_id[ii]);
    if(ii%4==3) printf("\n      ");
  } 
  fprintf(fd,"\n");

  fprintf(fd," bbcTimeDiff=%d spinBits=x%2.2x lastDSM[7]=x%4.4x\n",bbcTimeDiff(), spinBits(),lastDSM[7]);
  //  fprintf(fd," Yell fill=%d   Up=%d  Down=%d  UnPol=%d \n",bitYellFill() ,bitYellUp(),bitYellDown(), bitYellUnpol());  
  // fprintf(fd," Blue fill=%d   Up=%d  Down=%d  UnPol=%d \n",bitBlueFill() ,bitBlueUp(),bitBlueDown(), bitBlueUnpol());  
  if(k<=1) return;


  for(ii=0;ii<144;ii++)  {
    if(ii%16==8)   printf(" , ");
    if(ii%16==0)   printf("\n EEMC board-%d= ",ii/16+1);
    fprintf(fd,"x%2.2x  ",  EEMC[ii]);
  }
  fprintf(fd,"\n");

  int j;
  
  fprintf(fd," DSM VTX[j]   = ");
  for (j=0; j<8; j++) printf("[%d]=x%4.4x ",j, VTX[j]);
  fprintf(fd,"\n");
  fprintf(fd," emcDSM[j]= ");
  for (j=0; j<8; j++) printf("[%d]=x%4.4x ",j, EMC[j]);
  fprintf(fd,"\n");
  fprintf(fd," lastDSM[j]= ");
  for (j=0; j<8; j++) printf("[%d]=x%4.4x ",j, lastDSM[j]);
  fprintf(fd,"\n");

}


//--------------------------------------------------
//--------------------------------------------------
int EEstarTrig ::isTrigID(int trigId) { // works only for 2003 data
  int i;
  for(i=0;i<32;i++)  {
    if(trigId==offline_id[i]) return 1;
    if(offline_id[i]<=0) break; // no more triggers in the list
  }
  return 0;
}


