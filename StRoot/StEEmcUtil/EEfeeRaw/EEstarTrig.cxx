#include "EEstarTrig.h"
 
#include <iostream.h>

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



#define clearVec(x)  memset( x, 0, sizeof(x))
//--------------------------------------------------
//
//--------------------------------------------------
void EEstarTrig :: clear() {
  bX48hi=bX48lo=bX7bit=0;
  daqbits=0;
  clearVec( offline_id );
  clearVec( EEMC       );
  clearVec( EEMC_l1    );
  clearVec( BEMC       );
  clearVec( BEMC_l1    );
  clearVec( CTB        );
  // clearOne( );
}



//--------------------------------------------------
//
//--------------------------------------------------
void EEstarTrig :: print( int k) const{
  // printf("EEstarTrig:: print0() %d %d %d \n",bX48hi,bX48lo, bX7bit);
  unsigned long long int hi=bX48hi;
  unsigned long long int lo=bX48lo;
  unsigned long long int bx48=hi<<32;
  bx48+= lo;
  int bx=bx48%120;
  int off=bx-bX7bit;
  if(off<0) off+=120;  
  printf("EEstarTrig:: print() bX=%d bX7=%d off=%d\n",bx,bX7bit,off);
  if(k<=0) return;

  int ii;
  printf("  daqBits=0x%x\n",daqbits);
  
  printf("  trigID[] -->  ");
  for(ii=0;ii<32;ii++)  {
    if(offline_id[ii]==0) break;
    printf("[%2d]=0x%02X  [%u dec], ",ii,offline_id[ii],offline_id[ii]);
    if(ii%4==3) printf("\n      ");
  } 
  printf("\n");

  if(k<=1) return;


  for(ii=0;ii<144;ii++)  {
	   if(ii%16==8)   printf(" , ");
    if(ii%16==0)   printf("\n EEMC board-%d= ",ii/16+1);
    printf("x%02x  ",  EEMC[ii]);
}
  printf("\n");
}
 




