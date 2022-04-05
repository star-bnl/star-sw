#include <stdio.h>
#include <string.h>
#include <assert.h>
/*********************************************************************
 * $Id: L2BinEvent.cxx,v 1.2 2010/04/18 06:05:32 pibero Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * binary dump of TRIG, BTOW & ETOW data
 *********************************************************************
 */

#include "L2BinEvent.h"

//===============================
//===============================
L2BinEvent::L2BinEvent() {
  // nop
}

//===============================
//===============================
int
L2BinEvent::read(struct BinEveContainer* eve,FILE *fd, int dbg){
  assert(eve);
  assert(fd);
  
  //................ clear old event
  memset(eve->lenA,0,sizeof(eve->lenA));
  memset(eve->mHEAD,':',sizeof(eve->mHEAD));
  memset(eve->mBTOW_BANK,0,sizeof(eve->mBTOW_BANK));
  memset(eve->mETOW_BANK,0,sizeof(eve->mETOW_BANK));
  memset(eve->mTrigData ,0,sizeof(eve->mTrigData));
  eve->mBTOW_in=eve->mETOW_in=0;  
  //..............  read new event
 
  unsigned int i;
  char *c;
  c=(char *)eve->lenA;
  for(i=0;i<sizeof(eve->lenA);i++) c[i]=fgetc(fd);
  if(dbg) for(i=0;i<4;i++) printf("lenA[%d]=%d\n",i,eve->lenA[i]);


  if(eve->lenA[0]> sizeof(eve->mHEAD)) return -10;
  if(eve->lenA[1]> sizeof(eve-> mTrigData))return -11;
  if(eve->lenA[2]> sizeof(eve->mBTOW_BANK))return -12;
  if(eve->lenA[3]> sizeof(eve->mETOW_BANK))return -13;
  
  
  c=(char *)eve->mHEAD;
  for(i=0;i<eve->lenA[0];i++) c[i]=fgetc(fd);
  
  c=(char *)eve->mTrigData;
  for(i=0;i<eve->lenA[1];i++) c[i]=fgetc(fd);
  
  c=(char *)eve->mBTOW_BANK;
  for(i=0;i<eve->lenA[2];i++) c[i]=fgetc(fd);
  
  c=(char *)eve->mETOW_BANK;
  for(i=0;i<eve->lenA[3];i++) c[i]=fgetc(fd);


  char EOE[5];
  for(i=0;i<4;i++) EOE[i]=fgetc(fd);
  EOE[4]=0; // terminate this string
  if(!strstr(EOE,"EofE")) return -1;
  assert(strstr(EOE,"EofE"));  // end of events  marker

  eve->mBTOW_in=eve->lenA[2]>0;
  eve->mETOW_in=eve->lenA[3]>0;
  
  if(dbg)  printf("head='%s'\nBTOW_in=%d,  ETOW_in=%d \n",eve->mHEAD,eve->mBTOW_in,eve->mETOW_in);
  return 0;
}


//===============================
//===============================
void
L2BinEvent::write(char* headText, int trgLen, void * trgData,
		  int bemcLen, unsigned short *bemcData,
		  int eemcLen, unsigned short *eemcData,
		  FILE *binFd){
  assert(binFd);
  
  BinEveContainer eve; // only partially used

  // collect record length

  eve.lenA[0]=sizeof(eve.mHEAD);
  eve.lenA[1]=trgLen; //sizeof(TrgDataType);
  eve.lenA[2]=bemcLen;
  eve.lenA[3]=eemcLen;
  
  // ready to write
  unsigned int i;

  const char *c=(char*)eve.lenA;
  for(i=0;i< sizeof(eve.lenA);i++) fputc(c[i],binFd);

  c=(char*)headText;
  for(i=0;i<eve.lenA[0];i++) fputc(c[i],binFd);

  c=(char*) trgData;
  for(i=0;i<eve.lenA[1];i++) fputc(c[i],binFd);

  c=(char*)bemcData;
  for(i=0;i< eve.lenA[2];i++) fputc(c[i],binFd);

  c=(char*)eemcData;
  for(i=0;i<eve.lenA[3];i++) fputc(c[i],binFd);

  c="EofE";
  for(i=0;i<4;i++)  fputc(c[i],binFd); // end of events  marker

}

/*
*********************************************************************
  $Log: L2BinEvent.cxx,v $
  Revision 1.2  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.1  2007/10/11 00:33:12  balewski
  L2algo added

  Revision 1.2  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/

