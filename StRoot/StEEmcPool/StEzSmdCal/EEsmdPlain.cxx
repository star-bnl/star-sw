#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "EEsmdPlain.h"
//-------------------------------------------------
//-------------------------------------------------
EEsmdPlain::EEsmdPlain(){
  thresE=-1; 
  xOffset=-2;
  uv='N';
  pattXX[0]=0;
  pattX[0]=0;
  clear();
}
//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::set(float th, int Xoff, char uv0) {
  thresE=th; 
  xOffset=Xoff;
  uv=uv0;
  int len=2*xOffset+2;
  assert(len>=2); // makes no sense to search for 0xx0, JB
  assert(len+2<MaxSmdStrips);

  memset(pattXX,'.',len);
  pattXX[len]=0; // string  terminatinon 
  pattXX[xOffset]='x';
  pattXX[xOffset+1]='x';

  memset(pattX,'.',len);
  pattX[len]=0; // string  terminatinon 
  pattX[xOffset]='x';

  printf("Set %c-plain , X-off=%d, thrE=%.2f, pattXX='%s'\n",uv,xOffset,thresE,pattXX);
}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::clear(){
  memset(hitOver,'.',sizeof(hitOver)); // clear it
  hitOver[288]=0; // terminate string
  nMatch=0;
  memset(iStrip,-1,sizeof(iStrip));
  memset(type,0,sizeof(type));
}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::print(int k){
  printf("nMatch=%d to plain='%c' \n",nMatch,uv);
  int i;
  for(i=0;i<nMatch;i++) printf("i=%d, iStrip=%d type=%d\n",i,iStrip[i],type[i]);

  if(k<=0) return;
  printf("%c-plain , X-off=%d, pattXX='%s'\n",uv,xOffset,pattXX);

  printf("iStrip:");
  for(i=0;i<MaxSmdStrips;i++) {
    if(hitOver[i]==0) break; // sense the terminating null
    if(i%50==0) printf("\ni=%3d ",i);
    if(i%10==0) printf(" ");
    printf("%c",hitOver[i]);
  }
  printf("\n");
}
  
//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::findMipPattern(){
  char *p=0;
  nMatch=0;

  // ............. search for XX
  p=hitOver;
  while((p=strstr(p,pattXX))) {
    iStrip[nMatch]=p-hitOver+xOffset;
    type[nMatch]=2;
    nMatch++;
    p+=xOffset;// move pointer a bit
  }

  // ............. search for X
  p=hitOver;
  while((p=strstr(p,pattX))) {
    iStrip[nMatch]=p-hitOver+xOffset;
    type[nMatch]=1;
    nMatch++;
    p+=xOffset;// move pointer a bit
  }

  //  print(1);
}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::scanAdc(float *val, float thr) {
  int istrip;
  for(istrip=0;istrip<MaxSmdStrips;istrip++) {
      if( val[istrip]>thr) hitOver[istrip]='x';
      // printf("iuv/istrip=%d/%3d  val=%6.2f --> '%c'\n",iuv,istrip,val[istrip],hitOver[istrip]);
    }
}
