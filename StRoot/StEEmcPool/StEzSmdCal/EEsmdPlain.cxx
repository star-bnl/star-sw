#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "EEsmdPlain.h"
//-------------------------------------------------
//-------------------------------------------------
EEsmdPlain::EEsmdPlain(){
  thresE=-1; 
  nDot=-2;
  uv='N';
  pattXX[0]=0;
  pattX[0]=0;
  hitOver=dotArray+oneOff; // map real strips in the middle of dot Arrary
  clear();
}
//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::set(float th, int nd, char uv0) {
  thresE=th; 
  nDot=nd;
  uv=uv0;
  int len=2*nDot+2;
  assert(len>=2); // makes no sense to search for 0xx0, JB
  assert(len+2<MaxSmdStrips);

  memset(pattXX,'.',len);
  pattXX[len]=0; // string  terminatinon 
  pattXX[nDot]='x';
  pattXX[nDot+1]='x';

  memset(pattX,'.',len);
  pattX[len]=0; // string  terminatinon 
  pattX[nDot]='x';

  printf("Set %c-plain , nDot=%d, thrE/MeV=%.2f, pattXX='%s'\n",uv,nDot,thresE*1000.,pattXX);
}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::clear(){
  memset(dotArray,'.',sizeof(dotArray)); // clear it
  dotArray[2*oneOff+MaxSmdStrips]=0;// terminate the string to make strstr work properly
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
  printf("%c-plain , X-off=%d, nDot='%s'\n",uv,nDot,pattXX);

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
  // print(1);

  // ............. search for XX
  p=dotArray;

  while((p=strstr(p,pattXX))) {
    int j=p-hitOver+nDot;
    assert(j>=0 && j<MaxSmdStrips); // logical error
    iStrip[nMatch]=j;
    type[nMatch]=2;
    nMatch++;
    p+=nDot;// move pointer a bit
  }

#if 1  //smdMap -- take it off for mapping test
  // ............. search for X 
  p=dotArray;
  while((p=strstr(p,pattX))) {
    int j=p-hitOver+nDot;
    //  printf("aa %d %d \n", p-dotArray, j);
    assert(j>=0 && j<MaxSmdStrips); // logical error
    iStrip[nMatch]=j;
    type[nMatch]=1;
    nMatch++;
    p+=nDot;// move pointer a bit
  }

#endif
}

//-------------------------------------------------
//-------------------------------------------------
void EEsmdPlain::scanAdc(float *val, float thr) {

  int istrip;
  for(istrip=0;istrip<MaxSmdStrips;istrip++) {
      if( val[istrip]>thr) hitOver[istrip]='x';
      // printf("istrip=%3d  val=%6.2f --> '%c'\n",istrip,val[istrip],hitOver[istrip]);
    }
}
