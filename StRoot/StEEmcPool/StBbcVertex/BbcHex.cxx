#include <math.h>
#include <stdio.h>
#include <string.h>

#include "BbcHex.h"

//-----------------------------
//-----------------------------
BbcHex::BbcHex(int i,char *nme) {
  id=i;
  strncpy(name,nme,len);
  name[len-1]=0;// just in case
  a=b=c=d=0;
  clear();
}

//-----------------------------
//-----------------------------
void BbcHex::clear() {
  tdc=adc=0;
  tof=-999;
}

//-----------------------------
//-----------------------------
void BbcHex::setHit(int T , int A) {
  tdc=T;
  adc=A;
  float xx=adc;
  if(adc>maxAdc) xx=maxAdc;
  float tCor=0;
  if(b!=.0)tCor=a+b*exp(-c*pow(xx,d));// do it better,JB
  tof=tdc-tCor;
}

//-----------------------------
//-----------------------------
void BbcHex::print(int k){
  printf(" tail=%s adc=%d tdc=%d tof=%f \n",name,adc,tdc,tof);
}



