#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "StEEmcDbIndexItem1.h"

 StEEmcDbIndexItem1:: StEEmcDbIndexItem1() {
   clear();
}


void StEEmcDbIndexItem1::print() const{
  printf("DbIndexItem: %s crate=%d chan=%3d sec=%d sub=%c eta=%d gain=%.3f hv=%6.1f ped=%.2f ADC thr=%.2f stat=0x%4x fail=0x%4x\n",name,crate,chan,sec,sub,eta,gain,hv,ped,thr,stat,fail);
}

void StEEmcDbIndexItem1::clear() {
  name[0]=0;
  crate= chan=-1; 
  gain=hv=-2;
  ped=-3;
  sec=-4;
  sub='Z';
  eta=-5;  
  thr=-6;
  stat=fail=0;
}

void StEEmcDbIndexItem1::setName(char *text) {
  strncpy(name,text,StEEmcNameLen); 
  sec=atoi(text);
  eta=atoi(text+4);
  sub=text[3];
}






