#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cstructs/eemcConstDB.hh"

#include "StEEmcDbIndexItem1.h"

 StEEmcDbIndexItem1:: StEEmcDbIndexItem1() {
   clear();
}


void StEEmcDbIndexItem1::print() const{
  printf("DbIndexItem:");

  if(name[0]==0) {
    printf(" item not defined ???\n");
    return;
  }

  if(strchr(name,'U') || strchr(name,'V') )
    printf(" %s crate=%d chan=%3d sec=%d  strip=%d gain=%.3f  ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x\n",name,crate,chan,sec,strip,gain,ped,thr,stat,fail);
  else
    printf(" %s crate=%d chan=%3d sec=%d sub=%c eta=%d gain=%.3f hv=%6.1f ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x\n",name,crate,chan,sec,sub,eta,gain,hv,ped,thr,stat,fail);
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
  strip=-299;
  stat=fail=0;
}

void StEEmcDbIndexItem1::setName(char *text) {
  strncpy(name,text,StEEmcNameLen); 
  sec=atoi(text);
  if(strchr(name,'U') || strchr(name,'V') ) {
    strip=atoi(text+3);
  }else {  
    eta=atoi(text+4);
    sub=text[3];
  }
  // cleanup termintaing character
  int i;
  for(i=0;i<StEEmcNameLen;i++) {
    if(name[i]==EEMCDbStringDelim) {
      name[i]=0;
      return;
    }
  }
  printf("Error in  StEEmcDbIndexItem1::setName(%s)\n",text);
  assert(1==2);
}





