#include <stdio.h>
#include <string.h>

#include "StEEmcDbIndexItem1.h"


void StEEmcDbIndexItem1::print() {
  printf("DbIndexItem: %s crate=%d chan=%d gain=%.3f hv=%.1f ped=%.2f sig=%.2f\n",name,crate,chan,gain,hv,ped,sig);
}

void StEEmcDbIndexItem1::clear() {

  name[0]=0;
  crate= chan=-1; 
  gain=hv=-2;
  ped=sig-3;
}

void StEEmcDbIndexItem1::setName(char *text) {
      strncpy(name,text,StEEmcNameLen); 
}





