#include <stdio.h>
#include <string.h>

#include "StEEmcDbIndexItem1.h"

 StEEmcDbIndexItem1:: StEEmcDbIndexItem1() {
   clear();
}


void StEEmcDbIndexItem1::print() const{
  printf("DbIndexItem: %s crate=%d chan=%d gain=%.3f hv=%.1f ped=%.2f\n",name,crate,chan,gain,hv,ped);
}

void StEEmcDbIndexItem1::clear() {

  name[0]=0;
  crate= chan=-1; 
  gain=hv=-2;
  ped=-3;
}

void StEEmcDbIndexItem1::setName(char *text) {
      strncpy(name,text,StEEmcNameLen); 
}





