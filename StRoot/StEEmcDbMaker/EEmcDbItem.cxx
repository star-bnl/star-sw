// $Id: EEmcDbItem.cxx,v 1.1 2003/11/20 16:01:25 balewski Exp $

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cstructs/eemcConstDB.hh"

#include "EEmcDbItem.h"

//--------------------------------------------------
//--------------------------------------------------
EEmcDbItem::EEmcDbItem() {
   clear();
}

//--------------------------------------------------
//--------------------------------------------------
int EEmcDbItem::isEmpty() const{
  return name[0]==0;
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::print() const{
  printf("DbIndexItem:");

  if(name[0]==0) {
    printf(" item not defined ???\n");
    return;
  }

  if(strchr(name,'U') || strchr(name,'V') )
    printf(" %s crate=%d chan=%3d sec=%d  strip=%d gain=%.3f  ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x pix=%s\n",name,crate,chan,sec,strip,gain,ped,thr,stat,fail,tube);
  else
    printf(" %s crate=%d chan=%3d sec=%d sub=%c eta=%d gain=%.3f  ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x tube=%s\n",name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::clear() {
  name[0]=0;
  tube[0]=0;
  crate= chan=-1; 
  gain=-2;
  ped=-3;
  sec=-4;
  sub='Z';
  eta=-5;  
  thr=-6;
  strip=-299;
  stat=fail=0;
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::setTube(char *text) {
  strncpy(tube,text,StEEmcNameLen); 
  // cleanup termintaing character
  int i;
  for(i=0;i<StEEmcNameLen;i++) {
    if(tube[i]==EEMCDbStringDelim) {
      tube[i]=0;
      return;
    }
  }
  printf("Error in  EEmcDbItem::setTube(%s), no terminating '%c'\n",text,EEMCDbStringDelim);
  assert(1==2);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::setName(char *text) {
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
  printf("Error in  EEmcDbItem::setName(%s), no terminating '%c'\n",text,EEMCDbStringDelim);
  assert(1==2);
}

// $Log: EEmcDbItem.cxx,v $
// Revision 1.1  2003/11/20 16:01:25  balewski
// towards run4
//



