// $Id: EEmcDbItem.cxx,v 1.5 2003/12/10 04:43:10 balewski Exp $

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
    printf(" %s crate=%d chan=%3d sec=%2d plane=%c strip=%3d gain=%.3f  ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x pix=%s key=%d\n",name,crate,chan,sec,plane,strip,gain,ped,thr,stat,fail,tube,key);
  else
    printf(" %s crate=%d chan=%3d sec=%2d sub=%c eta=%2d gain=%.3f  ped=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x tube=%s key=%d\n",name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube,key);
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::exportAscii(FILE *fd) const{
  
  if(name[0]==0) return; // item not defined

  if(strchr(name,'U') || strchr(name,'V') )
    fprintf(fd,"itemSMD %s %3d %3d %2d %c %4d %.3f %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,plane,strip,gain,ped,thr,stat,fail,tube,key);
  else
    fprintf(fd,"itemTWR %s %d %3d %2d %c %2d %.3f  %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube,key);
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
  key=-999;
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::setDefaultTube(int cr_off) {
  if(name[2]=='T') return; // do nothing for towers
  // view from the front of MAPMT , the same for Left & right column
  int ch2pix[16]={13, 14, 15, 16,   9, 10, 11, 12,   5, 6, 7, 8,   1,2,3,4};
  
  int iCrate=crate-cr_off;
  int iTube=chan/16;
  int tubeID=(iTube<=5) ? 2*iTube+1 :14-  2*(iTube-5); // tube ID counting from 1
  // int cwID=tubeID+ 12*(iCrate%8); // offset for every pair of subsectors, not used

  int secID=1 + ((iCrate/4)+11)%12;
  assert(secID==sec);

  int iBox=iCrate%4;
  int iPix=chan%16;
  int pixID=ch2pix[iPix]; 
  char text[100], boxName[100];
  sprintf(boxName,"S%d",iBox+1); // true for SMD
  if (iBox==3)sprintf(boxName,"P1"); // Pre/Post box
  sprintf(text,"%2.2d%2s-%2.2d:%2.2d%c",secID,boxName,tubeID,pixID,EEMCDbStringDelim);
setTube(text);
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
  if(name[2]=='U' || name[2]=='V' ) {
    plane=name[2];
    strip=atoi(text+3);
  }else {  
    assert(name[2]=='T' ||name[2]=='P' ||name[2]=='Q' ||name[2]=='R' );
    sub=text[3];
    eta=atoi(text+4);
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
// Revision 1.5  2003/12/10 04:43:10  balewski
// fisrt QA
//
// Revision 1.4  2003/12/04 18:27:46  balewski
// added MAPMT pixel names
//
// Revision 1.3  2003/12/01 05:01:40  balewski
// DB & SMD
//
// Revision 1.2  2003/11/22 05:35:36  balewski
// saves ped in DB format
//
// Revision 1.1  2003/11/20 16:01:25  balewski
// towards run4
//



