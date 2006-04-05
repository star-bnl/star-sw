#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "EEdsm3.h"


//--------------------------------------------------
//
//--------------------------------------------------
EEdsm3 ::EEdsm3() {
  clear();
  mYear=2005;
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm3::~EEdsm3() { }

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm3::clear() {
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm3::setWord(int ch, ushort val){
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getJPthr() {
  int ch=0;
  ushort val=data[ch]>>7;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getHTthr() {
  int ch=0;
  ushort val=data[ch]>>9;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getTPbit() {
  assert(mYear>=2006);
  int ch=0;
  ushort val=data[ch]>>12;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getHTTPbit() {
  assert(mYear>=2006);
  int ch=0;
  ushort val=data[ch]>>14;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEtbitE() {
  int ch=0;
  ushort val=data[ch]>>11;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEtbitB() {
  int ch=0;
  ushort val=data[ch]>>4;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEtbitBE() {
  int ch=0;
  ushort val=data[ch]>>15;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm3::print( int k) {
  printf("EEdsm3==TCU  INPUTS,  year=%d  \n",mYear); 
  printf("\n word=0x%04x   JPthr=%d   Tthr=%d   EtbitE=%d   EtbitB=%d  EtbitBE=%d  TPbit=%d  HTTPbit=%d\n\n",data[0],getJPthr(), getHTthr(), getEtbitE(), getEtbitB(), getEtbitBE(),getTPbit(),getHTTPbit());
}
 
// $Log: EEdsm3.cxx,v $
// Revision 1.3  2006/04/05 18:34:10  balewski
// new DSM bit assignment in 2006,
// possibly lost backward compatibility
// use tagged 2005 version if needed
//
// Revision 1.2  2005/02/01 22:13:40  perev
// Compatibility to redhat
//
// Revision 1.1  2004/11/29 20:12:59  balewski
// first
//
// Revision 1.1  2004/02/17 03:09:17  balewski
// *** empty log message ***
//

