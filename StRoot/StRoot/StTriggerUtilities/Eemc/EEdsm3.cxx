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
EEdsm3::getBarreJPthr2bit() const {
  int ch=0;
  ushort val=data[ch] & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getBarreHTthr2bit() const {
  int ch=0;
  ushort val=data[ch] >> 2;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getBarreEsumThr1bit() const {
  int ch=0;
  ushort val=data[ch] >> 4;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getJpsi1bit() const {
  int ch=0;
  ushort val=data[ch] >> 5;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getBarreHTTPthr1bit() const {
  int ch=0;
  ushort val=data[ch] >> 6;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getBarreTPthr1bit() const {
  int ch=0;
  ushort val=data[ch] >> 13;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEndcapJPthr2bit() const {
  int ch=0;
  ushort val=data[ch] >> 7;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEndcapHTthr2bit() const {
  int ch=0;
  ushort val=data[ch] >> 9;
  val=val & 3;
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEndcapEsumthr1bit() const {
  int ch=0;
  ushort val=data[ch] >> 11;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEndcapHTTPthr1bit() const {
  assert(mYear>=2006);
  int ch=0;
  ushort val=data[ch] >> 12;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEndcapTPthr1bit() const {
  assert(mYear>=2006);
  int ch=0;
  ushort val=data[ch] >> 14;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm3::getEtotThr1bit() const {
  int ch=0;
  ushort val=data[ch] >> 15;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm3::print( int k) const {
  printf("EEdsm3==TCU  INPUTS,  year=%d  \n",mYear); 
}
 
// $Log: EEdsm3.cxx,v $
// Revision 1.1  2009/10/12 18:04:26  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.5  2009/02/24 03:56:19  ogrebeny
// Corrected const-ness
//
// Revision 1.4  2007/08/17 01:15:36  balewski
// full blown Endcap trigger simu, by Xin
//
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

