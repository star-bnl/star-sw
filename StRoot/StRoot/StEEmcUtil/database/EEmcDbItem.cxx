// $Id: EEmcDbItem.cxx,v 1.2 2009/12/03 21:15:39 ogrebeny Exp $
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "TString.h"

#include "cstructs/eemcConstDB.hh"

#include "EEmcDbItem.h"

//--------------------------------------------------
//--------------------------------------------------
EEmcDbItem::EEmcDbItem() {
   clear();
}

//--------------------------------------------------
//--------------------------------------------------
bool EEmcDbItem::isEmpty() const{
  return name[0]==0;
}

//--------------------------------------------------
//--------------------------------------------------
bool EEmcDbItem::isTower() const{
  if (isEmpty()) return false;
  return (name[2]=='T');
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::print() const{
  LOG_INFO<<"EEmcDbItem::print() "<<endm;

  if(name[0]==0) {
    LOG_WARN<<" item not defined ???"<<endm;
    return;
  }
  if( isSMD() ) {
    LOG_INFO<<Form("EEmcDbItem::SMD %s crate=%d chan=%3d sec=%2d plane=%c strip=%3d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x pix=%s key=%d\n",name,crate,chan,sec,plane,strip,gain,ped,sigPed,thr,stat,fail,tube,key)<<endm;
  } else {
    LOG_INFO<<Form("EEmcDbItem::Tail %s crate=%d chan=%3d sec=%2d sub=%c eta=%2d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x tube=%s key=%d\n",name,crate,chan,sec,sub,eta,gain,ped,sigPed,thr,stat,fail,tube,key)<<endm;
  }
}

//----                                          ----
//----                                          ----
ostream &
EEmcDbItem::print( ostream &out ) const
{
  out << "EEmcDbItem: ";
  if ( isEmpty() ) { 
    out << "item not defined"; 
  }
  else if ( isSMD() ){
    out << Form("%s crate=%d chan=%3d sec=%2d plane=%c strip=%3d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x pix=%s key=%d\n",name,crate,chan,sec,plane,strip,gain,ped,sigPed,thr,stat,fail,tube,key);
  }
  else {
    out << Form(" %s crate=%d chan=%3d sec=%2d sub=%c eta=%2d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x tube=%s key=%d\n",name,crate,chan,sec,sub,eta,gain,ped,sigPed,thr,stat,fail,tube,key);
  }
  return out;
}

//----                                          ----
//----                                          ----
ostream &operator<<(ostream &out, const EEmcDbItem &item )
{
  return item.print(out);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::exportAscii(FILE *fd) const{
  
  if(name[0]==0) return; // item not defined

  if(strchr(name,'U') || strchr(name,'V') ) {
    fprintf(fd,"%s %3d %3d %2d %c %4d %.3f %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,plane,strip,gain,ped,thr,stat,fail,tube,key);
  } else {
    fprintf(fd,"%s %d %3d %2d %c %2d %.3f  %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube,key);
  }
}



//--------------------------------------------------
//--------------------------------------------------
int EEmcDbItem::importAscii(FILE *fd){
  /* return:
    <0 : error in input
     0 : EOF
     1 : line ignored
     2 : valid input
  */
 
  clear();
  const int mx=1000;
  char buf[mx];
  
  char * ret=fgets(buf,mx,fd);
 
  if(ret==0) return 0;  

  if(buf[0]=='#') return 1; 

  char name0[mx];
  int ret1=sscanf(buf,"%s",name0);


  if(ret1==0) return -1;

  int n=0; 
  // printf("aaa name='%s' n=%d\n",name,name[0]);
  if(name0[2]=='U' || name0[2]=='V') {     
    n=sscanf(buf,"%s %d %d %d %c %d %f %f %f %x %x %s %d",name,&crate,&chan,&sec,&plane,&strip,&gain,&ped,&thr,&stat,&fail,tube,&key);
  }
  else if (name0[2]=='T' || name0[2]=='P' || name0[2]=='Q' || name0[2]=='R' ) {    
    n=sscanf(buf,"%s %d %d %d %c %d %f  %f %f %x %x %s %d",name,&crate,&chan,&sec,&sub,&eta,&gain,&ped,&thr,&stat,&fail,tube,&key);
  }
  else {
    return -3;
  }


  if(n!=13) return -1000-n;
  //    print();
  return 2;
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
  sigPed=-7;
  strip=-299;
  plane='X';	
  stat=fail=0;
  key=-999;
}


//________________________________________________________
//________________________________________________________
int  
EEmcDbItem::mapmtId() const{
  if(isTower()) return 0;
  if(chan<0 || chan>=192) return 0; // nonsens channel value
  int iTube=chan/16;
  int tubeID=(iTube<=5) ? 2*iTube+1 :14-  2*(iTube-5); // tube ID counting from 1
  // printf("chan=%d mapmtID=%d\n",chan,tubeID);
  return tubeID;
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::setDefaultTube(int cr_off) {
  if(name[2]=='T') return; // do nothing for towers
  // view from the front of MAPMT , the same for Left & right column
  int ch2pix[16]={13, 14, 15, 16,   9, 10, 11, 12,   5, 6, 7, 8,   1,2,3,4};
  
  int iCrate=crate-cr_off;

  int tubeID=mapmtId();
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
  strncpy(tube,text,StEEmcNameLen-1); 
  // cleanup termintaing character
  int i;
  for(i=0;i<StEEmcNameLen;i++) {
    if(tube[i]==EEMCDbStringDelim) {
      tube[i]=0;
      return;
    }
  }
  LOG_WARN<<Form("Error in  EEmcDbItem::setTube(%s), no terminating '%c'\n",text,EEMCDbStringDelim)<<endm;
  assert(1==2);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbItem::setName(char *text) {
  strncpy(name,text,StEEmcNameLen-1); 
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
  LOG_WARN<<Form("Error in  EEmcDbItem::setName(%s), no terminating '%c'\n",text,EEMCDbStringDelim)<<endm;
  assert(1==2);
}

// $Log: EEmcDbItem.cxx,v $
// Revision 1.2  2009/12/03 21:15:39  ogrebeny
// Fixed compiler warnings
//
// Revision 1.1  2009/02/04 20:33:28  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.16  2007/05/30 02:38:34  balewski
// replace printf -->LOG_XXX
//
// Revision 1.15  2007/01/26 20:45:58  balewski
// now we have pure new Logger, thanks Jason, Jan
//
// Revision 1.14  2005/09/19 21:43:32  balewski
// I was wrong, there was no bug
//
// Revision 1.13  2005/09/19 21:41:51  balewski
// bug : '&' was missing in importAscii()
//
// Revision 1.12  2005/02/02 01:36:50  balewski
// few more access methods + sigPed visible in EEmcDbItem
//
// Revision 1.11  2004/04/28 20:38:10  jwebb
// Added StEEmcDbMaker::setAsciiDatabase().  Currently not working, since
// tube name missing for some towers, triggereing a "clear" of all EEmcDbItems.
//
// Revision 1.10  2004/04/04 06:10:37  balewski
// *** empty log message ***
//
// Revision 1.9  2004/03/19 21:31:53  balewski
// new EEMC data decoder
//
// Revision 1.8  2004/03/12 21:53:39  balewski
// bug with not cleared 'plane'
//
// Revision 1.7  2004/03/12 02:24:10  balewski
// to synchronize
//
// Revision 1.6  2004/02/26 04:21:17  balewski
// read ASCII dump
//
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



