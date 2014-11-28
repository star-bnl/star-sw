// $Id: EEmcDbCrate.cxx,v 1.1 2009/02/04 20:33:28 ogrebeny Exp $

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "TString.h"

#include "EEmcDbCrate.h"

//--------------------------------------------------
//--------------------------------------------------
EEmcDbCrate::EEmcDbCrate() {
   clear();
}

//--------------------------------------------------
//--------------------------------------------------
int EEmcDbCrate::isEmpty() const{
  return name[0]==0;
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::print() const{
  LOG_INFO<<"EEmcDbCrate::print():"<<endm;

  if(name[0]==0) {
    LOG_WARN<<" item not defined ???"<<endm;
    return;
  }

   LOG_INFO<<Form("EEmcDbCrate:: crID=%3d crIDswitch=%3d fiber=%d nCh=%d nHead=%d type=%c useIt=%d\n",name, crID, crIDswitch,fiber,nCh,nHead,type,useIt)<<endm;
}

ostream &
EEmcDbCrate::print( ostream &out ) const
{
  out << "DbCrate: ";
  if ( isEmpty() ) {
    out << "crate not defined";
    return out;
  }
  out << Form("%s crID=%3d crIDswitch=%3d fiber=%d nCh=%d nHead=%d type=%c useIt=%d",name, crID, crIDswitch,fiber,nCh,nHead,type,useIt);
  return out;
}

ostream &operator<<(ostream &out, const EEmcDbCrate &crate )
{
  return crate.print(out);
}
//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::clear() {
  name[0]=0;
  crID=-1;
  crIDswitch=-2; 
  fiber=-3;
  nCh=-4;
  nHead=-5;
  type='X';
  useIt=0;
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::setName(char *text) {
  strncpy(name,text,CrateNameLen-1); 
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::setAll(char *buf   ) {
  //printf("buf='%s'\n",buf);
  int ret=sscanf(buf,"%s %d %d %d %d %d %c %d",name,&crID,&crIDswitch,&fiber,&nCh,&nHead,&type,&useIt);
  //  printf("ret=%d\n",ret);
  assert(ret==8);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::exportAscii(FILE *fd) const{
  
  if(name[0]==0) return; // item not defined
  fprintf(fd,"%s  %d %d   %d   %d %d   %c %d \n",name,crID,crIDswitch,fiber,nCh,nHead,type,useIt);}


#if 0
//--------------------------------------------------
//--------------------------------------------------
int EEmcDbCrate::importAscii(FILE *fd){
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
  else 
    return -3;



  if(n!=13) return -1000-n;
  //    print();
  return 2;
}
#endif



// $Log: EEmcDbCrate.cxx,v $
// Revision 1.1  2009/02/04 20:33:28  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.7  2007/05/30 02:38:34  balewski
// replace printf -->LOG_XXX
//
// Revision 1.6  2007/01/26 20:45:58  balewski
// now we have pure new Logger, thanks Jason, Jan
//
// Revision 1.5  2004/04/04 06:10:36  balewski
// *** empty log message ***
//
// Revision 1.4  2004/04/03 06:32:48  balewski
// *** empty log message ***
//
// Revision 1.3  2004/03/30 04:44:57  balewski
// *** empty log message ***
//
// Revision 1.2  2004/03/28 04:09:08  balewski
// storage of EEMC raw data, not finished
//
// Revision 1.1  2004/03/19 21:31:52  balewski
// new EEMC data decoder
//
// Revision 1.8  2004/03/12 21:53:39  balewski



