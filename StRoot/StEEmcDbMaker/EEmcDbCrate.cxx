// $Id: EEmcDbCrate.cxx,v 1.1 2004/03/19 21:31:52 balewski Exp $

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

//#include "cstructs/eemcConstDB.hh"

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
  printf("DbCrate:");

  if(name[0]==0) {
    printf(" item not defined ???\n");
    return;
  }

  printf("%s crID=%d crIDswitch=%d fiber=%d nch=%d nHead=%d\n",name, crID, crIDswitch,fiber,nch,nHead);
}

//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::clear() {
  name[0]=0;
  crID=-1;
  crIDswitch=-2; 
  fiber=-3;
  nch=-4;
  nHead=-5;
}


//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::setName(char *text) {
  strncpy(name,text,CrateNameLen-1); 
}


#if 0
//--------------------------------------------------
//--------------------------------------------------
void EEmcDbCrate::exportAscii(FILE *fd) const{
  
  if(name[0]==0) return; // item not defined

  if(strchr(name,'U') || strchr(name,'V') )
    fprintf(fd,"%s %3d %3d %2d %c %4d %.3f %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,plane,strip,gain,ped,thr,stat,fail,tube,key);
  else
    fprintf(fd,"%s %d %3d %2d %c %2d %.3f  %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube,key);
}



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
// Revision 1.1  2004/03/19 21:31:52  balewski
// new EEMC data decoder
//
// Revision 1.8  2004/03/12 21:53:39  balewski



