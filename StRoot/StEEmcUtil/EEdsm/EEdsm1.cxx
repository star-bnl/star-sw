#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "EEdsm1.h"

//--------------------------------------------------
//
//--------------------------------------------------
EEdsm1 ::  EEdsm1() {
  type=0;
  clear();
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm1 ::  ~EEdsm1() { }

//--------------------------------------------------
//--------------------------------------------------
void EEdsm1 :: clear() {
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsm1::setWord(int ch, ushort val){
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort EEdsm1::getHTthr(int ch) { 
  assert(ch>=0 && ch<nc);
  ushort val=(data[ch]& 0xfff) >>10;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort EEdsm1::getTPsum(int ch) { 
  assert(ch>=0 && ch<nc);

  /* The first board has patches  .6 .3 .3 .6 .6 .3
     The second board has patches .3 .6 .6 .3 .3 .6
  */

  ushort val=0xfff;

  if (type == 1) {
    switch (ch) {
    case 0: // use last 10 bits for 0.6x1 patches
    case 3:
    case 4:
      val=data[ch]& 0x3ff;
      break;
    case 1: // use last 9 bits for 0.3x1 patches
    case 2:
    case 5:
      val=data[ch]& 0x1ff;
      break;
    }
  }
  else if (type == 2) {
    switch (ch) {
    case 1: // use last 10 bits for 0.6x1 patches
    case 2:
    case 5:
      val=data[ch]& 0x3ff;
      break;
    case 0: // use last 9 bits for 0.3x1 patches
    case 3:
    case 4:
      val=data[ch]& 0x1ff;
      break;
    }
  }
  else assert (1==2);

  return val;

}



//--------------------------------------------------
//--------------------------------------------------
void EEdsm1 :: print( int k ) {
  printf("EEdsm1:: print()  \n");
  int i;

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nword  = ");
  for(i=nc-1;i>=0;i--) printf(" x%4.4x ", data[i] );

  printf("\nTPsum = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ", getTPsum(i));

  printf("\nHTthr = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ", getHTthr(i));

  printf("\n");

}
 
// $Log: EEdsm1.cxx,v $
// Revision 1.2  2005/02/01 22:13:39  perev
// Compatibility to redhat
//
// Revision 1.1  2004/11/29 20:12:59  balewski
// first
//
// Revision 1.2  2004/04/23 20:16:56  balewski
// trig patch fix
//
// Revision 1.1  2004/02/17 03:09:17  balewski
// *** empty log message ***
//

