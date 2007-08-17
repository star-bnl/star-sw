/**************************************************************
 * $Id: EEdsm0Tree.cxx,v 1.1 2007/08/17 01:15:35 balewski Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>


#include "EEdsm0Tree.h"
#include "StEEmcUtil/EEdsm/EEdsm0.h"


//--------------------------------------------------
//
//--------------------------------------------------
EEdsm0Tree::EEdsm0Tree( char *nameX) {
  ee0=new EEdsm0[Nee0];
  ee0[2-1].setType(2); // serve 2 x 0.3 JP
  ee0[5-1].setType(2);
  ee0[8-1].setType(2);
  clear();
  strncpy(name,nameX,mxTxt);
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm0Tree::~EEdsm0Tree() {
  //fix it: delete 
 }

//--------------------------------------------------
//--------------------------------------------------
void EEdsm0Tree :: clear() {
  int i;
  for (i=0;i<Nee0;i++) ee0[i].clear();  
  memset(ee0outTPsum,0,sizeof(ee0outTPsum));
  memset( ee0outHT2bit,0,sizeof( ee0outHT2bit));
  memset( ee0outTP2bit,0,sizeof( ee0outTP2bit));
  memset(ee0outHTTP2bit,0,sizeof(ee0outHTTP2bit));
  memset( ee0out16bit,0,sizeof( ee0out16bit));
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0Tree::setInp12bit(int HankCh, short val){
  int ibr=HankCh/10; // board #
  assert(ibr>=0 && ibr<Nee0);
  int ch=HankCh%10;  
  ee0[ibr].setInp12bit(ch,val);
  // printf("add %d %d %d\n",ibr,ch,val);
}



//--------------------------------------------------
//--------------------------------------------------
int
EEdsm0Tree::getInp12bit(int HankCh){
  int ibr=HankCh/10; // board #
  assert(ibr>=0 && ibr<Nee0);
  int ch=HankCh%10;  
  return ee0[ibr].getInp12bit(ch);
}

//--------------------------------------------------
//--------------------------------------------------
void
EEdsm0Tree::setYear(int y, int*HTth, int*TPth ) {
  int i;
  for (i=0;i<Nee0;i++) ee0[i].setYear(y, HTth, TPth);
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0Tree::compute() {
  int i,j;
  
  for(i=0;i<Nee0; i++){
    // printf("\nee0[%d].compute()\n",i);
    ee0[i].compute();
    //  ee0[i].print();
  }

  j=0; 
  for(i=0;i<Nee0; i++){ // unpack outpt in arrays
       ee0outTPsum[j]= ee0[i].getOutTPsum();
      ee0outTP2bit[j]= ee0[i].getOutTP2bit();
      ee0outHT2bit[j]= ee0[i].getOutHT2bit();
    ee0outHTTP2bit[j]= ee0[i].getOutHTTP2bit();
       ee0out16bit[j]= ee0[i].getOut16bit();
    j++;
    if(i==1 || i==4 || i==7) { // double output
         ee0outTPsum[j]= ee0[i].getOutTPsum(1);
        ee0outTP2bit[j]= ee0[i].getOutTP2bit(1);
	ee0outHT2bit[j]= ee0[i].getOutHT2bit(1);
      ee0outHTTP2bit[j]= ee0[i].getOutHTTP2bit(1); 
         ee0out16bit[j]= ee0[i].getOut16bit(1);
      j++;
    }
  }
  
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0Tree::print( int k) {
  printf("EEdsm0Tree(%s) \n",name);

  int i;
  for(i=0;i<Nee0;i++) {
    printf("\n----------- level-0 Board %2d ",i+1);
    ee0[i].print();
  }

  printf("\n----------- level-0 emulated output \n   ch =");
  for(i=Nee0out-1;i>=0; i--) printf("  %2d ",i);
  printf("\n TPsum=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outTPsum[i]);
  printf("\n  HT2b=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outHT2bit[i]);

  printf("\nHTTP2b=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outHTTP2bit[i]);

  printf("\n");

}
 
/*
 * $Log: EEdsm0Tree.cxx,v $
 * Revision 1.1  2007/08/17 01:15:35  balewski
 * full blown Endcap trigger simu, by Xin
 *
 */

