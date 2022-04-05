/**************************************************************
 * $Id: EMCdsm2Tree.cxx,v 1.3 2009/11/18 15:50:59 pibero Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>
 

#include "EMCdsm2Tree.h"
#include "BEdsm2.h"


//--------------------------------------------------
//--------------------------------------------------
EMCdsm2Tree::EMCdsm2Tree(const char *nameX) {
  mYear=-999;
  BEsumthr8bit=1000;
  EEsumthr6bit=1000;
  JPSIthrSelc2bit=1000;
  BarreSide2bit=1000;
  EtotThr8bit=1000;

  be2=new BEdsm2[Nbe2];
  int i=0;
  for(i=0; i<Nbe2; i++) be2[i].setYear(-999); // must be defined before use
  clear();
  strncpy(name,nameX,mxTxt);
}

//--------------------------------------------------
//--------------------------------------------------
EMCdsm2Tree::~EMCdsm2Tree() {
  //fix it: delete 
 }

//--------------------------------------------------
//--------------------------------------------------
void 
EMCdsm2Tree::setYear(int x, int BEsumthr, int EEsumthr, int JPSIthrSelc, int BarreSide, int EtotThr) {
  mYear=x;
  BEsumthr8bit = BEsumthr;
  EEsumthr6bit = EEsumthr;
  JPSIthrSelc2bit = JPSIthrSelc;
  BarreSide2bit = BarreSide;
  EtotThr8bit = EtotThr;
  int i;
  for (i=0;i<Nbe2;i++) be2[i].setYear(mYear);
}

//--------------------------------------------------
//--------------------------------------------------
void 
EMCdsm2Tree::clear() {
  int i;
  for (i=0;i<Nbe2;i++) be2[i].clear();  
  OutEndcapJP2bit=0;
  OutEndcapHT2bit=0;
  OutEndcapSum1bit=0; 
  OutEndcapHTTP1bit=0;
  OutEndcapTP1bit=0;
  OutBarreJP2bit=0;
  OutBarreHT2bit=0;
  OutBarreSum1bit=0;
  OutBarreJPSi1bit=0;
  OutBarreHTTP1bit=0;
  OutBarreTP1bit=0;
  OutEtot1bit=0;

  intBarreSum=0;
  intEndcapSum=0;
  intEtot=0;
  // memset(,0,sizeof());

}


//--------------------------------------------------
//--------------------------------------------------
void 
EMCdsm2Tree::setInput16bit(int ibrd, int ch, ushort val){
  // printf("addY %d  %d %d\n",brd,ch,val);
  assert(ibrd>=0 && ibrd<Nbe2);
  be2[ibrd].setWord(ch,val);

}



//--------------------------------------------------
//--------------------------------------------------
void 
EMCdsm2Tree::compute() {
  //printf("EMCdsm2Tree(%s) , year=%d compute-EMPTY \n",name,mYear);  
  //printf("\n\n----------- level-2 -----------------\n ");


  intEndcapSum = getInpEsum5bit(0,0) + getInpEsum5bit(0,1);
  if ( intEndcapSum > EEsumthr6bit ) OutEndcapSum1bit = 1;
  else OutEndcapSum1bit = 0;

  int i;
  for (i=1; i<Nbe2; i++){
    intBarreSum = intBarreSum + getInpEsum5bit(i,0) + getInpEsum5bit(i,1);
  }
  if ( intBarreSum > BEsumthr8bit ) OutBarreSum1bit = 1;
  else OutBarreSum1bit = 0;

  intEtot = intEndcapSum + intBarreSum;
  if ( intEtot > EtotThr8bit ) OutEtot1bit = 1;
  else OutEtot1bit = 0;

  int j,k;
  for ( j=0; j<Nbe2; j++){
    for( k=0; k<Nbe2Cha; k++){
      if ( j==0 ){ 
	if ( OutEndcapJP2bit <= getInpJP2bit(j,k) ) OutEndcapJP2bit = getInpJP2bit(j,k) ;
	if ( OutEndcapHT2bit <= getInpHT2bit(j,k) ) OutEndcapHT2bit = getInpHT2bit(j,k);
	if ( OutEndcapHTTP1bit <= getInpHTTP1bit(j,k) ) OutEndcapHTTP1bit = getInpHTTP1bit(j,k);
	if ( OutEndcapTP1bit <= getInpTP1bit(j,k) ) OutEndcapTP1bit = getInpTP1bit(j,k);
      }
      if( j>0 ){

	if ( OutBarreJP2bit <= getInpJP2bit(j,k) ) OutBarreJP2bit = getInpJP2bit(j,k) ;
	if ( OutBarreHT2bit <= getInpHT2bit(j,k) ) OutBarreHT2bit = getInpHT2bit(j,k);
	if ( OutBarreHT2bit <= getInpHT2bit_2(j,k) ) OutBarreHT2bit = getInpHT2bit_2(j,k);
	if ( OutBarreHTTP1bit <= getInpHTTP1bit(j,k) ) OutBarreHTTP1bit = getInpHTTP1bit(j,k);
	if ( OutBarreTP1bit <= getInpTP1bit(j,k) ) OutBarreTP1bit = getInpTP1bit(j,k); 

      }
    }
  }

  int BarreEast_HT_JPSI[6]={0,0,0,0,0,0};
  int BarreWest_HT_JPSI[6]={0,0,0,0,0,0};  

  BarreEast_HT_JPSI[0]=getInpHT2bit(1,1);
  BarreEast_HT_JPSI[1]=getInpHT2bit_2(1,1);
  BarreEast_HT_JPSI[2]=getInpHT2bit(2,0);
  BarreEast_HT_JPSI[3]=getInpHT2bit_2(2,0);
  BarreEast_HT_JPSI[4]=getInpHT2bit(1,0);
  BarreEast_HT_JPSI[5]=getInpHT2bit_2(1,0);

  BarreWest_HT_JPSI[0]=getInpHT2bit(3,1);
  BarreWest_HT_JPSI[1]=getInpHT2bit_2(3,1);  
  BarreWest_HT_JPSI[2]=getInpHT2bit(2,1); 
  BarreWest_HT_JPSI[3]=getInpHT2bit_2(2,1);  
  BarreWest_HT_JPSI[4]=getInpHT2bit(3,0);  
  BarreWest_HT_JPSI[5]=getInpHT2bit_2(3,0);

  int l;
  for ( l=0; l<6; l++){
    if ( BarreEast_HT_JPSI[l] > JPSIthrSelc2bit ) BarreEast_HT_JPSI[l]=1;
    else BarreEast_HT_JPSI[l]=0;

    if ( BarreWest_HT_JPSI[l] > JPSIthrSelc2bit ) BarreWest_HT_JPSI[l]=1;
    else BarreWest_HT_JPSI[l]=0;
  }
  
  if ( BarreEast_HT_JPSI[0] && ( BarreEast_HT_JPSI[2] || BarreEast_HT_JPSI[3] ||BarreEast_HT_JPSI[4] )) OutBarreJPSi1bit=1;

  if ( BarreEast_HT_JPSI[1] && ( BarreEast_HT_JPSI[3] || BarreEast_HT_JPSI[4] ||BarreEast_HT_JPSI[5] )) OutBarreJPSi1bit=1;

  if ( BarreEast_HT_JPSI[2] && ( BarreEast_HT_JPSI[4] || BarreEast_HT_JPSI[5] )) OutBarreJPSi1bit=1;
  
  if ( BarreEast_HT_JPSI[3] && BarreEast_HT_JPSI[5] ) OutBarreJPSi1bit=1;

  if ( BarreWest_HT_JPSI[0] && ( BarreWest_HT_JPSI[2] || BarreWest_HT_JPSI[3] ||BarreWest_HT_JPSI[4] )) OutBarreJPSi1bit=1;

  if ( BarreWest_HT_JPSI[1] && ( BarreWest_HT_JPSI[3] || BarreWest_HT_JPSI[4] ||BarreWest_HT_JPSI[5] )) OutBarreJPSi1bit=1;

  if ( BarreWest_HT_JPSI[2] && ( BarreWest_HT_JPSI[4] || BarreWest_HT_JPSI[5] )) OutBarreJPSi1bit=1;

  if ( BarreWest_HT_JPSI[3] && BarreWest_HT_JPSI[5] ) OutBarreJPSi1bit=1;
}

//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpHT2bit(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpHT2bit(ch); 
}

//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpHT2bit_2(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpHT2bit_2(ch); 
}

//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpJP2bit(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpJP2bit(ch); 
}


//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpTP1bit(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpTP1bit(ch); 
}

//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpHTTP1bit(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpHTTP1bit(ch); 
}

//--------------------------------------------------
//--------------------------------------------------
int
EMCdsm2Tree::getInpEsum5bit(int ibr, int ch) const {
   assert(ibr>=0 && ibr<Nbe2);
   return  be2[ibr].getInpEsum5bit(ch); 
}



//--------------------------------------------------
//--------------------------------------------------
void 
EMCdsm2Tree::print( int k) const {
  printf("EMCdsm2Tree(%s) , year=%d  \n",name,mYear);
  
  printf("\n\n----------- level-2 -----------------\n ");
  int i;
  for (i=0;i<Nbe2;i++) {
    if(i==0)  printf("\nEndcap board=1 ");
    else  printf("\nBarrel board=%d ",i);
    be2[i].print();
    if(k==0) break;// reduce printing (k=0 :endcap only)
  }
}




/*
 * $Log: EMCdsm2Tree.cxx,v $
 * Revision 1.3  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.2  2009/10/13 17:04:03  pibero
 * Changed location of #include file
 *
 * Revision 1.1  2009/10/12 18:04:27  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:19  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:38  balewski
 * full blown Endcap trigger simu, by Xin
 *
 */

