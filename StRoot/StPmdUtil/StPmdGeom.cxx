/********************************************************
 *
 * $Id: StPmdGeom.cxx,v 1.3 2003/09/02 17:58:49 perev Exp $
 *
 * Author: Dipak Mishra
 *
 *********************************************************
 *
 * Description: This the class of PMD geometry for calculating
 * various utility functions from geometry parameters
 *
 *********************************************************
 * $Log: StPmdGeom.cxx,v $
 * Revision 1.3  2003/09/02 17:58:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 * Revision 1.2  2003/05/11 10:21:05  Dipak
 * Mapping of chain # and channel # to supmod,row,col
 **********************************************************/
 
#include "StPmdGeom.h"
#include <strings.h>
#include <stdlib.h>
#include <TROOT.h>
#include<TMatrix.h>
#include<TArrayF.h>
ClassImp(StPmdGeom)

  /*! mxcon[17],mycon[17] are the x and y position of the corner cell 
   *  of the supermodule which are output from GEANT. */
  Float_t StPmdGeom::mxcon[17]={22.63367,66.84988,112.60741,66.51461,112.3336,
				-23.71335,-1.75714,-1.80849,-69.33651,-69.33518,
				3.60214,-45.43217,-67.35937,-112.86835,3.60151,
				3.60214,-42.7025};

  Float_t StPmdGeom::mycon[17]={13.95724,-36.74996,-63.1415,41.57234,15.11893,
				12.62782,76.01182,128.98824,36.68728,89.59438,
				-27.89206,-27.81838,-40.5852,-67.40152,-53.35941, 
				-78.78845,-105.38004};
 /*! mdetxcon[12],mdetycon[12] are the x and y position of the corner cell 
  * of the supermodule after conversion from 17 to 12 */

  Float_t StPmdGeom::mdetxcon[12]={-69.33518,-1.80849,-69.33651,-1.75714,
				 -112.86835,-67.35937,-42.7025,66.51461,
				 112.3336,66.84988,112.60741,-40.31029};

  Float_t StPmdGeom::mdetycon[12]={89.59438,128.98824,36.68728,76.01182,
				 -67.40152,-40.5852,-105.38004,41.57234,
				 15.11893,-36.74996,-63.1415,-52.64566};

/*! inorm[192], jnorm[192], imirr[192], jmirr[192] are the row and col
  number of the electronic channels */
Int_t StPmdGeom::inorm[192] = {23,24,24,24,24,23,22,23,23,22,21,22,22,20,21,
			       21,21,19,18,20,19,18,20,20,17,17,19,18,17,17,
			       19,18,18,17,17,17,17,18,19,18,18,19,20,19,19,
			       21,20,20,20,22,23,21,22,23,21,21,24,24,22,23,
			       24,24,22,23,15,16,16,16,16,15,14,15,15,14,13,
			       14,14,12,13,13,13,11,10,12,11,10,12,12,9,9,11,
			       10,9,9,11,10,10,9,9,9,9,10,11,10,10,11,12,11,
			       11,13,12,12,12,14,15,13,14,15,13,13,16,16,14,
			       15,16,16,14,15,7,8,8,8,8,7,6,7,7,6,5,6,6,4,5,5,
			       5,3,2,4,3,2,4,4,1,1,3,2,1,1,3,2,2,1,1,1,1,2,3,
			       2,2,3,4,3,3,5,4,4,4,6,7,5,6,7,5,5,8,8,6,7,8,8,
			       6,7};

Int_t StPmdGeom::jnorm[192] = {4,4,3,2,1,3,4,2,1,1,1,2,3,1,4,2,3,1,1,2,2,2,4,
			       3,1,2,3,3,3,4,4,4,5,5,6,7,8,6,5,7,8,8,8,7,6,8,
			       5,7,6,8,8,7,7,7,5,6,8,7,6,6,6,5,5,5,4,4,3,2,1,
			       3,4,2,1,1,1,2,3,1,4,2,3,1,1,2,2,2,4,3,1,2,3,3,
			       3,4,4,4,5,5,6,7,8,6,5,7,8,8,8,7,6,8,5,7,6,8,8,
			       7,7,7,5,6,8,7,6,6,6,5,5,5,4,4,3,2,1,3,4,2,1,1,
			       1,2,3,1,4,2,3,1,1,2,2,2,4,3,1,2,3,3,3,4,4,4,5,
			       5,6,7,8,6,5,7,8,8,8,7,6,8,5,7,6,8,8,7,7,7,5,6,
			       8,7,6,6,6,5,5,5};

Int_t StPmdGeom::imirr[192] = {4,4,4,3,3,3,2,1,3,4,2,2,2,1,1,3,2,4,1,3,2,1,1,
			       1,2,4,3,1,2,3,4,4,5,5,5,6,6,6,7,8,6,5,7,7,7,8,
			       8,6,7,5,8,6,7,8,8,8,7,5,6,8,7,6,5,5,4,4,4,3,3,
			       3,2,1,3,4,2,2,2,1,1,3,2,4,1,3,2,1,1,1,2,4,3,1,
			       2,3,4,4,5,5,5,6,6,6,7,8,6,5,7,7,7,8,8,6,7,5,8,
			       6,7,8,8,8,7,5,6,8,7,6,5,5,4,4,4,3,3,3,2,1,3,4,
			       2,2,2,1,1,3,2,4,1,3,2,1,1,1,2,4,3,1,2,3,4,4,5,
			       5,5,6,6,6,7,8,6,5,7,7,7,8,8,6,7,5,8,6,7,8,8,8,
			       7,5,6,8,7,6,5,5};

Int_t StPmdGeom::jmirr[192] = {2,3,1,1,2,3,1,1,4,4,2,3,4,2,3,5,5,5,4,6,6,5,6,
			       7,7,6,7,8,8,8,8,7,7,6,8,8,7,6,8,8,5,5,7,6,5,7,
			       6,4,4,4,5,3,3,4,3,2,2,3,2,1,1,1,1,2,10,11,9,9,
			       10,11,9,9,12,12,10,11,12,10,11,13,13,13,12,14,
			       14,13,14,15,15,14,15,16,16,16,16,15,15,14,16,16,
			       15,14,16,16,13,13,15,14,13,15,14,12,12,12,13,
			       11,11,12,11,10,10,11,10,9,9,9,9,10,18,19,17,17,
			       18,19,17,17,20,20,18,19,20,18,19,21,21,21,20,
			       22,22,21,22,23,23,22,23,24,24,24,24,23,23,22,
			       24,24,23,22,24,24,21,21,23,22,21,23,22,20,20,
			       20,21,19,19,20,19,18,18,19,18,17,17,17,17,18};




StPmdGeom::StPmdGeom()         //! A constructor
{
  commonconstants();
}

StPmdGeom::~StPmdGeom(){/*none*/}  //! A destructor

//! conversion from sector,supermodule inside the sector to global supermodule 17
Int_t StPmdGeom::NModule(Int_t sector ,  Int_t super, Int_t& nmod)
{
  nmod=(sector-1)*5 + super;
  return nmod;
}
//! function for converting supermodule,row,col to x,y position,eta and phi values of the cell
void StPmdGeom::Cell_xy(Int_t nmod,Int_t row, Int_t col,Float_t& xreal, Float_t& yreal,Float_t& eta,Float_t& phi)
{
  if(nmod<=5)                    //! for first sector
    {
      xreal = mxcon[nmod-1] - (row-1)*mcelldia_y;
      yreal = mycon[nmod-1] + (col-1)*mcelldia_x + (row-1)*mcell_rad;
    }
  if (nmod>5 && nmod<=10)          //! for second sector 
    {
      xreal = mxcon[nmod-1] - (col - 1) * mconst2;
      yreal = mycon[nmod-1] - (col - 1) * mconst1 - (row - 1) * mcelldia_x;
    }
  if (nmod>10 && nmod<=17)          //! for third sector
    {
      xreal = mxcon[nmod-1] + (col - 1)*mconst2 + (row - 1)*mconst2;
      yreal = mycon[nmod-1] + (row - col ) * mconst1;
    }
  Cell_eta_phi(xreal,yreal,eta,phi);   
}

//! function for converting supermodule,row,col to x,y,eta,phi after conversionfrom 17 to 12 supermodule used in clustering
void StPmdGeom::DetCell_xy(Int_t nmod,Float_t row, Float_t col,Float_t& xreal, Float_t& yreal,Float_t& eta,Float_t& phi)
{
  if(nmod<=4)
    {
      xreal = mdetxcon[nmod-1] - (col - 1) * mconst2;
      yreal = mdetycon[nmod-1] - (col - 1) * mconst1 - (row - 1) * mcelldia_x;

    }
  if (nmod>4 && nmod<=7)
    {
      xreal = mdetxcon[nmod-1] + (col - 1)*mconst2 + (row - 1)*mconst2;
      yreal = mdetycon[nmod-1] + (row - col ) * mconst1;
    }

  if (nmod>7 && nmod<=11)
    {
      xreal = mdetxcon[nmod-1] - (row-1)*mcelldia_y;
      yreal = mdetycon[nmod-1] + (col-1)*mcelldia_x + (row-1)*mcell_rad;

    }
  if (nmod==12){
      xreal = mdetxcon[nmod-1] + (col - 1)*mconst2 + (row - 1)*mconst2;
      yreal = mdetycon[nmod-1] + (row - col ) * mconst1;
  }
  Cell_eta_phi(xreal,yreal,eta,phi);
}
//! function for converting supermodule,row,col to x,y,eta,phi after conversionfrom 17 to 12 supermodule 
void StPmdGeom::IntDetCell_xy(Int_t nmod,Int_t row, Int_t col,Float_t& xreal, Float_t& yreal,Float_t& eta,Float_t& phi)
{
  if(nmod<=4)
    {
      xreal = mdetxcon[nmod-1] - (col - 1) * mconst2;
      yreal = mdetycon[nmod-1] - (col - 1) * mconst1 - (row - 1) * mcelldia_x;

    }
  if (nmod>4 && nmod<=7)
    {
      xreal = mdetxcon[nmod-1] + (col - 1)*mconst2 + (row - 1)*mconst2;
      yreal = mdetycon[nmod-1] + (row - col ) * mconst1;
    }

  if (nmod>7 && nmod<=11)
    {
      xreal = mdetxcon[nmod-1] - (row-1)*mcelldia_y;
      yreal = mdetycon[nmod-1] + (col-1)*mcelldia_x + (row-1)*mcell_rad;

    }
  if (nmod==12){
      xreal = mdetxcon[nmod-1] + (col - 1)*mconst2 + (row - 1)*mconst2;
      yreal = mdetycon[nmod-1] + (row - col ) * mconst1;
  }
  Cell_eta_phi(xreal,yreal,eta,phi);
}

//! function for calculating eta,phi from x, y

void StPmdGeom::Cell_eta_phi(Float_t xreal,Float_t yreal,Float_t& eta,Float_t& phi){
  Float_t rdist = (::sqrt(xreal*xreal + yreal*yreal))/mzreal;
  Float_t theta = atan(rdist);
  eta = ::log(tan(0.5*theta));
  if( xreal==0) {
    if(yreal>0) phi = 1.571428;
    if(yreal<0) phi = -1.571428;
  }
  if(xreal != 0) phi = atan2(yreal,xreal);

}
//! function for convering supermodule,row,col (from GEANT) to supermodule,row,col as in hardware. 

  void StPmdGeom::Sim2Detmap(Int_t& nmod,Int_t& row,Int_t& col)
    {
      Int_t module;
      module = nmod;
      switch(module){
      case 1:
	nmod = 10;
	row = row + 48;
	col = col +24;
	break;
      case 2:
	nmod = 10;
	row = row;
	col = col;
	break;
      case 3:
	nmod = 11;
	row = row;
	col = col;
	break;
      case 4:
	nmod = 8;
	row = row;
	col = col;
	break;
      case 5:
	nmod = 9;
	row = row;
	col = col;
	break;
      case 6:
	nmod = 4;
	row = row + 48;
	col = col + 24;
	break;
      case 7:
	nmod = 4;
	row = row;
	col = col;
	break;
      case 8:
	nmod = 2;
	row = row;
	col = col;
	break;
      case 9:

	nmod = 3;
	row = row;
	col = col;
	break;
      case 10:
	nmod = 1;
	row = row;
	col = col;
	break;
      case 11:
	nmod = 12;
	row = row + 48;
	col = col;
	break;
      case 12:
	nmod = 6;
	row = row + 24;
	col = col;
	break;
      case 13:
	nmod = 6;
	row = row;
	col = col;
	break;      
      case 14:
	nmod = 5;
	row = row;
	col = col;
	break;
      case 15:
	nmod = 12;
	row = row + 24;
	col = col + 24;
	break;
      case 16:
	nmod = 12;
	row = row;
	col = col + 48;
	break;
      case 17:
	nmod = 7;
	row = row;
	col = col;
	break;
      }
    }

void StPmdGeom::ChainMapping(Int_t& chainno,Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  Int_t chain;
  Int_t col1,row1;
  chain = chainno;
  switch(chain){

  case 1:

      chain1(ch,supmod,col,row);
    break;
  case 2:
      chain2(ch,supmod,col,row);
    break;
  case 3:
      chain3(ch,supmod,col,row);
    break;
  case 4:
      chain2(ch,supmod,col,row);
      col = col; row = row + 24;
    break;
  case 5:
      chain5(ch,supmod,col,row);
    break;
  case 6:
      chain2(ch,supmod,col,row);
      supmod = 4;
    break;
  case 7:
      chain2(ch,supmod,col,row);
      col = col; row = row + 24; supmod = 4;
    break;
  case 8:
      chain2(ch,supmod,col,row);
      col = 73 - col; row = 25 - row; supmod = 5;
    break;
  case 9:
      chain9(ch,supmod,col,row);
    break;
  case 10:
      chain10(ch,supmod,col,row);
    break;
  case 11:
      chain10(ch,supmod,col,row);
      if(supmod==4)col = col - 24;
      if(supmod==6)row = row + 24;
    break;
  case 12:
      chain12(ch,supmod,col,row);
    break;
  case 13:
      chain12(ch,supmod,col,row);
      if(supmod==5) {row = row -24; supmod = 6;} 
      if(supmod==7) row = row + 24;	
    break;
  case 14:
      chain5(ch,supmod,col,row);
      supmod = 8;
    break;
  case 15:
      chain15(ch,supmod,col,row);
    break;
  case 16:
      chain5(ch,supmod,col,row);
      supmod = 8; col = col -24;
    break;
  case 17:
      chain17(ch,supmod,col,row);
    break;
  case 18:
      chain5(ch,supmod,col,row);
      supmod = 10; col = col + 24;
    break;
  case 19:
      chain2(ch,supmod,col,row);
      supmod = 11;
    break;
  case 20:
      chain5(ch,supmod,col,row);
      supmod = 10;
    break;
  case 21:
      chain21(ch,supmod,col,row);
    break;
  case 22:
      chain22(ch,supmod,col,row);
    break;
  case 23:
      chain23(ch,supmod,col,row);
    break;
  case 24:
      chain5(ch,supmod,col,row);
      supmod = 12; col = 121 - col; row = 73 - row;
    break;
  case 25:
      chain15(ch,supmod, col1, row1);
      supmod = 13; col = 49 - row1; row = 49 - col1;
    break;
  case 26:
      chain5(ch,supmod,col1,row1);
      supmod = 14; col = 73 - row1; row = 49 - col1;
    break;
  case 27:
      chain17(ch,supmod,col1,row1);
      supmod = supmod + 4; col = 49 - row1; row = 49 - col1;
      if(supmod==15) row = 73 - col1;
    break;
  case 28: 
      chain5(ch,supmod,col1,row1);
      supmod = 14; col = 73 - row1; row = 49 - col1 +24;
    break;
  case 29: 
      chain2(ch,supmod,col1,row1);
      supmod = 15; col = 49 - row1; row = 73 - col1;
    break;
  case 30: 
      chain5(ch,supmod,col1,row1);
      supmod = 16; col = 73 - row1; row = 49 - col1;
    break;
  case 31: 
      chain5(ch,supmod,col1,row1);
      supmod = 16; col = 73 - row1; row = 49 - col1 +24;
    break;
  case 32: 
      chain5(ch,supmod,col1,row1);
      supmod = 17; col = row1; row = col1 -24; 
    break;
  case 33: 
      chain22(ch,supmod,col1,row1);
      if(supmod==11){supmod = 15;
      col = 49 - row1; row = 73 - col1;}
      if(supmod==12){supmod = 17; col = 73 - row1;
      row = 97 - col1;}
    break;
  case 34: 
      chain34(ch,supmod,col,row);
    break;
  case 35: 
      chain34(ch,supmod,col,row);
      if(supmod == 16) col = col - 24;
      if(supmod == 18) row = row + 24;
    break;
  case 36: 
      chain34(ch,supmod,col,row);
      if(supmod == 16) {col = 121-col;row = 97 - row;}
      supmod = supmod + 1;
    break;
  case 37: 
      chain34(ch,supmod,col,row);
      if(supmod==18){row = row + 24;supmod = 19;}
      if(supmod == 16){
	col = 121-col; row = 73 - row; 
	supmod = 18;}
    break;
  case 38:
      chain2(ch,supmod,col1,row1);
      supmod = 20; col = 49 - row1; row = 73 - col1;
    break;
  case 39:
      chain1(ch,supmod,col1,row1);
      supmod = 21; col = 49 - row1; row = 49 - col1;
    break;
  case 40:
      chain2(ch,supmod,col1,row1);
      supmod = 20; col = 25 - row1; row = 73 - col1;
    break;
  case 41:
      chain3(ch,supmod,col1,row1);
      supmod = supmod +20;
      if(supmod==21){col = 49 - row1; row = 49 - col1;}
      if(supmod==23){col = 73 - row1; row = 49 - col1;}
    break;
  case 42:
      chain2(ch,supmod,col1,row1);
      supmod = 22; col = 73 - row1; row = 73 - col1;
    break;
  case 43:
      chain5(ch,supmod,col1,row1);
      supmod = 23; col = 73 - row1; row = 49 - col1;
    break;
  case 44:
      chain2(ch,supmod,col1,row1);
      supmod = 22; col = 49 - row1; row = 73 - col1;
    break;
  case 45:
      chain45(ch,supmod,col,row);
    break;
  case 46:
      chain9(ch,supmod,col1,row1);
      if(supmod==3){
	col = 73 - row1; row = 49 - col1;
	supmod = 23;}
      if(supmod==5){
	col =97 - row1; row = 73 - col1;
	supmod = 24;}
    break;
  case 47:
      chain45(ch,supmod,col,row);
      if(supmod ==22){col = 49 - col; row = 73 -row;}
      if(supmod==24){col = 48 + col; row = row - 48;}
      supmod = 24;
    break;
  case 48:
      chain2(ch,supmod,col1,row1);
      supmod = 24; col = 72 + row1; row = col1;
      break;
  }
}


void StPmdGeom::chain1(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 1;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = inorm[ch] + 24; row = jnorm[ch];
    break;
  case 1:
    col = inorm[ch-192]; row = jnorm[ch-192];
    break;
  case 2:
    col = 25 - inorm[ch-2*192]; row = 9 - jnorm[ch-2*192] + 8;
    break;
  case 3:
    col = 25 - inorm[ch-3*192] + 24; row = 9 - jnorm[ch-3*192] + 8;
    break;
  case 4:
    col = inorm[ch-4*192] + 24; row = jnorm[ch - 4*192] + 16;
    break;
  case 5:
    col = inorm[ch-5*192]; row = jnorm[ch-5*192] + 16;
    break;
  case 6:
    col = inorm[ch-6*192]; row = jnorm[ch-6*192] + 24;
    break;
  case 7:
    col =  25 - inorm[ch-7*192]; row = 9 - jnorm[ch-7*192] + 24 + 8;
    break;
  case 8:
    col = inorm[ch-8*192]; row = jnorm[ch-8*192] + 24 + 16;
    break;
  }
}


void StPmdGeom::chain2(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 2;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = inorm[ch] + 48; row = jnorm[ch];
    break;
  case 1:
    col = inorm[ch-192] + 24; row = jnorm[ch-192];
    break;
  case 2:
    col = inorm[ch-2*192]; row = jnorm[ch-2*192];
    break;
  case 3:
    col = 25 - inorm[ch-3*192]; row = 9 - jnorm[ch-3*192] + 8;
    break;
  case 4:
    col = 25 - inorm[ch-4*192] + 24; row = 9 - jnorm[ch-4*192] + 8;
    break;
  case 5:
    col = 25 - inorm[ch-5*192] + 48; row = 9 - jnorm[ch-5*192] + 8;
    break;
  case 6:
    col = inorm[ch-6*192] + 48; row = jnorm[ch-6*192] + 16;
    break;
  case 7:
    col = inorm[ch-7*192] + 24; row = jnorm[ch-7*192] + 16;
    break;
  case 8:
    col = inorm[ch-8*192]; row = jnorm[ch-8*192]  + 16;
    break;
  }
}

void StPmdGeom::chain3(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 1;
  int zone = ch/192;
  switch(zone){
  case 0:
    col =  inorm[ch] + 24; row = jnorm[ch] + 24;
    break;
  case 1:
    col =  25 - inorm[ch-192] + 24; row = 9 - jnorm[ch-192] + 24 + 8;
    break;
  case 2:
    col = inorm[ch-2*192] + 24; row = jnorm[ch-2*192] + 24 + 16;
    break;
  case 3:
    col = imirr[ch-3*192] + 16; row = jmirr[ch-3*192];
    supmod = 3;
    break;
  case 4:
    col = imirr[ch-4*192] + 16; row = jmirr[ch-4*192] + 24;
    supmod = 3;
    break;
  case 5:
    col = 9 - imirr[ch-5*192] + 8; row = 25 - jmirr[ch-5*192] + 24;
    supmod = 3;
    break;
  case 6:
    col = 9 - imirr[ch-6*192] + 8; row = 25 - jmirr[ch-6*192];
    supmod = 3;
    break;
  case 7:
    col = imirr[ch-7*192]; row = jmirr[ch-7*192];
    supmod = 3;
    break;
  case 8:
    col = imirr[ch-8*192] ; row = jmirr[ch-8*192] + 24;
    supmod = 3;
    break;
  }
}

void StPmdGeom::chain5(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 3;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = imirr[ch] + 24 + 16; row = jmirr[ch];
    break;
  case 1:
    col = imirr[ch-192] + 24 +16; row = jmirr[ch-192] + 24;
    break;
  case 2:
    col = imirr[ch-2*192] + 24 + 16; row = jmirr[ch-2*192] + 48;
    break;
  case 3:
    col = 9 - imirr[ch-3*192] + 24 + 8; row = 25 - jmirr[ch-3*192] +48;
    break;
  case 4:
    col = 9 - imirr[ch-4*192] + 24 +8; row = 25 -jmirr[ch-4*192] + 24;
    break;
  case 5:
    col = 9 - imirr[ch-5*192] + 24 +8; row = 25 -jmirr[ch-5*192];
    break;
  case 6:
    col = imirr[ch-6*192] + 24 ; row = jmirr[ch-6*192];
    break;
  case 7:
    col = imirr[ch-7*192] + 24; row = jmirr[ch-7*192] + 24;
    break;
  case 8:
    col = imirr[ch-8*192] + 24; row = jmirr[ch-8*192] + 48;
    break;
  }
}

void StPmdGeom::chain9(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 3;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = imirr[ch] + 16; row = jmirr[ch] + 48;
    break;
  case 1:
    col = 9 - imirr[ch-192] + 8; row = 25 - jmirr[ch-192] + 48;
    break;
  case 2:
    col = imirr[ch-2*192]; row = jmirr[ch-2*192] + 48;
    break;
  case 3:
    col = 25 - inorm[ch-3*192]; row = 9-jnorm[ch-3*192] +24 + 16;
    supmod = 5; 
    break;
  case 4:
    col = 25 - inorm[ch-4*192] + 24; row = 9-jnorm[ch-4*192] + 24 + 16;
    supmod = 5;
    break;
  case 5:
    col = inorm[ch-5*192] + 24; row = jnorm[ch-5*192] + 24 + 8;
    supmod = 5;
    break;
  case 6:
    col = inorm[ch-6*192]; row = jnorm[ch-6*192]+ 24 +8;
    supmod = 5;
    break;
  case 7:
    col = 25 - inorm[ch-7*192]; row = 9-jnorm[ch-7*192] + 24;
    supmod = 5;
    break;
  case 8:
    col = 25 - inorm[ch-8*192] + 24; row = 9-jnorm[ch-8*192] + 24;
    supmod = 5;
    break;
  }
}
void StPmdGeom::chain10(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 4;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = inorm[ch] + 48; row = jnorm[ch] + 48;
    break;
  case 1:
    col = 25 - inorm[ch-192] + 48; row = 9 - jnorm[ch-192] +48 + 8;
    break;
  case 2:
    col = inorm[ch-2*192] + 48; row = jnorm[ch-2*192] + 48 + 16;
    break;
  case 3:
    col = 25 - inorm[ch-3*192]; row = 9 -jnorm[ch-3*192] + 16;
    supmod = 6;
    break;
  case 4:
    col = 25 - inorm[ch-4*192] + 24; row = 9 -jnorm[ch-4*192] + 16;
    supmod = 6;
    break;
  case 5:
    col = inorm[ch-5*192] + 24; row = jnorm[ch-5*192] + 8;
    supmod = 6;
    break;
  case 6:
    col = inorm[ch-6*192]; row = jnorm[ch-6*192] +8;
    supmod = 6;
    break;
  case 7:
    col = 25 - inorm[ch-7*192]; row = 9 -jnorm[ch-7*192];
    supmod = 6;
    break;
  case 8:
    col = 25 - inorm[ch-8*192]+ 24; row = 9-jnorm[ch-8*192];
    supmod = 6;
    break;
  }
}
void StPmdGeom::chain12(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 5;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = 25 - inorm[ch]+ 48; row = 9-jnorm[ch] +24 + 16;
    break;
  case 1:
    col = inorm[ch-192] + 48; row = jnorm[ch-192] + 24 + 8;
    break;
  case 2:
    col = 25 - inorm[ch-2*192]+ 48; row = 9-jnorm[ch-2*192] +24;
    break;
  case 3:
    col = 25 - inorm[ch-3*192]; row = 9-jnorm[ch-3*192] + 16;
    supmod = 7;
    break;
  case 4:
    col = 25 - inorm[ch-4*192]+24; row = 9-jnorm[ch-4*192] + 16;
    supmod = 7;
    break;
  case 5:
    col = inorm[ch-5*192] + 24; row = jnorm[ch-5*192] + 8;
    supmod = 7;
    break;
  case 6:
    col = inorm[ch-6*192]; row = jnorm[ch-6*192] + 8;
    supmod = 7;
    break;
  case 7:
    col = 25 - inorm[ch-7*192]; row = 9-jnorm[ch-7*192];
    supmod = 7;
    break;
  case 8:
    col = 25 - inorm[ch-8*192]+24; row = 9-jnorm[ch-8*192];
    supmod = 7;
    break;
  }
}

void StPmdGeom::chain15(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 9;
  Int_t zone = ch/192;
  switch(zone){
  case 0:
    col = imirr[ch] + 24 + 16; row = jmirr[ch];
    break;
  case 1:
    col = imirr[ch-192] + 24 +16; row = jmirr[ch-192] + 24;
    break;
  case 2:
    col = 9 - imirr[ch-2*192] + 24 + 8; row = 25 - jmirr[ch-2*192] +24;
    break;
  case 3:
    col = 9 - imirr[ch-3*192] + 24 + 8; row = 25 - jmirr[ch-3*192];
    break;
  case 4:
    col = imirr[ch-4*192] + 24; row = jmirr[ch-4*192];
    break;
  case 5:
    col = imirr[ch-5*192] + 24; row = jmirr[ch-5*192] + 24;
    break;
  case 6:
    col = imirr[ch-6*192] + 16; row = jmirr[ch-6*192] + 24;
    break;
  case 7:
    col = 9 - imirr[ch-7*192] + 8; row = 25 - jmirr[ch-7*192] +24;
    break;
  case 8:
    col = imirr[ch-8*192]; row = jmirr[ch-8*192] + 24;
    break;
  }
}
void StPmdGeom:: chain17(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 9;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = imirr[ch] + 16; row = jmirr[ch];
    break;
  case 1:
    col = 9 - imirr[ch-192] + 8; row = 25 - jmirr[ch-192];
    break;
  case 2:
    col = imirr[ch-2*192]; row = jmirr[ch-2*192];
    break;
  case 3:
    col = inorm[ch-3*192] + 48; row = jnorm[ch-3*192] + 24;
    supmod = 11;
    break;
  case 4:
    col = inorm[ch-4*192] + 24; row = jnorm[ch-4*192] + 24;
    supmod = 11;
    break;
  case 5:
    col = 25 - inorm[ch-5*192] + 24; row = 9 - jnorm[ch-5*192] +24 + 8;
    supmod = 11;
    break;
  case 6:
    col = 25 - inorm[ch-6*192] + 48; row = 9 - jnorm[ch-6*192] +24 + 8;
    supmod = 11;
    break;
  case 7:
    col = inorm[ch-7*192] + 48; row = jnorm[ch-7*192] + 24 + 16;
    supmod = 11;
    break;
  case 8:
    col = inorm[ch-8*192] + 24; row = jnorm[ch-8*192] + 24 + 16;
    supmod = 11;
    break;
  }
}


void StPmdGeom::chain21(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 10;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = imirr[ch] + 16; row = jmirr[ch];
    break;
  case 1:
    col = imirr[ch-192] +16; row = jmirr[ch-192] + 24;
    break;
  case 2:
    col = 9 - imirr[ch-2*192] + 8; row = 25 - jmirr[ch-2*192] +24;
    break;
  case 3:
    col = 9 - imirr[ch-3*192] + 8; row = 25 - jmirr[ch-3*192];
    break;
  case 4:
    col = imirr[ch-4*192] ; row = jmirr[ch-4*192];
    break;
  case 5:
    col = imirr[ch-5*192]; row = jmirr[ch-5*192] + 24;
    break;
  case 6:
    col = 9 - imirr[ch-6*192]; row = 25 -jmirr[ch-6*192] + 48;
    supmod = 12;
    break;
  case 7:
    col = imirr[ch-7*192] + 8; row = jmirr[ch-7*192] + 48;
    supmod = 12;
    break;
  case 8:
    col = 9 - imirr[ch-8*192]+ 16; row = 25 -jmirr[ch-8*192] + 48;
    supmod = 12;
    break;
  }
}
void StPmdGeom::chain22(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 11;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = inorm[ch]; row = jnorm[ch] + 24;
    break;
  case 1:
    col = 25 - inorm[ch-192]; row = 9 - jnorm[ch-192] +24 + 8;
    break;
  case 2:
    col = inorm[ch-2*192]; row = jnorm[ch-2*192] + 24 +16;
    break;
  case 3:
    col = 9 - imirr[ch-3*192]+48; row = 25 -jmirr[ch-3*192] + 48;
    supmod = 12;
    break;
  case 4:
    col = 9 - imirr[ch-4*192]+48; row = 25 -jmirr[ch-4*192] + 24;
    supmod = 12;
    break;
  case 5:
    col = imirr[ch-5*192] +48+ 8; row = jmirr[ch-5*192] + 24;
    supmod = 12;
    break;
  case 6:
    col = imirr[ch-6*192] + 48 + 8; row = jmirr[ch-6*192] + 48;
    supmod = 12;
    break;
  case 7:
    col = 9 - imirr[ch-7*192]+48 + 16; row = 25 -jmirr[ch-7*192] + 48;
    supmod = 12;
    break;
  case 8:
    col = 9 - imirr[ch-8*192]+48 + 16; row = 25 -jmirr[ch-8*192] + 24;
    supmod = 12;
    break;
  }
}
void StPmdGeom::chain23(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 12;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = 9 - imirr[ch]+ 24; row = 25 -jmirr[ch] + 48;
    break;
  case 1:
    col = 9 - imirr[ch-192]+ 24; row = 25 -jmirr[ch-192] + 24;
    break;
  case 2:
    col = imirr[ch-2*192] +24+ 8; row = jmirr[ch-2*192] + 24;
    break;
  case 3:
    col = imirr[ch-3*192] +24+ 8; row = jmirr[ch-3*192] + 48;
    break;
  case 4:
    col = 9 - imirr[ch-4*192]+ 24 +16; row = 25 -jmirr[ch-4*192] + 48;
    break;
  case 5:
    col = 9 - imirr[ch-5*192]+ 24 +16; row = 25 -jmirr[ch-5*192] + 24;
    break;
  case 6:
    col = 9 - imirr[ch-6*192]+ 48; row = 25 -jmirr[ch-6*192];
    break;
  case 7:
    col = imirr[ch-7*192] +48+ 8; row = jmirr[ch-7*192];
    break;
  case 8:
    col = 9 - imirr[ch-8*192]+ 48 +16; row = 25 -jmirr[ch-8*192];
    break;
  }
}
void StPmdGeom::chain34(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 16;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = 73 - jmirr[ch]; row = 9 - imirr[ch] +48;
    break;
  case 1:
    col = 48 + jmirr[ch-192]; row = 48 + imirr[ch-192] +8;
    break;
  case 2:
    col = 73 - jmirr[ch-2*192]; row = 9- imirr[ch-2*192] +48 +16;
    break;
  case 3:
    col = jmirr[ch-3*192]; row = imirr[ch-3*192] + 16;
    supmod = 18;
    break;
  case 4:
    col = jmirr[ch-4*192] + 24; row = imirr[ch-4*192] +16;
    supmod = 18;
    break;
  case 5:
    col = 49 - jmirr[ch-5*192]; row = 9 - imirr[ch-5*192] + 8;
    supmod = 18;
    break;
  case 6:
    col = 25 - jmirr[ch-6*192]; row = 9 - imirr[ch-6*192] + 8;
    supmod = 18;
    break;
  case 7:
    col = jmirr[ch-7*192]; row = imirr[ch-7*192];
    supmod = 18;
    break;
  case 8:
    col = jmirr[ch-8*192]+24; row = imirr[ch-8*192];
    supmod = 18;
    break;
  }
}
void StPmdGeom::chain45(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 22;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = 25 - jnorm[ch]; row = 25 - inorm[ch];
    break;
  case 1:
    col = 25 - jnorm[ch-192]; row = 25 - inorm[ch-192] + 24;
    break;
  case 2:
    col = jnorm[ch-2*192] +8;row = inorm[ch-2*192]+24;
    break;
  case 3:
    col = jnorm[ch-3*192] +8;row = inorm[ch-3*192];
    break;
  case 4:
    col = 9 - jnorm[ch-4*192] ; row = 25 - inorm[ch-4*192];
    break;
  case 5:
    col = 9 - jnorm[ch-5*192] ; row = 25 - inorm[ch-5*192]+ 24;
    break;
  case 6:
    col = jnorm[ch-6*192]; row = 48 + inorm[ch-6*192];
    supmod = 24;
    break;
  case 7:
    col = 9 - jnorm[ch-7*192] + 8; row = 73 - inorm[ch-7*192];
    supmod = 24;
    break;
  case 8:
    col = jnorm[ch-8*192] + 16; row = 48 + inorm[ch-8*192];
    supmod = 24;
    break;
  }
}



















