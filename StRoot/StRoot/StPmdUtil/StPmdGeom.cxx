/*******************************************************
 *
 * $Id: StPmdGeom.cxx,v 1.30 2011/05/04 12:27:52 rashmi Exp $
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
 * Revision 1.30  2011/05/04 12:27:52  rashmi
 * year==12 fix in chains
 *
 * Revision 1.29  2011/04/26 13:16:06  rashmi
 * year==12 inserted for taking new mapping
 *
 * Revision 1.28  2010/04/15 06:55:48  rashmi
 * functions to draw XY and eta/phi coverage & modifcations to mapping
 *
 * Revision 1.27  2010/02/28 02:56:04  rashmi
 * included year11 as year number and also change in mapping after day48
 *
 * Revision 1.26  2009/12/24 17:55:45  rashmi
 * year10 mappiing
 *
 * Revision 1.25  2007/12/21 09:48:01  rashmi
 * Changes in readBoardDetail for dAu run by Subhasis
 *
 * Revision 1.24  2007/11/02 11:04:39  rashmi
 *  removed some print statements
 *
 * Revision 1.23  2007/08/31 10:56:27  rashmi
 * removed a warning on chtemp ondate 31/08/07; removed some print statements
 *
 * Revision 1.22  2007/07/12 19:52:31  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.21  2007/04/28 17:56:38  perev
 * Redundant StChain.h removed
 *
 * Revision 1.20  2007/04/28 07:46:02  rashmi
 * mapping after access on date 26/04/07
 *
 * Revision 1.19  2007/04/18 04:54:44  rashmi
 * status after 11April07 access
 *
 * Revision 1.18  2007/04/17 11:19:47  rashmi
 * Chain19 mapping corrected, functions to return nboards in a chain/SMs added
 *
 * Revision 1.17  2007/03/29 04:54:52  rashmi
 * BoardDetail of access on 29/03/07 added
 *
 * Revision 1.16  2007/03/21 16:39:58  rashmi
 * StPmdGeom after new mapping (run7) and with DrawPmd function for viewing PMD Geometry
 *
 * Revision 1.15  2005/03/07 05:02:23  subhasis
 * printout commented
 *
 * Revision 1.14  2005/02/22 07:36:52  subhasis
 * big prinout on picking.. commented
 *
 * Revision 1.13  2005/01/27 13:09:50  subhasis
 * New map for 2005 data
 *
 * Revision 1.12  2005/01/08 16:47:02  subhasis
 * status problem fixed for chain#7 (Rashmi)
 *
 * Revision 1.10  2004/11/15 23:27:23  subhasis
 * if() removed in ctor to stop valgrind error
 *
 * Revision 1.9  2004/09/19 00:08:21  perev
 * Small Walgrind leak fixed
 *
 * Revision 1.8  2004/04/13 20:12:55  subhasis
 * Rashmi's fix of chain46 mapping
 *
 * Revision 1.7  2004/04/01 15:41:00  subhasis
 *  3rd (or 4th?) access board detail added by Dipak
 *
 * Revision 1.6  2004/03/23 08:49:27  subhasis
 * biardDetail put by had
 *
 * Mapping modified according to final mounting of FEE : Dipak
 *
 * Revision 1.4  2003/11/27 12:31:56  subhasis
 * ADC2EDEP added by Supriya
 *
 * Revision 1.3  2003/09/02 17:58:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 * Revision 1.2  2003/05/11 10:21:05  Dipak
 * Mapping of chain # and channel # to supmod,row,col
 **********************************************************/
 
#include "Stypes.h"
#include "StPmdGeom.h"
#include <strings.h>
#include <stdlib.h>
#include <math.h>
#include <TROOT.h>
#include<TMatrix.h>
#include<TH2F.h>
#include<TMarker.h>
#include<TLatex.h>
#include<TCanvas.h>
#include<TPolyLine.h>
#include<TArrayF.h>
#include "TMath.h"
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

//Int_t status[48][27]; 
Int_t status[48][36];

StPmdGeom::StPmdGeom()         //! A constructor
{
  //  cout<<" StPmdGeom Constructor"<<endl;
  commonconstants();
  readBoardDetail();
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
  //  cout<<"eta,phi="<<eta<<","<<phi<<endl;
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
  if(nmod>12) 
    {   nmod=nmod-12;}
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
  Float_t rdist = (TMath::Sqrt(xreal*xreal + yreal*yreal))/mzreal;
  Float_t theta = TMath::ATan(rdist);
//Bedanga & pawan - added theta !=0.
  if(theta !=0.){ eta = TMath::Log(TMath::Tan(0.5*theta));}
  if( xreal==0) {
    if(yreal>0) {phi = 1.571428;}
    if(yreal<0) {phi = -1.571428;}
  }
  if(xreal != 0) {phi = atan2(yreal,xreal);}
  
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
// Function used for mapping from Chain#, Channel# to Detector SM, Col, Row
Int_t StPmdGeom::ChainMapping(Int_t& chainno,Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t&chmod)
{
 
  //  cout<<"picking chainmapping for year 5"<<endl;
  //Bedanga and pawan changed the chain mapping conditions
  
  Int_t chain;
  Int_t col1,row1;
  chain = chainno;
  Int_t chtemp=ch;
  // Int_t brd=int((ch-1)/64)+1;
  Int_t brd=int((ch)/64)+1;
  Int_t missing=0;
  Int_t brdCount=0;
  //	  for(Int_t ibrd=0;ibrd<27;ibrd++){
  for(Int_t ibrd=0;ibrd<36;ibrd++){
    if(brdCount<brd){
      if(status[chainno-1][ibrd]==0){missing++;}
      brdCount+=status[chainno-1][ibrd];
    }
  }
  chtemp=ch+missing*64;
  chmod=chtemp;
  
  if(chtemp>=1728)return kStWarn;
  switch(chain){
    
  case 1:
    chain1(chtemp,supmod,col,row);
    break;
  case 2:
    chain2(chtemp,supmod,col,row);
    break;
  case 3:
    chain3(chtemp,supmod,col,row);
    break;
  case 4:
    chain2(chtemp,supmod,col1,row1);
    col = col1; row = row1 + 24;
    break;
  case 5:
    chain5(chtemp,supmod,col,row);
    break;
  case 6:
    chain2(chtemp,supmod,col,row);
    supmod = 4;
    break;
  case 7:
    chain2(chtemp,supmod,col1,row1);
    col = col1; row = row1 + 24; supmod = 4;
    break;
  case 8:
    chain2(chtemp,supmod,col1,row1);
    col = 73 - col1; row = 25 - row1; supmod = 5;
    break;
  case 9:
    chain9(chtemp,supmod,col,row);
    break;
  case 10:
    chain10(chtemp,supmod,col,row);
    break;
  case 11:
    chain10(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod==4){col = col1 - 24;}
    if(supmod==6){row = row1 + 24;}
    break;
  case 12:
    chain12(chtemp,supmod,col,row);
    break;
  case 13:
    chain12(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod==5) {row = row1 -24; supmod = 6;} 
    if(supmod==7) {row = row1 + 24;}	
    break;
  case 14:
    chain5(chtemp,supmod,col,row);
    supmod = 8;
    break;
  case 15:
    chain15(chtemp,supmod,col,row);
    break;
  case 16:
    chain5(chtemp,supmod,col1,row1);
    supmod = 8; col = col1 -24; row =row1;
    break;
  case 17:
    chain17(chtemp,supmod,col,row);
    break;
  case 18:
    chain5(chtemp,supmod,col1,row1);
    supmod = 10; col = col1 + 24; row =row1;
    break;
  case 19:
    chain2(chtemp,supmod,col,row);
    supmod = 11;
    break;
  case 20:
    chain5(chtemp,supmod,col,row);
    supmod = 10;
    break;
  case 21:
    chain21(chtemp,supmod,col,row);
    break;
  case 22:
    chain22(chtemp,supmod,col,row);
    break;
  case 23:
    chain23(chtemp,supmod,col,row);
    break;
  case 24:
    chain5(chtemp,supmod,col1,row1);
    supmod = 12; col = 121 - col1; row = 73 - row1;
    break;
  case 25:
    chain15(chtemp,supmod, col1, row1);
    supmod = 13; col = 49 - row1; row = 49 - col1;
    break;
  case 26:
    chain5(chtemp,supmod,col1,row1);
    supmod = 14; col = 73 - row1; row = 49 - col1;
    break;
  case 27:
    chain17(chtemp,supmod,col1,row1);
    supmod = supmod + 4; 
    col = 49 - row1; 
    row = 49 - col1;
    if(supmod==15){ row = 73 - col1;}
    break;
  case 28:
    chain5(chtemp,supmod,col1,row1);
    supmod = 14; col = 73 - row1; row = 49 - col1 +24;
    break;
  case 29:
    chain2(chtemp,supmod,col1,row1);
    supmod = 15; col = 49 - row1; row = 73 - col1;
    break;
  case 30:
    chain5(chtemp,supmod,col1,row1);
    supmod = 16; col = 73 - row1; row = 49 - col1;
    break;
  case 31:
    chain5(chtemp,supmod,col1,row1);
    supmod = 16; col = 73 - row1; row = 49 - col1 +24;
    break;
  case 32:
    chain5(chtemp,supmod,col1,row1);
    supmod = 17; col = row1; row = col1 -24; 
    break;
  case 33:
    chain22(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod==11){
      supmod = 15;
      col = 49 - row1; 
      row = 73 - col1;
    }
    if(supmod==12){
      supmod = 17; 
      col = 73 - row1;
      row = 97 - col1;
    }
    break;
  case 34:
    chain34(chtemp,supmod,col,row);
    break;
  case 35:
    chain34(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod == 16){ col = col1 - 24;}
    if(supmod == 18){ row = row1 + 24;}
    break;
  case 36:
    chain34(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod == 16) {col = 121-col1;row = 97 - row1;}
    supmod = supmod + 1;
    break;
  case 37:
    chain34(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod==18){row = row1 + 24;supmod = 19;}
    if(supmod == 16){
      col = 121-col1; row = 73 - row1; 
      supmod = 18;}
    break;
  case 38:
    chain2(chtemp,supmod,col1,row1);
    supmod = 20; col = 49 - row1; row = 73 - col1;
    break;
  case 39:
    chain39(chtemp,supmod,col1,row1);
    col = 49 - row1; row = 49 - col1;
    break;
  case 40:
    chain2(chtemp,supmod,col1,row1);
    supmod = 20; col = 25 - row1; row = 73 - col1;
    break;
  case 41:
    chain41(chtemp,supmod,col1,row1);
    if(supmod==21){col = 49 - row1; row = 49 - col1;}
    if(supmod==23){col = 73 - row1; row = 49 - col1;}
    break;
  case 42:
      chain2(chtemp,supmod,col1,row1);
      supmod = 22; col = 73 - row1; row = 73 - col1;
    break;
  case 43:
      chain5(chtemp,supmod,col1,row1);
      supmod = 23; col = 73 - row1; row = 49 - col1;
    break;
  case 44:
      chain2(chtemp,supmod,col1,row1);
      supmod = 22; col = 49 - row1; row = 73 - col1;
    break;
  case 45:
    chain45(chtemp,supmod,col,row);
    break;
  case 46:
    chain46(chtemp,supmod,col1,row1);
    if(supmod==23){col = 73 - row1; row = 49 - col1;}
    if(supmod==24){col = col1; row = row1;}
    break;
  case 47:
    chain45(chtemp,supmod,col1,row1);
    row =row1;col =col1;
    if(supmod ==22){col = 49 - col1; row = 73 -row1;}
    if(supmod==24){col = 48 + col1; row = row1 - 48;}
    supmod = 24;
    break;
  case 48:
    chain2(chtemp,supmod,col1,row1);
    supmod = 24; col = 72 + row1; row = col1;
    break;
    
  }
  return kStOk;
}

// SM1 has been mounted worngly in the STAND, so mapping has been changed accordingly.
void StPmdGeom::chain1(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
 supmod = 1;
  int zone = ch/192;
  switch(zone){
  case 0:
    col = inorm[ch] + 24; row = jnorm[ch]; //old
    //    col = 25 - inorm[ch]; row = 9 - jnorm[ch];
    break;
  case 1:
    col = inorm[ch-192]; row = jnorm[ch-192]; //old
    // col = 25 - inorm[ch-192]+ 24; row = 9 - jnorm[ch-192];
    break;
  case 2:
    col = 25 - inorm[ch-2*192]; row = 9 - jnorm[ch-2*192] + 8; //old
    //col = inorm[ch-2*192] + 24; row = jnorm[ch-2*192] + 8;  
    break;
  case 3:
    col = 25 - inorm[ch-3*192] + 24; row = 9 - jnorm[ch-3*192] + 8; //old
    //col = inorm[ch-3*192]; row = jnorm[ch -3*192] + 8;
    break;
  case 4:
    col = inorm[ch-4*192] + 24; row = jnorm[ch - 4*192] + 16; //old
    //col = 25 - inorm[ch-4*192]; row = 9 - jnorm[ch-4*192] + 16;
    break;
  case 5:
    col = inorm[ch-5*192]; row = jnorm[ch-5*192] + 16; //old
    //col = 25 - inorm[ch-5*192]+ 24; row = 9 - jnorm[ch-5*192] + 16;
    break;
  case 6:
    col = inorm[ch-6*192]; row = jnorm[ch-6*192] + 24; //old
    //col = 25 - inorm[ch-6*192]; row = 9 - jnorm[ch-6*192] + 24;
    break;
  case 7:
    col =  25 - inorm[ch-7*192]; row = 9 - jnorm[ch-7*192] + 24 + 8; //old
    //col = inorm[ch-7*192]; row = jnorm[ch-7*192] + 24 + 8;
    break;
  case 8:
    col = inorm[ch-8*192]; row = jnorm[ch-8*192] + 24 + 16; //old
    //col = 25 - inorm[ch-8*192]; row = 9 - jnorm[ch-8*192] + 24 + 16;
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

// SM1 has been mounted worngly on the stand, so mapping has been changed accordingly
void StPmdGeom::chain3(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 1;
  int zone = ch/192;
  switch(zone){
  case 0:
    col =  inorm[ch] + 24; row = jnorm[ch] + 24; //old
    //    col = 25 - inorm[ch]+ 24; row = 9 - jnorm[ch] + 24;
    break;
  case 1:
        col =  25 - inorm[ch-192] + 24; row = 9 - jnorm[ch-192] + 24 + 8; //old
	// col = inorm[ch-192] + 24; row = jnorm[ch-192] + 8; 
    break;
  case 2:
    col = inorm[ch-2*192] + 24; row = jnorm[ch-2*192] + 24 + 16; //old
    // col = 25 - inorm[ch-2*192]+ 24; row = 9 - jnorm[ch-2*192] +24 + 16;
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
//Chain 9,10,11 have been combined(1 UM each) to make chain- 9
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
    //    col = inorm[ch-3*192] + 48; row = jnorm[ch-3*192] + 48;
        col = 25 - inorm[ch-3*192]; row = 9-jnorm[ch-3*192] +24 + 16; //old
    supmod = 5; 
    break;
  case 4:
    //col = inorm[ch-4*192] + 24; row = jnorm[ch-4*192] + 48;
        col = 25 - inorm[ch-4*192] + 24; row = 9-jnorm[ch-4*192] + 24 + 16; //old
    supmod = 5;
    break;
  case 5:
    //col = 25 - inorm[ch-5*192] + 24; row = 9 - jnorm[ch-5*192] +48 + 8;
        col = inorm[ch-5*192] + 24; row = jnorm[ch-5*192] + 24 + 8; //old
    supmod = 5;
    break;
  case 6:
    //col = 25 - inorm[ch-6*192] + 48; row = 9 - jnorm[ch-6*192] +48 + 8; 
        col = inorm[ch-6*192]; row = jnorm[ch-6*192]+ 24 +8; //old
    supmod = 5;
    break;
  case 7:
    //col = inorm[ch-7*192] + 48; row = jnorm[ch-7*192] + 48 + 16;
        col = 25 - inorm[ch-7*192]; row = 9-jnorm[ch-7*192] + 24; //old
    supmod = 5;
    break;
  case 8:
    //col = inorm[ch-8*192] + 24; row = jnorm[ch-8*192] + 48 + 16;
        col = 25 - inorm[ch-8*192] + 24; row = 9-jnorm[ch-8*192] + 24; //old
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

void StPmdGeom::chain39(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 21;
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

void StPmdGeom::chain41(int& ch,int& supmod,int& col,int& row)
{
  supmod = 21;
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
    supmod = 23;
    break;
  case 4:
    col = imirr[ch-4*192] + 16; row = jmirr[ch-4*192] + 24;
    supmod = 23;
    break;
  case 5:
    col = 9 - imirr[ch-5*192] + 8; row = 25 - jmirr[ch-5*192] + 24;
    supmod = 23;
    break;
  case 6:
    col = 9 - imirr[ch-6*192] + 8; row = 25 - jmirr[ch-6*192];
    supmod = 23;
    break;
  case 7:
    col = imirr[ch-7*192]; row = jmirr[ch-7*192];
    supmod = 23;
    break;
  case 8:
    col = imirr[ch-8*192] ; row = jmirr[ch-8*192] + 24;
    supmod = 23;
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

void StPmdGeom::chain46(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row)
{
  supmod = 23;
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
    col = 48 + jnorm[ch-3*192]; row = 48 + inorm[ch-3*192];
    supmod = 24;
    break; 
  case 4:
    col = 48 + jnorm[ch-4*192]; row = 24 + inorm[ch-4*192];
    supmod = 24;
    break;    	
  case 5:
    col = 9 - jnorm[ch-5*192] + 56; row = 49 - inorm[ch-5*192];
    supmod = 24;
    break; 
  case 6:
    col = 9 - jnorm[ch-6*192] + 56; row = 73 - inorm[ch-6*192];
    supmod = 24;
    break;    	
  case 7:
    col = jnorm[ch-7*192] + 64; row = 48 + inorm[ch-7*192];
    supmod = 24;
    break; 
  case 8:
    col = jnorm[ch-8*192] + 64; row = 24 + inorm[ch-8*192];
    supmod = 24;
    break;    	
  }
}
void StPmdGeom::ADC2Edep(Int_t ADC, Float_t& Edep)
{
  const Float_t Coeff[4] = {-0.7802, 0.09678, -3.054e-05, 0.0};
 //const Float_t ErrCoeff[4] = {0.4971, 0.00381, 4.466e-06, 0.0};
  
 Edep = Coeff[0] + Coeff[1]*ADC + Coeff[2]*pow(Float_t(ADC),2) + Coeff[3]*pow(Float_t(ADC),3);
}

void StPmdGeom::readBoardDetail()
{
 
  for(Int_t i=0;i<48;i++){
    //   for(Int_t ib=0;ib<27;ib++){
   for(Int_t ib=0;ib<36;ib++){ 
     status[i][ib]=1;
   }
  }
}

void StPmdGeom::readBoardDetail(Int_t runno1)
{
  //  char runfile[20];
  //  sprintf(runfile,"%d",runno1);
  
  //Initialise status array as 1 (good board)
  Int_t alive_stat[48];
  for(Int_t i=0;i<48;i++){alive_stat[i]=0;}  
  for(Int_t i=0;i<48;i++){
    //    for(Int_t ib=0;ib<27;ib++){
    for(Int_t ib=0;ib<36;ib++){
      status[i][ib]=1;
      if (ib>=27) {status[i][ib]=0;}
      alive_stat[i]=alive_stat[i]+status[i][ib];
    }
    //    cout<<" In readBoardDetail for chain "<<i+1<<" before reading status="<<alive_stat[i]<<" # of alive boards"<<endl;
    // resetting alive_stat
    alive_stat[i]=0;
  }
  // Fetch from the run # the day
  /*
  char iRun[8];
  char iyear[8];
  for (Int_t ik=0; ik<3; ik++)
    {
      iRun[ik] = runfile[ik+1];
    }
  iyear[0] = runfile[0];
  
  Int_t rn =0;
  Int_t year =0;
  rn=atoi(iRun);
  year=atoi(iyear);
  */
  Int_t rn =0;
  Int_t year =0;
  GetRunYear(runno1,rn,year);

  //  cout<<"runid="<<runno1<<" run#="<<rn<<" year="<<year<<endl;
  
  if(year==5){  // 2004 run
    // Once the day is known choose the mapping file
    //     if( rn >=1 && rn <21)
    //Following are the configuration of FEE from 1st Jan'04 to 21st Jan'04
    //cout<<"picking old board status"<<endl;
    if( rn >=1)    
      {
	//chain 0
	for(Int_t i=3;i<6;i++)status[0][i]=0;
	for(Int_t i=12;i<18;i++)status[0][i]=0;
	//chain 1
	status[1][19]=0;
	//chain 3
	status[3][22]=0; status[3][25]=0; status[3][26]=0;
	//chain 5
	status[5][1]=0;for(Int_t i=24;i<27;i++)status[5][i]=0;
	//chain 6
	status[6][24]=0;for(Int_t i=6;i<9;i++)status[6][i]=0;
	//chain 7
	for(Int_t i=0;i<27;i++)status[7][i]=0;
	//chain 8
	status[8][22]=0;
	//chain 9
	for(Int_t i=0;i<27;i++)status[9][i]=0;
	//chain10 
	for(Int_t i=0;i<27;i++)status[10][i]=0;
	//chain11 
	for(Int_t i=0;i<9;i++)status[11][i]=0;
	//chain12 
	for(Int_t i=0;i<9;i++)status[12][i]=0;
	//chain13 
	for(Int_t i=0;i<27;i++)status[13][i]=0;
	//chain14 
	for(Int_t i=0;i<27;i++)status[14][i]=0;
	//chain15 
	for(Int_t i=0;i<27;i++)status[15][i]=0;
	//chain16 
	for(Int_t i=0;i<9;i++)status[16][i]=0;
	//chain17 
	for(Int_t i=6;i<12;i++)status[17][i]=0;
	for(Int_t i=24;i<27;i++)status[17][i]=0;
	//chain19 
	for(Int_t i=3;i<6;i++)status[19][i]=0;
	for(Int_t i=21;i<24;i++)status[19][i]=0;
	status[19][12]=0;
	//chain20 
	status[20][18]=0;
	//chain21 
	status[21][0]=0;
	//chain22 
	status[22][0]=0;
	//chain23 
	status[23][6]=0;status[23][24]=0;
	//chain25 
	status[25][16]=0;
	//chain31 
	status[31][8]=0;
	//chain38 
	status[38][24]=0;status[38][25]=0;
	//chain39
	status[39][24]=0;
	//chain40 
	status[40][7]=0;
	//chain44 
	status[44][14]=0;
	//chain46 
	status[46][16]=0;
	
      }
    
    if( rn >=21)  //Following are the FEE config after 21st Jan'04 access
      {
	//chain30 
	status[30][21]=0;
	//chain38 
	status[38][5]=0;
	//chain43 
	status[43][20]=0;
	//chain44
	for(Int_t i=18;i<27;i++)status[44][i]=0;
	//chain45 
	for(Int_t i=3;i<6;i++)status[45][i]=0;
	for(Int_t i=9;i<27;i++)status[45][i]=0;
      }
    
    if( rn >=27) //Following are the FEE config after 27th Jan'04 access
      {
	//chain 24
	status[24][1]=0;
	status[24][25]=0;
	//chain35 
	status[35][14]=0;
	//chain44 
	for(Int_t i=18;i<27;i++)status[44][i]=1;
	//chain45 
	status[45][6]=0;
	for(Int_t i=9;i<27;i++)status[45][i]=1;
	//chain46 
	status[46][16]=0;
	
      }
    
    if( rn >=35) //Following are the FEE config after 4th Feb'04 access
      {
	//chain 0
	for(Int_t i=0;i<21;i++)status[0][i]=0;
	status[0][18]=1;
	//chain 1
	status[1][19]=1;status[1][9]=0;
	//chain 2
	status[2][3]=0;status[2][6]=0;
	status[2][7]=0;status[2][8]=0;
	status[2][10]=0;status[2][13]=0;
	//chain 3
	status[3][25]=1; status[3][26]=1;
	status[3][13]=0; status[3][15]=0;
	status[3][18]=0; status[3][19]=0;
	status[3][20]=0; status[3][22]=0;
	status[3][24]=0;
	//chain 4
	status[4][2]=0; status[4][17]=0;
	//chain 5
	status[5][1]=0; 
	for(Int_t i=21;i<27;i++)status[5][i]=0;
	status[5][23]=1; 
	//chain 6
	//       for(Int_t i=6;i<9;i++)status[6][i]=0; //already assigned
	for(Int_t i=2;i<4;i++)status[6][i]=0;
	status[6][15]=0;
	for(Int_t i=17;i<21;i++)status[6][i]=0;
	//       status[6][17]=0; status[6][18]=0;
	// status[6][19]=0; status[6][20]=0;
	status[6][24]=0;
	//chain 8
	status[8][9]=0; status[8][11]=0;
	//chain11 
	status[11][14]=0; status[11][26]=0;
	//chain17 
	for(Int_t i=5;i<13;i++)status[17][i]=0;
	status[17][14]=0; status[17][18]=0;
	status[17][21]=0; status[17][22]=0;
	for(Int_t i=24;i<27;i++)status[17][i]=0;
	//chain18 
	status[18][1]=0; status[18][6]=0;
	status[18][18]=0;
	for(Int_t i=24;i<27;i++)status[18][i]=0;
	// status[18][24]=0;
	//       status[18][25]=0; status[18][26]=0;
	//chain 20
	status[20][2]=0; status[20][8]=0;
	status[20][9]=0; status[20][18]=0;
	status[20][20]=0; status[20][24]=0;
	status[20][25]=0;
	//chain 24
	//       status[24][1]=0; status[24][25]=0; //already assigned
	//chain 28
	status[28][23]=0; status[28][24]=0;
	//chain 36
	status[36][25]=0;
	//chain44 
      //       for(Int_t i=18;i<27;i++)status[44][i]=1; //already assigned
	status[44][3]=0; status[44][4]=0;
	status[44][14]=0;
	//chain45 
	for(Int_t i=3;i<6;i++)status[45][i]=1;
      }
    
    //    if( rn >=43 && rn <49)
    if( rn >=43) //One board replaced on 12th Feb'04
      {
	//chain33 
	status[33][2]=0;
      }
    
    if( rn >=49 )//Following are the FEE config after 18th Feb'04 access
      {
	//chain 38
	//       status[38][5]=0;
	status[38][19]=0;
	status[38][22]=0;
	//       status[38][24]=0; //already assigned
	//       status[38][25]=0;
	//chain 8
	//       status[8][22]=0; //already assigned
      }
    
    if(rn>=63) //Following are the FEE config after 3rd Mar'04 access
      {
	//chain 24
	status[24][20]=0;
	//chain38
	status[38][14]=0;
	//chain46
	status[46][8] = 0;
      }
    
    if( rn >=78 )//Following are the FEE config after 18th Mar'04 access
      {
	//chain 25    
	status[24][12]=0;
	//chain 30    
	status[30][18]=0;
	//chain 32    
	status[32][21]=0;
	//chain 35    
	status[35][12]=0;
	//chain 36    
	status[36][24]=0;
	//chain 38    
	status[38][7]=0;
	//chain 42    
	status[42][7]=0;
	//chain 45    
	status[45][8]=0;
	//chain 44    
	status[44][6]=0;status[44][11]=0;
	//chain 46    
	status[46][8]=0;status[46][16]=0;
	//chain 47    
	status[47][11]=0;status[47][19]=0;
	status[47][20]=0;
	
      }
  }  // end of year==5
  
  //(Y2005, Rashmi's) Following are the configuration of FEE from 26th Nov'04 for pmd and 24th Dec'04 for cpv.
  if( rn >=1 && year==6)    
    {
      //chain 1
      for(Int_t i=3;i<9;i++)status[0][i]=0;
      for(Int_t i=24;i<27;i++)status[0][i]=0;
      //chain 2 
      // all alive
      //chain 3
      status[2][4]=0;status[2][6]=0;
      //chain 4
      status[3][4]=0;
      for(Int_t i=19;i<22;i++)status[3][i]=0;
      status[3][24]=0;
      //chain 5
      status[4][12]=0;
      //chain 6
      for(Int_t i=0;i<27;i++)status[5][i]=0;
      //chain 7
      status[6][0]=0;status[6][5]=0;
      for(Int_t i=12;i<14;i++)status[6][i]=0;
      for(Int_t i=21;i<23;i++)status[6][i]=0;
      //chain 8
      status[7][3]=0;status[7][23]=0;
      for(Int_t i=10;i<14;i++)status[7][i]=0;
      for(Int_t i=25;i<26;i++)status[7][i]=0;
      //chain 9
      status[8][0]=0;status[8][7]=0;
      status[8][19]=0;status[8][25]=0;status[8][26]=0;
      //chain 10
      status[9][5]=0; status[9][8]=0; status[9][22]=0;
      //chain 11
      status[10][0]=0;status[10][7]=0;
      for(Int_t i=12;i<14;i++)status[10][i]=0;
      for(Int_t i=22;i<27;i++)status[10][i]=0;
      //chain 12
      for(Int_t i=4;i<7;i++)status[11][i]=0;
      status[11][9]=0;
      for(Int_t i=12;i<14;i++)status[11][i]=0;
      for(Int_t i=18;i<27;i++)status[11][i]=0;
      //chain 13
      status[12][6]=0;
      for(Int_t i=10;i<12;i++)status[12][i]=0;
      status[12][15]=0;status[12][23]=0;status[12][24]=0;status[12][26]=0;
      //chain 14
      for(Int_t i=0;i<27;i++)status[13][i]=0;
      //chain 15
      for(Int_t i=18;i<27;i++)status[14][i]=0;
      //chain 16
      for(Int_t i=12;i<14;i++)status[15][i]=0;
      for(Int_t i=18;i<27;i++)status[15][i]=0;
      //chain 17
      status[16][0]=0; status[16][17]=0; status[16][19]=0;
      status[16][20]=0; status[16][25]=0;status[16][26]=0;
      //chain 18
      for(Int_t i=0;i<27;i++)status[17][i]=0;
      //chain 19
      status[18][1]=0; status[18][6]=0; status[18][12]=0;
      for(Int_t i=18;i<21;i++)status[18][i]=0;
      for(Int_t i=24;i<27;i++)status[18][i]=0;
      //chain 20
      for(Int_t i=0;i<27;i++)status[19][i]=0;
      //chain 21
      status[20][0]=0; status[20][6]=0; status[20][7]=0;
      for(Int_t i=10;i<16;i++)status[20][i]=0;
      status[20][20]=0; status[20][26]=0; 
      //chain 22
      for(Int_t i=0;i<27;i++)status[21][i]=0;
      //chain 23
      for(Int_t i=1;i<3;i++)status[22][i]=0;
      status[22][15]=0;
      //chain 24
      for(Int_t i=6;i<8;i++)status[23][i]=0;
      status[23][17]=0;
      for(Int_t i=23;i<27;i++)status[23][i]=0;
      //chain 25
      for(Int_t i=5;i<7;i++)status[24][i]=0;
      for(Int_t i=11;i<14;i++)status[24][i]=0;
      for(Int_t i=15;i<17;i++)status[24][i]=0;
      status[24][20]=0;
      //chain 26
      for(Int_t i=15;i<18;i++)status[25][i]=0;
      //chain 27
      for(Int_t i=1;i<3;i++)status[26][i]=0;
      //chain 28
      status[27][18]=0;
      //chain 29
      status[28][10]=0;status[28][24]=0;
      //chain 30
      for(Int_t i=0;i<3;i++)status[29][i]=0;
      for(Int_t i=15;i<21;i++)status[29][i]=0;
      //chain 31
      for(Int_t i=18;i<23;i++)status[30][i]=0;
      //chain 32
      //chain 33
      for(Int_t i=10;i<12;i++)status[32][i]=0;
      for(Int_t i=17;i<19;i++)status[32][i]=0;
      for(Int_t i=24;i<27;i++)status[32][i]=0;
      //chain 34
      status[33][18]=0;
      //chain 35
      for(Int_t i=0;i<3;i++)status[34][i]=0;
      //chain 36
      status[35][23]=0;
      //chain 37
      status[36][2]=0; status[36][12]=0;
      for(Int_t i=6;i<8;i++)status[36][i]=0;
      for(Int_t i=15;i<17;i++)status[36][i]=0;
      //chain 38
      status[37][8]=0;
      for(Int_t i=20;i<22;i++)status[37][i]=0;
      //chain 39
      status[38][14]=0;
      //chain 40
      status[39][2]=0; status[39][10]=0; status[39][20]=0;
      for(Int_t i=6;i<8;i++)status[39][i]=0;
      for(Int_t i=24;i<27;i++)status[39][i]=0;
      //chain 41
      status[40][7]=0; 
      for(Int_t i=9;i<11;i++)status[40][i]=0;
      for(Int_t i=13;i<15;i++)status[40][i]=0;
      //chain 42
      //chain 43
      status[42][7]=0;
      //chain 44
      //chain 45
      status[44][3]=0;status[44][6]=0;
      for(Int_t i=12;i<18;i++)status[44][i]=0;
      for(Int_t i=18;i<27;i++)status[44][i]=0;
      //chain 46
      for(Int_t i=9;i<27;i++)status[45][i]=0;
      //chain 47
      for(Int_t i=0;i<4;i++)status[46][i]=0;
      for(Int_t i=6;i<9;i++)status[46][i]=0;
      status[46][10]=0;status[46][15]=0;status[46][17]=0;status[46][24]=0;
      //chain 48
      status[47][0]=0;status[47][5]=0;status[47][11]=0;
      for(Int_t i=14;i<16;i++)status[47][i]=0;
      for(Int_t i=17;i<19;i++)status[47][i]=0;
      for(Int_t i=24;i<27;i++)status[47][i]=0;
    }
  
  if(rn >=1 && year==8){
      // RR 06/03/2007 Since some chains extend beyond 27 mapped boards
      // Setting status for boards beyond 27 for 
      // all chains except 1,7,19,20,23,24 to status 0 
      // beyond 27 board. 
      for(Int_t i=0;i<48;i++){
	for(Int_t ib=27;ib<36;ib++){
	  if (i==0 || i==6 || i==18 || i==19 || i==22 || i==23){ 
	    status[i][ib]=1;
	  }else {
	    status[i][ib]=0;
	  }
	}
      }
      
      //chain 1
      for(Int_t i=0;i<9;i++)status[0][i]=0;
      for(Int_t i=12;i<18;i++)status[0][i]=0;
      for(Int_t i=33;i<36;i++)status[0][i]=0;
      //chain 2
      
      //chain 3
      // For e.g. the following statements are for the 5th & 7th board
      status[2][4]=0;
      status[2][6]=0;
      //chain 4
      status[3][4]=0;
      for(Int_t i=19;i<22;i++)status[3][i]=0;
      status[3][24]=0;
      //chain 5
      status[4][12]=0;
      //chain 6

      //chain 7
      status[6][0]=0;
      status[6][5]=0;
      status[6][12]=0;
      status[6][13]=0;
      status[6][18]=0;
      for(Int_t i=22;i<32;i++)status[6][i]=0;
      //chain 8
      status[7][3]=0;
      for(Int_t i=10;i<14;i++)status[7][i]=0;
      status[7][23]=0;
      status[7][25]=0;
      status[7][26]=0;
      //chain 9
      status[8][0]=0;
      status[8][7]=0;
      status[8][19]=0;
      status[8][25]=0;
      status[8][26]=0;
      //chain 10
      status[9][5]=0;
      status[9][8]=0;
      status[9][22]=0;
      //chain 11
      status[10][0]=0;
      status[10][7]=0;
      status[10][12]=0;
      status[10][13]=0;
      for(Int_t i=22;i<27;i++)status[10][i]=0;
      //chain 12
      for(Int_t i=4;i<7;i++)status[11][i]=0;
      status[11][9]=0;
      status[11][12]=0;
      status[11][13]=0;
      for(Int_t i=18;i<27;i++)status[11][i]=0;
      //chain 13
      status[12][6]=0;
      status[12][10]=0;
      status[12][11]=0;
      status[12][15]=0;
      status[12][23]=0;
      status[12][24]=0;
      status[12][26]=0;
      //chain 14

      //chain 15
      for(Int_t i=24;i<27;i++)status[14][i]=0;
      //chain 16
      //chain 17
      //Does not exist
      //chain 18
      status[17][20]=0;
      //chain 19
      for(Int_t i=3;i<6;i++)status[18][i]=0;
      status[18][9]=0;
      for(Int_t i=15;i<18;i++)status[18][i]=0;
      status[18][34]=0;
      status[18][35]=0;
      //chain 20
      for(Int_t i=0;i<3;i++)status[19][i]=0;
      for(Int_t i=6;i<10;i++)status[19][i]=0;
      for(Int_t i=14;i<18;i++)status[19][i]=0;
      status[19][32]=0;
      status[19][33]=0;
      
      //chain 21

      //chain 22

      //chain 23
      for(Int_t i=18;i<25;i++)status[22][i]=0;
      status[22][29]=0;
      status[22][35]=0;
      //chain 24
      status[23][5]=0;
      status[23][6]=0;
      status[23][16]=0;
      for(Int_t i=22;i<30;i++)status[23][i]=0;
      //chain 25
      status[24][5]=0;
      status[24][6]=0;
      for(Int_t i=11;i<14;i++)status[24][i]=0;
      status[24][15]=0;
      status[24][16]=0;
      status[24][20]=0;
      //chain 26
      
      //chain 27
      status[26][1]=0;
      status[26][2]=0;
      //chain 28

      //chain 29 

      //chain 30
      for(Int_t i=0;i<3;i++)status[29][i]=0;
      status[29][8]=0;
      for(Int_t i=15;i<21;i++)status[29][i]=0;
      //chain 31
      for(Int_t i=18;i<23;i++)status[30][i]=0;
      //chain 32

      //chain 33
      status[32][10]=0;
      status[32][11]=0;
      status[32][17]=0;
      status[32][18]=0;
      for(Int_t i=23;i<27;i++)status[32][i]=0;
      //chain 34
      status[33][18]=0;
      //chain 35
      status[34][2]=0;
      status[34][10]=0;
      //chain 36
      status[35][23]=0;
      //chain 37
      status[36][2]=0;
      for(Int_t i=6;i<9;i++)status[36][i]=0;
      status[36][12]=0;
      for(Int_t i=14;i<17;i++)status[36][i]=0;
      //chain 38
      status[37][7]=0;
      status[37][8]=0;
      status[37][20]=0;
      status[37][21]=0;
      //chain 39
      status[38][14]=0;
      //chain 40
      status[39][2]=0;
      status[39][6]=0;
      status[39][7]=0;
      status[39][10]=0;
      status[39][20]=0;
      for(Int_t i=24;i<27;i++)status[39][i]=0;
      //chain 41
      status[40][7]=0;
      status[40][9]=0;
      status[40][10]=0;
      status[40][13]=0;
      status[40][14]=0;
      //chain 42
      
      //chain 43
      status[42][7]=0;
      //chain 44
    
      //chain 45
      for(Int_t i=10;i<27;i++)status[44][i]=0;      
      //chain 46
      //chain 47
      //chain 48
  }
  if(rn>87&&year==8){
    //chain 33 board 7
    status[32][6]=0;
    // chain 46 board 18,19
    status[45][17]=0;
    status[45][18]=0;
  }
  //access on 11April 2007
  if(rn>101&&year==8){
    //chain 25  FEE board 5
    status[24][4]=0;
    //chain 29   FEE board 7, 12, 26
    status[28][6]=0;
    status[28][11]=0;
    status[28][25]=0;
    //chain 33   FEE board 1, 5
    status[32][0]=0;
    status[32][4]=0;
  }
  //access on 26/04/06
  if(rn>114&&year==8){ 
    status[46][6]=0;
  }

//access on 26/04/06
// ZA wrote (mail on dec 6,07, in pmd list) following boards were removed:
// chain 39: 1,4,5,6
// chain26: 8,13
// Chain 23: 1,4,5
// Chain29: 7,12
// Chan37: 10
// Numbering is assumed to start from 1 and not from 0 , to be checked

  if(rn>300 && year==8){
    status[38][0]=0;
    status[38][3]=0;
    status[38][4]=0;
    status[38][5]=0;
    status[25][7]=0;
    status[25][12]=0;
    status[22][0]=0;
    status[22][3]=0;
    status[22][4]=0;
    status[28][6]=0;
    status[28][11]=0;
    status[36][9]=0;
  }

  if(rn >=1 && (year==10||year==11||year==12)){
    // RR 23/10/2009 Since some chains extend beyond 27 mapped boards
    // Setting status for boards beyond 27 for 
    // all chains except 1,7,19,20,23,24 to status 0 
    // beyond 27 board. 
    for(Int_t i=0;i<48;i++){
      for(Int_t ib=27;ib<36;ib++){
	if (i==7 || i==11 || i==22 || i==23){
	  status[i][ib]=1;
	}else {
	  status[i][ib]=0;
	}
      }
    }
    
    // chain 1
    for(Int_t brd = 0;brd<=2;brd++){
      status[0][brd]=0;
    }
    for(Int_t brd = 6;brd<=8;brd++){
      status[0][brd]=0;
    }
    for(Int_t brd = 24;brd<=26;brd++){
      status[0][brd]=0;
    }
    // chain 2
    // chain 3
    status[2][4]=0;
    status[2][6]=0;
    status[2][20]=0;
    // chain 4
    status[3][4]=0;
    status[3][19]=0;
    status[3][20]=0;
    status[3][21]=0;
    status[3][24]=0;
    // chain 5
    status[4][12]=0;
    // chain 6
    // chain 7
    // chain 8
    status[7][14]=0;
    status[7][16]=0;
    status[7][17]=0;
    for(Int_t brd=19;brd<=22;brd++){
      status[7][brd]=0;
    }
    status[7][30]=0;
    // chain 9
    // chain 10
    status[9][2]=0;
    status[9][5]=0;
    status[9][13]=0;
    // chain 11
    status[10][16]=0;
    // chain 12
    status[11][1]=0;
    status[11][2]=0;
    status[11][6]=0;
    status[11][14]=0;
    status[11][15]=0;
    status[11][17]=0;
    for(Int_t brd=22;brd<=24;brd++){
      status[11][brd]=0;
    }
    status[11][27]=0;
    status[11][30]=0;
    status[11][31]=0;
    // chain 13
    // chain 14
    // chain 15
    for(Int_t brd=18;brd<=20;brd++){
      status[14][brd]=0;
    }
    // chain 16
    status[15][3]=0;
    // chain 17
    // chain 18
    status[17][20]=0;
    // chain 19
    // chain 20
    status[19][14]=0;
    status[19][15]=0;
    // chain 21
    // chain 22
    for(Int_t brd = 11;brd<=14;brd++){
      status[21][brd]=0;
    }
    // chain 23
    status[22][11]=0;
    status[22][12]=0;
    for(Int_t brd = 18;brd <=24;brd++){
      status[22][brd]=0;
    }
    status[22][29]=0;
    status[22][35]=0;
    // chain 24
    status[23][6]=0;
    status[23][7]=0;
    status[23][17]=0;
    for(Int_t i=23;i<=30;i++){status[23][i]=0;}

    /*    status[23][11]=0;
    status[23][12]=0;
    for(Int_t brd = 18;brd<=24;brd++){
      status[23][brd]=0;
    }
    status[23][29]=0;
    status[23][35]=0;
    */
    //chain 25
    // chain 26
    for(Int_t brd = 3;brd<=5;brd++){
      status[25][brd]=0;
    }
    for(Int_t brd = 12;brd<=14;brd++){
      status[25][brd]=0;
    }
    for(Int_t brd = 21;brd<=23;brd++){
      status[25][brd]=0;
    }
    // chain 27
    // chain 28
    // chain 29
    // chain 30
    // chain 31
    // chain 32
    // chain 33
    // chain 34
    status[33][18]=0;
    // chain 35
    // chain 36
    // chain 37
    // chain 38
    status[37][7]=0;
    status[37][8]=0;
    status[37][20]=0;
    status[37][21]=0;
    // chain 39
    status[38][24]=0;
    // chain 40
    status[39][2]=0;
    status[39][6]=0;
    status[39][7]=0;
    status[39][10]=0;
    status[39][20]=0;
    for(Int_t brd=24;brd<=26;brd++){
      status[39][brd]=0;
    }
    // chain 41
    status[40][9]=0;
    status[40][10]=0;
    status[40][13]=0;
    status[40][14]=0;
    // chain 42
    // chain 43
    status[42][7]=0;
    // chain 44
    status[43][19]=0;
    // chain 45
    for(Int_t brd = 10;brd<=26;brd++){
      status[44][brd]=0;
    }
    // chain 46
    status[45][17]=0;    
    status[45][20]=0;
    // chain 47
    status[46][6]=0;
    status[46][8]=0;
    status[46][9]=0;
    status[46][10]=0;
    status[46][26]=0;
    // chain 48
  }
  if(rn>48 && (year==10||year==11)){
    // chain 30 board 18 is removed AFTER run 11048019 as reported by Zubayer on 27Feb2010
    status[29][17]=0;
  }
  
  if(rn>84 && (year==10||year==11)){
    // chain 26 board 11 removed as reported by Prithwish on 25/3/2010
    // chain 26 board 26 brought back as reported by Prithwish on 25/3/2010
    status[25][10]=0;
    status[25][25]=1;
  }
  if(rn>96 && (year==10||year==11)){
    // chain 29 modified as reported by Zubayer on 8/4/2010
    // chain 26 modified as reported by Zubayer on 8/4/2010
    for(Int_t brd = 6;brd<12;brd++){
      status[29][brd]=0;
    }
    status[29][8]=1;
    status[29][24]=0;
    status[29][25]=0;
    status[29][26]=0;
    status[25][25]=0;
  }


  for(Int_t i=0;i<48;i++){
    for(Int_t ib=0;ib<36;ib++){
      alive_stat[i]=alive_stat[i]+status[i][ib];
    }
    //  cout<<" In readBoardDetail for chain "<<i+1<<" after reading status="<<alive_stat[i]<<" # of alive boards"<<endl;
  }
}

// (Y2005, rashmi's code)Function used for mapping from Chain#, Channel# to Detector SM, Col, Row
Int_t StPmdGeom::ChainMapping(Int_t& chainno,Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t&chmod,Int_t year)
{
  //initialized to zero rr 27.1.2005
  //  cout<<"In chain mapping"<<endl;
  supmod =0;
  col =0; 
  row = 0;
  
  Int_t chain = chainno;
  Int_t chtemp=ch;
  Int_t brd=Int_t((ch)/64)+1;
  Int_t missing=0;
  Int_t brdCount=0;
  //  cout<<" Going to get missing"<<endl;
  //  for(Int_t ibrd=0;ibrd<27;ibrd++)
  for(Int_t ibrd=0;ibrd<36;ibrd++){
    if(brdCount<brd){
      if(status[chainno-1][ibrd]==0){missing++;}
      brdCount+=status[chainno-1][ibrd];
    }
  }
  chtemp=ch+missing*64;
  chmod=chtemp;
  //  cout<<"year,chain,ch,chtemp,missing="<<year<<","<<chainno<<","<<ch<<","<<chtemp<<","<<missing<<endl;
  
  if(year==8){
    if(chain==1 || chain==7 || chain==19 || chain==20 || chain==23 || chain==24) {
      //      if(chtemp>=2304) cout<<"ch,chtemp="<<ch<<","<<chtemp<<endl;
      if( chtemp>=2304){return kStWarn;}
    }else{
      //      if(chtemp>=1728) cout<<"ch,chtemp="<<ch<<","<<chtemp<<endl;
      if(chtemp >=1728){
	//	cout<<"returning kStWarn"<<endl;
	return kStWarn;
      }
    }
    //    Non existent chains
    if(chain==6 || chain==17 || chain == 21 ){return kStWarn;}
  }else{
    if(year==10||year==11||year==12){
      //  cout<<"year,chain,ch,chtemp,missing="<<year<<","<<chainno<<","<<ch<<","<<chtemp<<","<<missing<<endl;
      if(chain==8||chain==12||chain==23||chain==24){
	if(chtemp>=2304){return kStWarn;}
      }else{
	if(chtemp>=1728){return kStWarn;}
      }
      if(chain==6||chain==21){return kStWarn;}
    }else{
      //      if(chtemp>=1728) cout<<"ch,chtemp="<<ch<<","<<chtemp<<endl;
      if(chtemp>=1728){ return kStWarn;}
    }
  }
  //  cout<<"year,chain,ch,chtemp,missing="<<year<<","<<chainno<<","<<ch<<","<<chtemp<<","<<missing<<endl;
  
  switch(chain){
    
  case 1:
    chain1(chtemp,supmod,col,row,year);
    break;
  case 2:
    chain2(chtemp,supmod,col,row,year);
    break;
  case 3:
    chain3(chtemp,supmod,col,row,year);
    break;
  case 4:
    chain4(chtemp,supmod,col,row,year);
    break;
  case 5:
    chain5(chtemp,supmod,col,row,year);
    break;
  case 6:
    chain6(chtemp,supmod,col,row,year);
    break;
  case 7:
    chain7(chtemp,supmod,col,row,year);
    break;
  case 8:
    chain8(chtemp,supmod,col,row,year);
    break;
  case 9:
    chain9(chtemp,supmod,col,row,year);
    break;
  case 10:
    chain10(chtemp,supmod,col,row,year);
    break;
  case 11:
    chain11(chtemp,supmod,col,row,year);
    break;
  case 12:
    chain12(chtemp,supmod,col,row,year);
    break;
  case 13:
    chain13(chtemp,supmod,col,row,year);
    break;
  case 14:
    chain14(chtemp,supmod,col,row,year);
    break;
  case 15:
    chain15(chtemp,supmod,col,row,year);
    break;
  case 16:
    chain16(chtemp,supmod,col,row,year);
    break;
  case 17:
    chain17(chtemp,supmod,col,row,year);
    break;
  case 18:
    chain18(chtemp,supmod,col,row,year);
    break;
  case 19:
    chain19(chtemp,supmod,col,row,year);
    break;
  case 20:
    chain20(chtemp,supmod,col,row,year);
    break;
  case 21:
    chain21(chtemp,supmod,col,row,year);
    break;
  case 22:
    chain22(chtemp,supmod,col,row,year);
    break;
  case 23:
    chain23(chtemp,supmod,col,row,year);
    break;
  case 24:
    chain24(chtemp,supmod,col,row,year);
    break;
  case 25:
    chain25(chtemp,supmod,col,row,year);
    break;
  case 26:
    chain26(chtemp,supmod,col,row,year);
    break;
  case 27:
    chain27(chtemp,supmod,col,row,year);
    break;
  case 28:
    chain28(chtemp,supmod,col,row,year);
    break;
  case 29:
    chain29(chtemp,supmod,col,row,year);
    break;
  case 30:
    chain30(chtemp,supmod,col,row,year);
    break;
  case 31:
    chain31(chtemp,supmod,col,row,year);
    break;
  case 32:
    chain32(chtemp,supmod,col,row,year);
    break;
  case 33:
    chain33(chtemp,supmod,col,row,year);
    break;
  case 34:
    chain34(chtemp,supmod,col,row,year);
    break;
  case 35:
    chain35(chtemp,supmod,col,row,year);
    break;
  case 36:
    chain36(chtemp,supmod,col,row,year);
    break;
  case 37:
    chain37(chtemp,supmod,col,row,year);
    break;
  case 38:
    chain38(chtemp,supmod,col,row,year);
    break;
  case 39:
    chain39(chtemp,supmod,col,row,year);
    break;
  case 40:
    chain40(chtemp,supmod,col,row,year);
    break;
  case 41:
    chain41(chtemp,supmod,col,row,year);
    break;
  case 42:
    chain42(chtemp,supmod,col,row,year);
    break;
  case 43:
    chain43(chtemp,supmod,col,row,year);
    break;
  case 44:
    chain44(chtemp,supmod,col,row,year);
    break;
  case 45:
    chain45(chtemp,supmod,col,row,year);
    break;
  case 46:
    chain46(chtemp,supmod,col,row,year);
    break;
  case 47:
    chain47(chtemp,supmod,col,row,year);
    break;
  case 48:
    chain48(chtemp,supmod,col,row,year);
    break;
  }
  if(supmod<=0 || col<=0 || row<=0){
    //    cout<<"chain = "<<chain<<" chtemp="<<chtemp<<" supmod="<<supmod<<" col="<<col<<" row="<<row<<endl;
    return kStWarn;
  }

  //  cout<<"chain = "<<chain<<" chtemp="<<chtemp<<" supmod="<<supmod<<" row="<<row<<" col="<<col<<endl;
  return kStOk;
}

// The following chains [1-48] have been mapped for run05
void StPmdGeom::chain1(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
    /*
      case 5: {
      supmod = 1;
      switch(zone){
      case 0:
      col = inorm[ch] + 24; row = jnorm[ch]; //old
      //    col = 25 - inorm[ch]; row = 9 - jnorm[ch];
      break;
      case 1:
      col = inorm[ch-192]; row = jnorm[ch-192]; //old
      // col = 25 - inorm[ch-192]+ 24; row = 9 - jnorm[ch-192];
      break;
      case 2:
      col = 25 - inorm[ch-2*192]; row = 9 - jnorm[ch-2*192] + 8; //old
      //col = inorm[ch-2*192] + 24; row = jnorm[ch-2*192] + 8;  
      break;
      case 3:
      col = 25 - inorm[ch-3*192] + 24; row = 9 - jnorm[ch-3*192] + 8; //old
      //col = inorm[ch-3*192]; row = jnorm[ch -3*192] + 8;
      break;
      case 4:
      col = inorm[ch-4*192] + 24; row = jnorm[ch - 4*192] + 16; //old
      //col = 25 - inorm[ch-4*192]; row = 9 - jnorm[ch-4*192] + 16;
      break;
      case 5:
      col = inorm[ch-5*192]; row = jnorm[ch-5*192] + 16; //old
      //col = 25 - inorm[ch-5*192]+ 24; row = 9 - jnorm[ch-5*192] + 16;
      break;
      case 6:
      col = inorm[ch-6*192]; row = jnorm[ch-6*192] + 24; //old
      //col = 25 - inorm[ch-6*192]; row = 9 - jnorm[ch-6*192] + 24;
      break;
      case 7:
      col =  25 - inorm[ch-7*192]; row = 9 - jnorm[ch-7*192] + 24 + 8; //old
      //col = inorm[ch-7*192]; row = jnorm[ch-7*192] + 24 + 8;
      break;
      case 8:
      col = inorm[ch-8*192]; row = jnorm[ch-8*192] + 24 + 16; //old
      //col = 25 - inorm[ch-8*192]; row = 9 - jnorm[ch-8*192] + 24 + 16;
      break;
      }
      }
      break;
    */
  case 6: {
    
    switch(zone){
    case 0:
      col = inorm[ch]; row = 8 + jnorm[ch];
      supmod = 1;
      break;
    case 1:
      col = 25 - inorm[ch-192]; row = 25 - jnorm[ch-192];
      supmod = 1;
      break;
    case 2:
      col = 49 - inorm[ch-2*192]; row = 25 - jnorm[ch-2*192];
      supmod = 1;
      break;
    case 3:
      col = 24 + inorm[ch-3*192]; row = 24 + jnorm[ch-3*192];
      supmod = 1;
      break;
    case 4:
      col = inorm[ch-4*192]; row = 24 + jnorm[ch-4*192];
      supmod = 1;
      break;
    case 5:
      col = 25 - inorm[ch-5*192]; row = 41 - jnorm[ch-5*192];
      supmod = 1;
      break;
    case 6:
      col = 49 - inorm[ch-6*192]; row = 41 - jnorm[ch-6*192];
      supmod = 1;
      break;
    case 7:
      col = 24 + inorm[ch-7*192]; row = 40 + jnorm[ch-7*192];
      supmod = 1;
      break;
    case 8:
      col = inorm[ch-8*192]; row = 40 + jnorm[ch-8*192];
      supmod = 1;
      break;
    }
    // The following instruction are for changing year to 2007
    break;
  }
  case 8: {
    
    switch(zone){
    case 0:
      col = 25 - inorm[ch]; row = 9 - jnorm[ch];
      supmod = 1;
      break;
    case 1:
      col = 49 - inorm[ch-192]; row = 9 - jnorm[ch-192];
      supmod = 1;
      break;
    case 2:
      col = 24 + inorm[ch-2*192]; row = 8 + jnorm[ch-2*192];
      supmod = 1;
      break;
    case 3:
      col = inorm[ch-3*192]; row = 8 + jnorm[ch-3*192];
      supmod = 1;
      break;
    case 4:
      col = 25 - inorm[ch-4*192]; row = 25 - jnorm[ch-4*192];
      supmod = 1;
      break;
    case 5:
      col = 49 - inorm[ch-5*192]; row = 25 - jnorm[ch-5*192];
      supmod = 1;
      break;
    case 6:
      col = 24 + inorm[ch-6*192]; row = 24 + jnorm[ch-6*192];
      supmod = 1;
      break;
    case 7:
      col = inorm[ch-7*192]; row = 24 + jnorm[ch-7*192];
      supmod = 1;
      break;
    case 8:
      col = 25 - inorm[ch-8*192]; row = 41 - jnorm[ch-8*192];
      supmod = 1;
      break;
    case 9:
      col = 49 - inorm[ch-9*192]; row = 41 - jnorm[ch-9*192];
      supmod = 1;
      break;
    case 10:
      col = 24 + inorm[ch-10*192]; row = 40 + jnorm[ch-10*192];
      supmod = 1;
      break;
    case 11:
      col = inorm[ch-11*192]; row = 40 + jnorm[ch-11*192];
      supmod = 1;
      break;
    }
    break;
  }
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 25 - inorm[ch];
	row = 9 - jnorm[ch];
	supmod = 1;
	break;
      case 1:
	col = inorm[ch-192];
	row = 8 + jnorm[ch-192];
	supmod = 1;
	break;
      case 2:
	col = 25 - inorm[ch-2*192];
	row = 25 - jnorm[ch-2*192];
	supmod = 1;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 33 - jnorm[ch-3*192];
	supmod = 1;
	break;
      case 4:
	col = 49 - inorm[ch-4*192];
	row = 33 -jnorm[ch-4*192];
	supmod = 1;
	break;
      case 5:
	col = 24 + inorm[ch-5*192];
	row = 32 + jnorm[ch-5*192];
	supmod = 1;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 32 + jnorm[ch-6*192];
	supmod = 1;
	break;
      case 7:
	col = 25 - inorm[ch-7*192];
	row = 49 - jnorm[ch-7*192];
	supmod = 1;
	break;
      case 8:
	col = 49 - inorm[ch-8*192];
	row = 49 - jnorm[ch-8*192];
	supmod = 1;
	break;
      }
    break;
  }
}

void StPmdGeom::chain2(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 2;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 2;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 2;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 17 - jnorm[ch-3*192];
	supmod = 2;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 2;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 2;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 2;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 16 + jnorm[ch-7*192];
	supmod = 2;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 2;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 2;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 2;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 2;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 17 - jnorm[ch-3*192];
	supmod = 2;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 2;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 2;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 2;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 16 + jnorm[ch-7*192];
	supmod = 2;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 2;
	break;
      }
    break;
  }
}

void StPmdGeom::chain3(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
 
    switch(zone)
      {
      case 0:
	col = 16 + imirr[ch];
	row = jmirr[ch];
	supmod = 3;
	break;
      case 1:
	col = 16 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 3;
	break;
      case 2:
	col = 16 + imirr[ch-2*192];
	row = 48 + jmirr[ch-2*192];
	supmod = 3;
	break;
      case 3:
	col = 17 - imirr[ch-3*192];
	row = 73 - jmirr[ch-3*192];
	supmod = 3;
	break;
      case 4:
	col = 17 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 3;
	break;
      case 5:
	col = 17 - imirr[ch-5*192];
	row = 25 - jmirr[ch-5*192];
	supmod = 3;
	break;
      case 6:
	col = imirr[ch-6*192];
	row = jmirr[ch-6*192];
	supmod = 3;
	break;
      case 7:
	col =  imirr[ch-7*192];
	row =  24 + jmirr[ch-7*192];
	supmod = 3;
	break;
      case 8:
	col = imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 3;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 16 + imirr[ch];
	row = jmirr[ch];
	supmod = 3;
	break;
      case 1:
	col = 16 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 3;
	break;
      case 2:
	col = 16 + imirr[ch-2*192];
	row = 48 + jmirr[ch-2*192];
	supmod = 3;
	break;
      case 3:
	col = 17 - imirr[ch-3*192];
	row = 73 - jmirr[ch-3*192];
	supmod = 3;
	break;
      case 4:
	col = 17 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 3;
	break;
      case 5:
	col = 17 - imirr[ch-5*192];
	row = 25 - jmirr[ch-5*192];
	supmod = 3;
	break;
      case 6:
	col = imirr[ch-6*192];
	row = jmirr[ch-6*192];
	supmod = 3;
	break;
      case 7:
	col =  imirr[ch-7*192];
	row =  24 + jmirr[ch-7*192];
	supmod = 3;
	break;
      case 8:
	col = imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 3;
	break;
      }
    break;
  }
}
void StPmdGeom::chain4(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{

  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  24 + jnorm[ch];
	supmod = 2;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  24 + jnorm[ch-192];
	supmod = 2;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = 24 + jnorm[ch-2*192];
	supmod = 2;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 41 - jnorm[ch-3*192];
	supmod = 2;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  41 -  jnorm[ch-4*192];
	supmod = 2;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  41 - jnorm[ch-5*192];
	supmod = 2;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 40 + jnorm[ch-6*192];
	supmod = 2;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 40 + jnorm[ch-7*192];
	supmod = 2;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 40 + jnorm[ch-8*192];
	supmod = 2;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  24 + jnorm[ch];
	supmod = 2;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  24 + jnorm[ch-192];
	supmod = 2;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = 24 + jnorm[ch-2*192];
	supmod = 2;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 41 - jnorm[ch-3*192];
	supmod = 2;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  41 -  jnorm[ch-4*192];
	supmod = 2;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  41 - jnorm[ch-5*192];
	supmod = 2;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 40 + jnorm[ch-6*192];
	supmod = 2;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 40 + jnorm[ch-7*192];
	supmod = 2;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 40 + jnorm[ch-8*192];
	supmod = 2;
	break;
      }

  }
}
void StPmdGeom::chain5(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = jmirr[ch];
	supmod = 3;
	break;
      case 1:
	col = 40 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 3;
	break;
      case 2:
	col = 40 + imirr[ch-2*192];
	row = 48 + jmirr[ch-2*192];
	supmod = 3;
	break;
      case 3:
	col = 41 - imirr[ch-3*192];
	row = 73 - jmirr[ch-3*192];
	supmod = 3;
	break;
      case 4:
	col = 41 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 3;
	break;
      case 5:
	col = 41 - imirr[ch-5*192];
	row = 25 - jmirr[ch-5*192];
	supmod = 3;
	break;
      case 6:
	col = 24 + imirr[ch-6*192];
	row = jmirr[ch-6*192];
	supmod = 3;
	break;
      case 7:
	col =  24 + imirr[ch-7*192];
	row =  24 + jmirr[ch-7*192];
	supmod = 3;
	break;
      case 8:
	col = 24 + imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 3;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = jmirr[ch];
	supmod = 3;
	break;
      case 1:
	col = 40 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 3;
	break;
      case 2:
	col = 40 + imirr[ch-2*192];
	row = 48 + jmirr[ch-2*192];
	supmod = 3;
	break;
      case 3:
	col = 41 - imirr[ch-3*192];
	row = 73 - jmirr[ch-3*192];
	supmod = 3;
	break;
      case 4:
	col = 41 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 3;
	break;
      case 5:
	col = 41 - imirr[ch-5*192];
	row = 25 - jmirr[ch-5*192];
	supmod = 3;
	break;
      case 6:
	col = 24 + imirr[ch-6*192];
	row = jmirr[ch-6*192];
	supmod = 3;
	break;
      case 7:
	col =  24 + imirr[ch-7*192];
	row =  24 + jmirr[ch-7*192];
	supmod = 3;
	break;
      case 8:
	col = 24 + imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 3;
	break;
      }
    break;
  }
}
void StPmdGeom::chain6(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  supmod = 0;
  col = 0;
  row = 0;
}
void StPmdGeom::chain7(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:

    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 17 - jnorm[ch-3*192];
	supmod = 4;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 -  jnorm[ch-4*192];
	supmod = 4;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 4;
	break;
      case 6:
	col = 40 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 4;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 24 + jnorm[ch-7*192];
	supmod = 4;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 24 + jnorm[ch-8*192];
	supmod = 4;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 17 - jnorm[ch-3*192];
	supmod = 4;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 -  jnorm[ch-4*192];
	supmod = 4;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 4;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 4;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 16 + jnorm[ch-7*192];
	supmod = 4;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 4;
	break;
      case 9:
	col = 48 + inorm[ch-9*192];
	row = 24 + jnorm[ch-9*192];
	supmod = 4;
	break;
      case 10:
	col = 24 + inorm[ch-10*192];
	row = 24 + jnorm[ch-10*192];
	supmod = 4;
	break;
      case 11:
	col = inorm[ch-11*192];
	row = 24 + jnorm[ch-11*192];
	supmod = 4;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 17 - jnorm[ch-3*192];
	supmod = 4;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 4;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 4;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 4;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 16 + jnorm[ch-7*192];
	supmod = 4;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 4;
	break;
      }
    break;
  }
}

void StPmdGeom::chain8(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:

    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col =  inorm[ch-2*192];
	row =  jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  17 - jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = 73 - inorm[ch-5*192];
	row = 17 - jnorm[ch-5*192];
	supmod = 5;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 5;
	break;
      case 7:
	col =  24 + inorm[ch-7*192];
	row =  16 + jnorm[ch-7*192];
	supmod = 5;
	break;
      case 8:
	col =  inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 5;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col =  inorm[ch-2*192];
	row =  jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  17 - jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = 73 - inorm[ch-5*192];
	row = 17 - jnorm[ch-5*192];
	supmod = 5;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 5;
	break;
      case 7:
	col =  24 + inorm[ch-7*192];
	row =  16 + jnorm[ch-7*192];
	supmod = 5;
	break;
      case 8:
	col =  inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 5;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = 40 + jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = 73 - inorm[ch-192];
	row = 41 - jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col = 48 + inorm[ch-2*192];
	row = 24 + jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col = 48 + inorm[ch-3*192];
	row = 16 + jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col = 24 + inorm[ch-4*192];
	row = 16 +  jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = inorm[ch-5*192];
	row = 16 +  jnorm[ch-5*192];
	supmod = 5;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 17 - jnorm[ch-6*192];
	supmod = 5;
	break;
      case 7:
	col = 49 - inorm[ch-7*192];
	row = 17 - jnorm[ch-7*192];
	supmod = 5;
	break;
      case 8:
	col = 73 - inorm[ch-8*192];
	row = 17 - jnorm[ch-8*192];
	supmod = 5;
	break;
      case 9:
	col = 48 + inorm[ch-9*192];
	row = jnorm[ch-9*192];
	supmod = 5;
	break;
      case 10:
	col = 24 + inorm[ch-10*192];
	row = jnorm[ch-10*192];
	supmod = 5;
	break;
      case 11:
	col = inorm[ch-11*192];
	row = jnorm[ch-11*192];
	supmod = 5;
	break;
      }
    break;
  }
}

void StPmdGeom::chain9(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  48 + jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  73 - inorm[ch-192];
	row =  65 - jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = 48 + inorm[ch-2*192];
	row = 64 + jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 25 - jnorm[ch-3*192];
	supmod = 6;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  25 -  jnorm[ch-4*192];
	supmod = 6;
	break;
      case 5:
	col  = 24 + inorm[ch-5*192];
	row =  8  + jnorm[ch-5*192];
	supmod = 6;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 8 + jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = 25 - inorm[ch-7*192];
	row = 9  - jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 49 - inorm[ch-8*192];
	row = 9  - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  48 + jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  73 - inorm[ch-192];
	row =  65 - jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = 48 + inorm[ch-2*192];
	row = 64 + jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 25 - jnorm[ch-3*192];
	supmod = 6;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  25 -  jnorm[ch-4*192];
	supmod = 6;
	break;
      case 5:
	col =  24 + inorm[ch-5*192];
	row =  8  + jnorm[ch-5*192];
	supmod = 6;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 8 + jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = 25 - inorm[ch-7*192];
	row = 9  - jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 49 - inorm[ch-8*192];
	row = 9  - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone){
    case 0:
      col = 48+inorm[ch];
      row = 40+jnorm[ch];
      supmod = 4;
      break;
    case 1:
      col = 24+inorm[ch-192];
      row = 40+jnorm[ch-192];
      supmod = 4;
      break;
    case 2:
      col = inorm[ch-2*192];
      row = 40+jnorm[ch-2*192];
      supmod = 4;
      break;
    case 3:
      col = 25 - inorm[ch-3*192];
      row = 41 - jnorm[ch-3*192];
      supmod = 4;
      break;
    case 4:
      col = 49 - inorm[ch-4*192];
      row = 41 - jnorm[ch-4*192];
      supmod = 4;
      break;
    case 5:
      col = 73 - inorm[ch-5*192];
      row = 41 - jnorm[ch-5*192];
      supmod = 4;
      break;
    case 6:
      col = 48+inorm[ch-6*192];
      row = 24+jnorm[ch-6*192];
      supmod = 4;
      break;
    case 7:
      col = 24+inorm[ch-7*192];
      row = 24+jnorm[ch-7*192];
      supmod = 4;
      break;
    case 8:
      col = inorm[ch-8*192];
      row = 24+jnorm[ch-8*192];
      supmod = 4;
      break;
    }
    break;
  }
}

void StPmdGeom::chain10(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = 24 + jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = 24 + jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col =  inorm[ch-2*192];
	row =  24 + jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  41 - jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  41 - jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = 73 - inorm[ch-5*192];
	row = 41 - jnorm[ch-5*192];
	supmod = 5;  
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 40 + jnorm[ch-6*192];
	supmod = 5;
	break;
      case 7:
	col =  24 + inorm[ch-7*192];
	row =  40 + jnorm[ch-7*192];
	supmod = 5;
	break;
      case 8:
	col =  inorm[ch-8*192];
	row = 40 + jnorm[ch-8*192];
	supmod = 5;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = 24 + jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = 24 + jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col =  inorm[ch-2*192];
	row =  24 + jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  41 - jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  41 - jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = 73 - inorm[ch-5*192];
	row = 41 - jnorm[ch-5*192];
	supmod = 5;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 40 + jnorm[ch-6*192];
	supmod = 5;
	break;
      case 7:
	col =  24 + inorm[ch-7*192];
	row =  40 + jnorm[ch-7*192];
	supmod = 5;
	break;
      case 8:
	col =  inorm[ch-8*192];
	row = 40 + jnorm[ch-8*192];
	supmod = 5;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 24 + inorm[ch];
	row = 24 + jnorm[ch];
	supmod = 5;
	break;
      case 1:
	col = inorm[ch-192];
	row = 24 + jnorm[ch-192];
	supmod = 5;
	break;
      case 2:
	col = 25 - inorm[ch-2*192];
	row = 41 - jnorm[ch-2*192];
	supmod = 5;
	break;
      case 3:
	col = 49 - inorm[ch-3*192];
	row = 41 - jnorm[ch-3*192];
	supmod = 5;
	break;
      case 4:
	col = 24 + inorm[ch-4*192];
	row = 40 + jnorm[ch-4*192];
	supmod = 5;
	break;
      case 5:
	col = inorm[ch-5*192];
	row = 40 + jnorm[ch-5*192];
	supmod = 5;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 9 - jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = inorm[ch-7*192];
	row = 8 + jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 25 - inorm[ch-8*192];
	row = 25 - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  }    
}
void StPmdGeom::chain11(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  24 + inorm[ch];
	row =  48 + jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  49 - inorm[ch-192];
	row =  65 - jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = 24 + inorm[ch-2*192];
	row = 64 + jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 49 - jnorm[ch-3*192];
	supmod = 6;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  49 -  jnorm[ch-4*192];
	supmod = 6;
	break;
      case 5:
	col =  24 + inorm[ch-5*192];
	row =  32  + jnorm[ch-5*192];
	supmod = 6;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 32 + jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = 25 - inorm[ch-7*192];
	row = 33  - jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 49 - inorm[ch-8*192];
	row = 33  - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  24 + inorm[ch];
	row =  48 + jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col =  49 - inorm[ch-192];
	row =  65 - jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = 24 + inorm[ch-2*192];
	row = 64 + jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 25 - inorm[ch-3*192];
	row = 49 - jnorm[ch-3*192];
	supmod = 6;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  49 -  jnorm[ch-4*192];
	supmod = 6;
	break;
      case 5:
	col =  24 + inorm[ch-5*192];
	row =  32  + jnorm[ch-5*192];
	supmod = 6;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 32 + jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = 25 - inorm[ch-7*192];
	row = 33  - jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 49 - inorm[ch-8*192];
	row = 33  - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 48 + inorm[ch];
	row = 48 + jnorm[ch];
	supmod = 4;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = 48 + jnorm[ch-192];
	supmod = 4;
	break;
      case 2:
	col = 49 - inorm[ch-2*192];
	row = 65 - jnorm[ch-2*192];
	supmod = 4;
	break;
      case 3:
	col = 73 - inorm[ch-3*192];
	row = 65 - jnorm[ch-3*192];
	supmod = 4;
	break;
      case 4:
	col = 48 + inorm[ch-4*192];
	row = 64 + jnorm[ch-4*192];
	supmod = 4;
	break;
      case 5:
	col = 24 + inorm[ch-5*192];
	row = 64 + jnorm[ch-5*192];
	supmod = 4;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 49 - jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = inorm[ch-7*192];
	row = 32 + jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 25 - inorm[ch-8*192];
	row = 33 - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  }
}

void StPmdGeom::chain12(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  25 - inorm[ch];
	row =  25 - jnorm[ch];
	supmod = 7;
	break;
      case 1:
	col =  49 - inorm[ch-192];
	row =  25 - jnorm[ch-192];
	supmod = 7;
	break;
      case 2:
	col = 24 + inorm[ch-2*192];
	row = 8  + jnorm[ch-2*192];
	supmod = 7;
	break;
      case 3:
	col = inorm[ch-3*192];
	row = 8 + jnorm[ch-3*192];
	supmod = 7;
	break;
      case 4:
	col =  25 - inorm[ch-4*192];
	row =  9  - jnorm[ch-4*192];
	supmod = 7;
	break;
      case 5:
	col =  49 - inorm[ch-5*192];
	row =  9  - jnorm[ch-5*192];
	supmod = 7;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  25 - inorm[ch];
	row =  25 - jnorm[ch];
	supmod = 7;
	break;
      case 1:
	col =  49 - inorm[ch-192];
	row =  25 - jnorm[ch-192];
	supmod = 7;
	break;
      case 2:
	col = 24 + inorm[ch-2*192];
	row = 8  + jnorm[ch-2*192];
	supmod = 7;
	break;
      case 3:
	col = inorm[ch-3*192];
	row = 8 + jnorm[ch-3*192];
	supmod = 7;
	break;
      case 4:
	col =  25 - inorm[ch-4*192];
	row =  9  - jnorm[ch-4*192];
	supmod = 7;
	break;
      case 5:
	col =  49 - inorm[ch-5*192];
	row =  9  - jnorm[ch-5*192];
	supmod = 7;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 25 - inorm[ch];
	row = 49 - jnorm[ch];
	supmod = 7;
	break;
      case 1:
	col = 49 - inorm[ch-192];
	row = 49 - jnorm[ch-192];
	supmod = 7;
	break;
      case 2:
	col = 24 + inorm[ch-2*192];
	row = 32 + jnorm[ch-2*192];
	supmod = 7;
	break;
      case 3:
	col = inorm[ch-3*192];
	row = 32 + jnorm[ch-3*192];
	supmod = 7;
	break;
      case 4:
	col = 25 - inorm[ch-4*192];
	row = 33 - jnorm[ch-4*192];
	supmod = 7; 
	break;
      case 5: 
	col = 49 - inorm[ch-5*192];
	row = 33 - jnorm[ch-5*192];
	supmod = 7;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 25 - jnorm[ch-6*192];
	supmod = 7;
	break;
      case 7:
	col = 49 - inorm[ch-7*192];
	row = 25 - jnorm[ch-7*192];
	supmod = 7;
	break;
      case 8:
	col = 24 + inorm[ch-8*192];
	row = 8 + jnorm[ch-8*192];
	supmod = 7;
	break;
      case 9:
	col = inorm[ch-9*192];
	row = 8 + jnorm[ch-9*192];
	supmod = 7;
	break;
      case 10:
	col = 25 -inorm[ch-10*192];
	row = 9 - jnorm[ch-10*192];
	supmod = 7;
	break;
      case 11:
	col = 49 -inorm[ch-11*192];
	row = 9 - jnorm[ch-11*192];
	supmod = 7;
	break;
      }
    break;
  } 
}

void StPmdGeom::chain13(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  73 - inorm[ch];
	row =  9 - jnorm[ch];
	supmod = 6;
	break;
      case 1:
	col =  48 + inorm[ch-192];
	row =  8  + jnorm[ch-192];
	supmod = 6;
	break;
      case 2:
	col = 73 - inorm[ch-2*192];
	row = 25 - jnorm[ch-2*192];
	supmod = 6;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  49 - jnorm[ch-3*192];
	supmod = 7;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  49 - jnorm[ch-4*192];
	supmod = 7;
	break;
      case 5:
	col = 24 + inorm[ch-5*192];
	row = 32  + jnorm[ch-5*192];
	supmod = 7;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 32 + jnorm[ch-6*192];
	supmod = 7;
	break;
      case 7:
	col =  25 - inorm[ch-7*192];
	row =  33  - jnorm[ch-7*192];
	supmod = 7;
	break;
      case 8:
	col =  49 - inorm[ch-8*192];
	row =  33  - jnorm[ch-8*192];
	supmod = 7;
	break;
      }
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  73 - inorm[ch];
	row =  9 - jnorm[ch];
	supmod = 6;
	break;
      case 1:
	col =  48 + inorm[ch-192];
	row =  8  + jnorm[ch-192];
	supmod = 6;
	break;
      case 2:
	col = 73 - inorm[ch-2*192];
	row = 25 - jnorm[ch-2*192];
	supmod = 6;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  49 - jnorm[ch-3*192];
	supmod = 7;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  49 - jnorm[ch-4*192];
	supmod = 7;
	break;
      case 5:
	col = 24 + inorm[ch-5*192];
	row = 32  + jnorm[ch-5*192];
	supmod = 7;
	break;
      case 6:
	col = inorm[ch-6*192];
	row = 32 + jnorm[ch-6*192];
	supmod = 7;
	break;
      case 7:
	col =  25 - inorm[ch-7*192];
	row =  33  - jnorm[ch-7*192];
	supmod = 7;
	break;
      case 8:
	col =  49 - inorm[ch-8*192];
	row =  33  - jnorm[ch-8*192];
	supmod = 7;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 49 - inorm[ch];
	row = 49 - jnorm[ch];
	supmod = 6;
	break;
      case 1:
	col = 24 + inorm[ch-192];
	row = 32 + jnorm[ch-192];
	supmod = 6;
	break;
      case 2:
	col = 49 - inorm[ch-2*192];
	row = 33 - jnorm[ch-2*192];
	supmod = 6;
	break;
      case 3:
	col = 49 - inorm[ch-3*192];
	row = 25 - jnorm[ch-3*192];
	supmod = 6;
	break;
      case 4:
	col = 73 - inorm[ch-4*192];
	row = 25 - jnorm[ch-4*192];
	supmod = 6;
	break;
      case 5:
	col = 48 + inorm[ch-5*192];
	row = 8 + jnorm[ch-5*192];
	supmod = 6;
	break;
      case 6:
	col = 24 + inorm[ch-6*192];
	row = 8 + jnorm[ch-6*192];
	supmod = 6;
	break;
      case 7:
	col = 49 - inorm[ch-7*192];
	row = 9 - jnorm[ch-7*192];
	supmod = 6;
	break;
      case 8:
	col = 73 - inorm[ch-8*192];
	row = 9 - jnorm[ch-8*192];
	supmod = 6;
	break;
      }
    break;
  }
}

void StPmdGeom::chain14(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    supmod = 0;
    col = 0;
    row = 0;
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = 73 - jmirr[ch];
	supmod = 8;
	break;
      case 1:
	col= 40 + imirr[ch-192];
	row= 49 - jmirr[ch-192];
	supmod = 8;
	break;
      case 2:
	col= 40 + imirr[ch-2*192];
	row= 25 - jmirr[ch-2*192];
	supmod = 8;
	break;
      case 3:
	col = 41 - imirr[ch-3*192];
	row = jmirr[ch-3*192];
	supmod = 8;
	break;
      case 4:
	col = 41 - imirr[ch-4*192];
	row = 24 + jmirr[ch-4*192];
	supmod = 8;
	break;
      case 5:
	col = 41 - imirr[ch-5*192];
	row = 48 + jmirr[ch-5*192];
	supmod = 8;
	break;
      case 6:
	col= 24 + imirr[ch-6*192];
	row= 73 - jmirr[ch-6*192];
	supmod = 8;
	break;
      case 7:
	col=  24 + imirr[ch-7*192];
	row=  49 - jmirr[ch-7*192];
	supmod = 8;
	break;
      case 8:
	col= 24 + imirr[ch-8*192];
	row= 25 - jmirr[ch-8*192];
	supmod = 8;
	break;
      }
    break;
  }
}

void StPmdGeom::chain15(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = jmirr[ch];
	supmod = 9;
	break;
      case 1:
	col = 40 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 9;
	break;
      case 2:
	col = 41 - imirr[ch-2*192];
	row = 49 - jmirr[ch-2*192];
	supmod = 9;
	break;
      case 3:
	col = 41 - imirr[ch-3*192];
	row = 25 - jmirr[ch-3*192];
	supmod = 9;
	break;
      case 4:
	col = 24 + imirr[ch-4*192];
	row = jmirr[ch-4*192];
	supmod = 9;
	break;
      case 5:
	col = 24 + imirr[ch-5*192];
	row = 24 + jmirr[ch-5*192];
	supmod = 9;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = jmirr[ch];
	supmod = 9;
	break;
      case 1:
	col = 40 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 9;
	break;
      case 2:
	col = 41 - imirr[ch-2*192];
	row = 49 - jmirr[ch-2*192];
	supmod = 9;
	break;
      case 3:
	col = 41 - imirr[ch-3*192];
	row = 25 - jmirr[ch-3*192];
	supmod = 9;
	break;
      case 4:
	col = 24 + imirr[ch-4*192];
	row = jmirr[ch-4*192];
	supmod = 9;
	break;
      case 5:
	col = 24 + imirr[ch-5*192];
	row = 24 + jmirr[ch-5*192];
	supmod = 9;
	break;
      case 6:
        col = imirr[ch-6*192];
        row = jmirr[ch-6*192];
        supmod = 9;
        break;
      case 7:
        col = 8 + imirr[ch-7*192];
        row = 25 - jmirr[ch-7*192];
        supmod = 9;
        break;
      case 8:
        col = 16 + imirr[ch-8*192];
        row = jmirr[ch-8*192];
        supmod = 9;
        break;
      }
    break;
  }
}

void StPmdGeom::chain16(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 16 + imirr[ch];
	row = jmirr[ch];
	supmod = 9;
	break;
      case 1:
	col = 16 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 9;
	break;
      case 2:
	col = 17 - imirr[ch-2*192];
	row = 49 - jmirr[ch-2*192];
	supmod = 9;
	break;
      case 3:
	col = 17 - imirr[ch-3*192];
	row = 25 - jmirr[ch-3*192];
	supmod = 9;
	break;
      case 4:
	col = imirr[ch-4*192];
	row = jmirr[ch-4*192];
	supmod = 9;
	break;
      case 5:
	col = imirr[ch-5*192];
	row = 24 + jmirr[ch-5*192];
	supmod = 9;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 9 - imirr[ch];
	row = jmirr[ch];
	supmod = 8;
	break;
      case 1:
	col = 8 + imirr[ch-192];
	row = 25 - jmirr[ch-192];
	supmod = 8;
	break;
      case 2:
	col = 25 - imirr[ch-2*192];
	row = jmirr[ch-2*192];
	supmod = 8;
	break;
      case 3:
	col = 17 - imirr[ch-3*192];
	row = 24 + jmirr[ch-3*192];
	supmod = 8;
	break;
      case 4:
	col = 17 - imirr[ch-4*192];
	row = 48 + jmirr[ch-4*192];
	supmod = 8;
	break;
      case 5:
	col = 16 + imirr[ch-5*192];
	row = 73 - jmirr[ch-5*192];
	supmod = 8;
	break;
      case 6:
	col = 16 + imirr[ch-6*192];
	row = 49 - jmirr[ch-6*192];
	supmod = 8;
	break;
      case 7:
	col = imirr[ch-7*192];
	row = 73 - jmirr[ch-7*192];
	supmod = 8;
	break;
      case 8:
	col = imirr[ch-8*192];
	row = 49 - jmirr[ch-8*192];
	supmod = 8;
	break;
      }
    break;
  }
}

void StPmdGeom::chain17(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  24 + jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  24 + jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = 24 + jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  41 - jnorm[ch-3*192];
	supmod = 11;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  41 - jnorm[ch-4*192];
	supmod = 11;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  41 - jnorm[ch-5*192];
	supmod = 11;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 40 + jnorm[ch-6*192];
	supmod = 11;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 40 + jnorm[ch-7*192];
	supmod = 11;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 40 + jnorm[ch-8*192];
	supmod = 11;
	break;
    }
    break;
  case 8:
    supmod = 0;
    col = 0;
    row = 0;
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 25 - inorm[ch];
	row = 33 - jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col = 49 - inorm[ch-192];
	row = 33 - jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col = 73 - inorm[ch-2*192];
	row = 33 - jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col = 48 + inorm[ch-3*192];
	row = 32 + jnorm[ch-3*192];
	supmod = 11;
	break;
      case 4:
	col = 24 + inorm[ch-4*192];
	row = 32 + jnorm[ch-4*192];
	supmod = 11;
	break;
      case 5:
	col = inorm[ch-5*192];
	row = 32 + jnorm[ch-5*192];
	supmod = 11;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 49 - jnorm[ch-6*192];
	supmod = 11;
	break;
      case 7:
	col = 49 - inorm[ch-7*192];
	row = 49 - jnorm[ch-7*192];
	supmod = 11;
	break;
      case 8:
	col = 73 - inorm[ch-8*192];
	row = 49 - jnorm[ch-8*192];
	supmod = 11;
	break;
      }
    break;
  }
}

void StPmdGeom::chain18(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  
  switch(year){
  case 6:
    
    supmod = 0;
    col = 0;
    row = 0;
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col =  64 + imirr[ch];
	row =  48 + jmirr[ch];
	supmod = 10;
	break;
      case 1:
	col =  65 - imirr[ch-192];
	row =  73 - jmirr[ch-192];
	supmod = 10;
	break;
      case 2:
	col = 48 + imirr[ch-2*192];
	row = 48 + jmirr[ch-2*192];
	supmod = 10;
	break;
      case 3:
	col = 40 +  imirr[ch-3*192];
	row =  24 +  jmirr[ch-3*192];
	supmod = 10;
	break;
      case 4:
	col =  40 + imirr[ch-4*192];
	row =  48 + jmirr[ch-4*192];
	supmod = 10;
	break;
      case 5:
	col =  41 - imirr[ch-5*192];
	row =  73 - jmirr[ch-5*192];
	supmod = 10;
	break;
      case 6:
	col = 41 - imirr[ch-6*192];
	row = 49 - jmirr[ch-6*192];
	supmod = 10;
	break;
      case 7:
	col = 24 + imirr[ch-7*192];
	row = 24 + jmirr[ch-7*192];
	supmod = 10;
	break;
      case 8:
	col = 24 + imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 10;
	break;
      }
    break;
  }
}

void StPmdGeom::chain19(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    switch(zone)
      {
      case 0:
	col =  48 + inorm[ch];
	row =  jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col =  24 + inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col = inorm[ch-2*192];
	row = jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col =  25 - inorm[ch-3*192];
	row =  17 - jnorm[ch-3*192];
	supmod = 11;
	break;
      case 4:
	col =  49 - inorm[ch-4*192];
	row =  17 - jnorm[ch-4*192];
	supmod = 11;
	break;
      case 5:
	col =  73 - inorm[ch-5*192];
	row =  17 - jnorm[ch-5*192];
	supmod = 11;
	break;
      case 6:
	col = 48 + inorm[ch-6*192];
	row = 16 + jnorm[ch-6*192];
	supmod = 11;
	break;
      case 7:
	col = 24 + inorm[ch-7*192];
	row = 16 + jnorm[ch-7*192];
	supmod = 11;
	break;
      case 8:
	col = inorm[ch-8*192];
	row = 16 + jnorm[ch-8*192];
	supmod = 11;
	break;
      }
    break;
  case 8:
    
    switch(zone)
      {
      case 0:
	col =  24 + inorm[ch];
	row =  jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col =  inorm[ch-192];
	row =  jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col = 25 - inorm[ch-2*192];
	row = 17 - jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col =  49 - inorm[ch-3*192];
	row =  17 - jnorm[ch-3*192];
	supmod = 11;
	break;
      case 4:
	col =  24 + inorm[ch-4*192];
	row =  16 + jnorm[ch-4*192];
	supmod = 11;
	break;
      case 5:
	col = inorm[ch-5*192];
	row = 16 + jnorm[ch-5*192];
	supmod = 11;
	break;
      case 6:
	col =  24 + inorm[ch-6*192];
	row =  24 + jnorm[ch-6*192];
	supmod = 11;
	break;
      case 7:
	col =  inorm[ch-7*192];
	row = 24 + jnorm[ch-7*192];
	supmod = 11;
	break;
      case 8:
	col= 25 - inorm[ch-8*192];
	row= 41 - jnorm[ch-8*192];
	supmod = 11;
	break;
      case 9:
	col= 49 - inorm[ch-9*192];
	row= 41 - jnorm[ch-9*192];
	supmod = 11;
	break;
      case 10:
	col =  24 + inorm[ch-10*192];
	row =  40 + jnorm[ch-10*192];
	supmod = 11;
	break;
      case 11:
	col = inorm[ch-11*192];
	row = 40 + jnorm[ch-11*192];
	supmod = 11;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 25 -inorm[ch];
	row = 9 - jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col = 49 -inorm[ch-192];
	row = 9 - jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col = 73 -inorm[ch-2*192];
	row = 9 - jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col = 48 + inorm[ch-3*192]; 
	row = 8+ jnorm[ch-3*192]; 
	supmod = 11;
	break;
      case 4:
	col = 24 + inorm[ch-4*192]; 
	row = 8+ jnorm[ch-4*192]; 
	supmod = 11;
	break;
      case 5:
	col = inorm[ch-5*192]; 
	row = 8+ jnorm[ch-5*192]; 
	supmod = 11;
	break;
      case 6:
	col = 25 - inorm[ch-6*192];
	row = 25 - jnorm[ch-6*192];
	supmod = 11;
	break;
      case 7:
	col = 49 - inorm[ch-7*192];
	row = 25 - jnorm[ch-7*192];
	supmod = 11;
	break;
      case 8:
	col = 73 - inorm[ch-8*192];
	row = 25 - jnorm[ch-8*192];
	supmod = 11;
	break;
      }
    break;
  }
}

void StPmdGeom::chain20(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{

  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    supmod = 0;
    col = 0;
    row = 0;
    break;
  case 8:

    switch(zone)
      {
      case 0:
	col =  48 +  inorm[ch];
	row =  jnorm[ch];
	supmod = 11;
	break;
      case 1:
	col =  73 - inorm[ch-192];
	row =  17 - jnorm[ch-192];
	supmod = 11;
	break;
      case 2:
	col =  48 + inorm[ch-2*192];
	row =  16 + jnorm[ch-2*192];
	supmod = 11;
	break;
      case 3:
	col =  48 + inorm[ch-3*192];
	row =  24 + jnorm[ch-3*192];
	supmod = 11;
	break;
      case 4:
	col = 73 - inorm[ch-4*192];
	row = 41 - jnorm[ch-4*192];
	supmod = 11;
	break;
      case 5:
	col = 48 + inorm[ch-5*192];
	row = 40 - jnorm[ch-5*192];
	supmod = 11;
	break;
      case 6:
	col =  64 + imirr[ch-6*192];
	row =   jmirr[ch-6*192];
	supmod = 10;
	break;
      case 7:
	col =  64 + imirr[ch-7*192];
	row =  24 + jmirr[ch-7*192];
	supmod = 10;
	break;
      case 8:
	col =  65 - imirr[ch-8*192];
	row =  49 - jmirr[ch-8*192];
	supmod = 10;
	break;
      case 9:
	col =  65 - imirr[ch-9*192];
	row = 25 - jmirr[ch-9*192];
	supmod = 10;
	break;
      case 10:
	col = 48 + imirr[ch-10*192];
	row =  jmirr[ch-10*192];
	supmod = 10;
	break;
      case 11:
	col = 48 + imirr[ch-11*192];
	row = 24 + jmirr[ch-11*192];
	supmod = 10;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 64 + imirr[ch];
	row = jmirr[ch];
	supmod = 10;
	break;
      case 1:
	col = 64 + imirr[ch-192];
	row = 24 + jmirr[ch-192];
	supmod = 10;
	break;
      case 2:
	col = 65 - imirr[ch-2*192];
	row = 49 - jmirr[ch-2*192];
	supmod = 10;
	break;
      case 3:
	col = 65 - imirr[ch-3*192];
	row = 25 - jmirr[ch-3*192];
	supmod = 10;
	break;
      case 4:
	col =  48 + imirr[ch-4*192];
	row = jmirr[ch-4*192];
	supmod = 10;
	break;
      case 5:
	col = 48 + imirr[ch-5*192];
	row = 24 + jmirr[ch-5*192]; 
	supmod = 10;
	break;
      case 6:
	supmod = 0;
	col = 0;
	row = 0;
	break;
      case 7:
	supmod = 0;
	col = 0;
	row = 0;
	break;
      case 8:
	supmod = 0;
	col = 0;
	row = 0;
	break;
      }
    break;
  }
}

void StPmdGeom::chain21(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:{
    
    switch(zone)
      {
      case 0:
	col = 9 - imirr[ch];
	row = 73 - jmirr[ch];
	supmod = 12;
	break;
      case 1:
	col = 8 + imirr[ch-192];
	row = 48 + jmirr[ch-192];
	supmod = 12;
	break;
      case 2:
	col = 25 - imirr[ch-2*192];
	row = 73 - jmirr[ch-2*192];
	supmod = 12;
	break;
      case 3:
	col = 33 - imirr[ch-3*192];
	row = 73 - jmirr[ch-3*192];
	supmod = 12;
	break;
      case 4:
	col = 33 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 12;
	break;
      case 5:
	col = 32 + imirr[ch-5*192];
	row = 24 + jmirr[ch-5*192];
	supmod = 12;
	break;
      case 6:
	col = 32 + imirr[ch-6*192];
	row = 48 + jmirr[ch-6*192];
	supmod = 12;
	break;
      case 7:
	col =  49 - imirr[ch-7*192];
	row =  73 - jmirr[ch-7*192];
	supmod = 12;
	break;
      case 8:
	col = 49 - imirr[ch-8*192];
	row = 49 - jmirr[ch-8*192];
	supmod = 12;
	break;
      }
    break;
  }
  case 8:
  case 10:
  case 11:
  case 12:
    {
      supmod = 0;
      col = 0;
      row = 0;
      break;
    }
  }
}

void StPmdGeom::chain22(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  
  switch(year){
  case 6:
    
    supmod = 0;
    col = 0;
    row = 0;
    break;
  case 8:
    switch(zone)
      {
      case 0:
	col = 40 + imirr[ch];
	row = 25 - jmirr[ch];
	supmod = 10;
	break;
      case 1:
	col = 41 - imirr[ch-192];
	row =  jmirr[ch-192];
	supmod = 10;
	break;
      case 2:
	col = 24 + imirr[ch-2*192];
	row = 25 - jmirr[ch-2*192];
	supmod = 10;
	break;
      case 3:
	col = 25 - imirr[ch-3*192];
	row = 49 - jmirr[ch-3*192];
	supmod = 10;
	break;
      case 4:
	col = 25 - imirr[ch-4*192];
	row = 25 - jmirr[ch-4*192];
	supmod = 10;
	break;
      case 5:
	col = 8 + imirr[ch-5*192];
	row = jmirr[ch-5*192];
	supmod = 10;
	break;
      case 6:
	col = 8 + imirr[ch-6*192];
	row = 24 + jmirr[ch-6*192];
	supmod = 10;
	break;
      case 7:
	col =  9 - imirr[ch-7*192];
	row =  49 - jmirr[ch-7*192];
	supmod = 10;
	break;
      case 8:
	col = 9 - imirr[ch-8*192];
	row = 25 - jmirr[ch-8*192];
	supmod = 10;
	break;
      }
    break;
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = imirr[ch];
	row = jmirr[ch];
	supmod = 10;
	break;
      case 1:
	col = imirr[ch-1*192];
	row = 24 + jmirr[ch -1*192];
	supmod = 10;
	break;
      case 2:
	col = 17 - imirr[ch-2*192];
	row = 49 - jmirr[ch-2*192];
	supmod = 10;
	break;
      case 3: 
	col = 17 - imirr[ch-3*192];
	row = 25 - jmirr[ch-3*192];
	supmod = 10;
	break;
      case 4:
	col = 16 + imirr[ch-4*192]; 
	row = jmirr[ch-4*192];
	supmod = 10;
	break;
      case 5:
	col = 16 + imirr[ch-5*192]; 
	row = 24 + jmirr[ch-5*192];
	supmod = 10;
	break;
      case 6:
	col = 24 + imirr[ch-6*192];
	row = jmirr[ch-6*192];
	supmod = 10;
	break;
      case 7: 
	col = 41 - imirr[ch-7*192];
	row = 25 - jmirr[ch-7*192];
	supmod = 10;
	break;
      case 8:
	col = 40 + imirr[ch-8*192];
	row = jmirr[ch-8*192];
	supmod = 10;
	break;
	
      }
    break;
  }
}

void StPmdGeom::chain23(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col =  57 - imirr[ch];
	row =  73 - jmirr[ch];
	supmod = 12;
	break;
      case 1:
	col =  57 - imirr[ch-192];
	row =  49 - jmirr[ch-192];
	supmod = 12;
	break;
      case 2:
	col =  57 - imirr[ch-2*192];
	row =  25 - jmirr[ch-2*192];
	supmod = 12;
	break;
      case 3:
	col = 56 + imirr[ch-3*192];
	row = jmirr[ch-3*192];
	supmod = 12;
	break;
      case 4:
	col = 56 + imirr[ch-4*192];
	row = 24 + jmirr[ch-4*192];
	supmod = 12;
	break;
      case 5:
	col = 56 + imirr[ch-5*192];
	row = 48 + jmirr[ch-5*192];
	supmod = 12;
	break;
      case 6:
	col = 73 - imirr[ch-6*192];
	row = 73 - jmirr[ch-6*192];
	supmod = 12;
	break;
      case 7:
	col =  73 - imirr[ch-7*192];
	row =  49 - jmirr[ch-7*192];
	supmod = 12;
	break;
      case 8:
	col = 73 - imirr[ch-8*192];
	row = 25 - jmirr[ch-8*192];
	supmod = 12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col =  73 - imirr[ch];
	row =  49 - jmirr[ch];
	supmod = 12;
	break;
      case 1:
	col =  73 - imirr[ch-192];
	row =  25 - jmirr[ch-192];
	supmod = 12;
	break;
      case 2:
	col =  56 + imirr[ch-2*192];
	row =   jmirr[ch-2*192];
	supmod = 12;
	break;
      case 3:
	col = 56 + imirr[ch-3*192];
	row = 24 + jmirr[ch-3*192];
	supmod = 12;
	break;
      case 4:
	col = 57 - imirr[ch-4*192];
	row = 49 - jmirr[ch-4*192];
	supmod = 12;
	break;
      case 5:
	col = 57 - imirr[ch-5*192];
	row = 25 - jmirr[ch-5*192];
	supmod = 12;
	break;
      case 6:
	col = 33 - imirr[ch-6*192];
	row = 73 - jmirr[ch-6*192];
	supmod = 12;
	break;
      case 7:
	col = 33 - imirr[ch-7*192];
	row = 49 - jmirr[ch-7*192];
	supmod = 12;
	break;
      case 8:
	col = 32 + imirr[ch-8*192];
	row = 24 + jmirr[ch-8*192];
	supmod = 12;
	break;
      case 9:
	col =  32 + imirr[ch-9*192];
	row =  48 + jmirr[ch-9*192];
	supmod = 12;
	break;
      case 10:
	col = 49 - imirr[ch-10*192];
	row = 73 - jmirr[ch-10*192];
	supmod = 12;
	break;
      case 11:
	col = 49 - imirr[ch-11*192];
	row = 49 - jmirr[ch-11*192];
	supmod = 12;
	break;
      }
    break;
  }
}
void StPmdGeom::chain24(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 97 - imirr[ch];
	row = 73 - jmirr[ch];
	supmod = 12;
	break;
      case 1:
	col = 97 - imirr[ch-192];
	row = 49 - jmirr[ch-192];
	supmod = 12;
	break;
      case 2:
	col = 97 - imirr[ch-2*192];
	row = 25 - jmirr[ch-2*192];
	supmod = 12;
	break;
      case 3:
	col = 80 + imirr[ch-3*192];
	row = jmirr[ch-3*192];
	supmod = 12;
	break;
      case 4:
	col = 80 + imirr[ch-4*192];
	row = 24 + jmirr[ch-4*192];
	supmod = 12;
	break;
      case 5:
	col = 80 + imirr[ch-5*192];
	row = 48 + jmirr[ch-5*192];
	supmod = 12;
	break;
      case 6:
	col = 81 - imirr[ch-6*192];
	row = 73 - jmirr[ch-6*192];
	supmod = 12;
	break;
      case 7:
	col =  81 - imirr[ch-7*192];
	row =  49 - jmirr[ch-7*192];
	supmod = 12;
	break;
      case 8:
	col = 81 - imirr[ch-8*192];
	row = 25 - jmirr[ch-8*192];
	supmod = 12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 97 - imirr[ch];
	row = 73 - jmirr[ch];
	supmod = 12;
	break;
      case 1:
	col = 97 - imirr[ch-192];
	row = 49 - jmirr[ch-192];
	supmod = 12;
	break;
      case 2:
	col = 97 - imirr[ch-2*192];
	row = 25 - jmirr[ch-2*192];
	supmod = 12;
	break;
      case 3:
	col = 80 + imirr[ch-3*192];
	row = jmirr[ch-3*192];
	supmod = 12;
	break;
      case 4:
	col = 80 + imirr[ch-4*192];
	row = 24 + jmirr[ch-4*192];
	supmod = 12;
	break;
      case 5:
	col = 80 + imirr[ch-5*192];
	row = 48 + jmirr[ch-5*192];
	supmod = 12;
	break;
      case 6:
	col = 81 - imirr[ch-6*192];
	row = 73 - jmirr[ch-6*192];
	supmod = 12;
	break;
      case 7:
	col =  81 - imirr[ch-7*192];
	row =  49 - jmirr[ch-7*192];
	supmod = 12;
	break;
      case 8:
	col = 81 - imirr[ch-8*192];
	row = 48 + jmirr[ch-8*192];
	supmod = 12;
	break;
      case 9:
	col = 57 - imirr[ch-9*192];
	row = 73 - jmirr[ch-9*192];
	supmod = 12;
	break;
      case 10:
	col =  56 + imirr[ch-10*192];
	row =  48 + jmirr[ch-10*192];
	supmod = 12;
	break;
      case 11:
	col =  73 - imirr[ch-11*192];
	row = 73 - jmirr[ch-11*192];
	supmod = 12;
	break;
      }
    break;
  }
}

void StPmdGeom::chain25(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jmirr[ch];
      row = 9 - imirr[ch];
      supmod = 1+12;
      break;
    case 1:
      col = 25 - jmirr[ch-192];
      row = 9 - imirr[ch-192];
      supmod = 1+12;
      break;
    case 2:
      col = jmirr[ch-2*192];
      row = 8 + imirr[ch-2*192];
      supmod = 1+12;
      break;
    case 3:
      col = 24 + jmirr[ch-3*192];
      row = 8 + imirr[ch-3*192];
      supmod = 1+12;
      break;
    case 4:
      col = 49 - jmirr[ch-4*192];
      row = 25 - imirr[ch-4*192];
      supmod = 1+12;
      break;
    case 5:
      col = 25 - jmirr[ch-5*192];
      row = 25 - imirr[ch-5*192];
      supmod = 1+12;
      break;
    case 6:
      col = 25 - jmirr[ch-6*192];
      row = 33 - imirr[ch-6*192];
      supmod = 1+12;
      break;
    case 7:
      col = jmirr[ch-7*192];
      row = 32 + imirr[ch-7*192];
      supmod = 1+12;
      break;
    case 8:
      col = 25 - jmirr[ch-8*192];
      row = 49 - imirr[ch-8*192];
      supmod = 1+12;
      break;
    }
}

void StPmdGeom::chain26(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 73 - jmirr[ch];
      row = 9 - imirr[ch];
      supmod = 2+12;
      break;
    case 1:
      col = 49 - jmirr[ch-192];
      row = 9 - imirr[ch-192];
      supmod = 2+12;
      break;
    case 2:
      col = 25 - jmirr[ch-2*192];
      row = 9 - imirr[ch-2*192];
      supmod = 2+12;
      break;
    case 3:
      col = jmirr[ch-3*192];
      row = 8 + imirr[ch-3*192];
      supmod = 2+12;
      break;
    case 4:
      col = 24 + jmirr[ch-4*192];
      row = 8 + imirr[ch-4*192];
      supmod = 2+12;
      break;
    case 5:
      col = 48 + jmirr[ch-5*192];
      row = 8 + imirr[ch-5*192];
      supmod = 2+12;
      break;
    case 6:
      col = 73 - jmirr[ch-6*192];
      row = 25 - imirr[ch-6*192];
      supmod = 2+12;
      break;
    case 7:
      col = 49 - jmirr[ch-7*192];
      row = 25 - imirr[ch-7*192];
      supmod = 2+12;
      break;
    case 8:
      col = 25 - jmirr[ch-8*192];
      row = 25 - imirr[ch-8*192];
      supmod = 2+12;
      break;
    }
}
void StPmdGeom::chain27(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jmirr[ch];
      row = 49 - 16 - imirr[ch];
      supmod = 1+12;
      break;
    case 1:
      col = 24 + jmirr[ch-192];
      row = 32 + imirr[ch-192];
      supmod = 1+12;
      break;
    case 2:
      col = 49 - jmirr[ch-2*192];
      row = 49- imirr[ch-2*192];
      supmod = 1+12;
      break;
    case 3:
      col = 25-jnorm[ch-3*192];
      row = 25-inorm[ch-3*192];
      supmod = 3+12;
      break;
    case 4:
      col = 25-jnorm[ch-4*192];
      row = 49-inorm[ch-4*192];
      supmod = 3+12;
      break;
    case 5:
      col = 8+jnorm[ch-5*192];
      row = 24+inorm[ch-5*192];
      supmod = 3+12;
      break;
    case 6:
      col = 8+jnorm[ch-6*192];
      row = inorm[ch-6*192];
      supmod = 3+12;
      break;
    case 7:
      col = 9-jnorm[ch-7*192];
      row = 25-inorm[ch-7*192];
      supmod = 3+12;
      break;
    case 8:
      col = 9-jnorm[ch-8*192];
      row = 49-inorm[ch-8*192];
      supmod = 3+12;
      break;
    } 
}

void StPmdGeom::chain28(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 73 - jmirr[ch];
      row = 33 - imirr[ch];
      supmod = 2+12;
      break;
    case 1:
      col = 49 - jmirr[ch-192];
      row = 33 - imirr[ch-192];
      supmod = 2+12;
      break;
    case 2:
      col = 25 - jmirr[ch-2*192];
      row = 33 - imirr[ch-2*192];
      supmod = 2+12;
      break;
    case 3:
      col = jmirr[ch-3*192];
      row = 32 + imirr[ch-3*192];
      supmod = 2+12;
      break;
    case 4:
      col = 24 + jmirr[ch-4*192];
      row = 32 + imirr[ch-4*192];
      supmod = 2+12;
      break;
    case 5:
      col = 48 + jmirr[ch-5*192];
      row = 32 + imirr[ch-5*192];
      supmod = 2+12;
      break;
    case 6:
      col = 73 - jmirr[ch-6*192];
      row = 49 - imirr[ch-6*192];
      supmod = 2+12;
      break;
    case 7:
      col = 49 - jmirr[ch-7*192];
      row = 49 - imirr[ch-7*192];
      supmod = 2+12;
      break;
    case 8:
      col = 25 - jmirr[ch-8*192];
      row = 49 - imirr[ch-8*192];
      supmod = 2+12;
      break;
    }
}

void StPmdGeom::chain29(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{   
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 3+12;
      break;
    case 1:
      col = 49 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 3+12;
      break;
    case 2:
      col = 49 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 3+12;
      break;
    case 3:
      col = 32 + jnorm[ch-3*192];
      row = 48 + inorm[ch-3*192];
      supmod = 3+12;
      break;
    case 4:
      col = 32 + jnorm[ch-4*192];
      row = 24 + inorm[ch-4*192];
      supmod = 3+12;
      break;
    case 5:
      col = 32 + jnorm[ch-5*192];
      row = inorm[ch-5*192];
      supmod = 3+12;
      break;
    case 6:
      col = 33 - jnorm[ch-6*192];
      row = 25 - inorm[ch-6*192];
      supmod = 3+12;
      break;
    case 7:
      col = 33 - jnorm[ch-7*192];
      row = 49 - inorm[ch-7*192];
      supmod = 3+12;
      break;
    case 8:
      col = 33 - jnorm[ch-8*192];
      row = 73 - inorm[ch-8*192];
      supmod = 3+12;
      break;
    }
}
void StPmdGeom::chain30(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 73 - jmirr[ch];
      row = 9 - imirr[ch];
      supmod = 4+12;
      break;
    case 1:
      col = 49 - jmirr[ch-192];
      row = 9 - imirr[ch-192];
      supmod = 4+12;
      break;
    case 2:
      col = 25 - jmirr[ch-2*192];
      row = 9 - imirr[ch-2*192];
      supmod = 4+12;
      break;
    case 3:
      col = jmirr[ch-3*192];
      row = 8 + imirr[ch-3*192];
      supmod = 4+12;
      break;
    case 4:
      col = 24 + jmirr[ch-4*192];
      row = 8 + imirr[ch-4*192];
      supmod = 4+12;
      break;
    case 5:
      col = 48 + jmirr[ch-5*192];
      row = 8 + imirr[ch-5*192];
      supmod = 4+12;
      break;
      
    case 6:
      col = 73 - jmirr[ch-6*192];
      row = 25 - imirr[ch-6*192];
      supmod = 4+12;
      break;
    case 7:
      col = 49 - jmirr[ch-7*192];
      row = 25 - imirr[ch-7*192];
      supmod = 4+12;
      break;
    case 8:
      col = 25 - jmirr[ch-8*192];
      row = 25 - imirr[ch-8*192];
      supmod = 4+12;
      break;
    }
}

void StPmdGeom::chain31(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 73 - jmirr[ch];
      row = 33 - imirr[ch];
      supmod = 4+12;
      break;
    case 1:
      col = 49 - jmirr[ch-192];
      row = 33 - imirr[ch-192];
      supmod = 4+12;
      break;
    case 2:
      col = 25 - jmirr[ch-2*192];
      row = 33 - imirr[ch-2*192];
      supmod = 4+12;
      break;
    case 3:
      col = jmirr[ch-3*192];
      row = 32 + imirr[ch-3*192];
      supmod = 4+12;
      break;
    case 4:
      col = 24 + jmirr[ch-4*192];
      row = 32 + imirr[ch-4*192];
      supmod = 4+12;
      break;
    case 5:
      col = 48 + jmirr[ch-5*192];
      row = 32 + imirr[ch-5*192];
      supmod = 4+12;
      break;
    case 6:
      col = 73 - jmirr[ch-6*192];
      row = 49 - imirr[ch-6*192];
      supmod = 4+12;
      break;
    case 7:
      col = 49 - jmirr[ch-7*192];
      row = 49 - imirr[ch-7*192];
      supmod = 4+12;
      break;
    case 8:
      col = 25 - jmirr[ch-8*192];
      row = 49 - imirr[ch-8*192];
      supmod = 4+12;
      break;
    }
  
}


void StPmdGeom::chain32(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = jmirr[ch];
      row = 16 + imirr[ch];
      supmod = 5+12;
      break;
    case 1:
      col = 24 + jmirr[ch-192];
      row = 16 + imirr[ch-192];
      supmod = 5+12;
      break;
    case 2:
      col = 48 + jmirr[ch-2*192];
      row = 16 + imirr[ch-2*192];
      supmod = 5+12;
      break;
    case 3:
      col = 73 - jmirr[ch-3*192];
      row = 17 - imirr[ch-3*192];
      supmod = 5+12;
      break;
    case 4:
      col = 49 - jmirr[ch-4*192];
      row = 17 - imirr[ch-4*192];
      supmod = 5+12;
      break;
    case 5:
      col = 25 - jmirr[ch-5*192];
      row = 17 - imirr[ch-5*192];
      supmod = 5+12;
      break;
    case 6:
      col = jmirr[ch-6*192];
      row = imirr[ch-6*192];
      supmod = 5+12;
      break;
    case 7:
      col = 24 + jmirr[ch-7*192];
      row = imirr[ch-7*192];
      supmod = 5+12;
      break;
    case 8:
      col = 48 + jmirr[ch-8*192];
      row = imirr[ch-8*192];
      supmod = 5+12;
      break;
    }
}



                                                                                                             
                                                                                                             
void StPmdGeom::chain33(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 25 - jnorm[ch];
      row = 73 - inorm[ch];
      supmod = 3+12;
      break;
    case 1:
      col = 8 + jnorm[ch-192];
      row = 48 + inorm[ch-192];
      supmod = 3+12;
      break;
    case 2:
      col = 9 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 3+12;
      break;
    case 3:
      col = 73 - jmirr[ch-3*192];
      row = 57 - imirr[ch-3*192];
      supmod = 4+12;
      break;
    case 4:
      col = 49 - jmirr[ch-4*192];
      row = 57 - imirr[ch-4*192];
      supmod = 4+12;
      break;
    case 5:
      col = 24 + jmirr[ch-5*192];
      row = 56 + imirr[ch-5*192];
      supmod = 4+12;
      break;
    case 6:
      col = 48 + jmirr[ch-6*192];
      row = 56 + imirr[ch-6*192];
      supmod = 4+12;
      break;
    case 7:
      col = 73 - jmirr[ch-7*192];
      row = 73 - imirr[ch-7*192];
      supmod = 4+12;
      break;
    case 8:
      col = 49 - jmirr[ch-8*192];
      row = 73 - imirr[ch-8*192];
      supmod = 4+12;
      break;
    }
}

void StPmdGeom::chain34(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = jmirr[ch];
      row = 40 + imirr[ch];
      supmod = 5+12;
      break;
    case 1:
      col = 24 + jmirr[ch-192];
      row = 40 + imirr[ch-192];
      supmod = 5+12;
      break;
    case 2:
      col = 48 + jmirr[ch - 2*192];
      row = 40 + imirr[ch - 2*192];
      supmod = 5+12;
      break;
    case 3:
      col = 73 - jmirr[ch - 3*192];
      row = 41 - imirr[ch - 3*192];
      supmod = 5+12;
      break;
    case 4:
      col = 49 - jmirr[ch-4*192];
      row = 41 - imirr[ch-4*192];
      supmod = 5+12;
      break;
    case 5:
      col = 25 - jmirr[ch-5*192];
      row = 41 - imirr[ch-5*192];
      supmod = 5+12;
      break;
    case 6:
      col = jmirr[ch-6*192];
      row = 24 + imirr[ch-6*192];
      supmod = 5+12;
      break;
    case 7:
      col = 24 + jmirr[ch-7*192];
      row = 24 + imirr[ch-7*192];
      supmod = 5+12;
      break;
    case 8:
      col = 48 + jmirr[ch-8*192];
      row = 24 + imirr[ch-8*192];
      supmod = 5+12;
      break;
    }
}
void StPmdGeom::chain35(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = jmirr[ch];
      row = 40 + imirr[ch];
      supmod = 6+12;
      break;
    case 1:
      col = 25 - jmirr[ch-192];
      row = 41 - imirr[ch-192];
      supmod = 6+12;
      break;
    case 2:
      col = jmirr[ch-2*192];
      row = 24 + imirr[ch-2*192];
      supmod = 6+12;
      break;
    case 3:
      col = jmirr[ch-3*192];
      row = 16 + imirr[ch-3*192];
      supmod = 6+12;
      break;
    case 4:
      col = 24 + jmirr[ch-4*192];
      row = 16 + imirr[ch-4*192];
      supmod = 6+12;
      break;
    case 5:
      col = 49 - jmirr[ch-5*192];
      row = 17 - imirr[ch-5*192];
      supmod = 6+12;
      break;
    case 6:
      col = 25 - jmirr[ch-6*192];
      row = 17 - imirr[ch-6*192];
      supmod = 6+12;
      break;
    case 7:
      col =  jmirr[ch-7*192];
      row =  imirr[ch-7*192];
      supmod = 6+12;
      break;
    case 8:
      col = 24 + jmirr[ch-8*192];
      row = imirr[ch-8*192];
      supmod = 6+12;
      break;
    }
}

void StPmdGeom::chain36(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 24 + jmirr[ch];
      row = 40 + imirr[ch];
      supmod = 6+12;
      break;
    case 1:
      col = 49 - jmirr[ch-192];
      row = 41 - imirr[ch-192];
      supmod = 6+12;
      break;
    case 2:
      col = 24 + jmirr[ch-2*192];
      row = 24 + imirr[ch-2*192];
      supmod = 6+12;
      break;
    case 3:
      col = 48 + jmirr[ch-3*192];
      row = 16 + imirr[ch-3*192];
      supmod = 6+12;
      break;
    case 4:
      col = 73 - jmirr[ch-4*192];
      row = 17 - imirr[ch-4*192];
      supmod = 6+12;
      break;
    case 5:
      col = 48 + jmirr[ch-5*192];
      row = imirr[ch-5*192];
      supmod = 6+12;
      break;
    case 6:
      col = jmirr[ch-6*192];
      row = 40 + imirr[ch-6*192];
      supmod = 7+12;
      break;
    case 7:
      col = 25 - jmirr[ch-7*192];
      row = 41 - imirr[ch-7*192];
      supmod = 7+12;
      break;
    case 8:
      col = jmirr[ch-8*192];
      row = 24 + imirr[ch-8*192];
      supmod = 7+12;
      break;
    }
}
void StPmdGeom::chain37(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
  case 8:
    switch(zone)
      {
      case 0:
	col = 24 + jmirr[ch];
	row = 40 + imirr[ch];
	supmod = 7+12;
	break;
      case 1:
	col = 49 - jmirr[ch-192];
	row = 41 - imirr[ch-192];
	supmod = 7+12;
	break;
      case 2:
	col = 24 + jmirr[ch-2*192];
	row = 24 + imirr[ch-2*192];
	supmod = 7+12;
	break;
      case 3:
	col = jmirr[ch-3*192];
	row = 16 + imirr[ch-3*192];
	supmod = 7+12;
	break;
      case 4:
	col = 24 + jmirr[ch-4*192];
	row = 16 + imirr[ch-4*192];
	supmod = 7+12;
	break;
      case 5:
	col = 49 - jmirr[ch-5*192];
	row = 17 - imirr[ch-5*192];
	supmod = 7+12;
	break;
      case 6:
	col = 25 - jmirr[ch-6*192];
	row = 17 - imirr[ch-6*192];
	supmod = 7+12;
	break;
      case 7:
	col = jmirr[ch-7*192];
	row = imirr[ch-7*192];
	supmod = 7+12;
	break;
      case 8:
	col = 24 + jmirr[ch-8*192];
	row = imirr[ch-8*192];
	supmod = 7+12;
	break;
      }
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = jmirr[ch];
	row = imirr[ch];
	supmod = 7+12;
	break;
      case 1:
	col = 24 + jmirr[ch-192];
	row = imirr[ch-192];
	supmod = 7 + 12;
	break;
      case 2:
	col = 49 - jmirr[ch-2*192];
	row = 17 - imirr[ch-2*192];
	supmod = 7 + 12;
	break;
      case 3:
	col = 25 - jmirr[ch-3*192];
	row = 17 - imirr[ch-3*192];
	supmod = 7 + 12;
	break;
      case 4:
	col = jmirr[ch-4*192];
	row = 16 + imirr[ch-4*192];
	supmod = 7 + 12;
	break;
      case 5:
	col = 24 + jmirr[ch-5*192];
	row = 16 + imirr[ch-5*192];
	supmod = 7 + 12;
	break;
      case 6:
	col = 24 + jmirr[ch-6*192];
	row = 24 + imirr[ch-6*192];
	supmod = 7 + 12;
	break;
      case 7:
	col = 49 - jmirr[ch-7*192];
	row = 41 - imirr[ch-7*192];
	supmod = 7 + 12;
	break;
      case 8:
	col = 24 + jmirr[ch-8*192];
	row = 40 + imirr[ch-8*192];
	supmod = 7 + 12;
	break;
      }
    break;
  }
}
void StPmdGeom::chain38(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 8+12;
      break;
    case 1:
      col = 49 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 8+12;
      break;
    case 2:
      col = 49 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 8+12;
      break;
    case 3:
      col = 32 + jnorm[ch-3*192];
      row = 48 + inorm[ch-3*192];
      supmod = 8+12;
      break;
    case 4:
      col = 32 + jnorm[ch-4*192];
      row = 24 + inorm[ch-4*192];
      supmod = 8+12;
      break;
    case 5:
      col = 32 + jnorm[ch-5*192];
      row = inorm[ch-5*192];
      supmod = 8+12;
      break;
    case 6:
      col = 33 - jnorm[ch-6*192];
      row = 25 - inorm[ch-6*192];
      supmod = 8+12;
      break;
    case 7:
      col = 33 - jnorm[ch-7*192];
      row = 49 - inorm[ch-7*192];
      supmod = 8+12;
      break;
    case 8:
      col = 33 - jnorm[ch-8*192];
      row = 73 - inorm[ch-8*192];
      supmod = 8+12;
      break;
    }
}

void StPmdGeom::chain39(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 9+12;
      break;
    case 1:
      col = 49 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 9+12;
      break;
    case 2:
      col = 32 + jnorm[ch-2*192];
      row = 24 + inorm[ch-2*192];
      supmod = 9+12;
      break;
    case 3:
      col = 32 + jnorm[ch-3*192];
      row =  inorm[ch-3*192];
      supmod = 9+12;
      break;
    case 4:
      col = 33 - jnorm[ch-4*192];
      row = 25 - inorm[ch-4*192];
      supmod = 9+12;
      break;
    case 5:
      col = 33 - jnorm[ch-5*192];
      row = 49 - inorm[ch-5*192];
      supmod = 9+12;
      break;
    case 6:
      col = 25 - jnorm[ch-6*192];
      row = 49 - inorm[ch-6*192];
      supmod = 9+12;
      break;
    case 7:
      col = 8 + jnorm[ch-7*192];
      row = 24 + inorm[ch-7*192];
      supmod = 9+12;
      break;
    case 8:
      col = 9 - jnorm[ch-8*192];
      row = 49 - inorm[ch-8*192];
      supmod = 9+12;
      break;
    }
}
void StPmdGeom::chain40(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 25 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 8+12;
      break;
    case 1:
      col = 25 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 8+12;
      break;
    case 2:
      col = 25 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 8+12;
      break;
    case 3:
      col = 8 + jnorm[ch-3*192];
      row = 48 + inorm[ch-3*192];
      supmod = 8+12;
      break;
    case 4:
      col = 8 + jnorm[ch-4*192];
      row = 24 + inorm[ch-4*192];
      supmod = 8+12;
      break;
    case 5:
      col = 8 + jnorm[ch-5*192];
      row = inorm[ch-5*192];
      supmod = 8+12;
      break;
    case 6:
      col = 9 - jnorm[ch-6*192];
      row = 25 - inorm[ch-6*192];
      supmod = 8+12;
      break;
    case 7:
      col = 9 - jnorm[ch-7*192];
      row = 49 - inorm[ch-7*192];
      supmod = 8+12;
      break;
    case 8:
      col = 9 - jnorm[ch-8*192];
      row = 73 - inorm[ch-8*192];
      supmod = 8+12;
      break;
    }
}

void StPmdGeom::chain41(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 25 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 9+12;
      break;
    case 1:
      col = 8 + jnorm[ch-192];
      row = inorm[ch-192];
      supmod = 9+12;
      break;
    case 2:
      col = 9 - jnorm[ch-2*192];
      row = 25 - inorm[ch-2*192];
      supmod = 9+12;
      break;
    case 3:
      col = 73 - jmirr[ch-3*192];
      row = 33 - imirr[ch-3*192];
      supmod = 11+12;
      break;
    case 4:
      col = 49 - jmirr[ch-4*192];
      row = 33 - imirr[ch-4*192];
      supmod = 11+12;
      break;
    case 5:
      col = 24 + jmirr[ch-5*192];
      row = 32 + imirr[ch-5*192];
      supmod = 11+12;
      break;
    case 6:
      col = 48 + jmirr[ch-6*192];
      row = 32 + imirr[ch-6*192];
      supmod = 11+12;
      break;
    case 7:
      col = 73 - jmirr[ch-7*192];
      row = 49 - imirr[ch-7*192];
      supmod = 11+12;
      break;
    case 8:
      col = 49 - jmirr[ch-8*192];
      row = 49 - imirr[ch-8*192];
      supmod = 11+12;
      break;
    }
}
void StPmdGeom::chain42(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 73 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 10+12;
      break;
    case 1:
      col = 73 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 10+12;
      break;
    case 2:
      col = 73 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 10+12;
      break;
    case 3:
      col = 56 + jnorm[ch-3*192];
      row = 48 + inorm[ch-3*192];
      supmod = 10+12;
      break;
    case 4:
      col = 56 + jnorm[ch-4*192];
      row = 24 + inorm[ch-4*192];
      supmod = 10+12;
      break;
    case 5:
      col = 56 + jnorm[ch-5*192];
      row = inorm[ch-5*192];
      supmod = 10+12;
      break;
    case 6:
      col = 57 - jnorm[ch-6*192];
      row = 25 - inorm[ch-6*192];
      supmod = 10+12;
      break;
    case 7:
      col = 57 - jnorm[ch-7*192];
      row = 49 - inorm[ch-7*192];
      supmod = 10+12;
      break;
    case 8:
      col = 57 - jnorm[ch-8*192];
      row = 73 - inorm[ch-8*192];
      supmod = 10+12;
      break;
    }
}
void StPmdGeom::chain43(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
 Int_t zone = ch/192;
 switch(zone)
   {
   case 0:
     col = 73 - jmirr[ch];
     row = 9 - imirr[ch];
     supmod = 11+12;
     break;
   case 1:
     col = 49 - jmirr[ch-192];
     row = 9 - imirr[ch-192];
     supmod = 11+12;
     break;
   case 2:
     col = 25 - jmirr[ch-2*192];
     row = 9 - imirr[ch-2*192];
     supmod = 11+12;
     break;
   case 3:
     col = jmirr[ch-3*192];
     row = 8 + imirr[ch-3*192];
     supmod = 11+12;
     break;
   case 4:
     col = 24 + jmirr[ch-4*192];
     row = 8 + imirr[ch-4*192];
     supmod = 11+12;
     break;
   case 5:
     col = 48 + jmirr[ch-5*192];
     row = 8 + imirr[ch-5*192];
     supmod = 11+12;
     break;
   case 6:
     col = 73 - jmirr[ch-6*192];
     row = 25 - imirr[ch-6*192];
     supmod = 11+12;
     break;
   case 7:
     col = 49 - jmirr[ch-7*192];
     row = 25 - imirr[ch-7*192];
     supmod = 11+12;
     break;
   case 8:
     col = 25 - jmirr[ch-8*192];
     row = 25 - imirr[ch-8*192];
     supmod = 11+12;
     break;
   }
}
void StPmdGeom::chain44(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(zone)
    {
    case 0:
      col = 49 - jnorm[ch];
      row = 25 - inorm[ch];
      supmod = 10+12;
      break;
    case 1:
      col = 49 - jnorm[ch-192];
      row = 49 - inorm[ch-192];
      supmod = 10+12;
      break;
    case 2:
      col = 49 - jnorm[ch-2*192];
      row = 73 - inorm[ch-2*192];
      supmod = 10+12;
      break;
    case 3:
      col = 32 + jnorm[ch-3*192];
      row = 48 + inorm[ch-3*192];
      supmod = 10+12;
      break;
    case 4:
      col = 32 + jnorm[ch-4*192];
      row = 24 + inorm[ch-4*192];
      supmod = 10+12;
      break;
    case 5:
      col = 32 + jnorm[ch-5*192];
      row = inorm[ch-5*192];
      supmod = 10+12;
      break;
    case 6:
      col =33 - jnorm[ch-6*192];
      row =25 - inorm[ch-6*192];
      supmod = 10+12;
      break;
    case 7:
      col =33 - jnorm[ch-7*192];
      row =49 - inorm[ch-7*192];
      supmod = 10+12;
      break;
    case 8:
      col =33 - jnorm[ch-8*192];
      row =73 - inorm[ch-8*192];
      supmod = 10+12;
      break;
    }
}
void StPmdGeom::chain45(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 25 - jnorm[ch];
	row = 25 - inorm[ch];
	supmod = 10+12;
	break;
      case 1:
	col = 25 - jnorm[ch-192];
	row = 49 - inorm[ch-192];
	supmod = 10+12;
	break;
      case 2:
	col = 8 + jnorm[ch-2*192];
	row = 24 + inorm[ch-2*192];
	supmod = 10+12;
	break;
      case 3:
	col = 8 + jnorm[ch-3*192];
	row = inorm[ch-3*192];
	supmod = 10+12;
	break;
      case 4:
	col = 9 - jnorm[ch-4*192];
	row = 25 - inorm[ch-4*192];
	supmod = 10+12;
	break;
      case 5:
	col = 9 - jnorm[ch-5*192];
	row = 49 - inorm[ch-5*192];
	supmod = 10+12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 25 - jmirr[ch];
	row = 33 - imirr[ch];
	supmod = 11+12;
	break;
      case 1:
	col =  jmirr[ch-192];
	row =  32 + imirr[ch-192];
	supmod = 11+12;
	break;
      case 2:
	col = 25 - jmirr[ch-2*192];
	row = 49- imirr[ch-2*192];
	supmod = 11+12;
	break;
      case 3:
	col = 25 - jnorm[ch-3*192];
	row = 25 - inorm[ch-3*192];
	supmod = 10+12;
	break;
      case 4:
	col = 25 - jnorm[ch-4*192];
	row = 49 - inorm[ch-4*192];
	supmod = 10+12;
	break;
      case 5:
	col = 8 + jnorm[ch-5*192];
	row = 24 + inorm[ch-5*192];
	supmod = 10+12;
	break;
      case 6:
	col = 8 + jnorm[ch-6*192];
	row = inorm[ch-6*192];
	supmod = 10+12;
	break;
      case 7:
	col = 9 - jnorm[ch-7*192];
	row = 25 - inorm[ch-7*192];
	supmod = 10+12;
	break;
      case 8:
	col = 9 - jnorm[ch-8*192];
	row = 49 - inorm[ch-8*192];
	supmod = 10+12;
	break;
      }
    break;
  }
}

void StPmdGeom::chain46(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
   switch(zone)
      {
      case 0:
	col = 25 - jmirr[ch];
	row = 33 - imirr[ch];
	supmod = 11+12;
	break;
      case 1:
	col =  jmirr[ch-192];
	row =  32 + imirr[ch-192];
	supmod = 11+12;
	break;
      case 2:
	col = 25 - jmirr[ch-2*192];
	row = 49- imirr[ch-2*192];
	supmod = 11+12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = jnorm[ch];
	row = 48 + inorm[ch];
	supmod = 12 + 12;
	break;
      case 1:
	col = 17 - jnorm[ch-192];
	row = 73 - inorm[ch-192];
	supmod = 12 + 12;
	break;
      case 2:
	col = 16 + jnorm[ch-2*192];
	row = 48 + inorm[ch-2*192];
	supmod = 12 + 12;
	break;
      case 3:
	col = 24 + jnorm[ch-3*192];
	row = 48 + inorm[ch-3*192];
	supmod = 12+12;
	break;
      case 4:
	col = 24 + jnorm[ch-4*192];
	row = 24 + inorm[ch-4*192];
	supmod = 12+12;
	break;
      case 5:
	col = 41 - jnorm[ch-5*192];
	row = 49 - inorm[ch-5*192];
	supmod = 12+12;
	break;
      case 6:
	col = 41 - jnorm[ch-6*192];
	row = 73 - inorm[ch-6*192];
	supmod = 12+12;
	break;
      case 7:
	col = 40 + jnorm[ch-7*192];
	row = 48 + inorm[ch-7*192];
	supmod = 12+12;
	break;
      case 8:
	col = 40 + jnorm[ch-8*192];
	row = 24 + inorm[ch-8*192];
	supmod = 12+12;
	break;
      }
    break;
  }
}

void StPmdGeom::chain47(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 24 + jnorm[ch];
	row = 48 + inorm[ch];
	supmod = 12+12;
	break;
      case 1:
	col = 24 + jnorm[ch-192];
	row = 24 + inorm[ch-192];
	supmod = 12+12;
	break;
      case 2:
	col = 41 - jnorm[ch-2*192];
	row = 49 - inorm[ch-2*192];
	supmod = 12+12;
	break;
      case 3:
	col = 41 - jnorm[ch-3*192];
	row = 73 - inorm[ch-3*192];
	supmod = 12+12;
	break;
      case 4:
	col = 40 + jnorm[ch-4*192];
	row = 48 + inorm[ch-4*192];
	supmod = 12+12;
	break;
      case 5:
	col = 40 + jnorm[ch-5*192];
	row = 24 + inorm[ch-5*192];
	supmod = 12+12;
	break;
      case 6:
	col = 48 + jnorm[ch-6*192];
	row = inorm[ch-6*192];
	supmod = 12+12;
	break;
      case 7:
	col = 65 - jnorm[ch-7*192];
	row = 25 - inorm[ch-7*192];
	supmod = 12+12;
	break;
      case 8:
	col = 64 + jnorm[ch-8*192];
	row = inorm[ch-8*192];
	supmod = 12+12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 48 + jnorm[ch];
	row = 48 + inorm[ch];
	supmod = 12+12;
	break;
      case 1:
	col = 48 + jnorm[ch-192];
	row = 24 + inorm[ch-192];
	supmod = 12 + 12;
	break;
      case 2:
	col = 48 + jnorm[ch-2*192];
	row = inorm[ch-2*192];
	supmod = 12 + 12;
	break;
      case 3:
	col = 65 - jnorm[ch-3*192];
	row = 25 - inorm[ch-3*192];
	supmod = 12+12;
	break;
      case 4:
	col = 65 - jnorm[ch-4*192];
	row = 49 - inorm[ch-4*192];
	supmod = 12+12;
	break;
      case 5:
	col = 65 - jnorm[ch-5*192];
	row = 73 - inorm[ch-5*192];
	supmod = 12+12;
	break;
      case 6:
	col = 64 + jnorm[ch-6*192];
	row = 48 + inorm[ch-6*192];
	supmod = 12+12;
	break;
      case 7:
	col = 64 + jnorm[ch-7*192];
	row = 24 + inorm[ch-7*192];
	supmod = 12+12;
	break;
      case 8:
	col = 64 + jnorm[ch-8*192];
	row = inorm[ch-8*192];
	supmod = 12+12;
	break;
      }
    break;
  }
}
void StPmdGeom::chain48(Int_t& ch,Int_t& supmod,Int_t& col,Int_t& row,Int_t year)
{
  Int_t zone = ch/192;
  switch(year){
  case 6:
    
    switch(zone)
      {
      case 0:
	col = 48 + jnorm[ch];
	row = 48 + inorm[ch];
	supmod = 12+12;
	break;
      case 1:
	col = 65 - jnorm[ch-192];
	row = 73 - inorm[ch-192];
	supmod = 12+12;
	break;
      case 2:
	col = 64 + jnorm[ch-2*192];
	row = 48 + inorm[ch-2*192];
	supmod = 12+12;
	break;
      case 3:
	col = 72 + jnorm[ch-3*192];
	row = 48 + inorm[ch-3*192];
	supmod = 12+12;
	break;
      case 4:
	col = 72 + jnorm[ch-4*192];
	row = 24 + inorm[ch-4*192];
	supmod = 12+12;
	break;
      case 5:
	col = 89 - jnorm[ch-5*192];
	row = 49 - inorm[ch-5*192];
	supmod = 12+12;
	break;
      case 6:
	col = 89 - jnorm[ch-6*192];
	row = 73 - inorm[ch-6*192];
	supmod = 12+12;
	break;
      case 7:
	col = 88 + jnorm[ch-7*192];
	row = 48 + inorm[ch-7*192];
	supmod = 12+12;
	break;
      case 8:
	col = 88 + jnorm[ch-8*192];
	row = 24 + inorm[ch-8*192];
	supmod = 12+12;
	break;
      }
    break;
  case 8:
  case 10:
  case 11:
  case 12:
    switch(zone)
      {
      case 0:
	col = 72 + jnorm[ch];
	row = 48 + inorm[ch];
	supmod = 12+12;
	break;
      case 1:
	col = 72 + jnorm[ch-192];
	row = 24 + inorm[ch-192];
	supmod = 12 + 12;
	break;
      case 2:
	col = 72 + jnorm[ch-2*192];
	row = inorm[ch-2*192];
	supmod = 12 + 12;
	break;
      case 3:
	col = 89 - jnorm[ch-3*192];
	row = 25 - inorm[ch-3*192];
	supmod = 12+12;
	break;
      case 4:
	col = 89 - jnorm[ch-4*192];
	row = 49 - inorm[ch-4*192];
	supmod = 12+12;
	break;
      case 5:
	col = 89 - jnorm[ch-5*192];
	row = 73 - inorm[ch-5*192];
	supmod = 12+12;
	break;
      case 6:
	col = 88 + jnorm[ch-6*192];
	row = 48 + inorm[ch-6*192];
	supmod = 12+12;
	break;
      case 7:
	col = 88 + jnorm[ch-7*192];
	row = 24 + inorm[ch-7*192];
	supmod = 12+12;
	break;
      case 8:
	col = 88 + jnorm[ch-8*192];
	row = inorm[ch-8*192];
	supmod = 12+12;
	break;
      }
    break;
  }
}

void StPmdGeom::drawPMDXY(Int_t firstchain,Int_t lastchain, Int_t runno){
  ///  cout<<" I am drawing the PMD XY"<<endl;
  readBoardDetail(runno);
 
  Int_t rn=0,year=0;
  GetRunYear(runno,rn,year);
  cout<<"runnumber = "<<rn<<" year="<<year<<endl;
  
  char histname[20];
  char gifname[20];
  if (firstchain<25){
    sprintf(histname,"CPVXY%d",runno);
    sprintf(gifname,"CPVXY%d.gif",runno);
  }else{
    sprintf(histname,"PMDXY%d",runno);
    sprintf(gifname,"PMDXY%d.gif",runno);
  }
  TH2F * hpmdXY = new TH2F("hpmdXY",histname,100,-135,135,100,-135,135);
  TCanvas *pmdC2 = new TCanvas("pmdC2","PMDXY canvas",800,800);  
  hpmdXY->SetStats(kFALSE);
  pmdC2->cd(1);
  hpmdXY->Draw("pmdC2");

  Int_t sm=0,row=0,col=0,chmod=-1;
  Int_t bmax[49];
  for(Int_t chain=1;chain<49;chain++){
    bmax[chain]=27;
    
    if (year==8){ 
      if(chain==1 || chain==7 || chain==19 || chain==20 || 
	 chain==23 || chain==24) 
	{bmax[chain]=36;}
    }
    if(year==10||year==11||year==12){
      if(chain==8||chain==12||chain==23||chain==24){bmax[chain]=36;}
    }
  }
  for (Int_t chain=firstchain;chain<=lastchain;chain++){
    //    cout<<"Drawing chain="<<chain<<" with "<<bmax[chain]<<" mapped boards"<<endl;
    //    Int_t workingchannel = 0;
    for(Int_t chan = 0;chan<27*64;chan++){
      Int_t mapcheck;
      if (year==5){
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod);
      }else{
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod,year);
      }
      
      if(mapcheck==kStOK && sm>0){
	Float_t xreal = 0, yreal =0, eta=0,phi=0;
	IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
	//	cout<<"chain,chan,sm,chmod,mapcheck="<<chain<<","<<chan<<","<<sm<<","<<chmod<<","<<mapcheck<<" "<<xreal<<" / "<<yreal<<" "<<eta<<" / "<<phi<<endl;
	hpmdXY->Fill(xreal,yreal);
      }
    }
  }
  pmdC2->Print(gifname,"gif");
}

void StPmdGeom::drawPMDetaphi(Int_t firstchain,Int_t lastchain, Int_t runno){
  ///  cout<<" I am drawing the PMD etaphi"<<endl;
  readBoardDetail(runno);
 
  Int_t rn=0,year=0;
  GetRunYear(runno,rn,year);
  cout<<"runnumber = "<<rn<<" year="<<year<<endl;
  
  char histname[20];
  char gifname[20];
  if (firstchain<25){
    sprintf(histname,"CPVetaphi%d",runno);
    sprintf(gifname,"CPVetaphi%d.gif",runno);
  }else{
    sprintf(histname,"PMDetaphi%d",runno);
    sprintf(gifname,"PMDetaphi%d.gif",runno);
  }
  TH2F * hpmdetaphi = new TH2F("hpmdetaphi",histname,40,-4.0,-2.0,180,-1.0*TMath::Pi(),TMath::Pi());
  TCanvas *pmdC3 = new TCanvas("pmdC3","PMDetaphi canvas",800,800);  
  hpmdetaphi->SetStats(kFALSE);
  pmdC3->cd(1);
  hpmdetaphi->Draw("pmdC3");

  Int_t sm,row,col,chmod;
  Int_t bmax[49];
  for(Int_t chain=1;chain<49;chain++){
    bmax[chain]=27;
    
    if (year==8){ 
      if(chain==1 || chain==7 || chain==19 || chain==20 || 
	 chain==23 || chain==24) 
	{bmax[chain]=36;}
    }
    if(year==10||year==11||year==12){
      if(chain==8||chain==12||chain==23||chain==24){bmax[chain]=36;}
    }
  }
  for (Int_t chain=firstchain;chain<=lastchain;chain++){
    //    cout<<"Drawing chain="<<chain<<" with "<<bmax[chain]<<" mapped boards"<<endl;
    //    Int_t workingchannel = 0;
    for(Int_t chan = 0;chan<27*64;chan++){
      Int_t mapcheck;
      if (year==5){
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod);
      }else{
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod,year);
      }
      
      if(mapcheck==kStOK && sm>0){
	Float_t xreal = 0, yreal =0, eta=0,phi=0;
	IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
	//	cout<<"chain,chan,sm,chmod,mapcheck="<<chain<<","<<chan<<","<<sm<<","<<chmod<<","<<mapcheck<<" "<<xreal<<" / "<<yreal<<" "<<eta<<" / "<<phi<<endl;
	hpmdetaphi->Fill(eta,phi);
      }
    }
  }
  pmdC3->Print(gifname,"gif");
}

void StPmdGeom::drawPMD(Int_t firstchain,Int_t lastchain, Int_t runno){
  ///  cout<<" I am drawing the PMD"<<endl;
  readBoardDetail(runno);
 
  Int_t rn=0,year=0;
  GetRunYear(runno,rn,year);
  //  cout<<"runnumber = "<<rn<<" year="<<year<<endl;
  
  char histname[20];
  char gifname[20];
  if (firstchain<25){
    sprintf(histname,"CPV%d",runno);
    sprintf(gifname,"CPV%d.gif",runno);
  }else{
    sprintf(histname,"PMD%d",runno);
    sprintf(gifname,"PMD%d.gif",runno);
  }
    //  TH2F *hpmdxy = new TH2F("hpmdxy","NEW CPV CHAINS ",100,-135,135,100,-135,135);
  TH2F * hpmdxy = new TH2F("hpmdxy",histname,100,-135,135,100,-135,135);
  TCanvas *pmdC = new TCanvas("pmdC","PMD canvas",800,800);  
  hpmdxy->SetStats(kFALSE);
  pmdC->cd(1);
  hpmdxy->Draw("pmdC");
  Int_t sm,row,col,chmod;
  Float_t xcon[5],ycon[5];
  Int_t icorner=0;
  Int_t bmax[49];
  for(Int_t chain=1;chain<49;chain++){
    bmax[chain]=27;
    
    if (year==8){ 
      if(chain==1 || chain==7 || chain==19 || chain==20 || 
	 chain==23 || chain==24) 
	{bmax[chain]=36;}
    }
    if(year==10||year==11||year==12){
      if(chain==8||chain==12||chain==23||chain==24){bmax[chain]=36;}
    }
  }
  for (Int_t chain=firstchain;chain<=lastchain;chain++){
    //    cout<<"Drawing chain="<<chain<<" with "<<bmax[chain]<<" mapped boards"<<endl;
    Int_t workingboard = 0;
    Float_t xboard[27],yboard[27];
    Int_t workingchannel = 0;
    //    for(Int_t chan = 0;chan<bmax[chain]*64;chan++){
    for(Int_t chan = 0;chan<27*64;chan++){
      Int_t mapcheck;
      if (year==5){
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod);
      }else{
	mapcheck = ChainMapping(chain,chan,sm,col,row,chmod,year);
      }
      
      //  cout<<"chain,chan,sm,chmod,mapcheck="<<chain<<","<<chan<<","<<sm<<","<<chmod<<","<<mapcheck<<endl;
      if(mapcheck==kStOk && sm > 0){
	//	cout<<chain<<","<<chan<<","<<sm<<","<<row<<","<<col<<","<<icorner<<endl;
	DrawRhombus(chain,chan,sm,row,col,icorner,xcon,ycon);
	if(icorner==4){
	  xboard[workingboard]=0;
	  yboard[workingboard]=0;
	  for(Int_t i=0;i<4;i++){
	    xboard[workingboard]=xboard[workingboard]+xcon[i];
	    yboard[workingboard]=yboard[workingboard]+ycon[i];
	    //	    if(chain==24) cout<<"chain24,xycon="<<xcon[i]<<","<<ycon[i]<<endl;	
	  }
	  xboard[workingboard]=xboard[workingboard]/4;
	  yboard[workingboard]=yboard[workingboard]/4;
	  //	  cout<<"board# = "<<workingboard<<" x,y="<<xboard[workingboard]<<","<<yboard[workingboard]<<endl;
	  workingboard++;
	  icorner = 0;
	}
	//	if (icorner>0)	cout<<"icorner="<<icorner<<endl;
	workingchannel++;
      }
      //      if(chan==(bmax[chain]*192-1))cout<<"working channels are "<<workingchannel<<endl;
    }
    if(workingboard>0){
      //chmod==(bmax[chain]*192-1)){
      cout<<"working channels are "<<workingchannel<<endl;
      cout<<"working boards are "<<workingboard<<endl;
      //      TPolyLine * dchain = new TPolyLine(workingboard,xboard,yboard);
      TPolyLine * dchain;
      if (chain>24){
	Float_t xmboard[27];
	for(Int_t ib=0;ib<27;ib++){xmboard[ib]=-1*xboard[ib];}
	dchain = new TPolyLine(workingboard,xmboard,yboard);
      }else{
	dchain = new TPolyLine(workingboard,xboard,yboard);
      }
      dchain->SetLineWidth(2);
      dchain->SetLineColor(2);
      dchain->Draw();
      char chainnum[3];
      sprintf(chainnum,"%d",chain);
      if (chain>24){
	TLatex* chainno = new TLatex(-xboard[0]+1,yboard[0]+1,chainnum);
	TMarker * startchain = new TMarker(-xboard[0],yboard[0],20);
	startchain->SetMarkerColor(2);
	startchain->Draw();
	chainno->SetTextSize(0.04);
	chainno->Draw();
      }else{
	TLatex* chainno = new TLatex(xboard[0]+1,yboard[0]+1,chainnum);
	TMarker * startchain = new TMarker(xboard[0],yboard[0],20);
	startchain->SetMarkerColor(2);
	startchain->Draw();
	chainno->SetTextSize(0.05);
	chainno->Draw();
      }
    }
  }
  pmdC->Print(gifname,"gif");
  
}

void StPmdGeom::DrawRhombus(Int_t chain,Int_t chan,Int_t sm, Int_t row, Int_t col, Int_t& icorner,Float_t* xcon,Float_t* ycon)
{
  Float_t xreal,yreal,eta,phi;
  if(row%8==1 && col%8==1){
    IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
    xcon[0]=xreal;ycon[0]=yreal;
    xcon[4]=xreal;ycon[4]=yreal;
    icorner++;
  }
  if(row%8==1 && col%8==0){
    IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
    xcon[1]=xreal;ycon[1]=yreal;
    icorner++;
  }
  if(row%8==0 && col%8==0){
    IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
    xcon[2]=xreal;ycon[2]=yreal;
    icorner++;
  }
  if(row%8==0 && col%8==1){
    IntDetCell_xy(sm,row,col,xreal,yreal,eta,phi);
    xcon[3]=xreal; ycon[3]=yreal;
    icorner++;
  }
  if(icorner==4){
    //    Int_t brd=int((chan)/64)+1;
    //    TPolyLine * rhombus = new TPolyLine(5,xcon,ycon);
    //--------------------------
    // To draw a mirror image of PMD if chain>24 so that the image is 
    // as seen from the tunnel and not from the vertex.
    // This leaves the image of CPV unchanged
    Float_t xrhomb[5];
    TPolyLine * rhombus;
    if (chain>24){
      for(Int_t rh=0;rh<=5;rh++){xrhomb[rh]=-1*xcon[rh];}
      rhombus = new TPolyLine(5,xrhomb,ycon);
    }else{
      rhombus = new TPolyLine(5,xcon,ycon);
    }
    //----------------------------
    rhombus->SetLineWidth(1);
    rhombus->SetLineColor(1);
    //    cout<<"chain="<<chain<<" brd="<<brd<<"  status="<<status[chain-1][brd]<<endl;
    /*
      if (status[chain-1][brd]==0)
      {
      rhombus->SetLineColor(43);
      rhombus->SetLineWidth(1);
      }
    */
    rhombus->Draw();
    //    icorner=0;
  }
}

void StPmdGeom::GetRunYear(Int_t runno,Int_t&rn,Int_t&year){

  // returns the year number(year) and the day number(rn) 

  year = Int_t(runno/1000000);
  Int_t rest = runno-year*1000000;
  rn = Int_t(rest/1000);

  //  cout<<"rn="<<rn<<" year="<<year<<endl;
}

Int_t StPmdGeom::GetNBoardsChain(Int_t chain){

  Int_t aliveboard = 0;
  for(Int_t i=0;i<36;i++){
    if (status[chain-1][i]==1) aliveboard++;
  }
  //  cout<<"aliveboards in chain "<<chain<<" are "<<aliveboard<<endl;
  return aliveboard;
  
}

void StPmdGeom::GetNBoardsSM(Int_t year , Int_t * aliveboard){

  Int_t channel;
  //  Int_t aliveboard[24];
  for(Int_t i=0;i<24;i++){
    aliveboard[i] = 0;
  }

  for(Int_t chain = 1;chain<=48;chain++){
    for(Int_t brd = 0;brd<27;brd++){
      channel = brd*64 + 10; // 10th channel of every board is studied
      Int_t supmod,col,row,chmod;
      Int_t mapcheck;
      if (year==5){
	mapcheck = ChainMapping(chain,channel,supmod,col,row,chmod);
      }else{
        mapcheck = ChainMapping(chain,channel,supmod,col,row,chmod,year);
      }
      if(mapcheck==kStOk && supmod > 0) {
	aliveboard[supmod-1]++;
      }
    }
  }
  //  return aliveboard;
}
