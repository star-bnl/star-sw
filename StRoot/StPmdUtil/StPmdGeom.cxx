/********************************************************
 *
 * $Id: StPmdGeom.cxx,v 1.1 2002/08/27 12:23:52 subhasis Exp $
 *
 * Author: 
 *
 *********************************************************
 *
 * Description: This the class of PMD geometry for calculating
 * various utility functions from geometry parameters
 *
 *********************************************************
 * $Log: StPmdGeom.cxx,v $
 * Revision 1.1  2002/08/27 12:23:52  subhasis
 * First version
 *
 *
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

/*! function for converting supermodule,row,col to x,y,eta,phi after conversion
 * from 17 to 12 supermodule */
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

//! function for calculating eta,phi from x, y

void StPmdGeom::Cell_eta_phi(Float_t xreal,Float_t yreal,Float_t& eta,Float_t& phi){
  Float_t rdist = (sqrt(xreal*xreal + yreal*yreal))/mzreal;
  Float_t theta = atan(rdist);
  eta = log(tan(0.5*theta));
  if( xreal==0) {
    if(yreal>0) phi = 1.571428;
    if(yreal<0) phi = -1.571428;
  }
  if(xreal != 0) phi = atan2(yreal,xreal);

}
/*! function for convering supermodule,row,col (from GEANT) to 
 *supermodule,row,col as in hardware. */

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





















