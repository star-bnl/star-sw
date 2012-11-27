///////////////////////////////////////////////////////////////////////////
//
//    Copyright 2010
//
//    This file is part of starlight.
//
//    starlight is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    starlight is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with starlight. If not, see <http://www.gnu.org/licenses/>.
//
///////////////////////////////////////////////////////////////////////////
//
// File and Version Information:
// $Rev::                             $: revision of last commit
// $Author: jwebb $: author of last commit
// $Date: 2012/11/27 22:27:33 $: date of last commit
//
// Description:
//    Added 18->19 for reading in the luminosity table
//    Incoherent factor added to table --Joey
//
//
//
///////////////////////////////////////////////////////////////////////////


#include <iostream>
#include <fstream>

#include "readinluminosity.h"
#include "starlightconstants.h"
#include "inputParameters.h"


using namespace std;


//______________________________________________________________________________
readLuminosity::readLuminosity(const inputParameters& input)//:inputread(input)
{
  //storing inputparameters into protected variables for the object to use them
  _ReadInputNPT=input.nmbPtBinsInterference();
  _ReadInputnumy=input.nmbRapidityBins();
  _ReadInputnumw=input.nmbWBins();
  _ReadInputgg_or_gP=input.productionMode();
  _ReadInputinterferencemode=input.interferenceEnabled();
  
}


//______________________________________________________________________________
readLuminosity::~readLuminosity()
{ }


//______________________________________________________________________________
void readLuminosity::read()
{
  double dummy[19]; //14//18
  double (*finterm)[starlightLimits::MAXWBINS]=new double[starlightLimits::MAXWBINS][starlightLimits::MAXYBINS];  
  //decreased from 1000*1000; too big! causes fault!
  double fpart =0.;
  double fptsum=0.;
  ifstream wylumfile;

  _f_max=0.0;

  wylumfile.open("slight.txt");
  for(int i=0;i < 19;i++){ // was 14; this is to account for sergei's additional parameters ie d-Au//was19
    wylumfile >> dummy[i];
  }
  for(int i=0;i<_ReadInputnumw;i++){
    wylumfile >> _Warray[i];
  }
  for(int i=0;i<_ReadInputnumy;i++){
    wylumfile >> _Yarray[i];
  }
  for(int i=0;i<_ReadInputnumw;i++){
    for(int j=0;j<_ReadInputnumy;j++){
      wylumfile >> _Farray[i][j];
      if( _Farray[i][j] > _f_max ) _f_max=_Farray[i][j];
    }
  }
  //Normalize farray (JN 010402)
  for(int i=0;i<_ReadInputnumw;i++){
    for(int j=0;j<_ReadInputnumy;j++){
      _Farray[i][j]=_Farray[i][j]/_f_max;
    }
  }

  if(_ReadInputgg_or_gP == 1) goto L1000;
  if(_ReadInputinterferencemode == 0) goto L1000;
  // only numy/2 y bins here, from 0 (not -ymax) to ymax
 
  for (int i=0;i<_ReadInputnumy/2;i++){
    //fmax=0;
    //we want to convert _fptarray to an integral array where fpt(i,j) is near 0, and fpt(j,NPT) ~1. This will facilitate a simple table loookup
    fptsum=0.;
    for(int j=0;j<_ReadInputNPT;j++){
      wylumfile >> fpart;
      finterm[i][j] = fpart;
      _fptarray[i][j]=0.;
      fptsum=fptsum+fpart;
    }
    //convert array to integral
    _fptarray[i][0]=finterm[i][0]/fptsum;
    for(int j=1;j<_ReadInputNPT;j++){
      for(int k=0;k<=j;k++){
	_fptarray[i][j]=_fptarray[i][j]+finterm[i][k];
      }
      _fptarray[i][j]=_fptarray[i][j]/fptsum;
    }
  }

 L1000:

  wylumfile >> _bwnormsave;
  wylumfile.close();
  delete[] finterm;	
  return;
}
