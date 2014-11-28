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
//
//
//
///////////////////////////////////////////////////////////////////////////


#ifndef READINLUMINOSITY_H
#define READINLUMINOSITY_H


#include "inputParameters.h"
#include "starlightlimits.h"


class readLuminosity
{
 public:
  readLuminosity(const inputParameters& input);
  ~readLuminosity();
  
  void read();
  double _Warray[starlightLimits::MAXWBINS];   //decreased from 1000; too big! causes fault!
  double _Yarray[starlightLimits::MAXYBINS];    
  double _Farray[starlightLimits::MAXWBINS][starlightLimits::MAXYBINS];
  double _f_max;
  double _fptarray[500][500];
  //		inputParameters inputread;
  double _bwnormsave;

 protected:
  int _ReadInputNPT;
  int _ReadInputnumy;
  int _ReadInputnumw;
  int _ReadInputgg_or_gP;
  int _ReadInputinterferencemode;
};


#endif  // READINLUMINOSITY_H
