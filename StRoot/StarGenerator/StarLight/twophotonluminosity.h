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


#ifndef TWOPHOTONLUMINOSITY_H
#define TWOPHOTONLUMINOSITY_H

#include "nucleus.h"
#include "beam.h"
#include "beambeamsystem.h"
#include "starlightlimits.h"


class twoPhotonLuminosity : public beamBeamSystem
{
 public:
  twoPhotonLuminosity(beam beam_1,beam beam_2,int mode,double luminosity,inputParameters& input);
  twoPhotonLuminosity(beam beam_1, beam beam_2,int mode,inputParameters& input);
  ~twoPhotonLuminosity();
  
 private:
  void twoPhotonDifferentialLuminosity();
  double D2LDMDY(double M,double Y,double &Normalize);
  double integral(double Normalize);
  double radmul(int N,double *Lower,double *Upper,int NIterMin,int NIterMax,double EPS,double *WK,int NIter,double &Result,double &ResErr,double &NFNEVL,double &Summary);
  double integrand(double N,double X[15]);
  double Nphoton(double W,double gamma,double Rho);
  
  double _W1; //Energy of photon #1
  double _W2; //Energy of photon #2
  double _gamma; //Gamma of the system
  inputParameters _input2photon;
};


#endif  // TWOPHOTONLUMINOSITY_H
