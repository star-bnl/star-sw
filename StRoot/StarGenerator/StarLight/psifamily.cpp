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
// $Date: 2012/11/27 22:27:32 $: date of last commit
//
// Description:
//
//
//
///////////////////////////////////////////////////////////////////////////


#include <iostream>
#include <fstream>
#include <cmath>

#include "psifamily.h"


using namespace std;


psiFamily::psiFamily(inputParameters& input,beamBeamSystem& bbsystem):Gammaanarrowvm(input,bbsystem)
{
//Defining _width and mass...
 

	// switch(input.prodParticleType()){
	// 	case starlightConstants::JPSI:
	// 	  cout << "JPSI goddamnit!" << endl;
        //                 _width=0.000091;
        //                 mass=3.09692;
        //         break;
        //         case starlightConstants::JPSI2S:
        //                 _width=0.000337;
        //                 mass=3.686093;
        //         break;
	// 	default: cout<<"This PSI Family Member Has Not Been Defined, psiFamily::psiFamily()"<<endl;
	// }

}


psiFamily::~psiFamily()
{ }


double psiFamily::getTheta(starlightConstants::particleTypeEnum)
{
//should probably merge the psi fmaily back to the vm stuff.

//This depends on the decay angular distribution
//Valid for J/Psi, Psi(2s)?
//cout<<"Psi family theta"<<endl;
double theta=0.;
double xtest=0.;
double dndtheta=0.;
        L200td:
          theta = starlightConstants::pi*_randy.Rndom();//random()/(RAND_MAX+1.0);
          xtest = _randy.Rndom();//random()/(RAND_MAX+1.0);
          //  Follow distribution for helicity +/-1
          //  Eq. 19 of J. Breitweg et al., Eur. Phys. J. C2, 247 (1998)//Does Not Apply for J/psi?
          //  SRK 11/14/2000

          dndtheta = sin(theta)*(1.+((cos(theta))*(cos(theta))));
          if(xtest > dndtheta)
            goto L200td;
        return theta;
}


double psiFamily::getDaughterMass(starlightConstants::particleTypeEnum &ipid)
{
	double ytest=0.,mdec=0.;
	//  decays 50% to e+/e-, 50% to mu+/mu-
        ytest = _randy.Rndom();//random()/(RAND_MAX+1.0);
        if(ytest >= 0.5)
        {
	        mdec = starlightConstants::mel;
        	ipid = starlightConstants::ELECTRON;
        }
        else
        {
	        mdec = starlightConstants::muonMass;
        	ipid = starlightConstants::MUON;
        }
	return mdec;
}
