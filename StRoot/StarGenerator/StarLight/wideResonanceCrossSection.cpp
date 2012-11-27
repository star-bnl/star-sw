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


#include <iostream>
#include <fstream>
#include <cmath>

#include "starlightconstants.h"
#include "wideResonanceCrossSection.h"


using namespace std;
using namespace starlightConstants;


//______________________________________________________________________________
wideResonanceCrossSection::wideResonanceCrossSection(const inputParameters& input,
                                                     const beamBeamSystem&  bbsystem)
	: photonNucleusCrossSection(input, bbsystem)//hrm
{
	_wideWmax = input.maxW();
	_wideWmin = input.minW();
	_wideYmax = input.maxRapidity();
	_wideYmin = -1.0 * _wideYmax;
	_Ep       = input.getProtonEnergy();
}


//______________________________________________________________________________
wideResonanceCrossSection::~wideResonanceCrossSection()
{

}


//______________________________________________________________________________
void
wideResonanceCrossSection::crossSectionCalculation(const double bwnormsave)
{
	//     This subroutine calculates the cross-section assuming a wide
	//     (Breit-Wigner) resonance.

	// double Av,Wgp,cs,cvma;
	double W,dW,dY;
	double y1,y2,y12,ega1,ega2,ega12;
	// double t,tmin,tmax;
	double csgA1,csgA2,csgA12,int_r,dR,rate;
	double dsigdW,dsigdWalt,dndW,tmp;
	double dsigdW2;
	// double ax,bx;
	double Eth;
	int    I,J,NW,NY;
	// int    K,NGAUSS;
                                                                                                                                                      
	// ----------------- !!!!!!!!!!!!!!!!!!!! -----------------------------
                                                                                                                                                      
	double bwnorm =bwnormsave;//used to transfer the bwnorm from the luminosity tables

	// --------------------------------------------------------------------
	//gamma+nucleon threshold.

	Eth=0.5*(((_wideWmin+protonMass)*(_wideWmin+protonMass)
	          -protonMass*protonMass)/(_Ep+sqrt(_Ep*_Ep-protonMass*protonMass)));
                                                                                                                                                      
	NW   = 100;
	dW   = (_wideWmax-_wideWmin)/double(NW);
  
	NY   =  1200;
	dY   = (_wideYmax-_wideYmin)/double(NY);
  
	if (getBNORM()  ==  0.){
		cout<<" Using Breit-Wigner Resonance Profile."<<endl;
	}
	else{
		cout<<" Using Breit-Wigner plus direct pi+pi- profile."<<endl;
	}
  
	cout<<" Integrating over W from "<<_wideWmin<<" to "<<_wideWmax<<endl;
                                                                                                                                                      
	int_r=0.;
 
	for(I=0;I<=NW-1;I++){
    
		W = _wideWmin + double(I)*dW + 0.5*dW;
    
		tmp = 0.0;
		dsigdW=0.0;
		dsigdW2=0.0;
		dsigdWalt=0.0;
		dndW=0.0;
    
		for(J=0;J<=NY-1;J++){
      
			y1  = _wideYmin + double(J)*dY;
			y2  = _wideYmin + double(J+1)*dY;
			y12 = 0.5*(y1+y2);
      
			ega1  = 0.5*W*exp(y1);
			ega2  = 0.5*W*exp(y2);
			ega12 = 0.5*W*exp(y12);
      
			if(ega1 < Eth) continue;
			if(ega2 > maxPhotonEnergy()) continue;
			// check it !!
          
			if(J == 0){
				// >> 1st Point (Calculated only the first time)     =====>>>
				//ega1 used.                                                        
				csgA1=getcsgA(ega1,W);
			}
			else{
				csgA1 = csgA2;
			}
          
			//         >> Middle Point                      =====>>>
			csgA12=getcsgA(ega12,W);         

			//         >> Second Point                      =====>>>
			csgA2=getcsgA(ega2,W);
      
			//>> Sum the contribution for this W,Y. The 2 accounts for the 2 beams
			dR  = ega1*photonFlux(ega1)*csgA1;
			dR  = dR + 4.*ega12*photonFlux(ega12)*csgA12;
			dR  = dR + ega2*photonFlux(ega2)*csgA2;
			tmp = tmp+2.*dR*(dY/6.);
			dR  = dR*(dY/6.)*breitWigner(W,bwnorm)*dW;
      
			//For identical beams, we double.  Either may emit photon/scatter
			//For large differences in Z, we approx, that only beam1 emits photon
			//and beam2 scatters, eg d-Au where beam1=au and beam2=d
			if(getbbs().beam1().A()==getbbs().beam2().A()){
				dR  = 2.*dR;
			}
			int_r = int_r+dR;  
		}
	}
                                                                                                                                                      
	rate=luminosity()*int_r;
  
	cout<<" Cross section (mb): "<<10.*int_r<<endl;
	cout<<" Production rate   : "<<rate<<" Hz"<<endl;
}
