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
//    Converted all nuclear formfactors to come from the scattering nucleus(nucleus2)
//    Added incoherent condition to the cross-section that follows a similar approach as pp
//    Could not figure out the scaling exactly for incoherent(possibly units) so divided by
//    1E-4 and there is a incoherence factor that can be selected in the input file,
//    starlight.in--JWB
//    Also, it has not been implemented for interference.
//
//
//
///////////////////////////////////////////////////////////////////////////


#include <iostream>
#include <fstream>
#include <cmath>

#include "reportingUtils.h"
#include "starlightconstants.h"
#include "bessel.h"
#include "photonNucleusCrossSection.h"


using namespace std;
using namespace starlightConstants;


//______________________________________________________________________________
photonNucleusCrossSection::photonNucleusCrossSection(const inputParameters& input,
                                                     const beamBeamSystem&  bbsystem)
	: _bbs               (bbsystem                  ),
	  _protonEnergy      (input.getProtonEnergy()   ),
	  _beamLorentzGamma  (input.beamLorentzGamma()  ),
	  _particleType      (input.prodParticleType()  ),
	  _beamBreakupMode   (input.beamBreakupMode()   ),
	  _coherentProduction(input.coherentProduction()),
	  _incoherentFactor  (input.incoherentFactor()  ),
	  _sigmaNucleus      (_bbs.beam2().A()          )
{
	// define luminosity for various beam particles in units of 10^{26} cm^{-2} sec^{-1}
	switch(_bbs.beam1().Z()) {
	case 1:   // proton
		_luminosity = 1.E8;
		break;
	case 8:   // O
		_luminosity = 980.;
		break;
	case 14:  // Si
		_luminosity = 440.;
		break;
	case 20:  // Ca
		_luminosity = 2000.;
		break;
	case 29:  // Cu
		_luminosity = 95.;
		break;
	case 49:  // Indium, uses same as Iodine
		_luminosity = 27.;
		break;
	case 53:  // I
		_luminosity = 27.;
		break;
	case 79:  // Au
		_luminosity = 2.0;
		break;
	case 82:  // Pb
		_luminosity = 1.;
		break;
	default:
		printWarn << "luminosity is not defined for beam with Z = " << _bbs.beam1().Z()
		          << ". using " << _luminosity << " 10^{26} cm^{-2} sec^{-1}" << endl;
	}

	switch(_particleType) {
	case RHO:
		_slopeParameter = 11.0;  // [(GeV/c)^{-2}]
		_vmPhotonCoupling = 2.02;
		_ANORM       = -2.75;
		_BNORM       = 0.0;
		_defaultC    = 1.0;
		_channelMass = 0.7685;  // [GeV/c^2]
		_width       = 0.1507;  // [GeV/c^2]
		break;
	case RHOZEUS:
		_slopeParameter =11.0;
		_vmPhotonCoupling=2.02;
		_ANORM=-2.75;
		_BNORM=1.84;
		_defaultC=1.0;
		_channelMass = 0.7685;
		_width=0.1507;
		break;
	case FOURPRONG:
		_slopeParameter      = 11.0;
		_vmPhotonCoupling      = 2.02;
		_ANORM       = -2.75;
		_BNORM       = 0;  // no coherent background component is implemented for four-prong
		_defaultC    = 11.0;
		_channelMass = 1.350;
		_width       = 0.360;
		break;
	case OMEGA:
		_slopeParameter=10.0;
		_vmPhotonCoupling=23.13;
		_ANORM=-2.75;
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=0.78194;
		_width=0.00843;
		break;
	case PHI:
		_slopeParameter=7.0;
		_vmPhotonCoupling=13.71;
		_ANORM=-2.75;
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=1.019413;
		_width=0.00443;
		break;
	case JPSI:
	case JPSI_ee:
	case JPSI_mumu:
		_slopeParameter=4.0;
		_vmPhotonCoupling=10.45;
		_ANORM=-2.75;//Artificial Breit-Wigner parameters--no direct pions
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=3.09692;//JN 3.09688
		_width=0.000091;//JN 0.000087
		break;
	case JPSI2S:
	case JPSI2S_ee:
	case JPSI2S_mumu:
		_slopeParameter=4.3;
		_vmPhotonCoupling=26.39;
		_ANORM=-2.75;//Artificial
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=3.686093;
		_width=0.000337;
		break;
	case UPSILON:
	case UPSILON_ee:
	case UPSILON_mumu:
		_slopeParameter=4.0;
		_vmPhotonCoupling=125.37;
		_ANORM=-2.75;//Artificial
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=9.46030;
		_width=0.00005402;
		break;
	case UPSILON2S:
	case UPSILON2S_ee:
	case UPSILON2S_mumu:
		_slopeParameter=4.0;
		_vmPhotonCoupling=290.84;
		_ANORM=-2.75;
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=10.02326;
		_width=0.00003198;
		break;
	case UPSILON3S:
	case UPSILON3S_ee:
	case UPSILON3S_mumu:
		_slopeParameter=4.0;
		_vmPhotonCoupling=415.10;
		_ANORM=-2.75;
		_BNORM=0.0;
		_defaultC=1.0;
		_channelMass=10.3552;
		_width=0.00002032;
		break;
	default:
		cout <<"No sigma constants parameterized for pid: "<<_particleType
		     <<" GammaAcrosssection"<<endl;
	}

	_maxPhotonEnergy = 4. * _beamLorentzGamma * hbarc/_bbs.beam1().nuclearRadius(); 
	//Max photon energy( for VM only, in GeV, lab frame, use beam energy
	//, nuclear size cutoff)
}


//______________________________________________________________________________
photonNucleusCrossSection::~photonNucleusCrossSection()
{ }


//______________________________________________________________________________
void
photonNucleusCrossSection::crossSectionCalculation(const double)
{
	cout << "Neither narrow/wide resonance cross-section calculation.--Derived" << endl;
}


//______________________________________________________________________________
double
photonNucleusCrossSection::getcsgA(const double Egamma,
                                   const double W)
{
	//This function returns the cross-section for photon-nucleus interaction 
	//producing vectormesons
  
	double Av,Wgp,cs,cvma;
	double t,tmin,tmax;
	double csgA,ax,bx;
	int NGAUSS;                                                                                                                                       
  
	//     DATA FOR GAUSS INTEGRATION
	double xg[6] = {0, 0.1488743390, 0.4333953941, 0.6794095683, 0.8650633667, 0.9739065285};
	double ag[6] = {0, 0.2955242247, 0.2692667193, 0.2190863625, 0.1494513492, 0.0666713443};
	NGAUSS = 6;
  
	//       Find gamma-proton CM energy
	Wgp = sqrt(2. * Egamma * (_protonEnergy
	                          + sqrt(_protonEnergy * _protonEnergy - protonMass * protonMass))
	           + protonMass * protonMass);
	
	//Used for d-A and A-A
	tmin = (W * W / (4. * Egamma * _beamLorentzGamma)) * (W * W / (4. * Egamma * _beamLorentzGamma));
  
	if ((_bbs.beam1().A() == 1) && (_bbs.beam2().A() == 1))  // proton-proton, no scaling needed
		csgA = sigmagp(Wgp);
	else if ((_bbs.beam2().Z() == 1) && (_bbs.beam2().A() == 2)) {  // deuteron-A interaction
		Av = _slopeParameter * sigmagp(Wgp);
      
		tmax   = tmin + 0.64;   //0.64
		ax     = 0.5 * (tmax - tmin);
		bx     = 0.5 * (tmax + tmin);
		csgA   = 0.;
      
		for (int k = 1; k < NGAUSS; ++k) { 
			t    = ax * xg[k] + bx;
			// We use beam2 here since the input stores the deuteron as nucleus 2
			// and nucleus 2 is the pomeron field source
			// Also this is the way sergey formatted the formfactor.
			csgA = csgA + ag[k] * _bbs.beam2().formFactor(t); 
			t    = ax * (-xg[k]) + bx;
			csgA = csgA + ag[k] * _bbs.beam2().formFactor(t);
		}
		csgA = 0.5 * (tmax - tmin) * csgA;
		csgA = Av * csgA;
	}	else if (!_coherentProduction &&
		         (!((_bbs.beam2().Z() == 1) && (_bbs.beam2().A() == 2)))) {  // incoherent AA interactions
		// For incoherent AA interactions, since incoherent treating it as gamma-p
		// Calculate the differential V.M.+proton cross section
		//artifical 1E-3 to scale down sigma
		csgA = 1.E-4 * _incoherentFactor * _sigmaNucleus * _slopeParameter * sigmagp(Wgp);

		// Calculating int |F(t)| dt
		// Using proton formfactor for this case
		// Note the coherence scaling factor being intergrated with the F(t)
		// Should it just be F(t)^2?   
		// Pay attention to the way the formfactor is implemented in nucleus class
		// Also, notice the tmin value.  It starts higher for dAu, should we proceed
		// in a similar fashion
		// Why don't we use formfactors for pp? Is it because it is incorporated in 
		// the gamma-p fits done for dsigma/dt? Yes?
	}	else {	// coherent AA interactions
		// For typical AA interactions.
		// Calculate V.M.+proton cross section
		cs = sqrt(16. * pi * _vmPhotonCoupling * _slopeParameter * hbarc * hbarc * sigmagp(Wgp) / alpha);
    
		// Calculate V.M.+nucleus cross section
		cvma = sigma_A(cs); 
 
		// Calculate Av = dsigma/dt(t=0) Note Units: fm**s/Gev**2
		Av = (alpha * cvma * cvma) / (16. * pi * _vmPhotonCoupling * hbarc * hbarc);
    
		tmax   = tmin + 0.25;
		ax     = 0.5 * (tmax - tmin);
		bx     = 0.5 * (tmax + tmin);
		csgA   = 0.;
		for (int k = 1; k < NGAUSS; ++k) { 
			t    = ax * xg[k] + bx;
			csgA = csgA + ag[k] * _bbs.beam2().formFactor(t) * _bbs.beam2().formFactor(t);
			t    = ax * (-xg[k]) + bx;
			csgA = csgA + ag[k] * _bbs.beam2().formFactor(t) * _bbs.beam2().formFactor(t);
		}
		csgA = 0.5 * (tmax - tmin) * csgA;
		csgA = Av * csgA;
	}

	return csgA;	
}


//______________________________________________________________________________
double
photonNucleusCrossSection::photonFlux(const double Egamma)
{
	// This routine gives the photon flux as a function of energy Egamma
	// It works for arbitrary nuclei and gamma; the first time it is
	// called, it calculates a lookup table which is used on
	// subsequent calls.
	// It returns dN_gamma/dE (dimensions 1/E), not dI/dE
	// energies are in GeV, in the lab frame
	// rewritten 4/25/2001 by SRK
  
	double lEgamma,Emin,Emax;
	static double lnEmax, lnEmin, dlnE;
	double stepmult,energy,rZ;
	int nbstep,nrstep,nphistep,nstep;
	double bmin,bmax,bmult,biter,bold,integratedflux;
	double fluxelement,deltar,riter;
	double deltaphi,phiiter,dist;
	static double dide[401];
	double lnElt;
	double flux_r; //Returns the flux.
	double Xvar;
	int Ilt;
	double RNuc=0.,RNuc2=0.;

	RNuc=_bbs.beam1().nuclearRadius();
	RNuc2=_bbs.beam2().nuclearRadius();
	// static ->>> dide,lnEMax,lnEmin,dlnE
	static int  Icheck = 0;
  
	//Check first to see if pp (JN0705)
	if( _bbs.beam1().A()==1 && _bbs.beam2().A()==1 ){
		int nbsteps = 200;
		double bmin = 0.5;
		double bmax = 5.0 + (5.0*_beamLorentzGamma*hbarc/Egamma);
		double dlnb = (log(bmax)-log(bmin))/(1.*nbsteps);

		double local_sum=0.0;

		// Impact parameter loop 
		for (int i = 0; i<=nbsteps;i++){

			double bnn0 = bmin*exp(i*dlnb);
			double bnn1 = bmin*exp((i+1)*dlnb);
			double db   = bnn1-bnn0;
        
			//      double PofB0 = 1.0; 
			//      if( bnn0 > 1.4 )PofB0=0.0;
			//      double PofB1 = 1.0; 
			//      if( bnn1 > 1.4 )PofB1=0.0;
      
			double ppslope = 19.0; 
			double GammaProfile = exp(-bnn0*bnn0/(2.*hbarc*hbarc*ppslope));  
			double PofB0 = 1. - (1. - GammaProfile)*(1. - GammaProfile);   
			GammaProfile = exp(-bnn1*bnn1/(2.*hbarc*hbarc*ppslope));  
			double PofB1 = 1. - (1. - GammaProfile)*(1. - GammaProfile);   

			double Xarg = Egamma*bnn0/(hbarc*_beamLorentzGamma);
			double loc_nofe0 = (_bbs.beam1().Z()*_bbs.beam1().Z()*alpha)/
				(pi*pi); 
			loc_nofe0 *= (1./(Egamma*bnn0*bnn0)); 
			loc_nofe0 *= Xarg*Xarg*(bessel::dbesk1(Xarg))*(bessel::dbesk1(Xarg)); 

			Xarg = Egamma*bnn1/(hbarc*_beamLorentzGamma);
			double loc_nofe1 = (_bbs.beam1().Z()*_bbs.beam1().Z()*alpha)/
				(pi*pi); 
			loc_nofe1 *= (1./(Egamma*bnn1*bnn1)); 
			loc_nofe1 *= Xarg*Xarg*(bessel::dbesk1(Xarg))*(bessel::dbesk1(Xarg)); 

			local_sum += loc_nofe0*(1. - PofB0)*bnn0*db; 
			local_sum += loc_nofe1*(1. - PofB1)*bnn1*db; 

		}
		// End Impact parameter loop 

		// Note: 2*pi --> pi because of no factor 2 above 
		double flux_r=local_sum*pi; 
		return flux_r;

		//    bmin = nuclearRadius+nuclearRadius;
		//    flux_r = nepoint(Egamma,bmin);
		//    return flux_r;
	}

	//   first call?  - initialize - calculate photon flux
	Icheck=Icheck+1;
	if(Icheck > 1) goto L1000f;
  
	rZ=double(_bbs.beam1().Z());
  
	//  Nuclear breakup is done by PofB
	//  collect number of integration steps here, in one place
  
	nbstep=400;
	nrstep=60;
	nphistep=40;
  
	//  this last one is the number of energy steps
	nstep=100;
  
	// following previous choices, take Emin=10 keV at LHC, Emin = 1 MeV at RHIC
  
	Emin=1.E-5;
	if (_beamLorentzGamma < 500) 
		Emin=1.E-3;
  
	//  maximum energy is 12 times the cutoff
	//  25 GeV for gold at RHIC, 650 GeV for lead at LHC
  
	Emax=12.*hbarc*_beamLorentzGamma/RNuc;
	//Will this be diff for dAu?
  
	//     >> lnEmin <-> ln(Egamma) for the 0th bin
	//     >> lnEmax <-> ln(Egamma) for the last bin
  
	lnEmin=log(Emin);
	lnEmax=log(Emax);
	dlnE=(lnEmax-lnEmin)/nstep;                                                                                                                  

	cout<<" Calculating flux for photon energies from E= "<<Emin 
	    <<" to  "<<Emax<<"  GeV (lab frame) "<<endl;


	stepmult= exp(log(Emax/Emin)/double(nstep));
	energy=Emin;
  
	for (int j = 1; j<=nstep;j++){
		energy=energy*stepmult;
    
		//  integrate flux over 2R_A < b < 2R_A+ 6* gamma hbar/energy
		//  use exponential steps
    
		bmin=RNuc+RNuc2; //2.*nuclearRadius; Sergey
		bmax=bmin + 6.*hbarc*_beamLorentzGamma/energy;
    
		bmult=exp(log(bmax/bmin)/double(nbstep));
		biter=bmin;
		integratedflux=0.;
    
		if (_bbs.beam2().Z()==1&&_bbs.beam1().A()==2){
			//This is for deuteron-gold
			Xvar = (RNuc+RNuc2)*energy/(hbarc*(_beamLorentzGamma));
      
			fluxelement = (2.0/pi)*rZ*rZ*alpha/
				energy*(Xvar*bessel::dbesk0(Xvar)*bessel::dbesk1(Xvar)-(1/2)*Xvar*Xvar*
				        (bessel::dbesk1(Xvar)*bessel::dbesk1(Xvar)-bessel::dbesk0(Xvar)*bessel::dbesk0(Xvar)));
      
			integratedflux=integratedflux+fluxelement;
                
		}//if dAu
		else{ 
			for (int jb = 1; jb<=nbstep;jb++){
				bold=biter;
				biter=biter*bmult;
				// When we get to b>20R_A change methods - just take the photon flux
				//  at the center of the nucleus.
				if (biter > (10.*RNuc))
					{
						// if there is no nuclear breakup or only hadronic breakup, which only
						// occurs at smaller b, we can analytically integrate the flux from b~20R_A
						// to infinity, following Jackson (2nd edition), Eq. 15.54
						Xvar=energy*biter/(hbarc*_beamLorentzGamma);
						// Here, there is nuclear breakup.  So, we can't use the integrated flux
						//  However, we can do a single flux calculation, at the center of the
						//  nucleus
	    
						// Eq. 41 of Vidovic, Greiner and Soff, Phys.Rev.C47,2308(1993), among other places
						//  this is the flux per unit area
						fluxelement  = (rZ*rZ*alpha*energy)*
							(bessel::dbesk1(Xvar))*(bessel::dbesk1(Xvar))/
							((pi*_beamLorentzGamma*hbarc)*
							 (pi*_beamLorentzGamma*hbarc));
	    
					}//if biter>10
				else{
					// integrate over nuclear surface. n.b. this assumes total shadowing -
					// treat photons hitting the nucleus the same no matter where they strike
					fluxelement=0.;
					deltar=RNuc/double(nrstep);
					riter=-deltar/2.;
          
					for (int jr =1; jr<=nrstep;jr++){
						riter=riter+deltar;
						// use symmetry;  only integrate from 0 to pi (half circle)
						deltaphi=pi/double(nphistep);
						phiiter=0.;
            
						for( int jphi=1;jphi<= nphistep;jphi++){
							phiiter=(double(jphi)-0.5)*deltaphi;
							//  dist is the distance from the center of the emitting nucleus to the point in question
							dist=sqrt((biter+riter*cos(phiiter))*(biter+riter*
							                                      cos(phiiter))+(riter*sin(phiiter))*(riter*sin(phiiter)));
	      
							Xvar=energy*dist/(hbarc*_beamLorentzGamma);				
	      
							flux_r = (rZ*rZ*alpha*energy)*
								(bessel::dbesk1(Xvar)*bessel::dbesk1(Xvar))/
								((pi*_beamLorentzGamma*hbarc)*
								 (pi*_beamLorentzGamma*hbarc));
	      
							//  The surface  element is 2.* delta phi* r * delta r
							//  The '2' is because the phi integral only goes from 0 to pi
							fluxelement=fluxelement+flux_r*2.*deltaphi*riter*deltar;
							//  end phi and r integrations
						}//for(jphi)
					}//for(jr)
					//  average fluxelement over the nuclear surface
					fluxelement=fluxelement/(pi*RNuc*RNuc);
				}//else
				//  multiply by volume element to get total flux in the volume element
				fluxelement=fluxelement*2.*pi*biter*(biter-bold);
				//  modulate by the probability of nuclear breakup as f(biter)
				if (_beamBreakupMode > 1){
					fluxelement=fluxelement*_bbs.probabilityOfBreakup(biter);
				}
				integratedflux=integratedflux+fluxelement;
	
			}//end of for
		}  //end of else
		// end energy integration
		// nobody going here any more
    
		//  In lookup table, store k*dN/dk because it changes less
		//  so the interpolation should be better
    
		dide[j]=integratedflux*energy;
                                     
	}//end of for.
       
	//  for 2nd and subsequent calls, use lookup table immediately
  
 L1000f:
  
	lEgamma=log(Egamma);
	if (lEgamma < (lnEmin+dlnE) ||  lEgamma  > lnEmax){
		flux_r=0.0;
		cout<<"  ERROR: Egamma outside defined range. Egamma= "<<Egamma
		    <<"   "<<lnEmax<<" "<<(lnEmin+dlnE)<<endl;
	}
	else{
		//       >> Egamma between Ilt and Ilt+1
		Ilt = int((lEgamma-lnEmin)/dlnE);
		//       >> ln(Egamma) for first point 
		lnElt = lnEmin + Ilt*dlnE; 
		//       >> Interpolate
		flux_r = dide[Ilt] + ((lEgamma-lnElt)/dlnE)*(dide[Ilt+1]- dide[Ilt]);
		flux_r = flux_r/Egamma;
	}
  
	return flux_r;
}


//______________________________________________________________________________
double
photonNucleusCrossSection::nepoint(const double Egamma,
                                   const double bmin)
{
	//     >> Function for the spectrum of virtual photons,
	//     >> dn/dEgamma, for a point charge q=Ze sweeping
	//     >> past the origin with velocity gamma
	//     >> (=1/SQRT(1-(V/c)**2)) integrated over impact
	//     >> parameter from bmin to infinity
	//     >> See Jackson eq15.54 Classical Electrodynamics
	//     >> Declare Local Variables
	double beta,X,C1,bracket,nepoint_r;
  
	beta = sqrt(1.-(1./(_beamLorentzGamma*_beamLorentzGamma)));
	X = (bmin*Egamma)/(beta*_beamLorentzGamma*hbarc);
  
	bracket = -0.5*beta*beta*X*X*(bessel::dbesk1(X)*bessel::dbesk1(X)
	                              -bessel::dbesk0(X)*bessel::dbesk0(X));

	bracket = bracket+X*bessel::dbesk0(X)*bessel::dbesk1(X);
  
	C1=(2.*double((_bbs.beam1().Z())*(_bbs.beam1().Z()))*
	    alpha)/pi;
  
	//Looks like this is only used in photon flux for the case of pp collisions..
	//might be able to remove the Zs.
	nepoint_r = C1*(1./beta)*(1./beta)*(1./Egamma)*bracket;
  
	return nepoint_r;
  
}


//______________________________________________________________________________
double
photonNucleusCrossSection::sigmagp(const double Wgp)
{
	//     >> Function for the gamma-proton --> VectorMeson
	//     >> cross section. Wgp is the gamma-proton CM energy.
	//     >> Unit for cross section: fm**2
  
	double sigmagp_r=0.;
  
	switch(_particleType)
		{ 
		case RHO:
		case RHOZEUS:
		case FOURPRONG:
			sigmagp_r=1.E-4*(5.0*exp(0.22*log(Wgp))+26.0*exp(-1.23*log(Wgp)));
			break;
		case OMEGA:
			sigmagp_r=1.E-4*(0.55*exp(0.22*log(Wgp))+18.0*exp(-1.92*log(Wgp)));
			break;                                                      
		case PHI:
			sigmagp_r=1.E-4*0.34*exp(0.22*log(Wgp));
			break;
		case JPSI:
		case JPSI_ee:
		case JPSI_mumu:
			sigmagp_r=(1.0-((_channelMass+protonMass)*(_channelMass+protonMass))/(Wgp*Wgp));
			sigmagp_r*=sigmagp_r;
			sigmagp_r*=1.E-4*0.00406*exp(0.65*log(Wgp));
			// sigmagp_r=1.E-4*0.0015*exp(0.80*log(Wgp));
			break;
		case JPSI2S:
		case JPSI2S_ee:
		case JPSI2S_mumu:
			sigmagp_r=(1.0-((_channelMass+protonMass)*(_channelMass+protonMass))/(Wgp*Wgp));
			sigmagp_r*=sigmagp_r;
			sigmagp_r*=1.E-4*0.00406*exp(0.65*log(Wgp));
			sigmagp_r*=0.166;  
			//      sigmagp_r=0.166*(1.E-4*0.0015*exp(0.80*log(Wgp)));
			break;
		case UPSILON:
		case UPSILON_ee:
		case UPSILON_mumu:
			//       >> This is W**1.7 dependence from QCD calculations
			sigmagp_r=1.E-10*(0.060)*exp(1.70*log(Wgp));
			break;
		case UPSILON2S:
		case UPSILON2S_ee:
		case UPSILON2S_mumu:
			sigmagp_r=1.E-10*(0.0259)*exp(1.70*log(Wgp));
			break;
		case UPSILON3S:
		case UPSILON3S_ee:
		case UPSILON3S_mumu:
			sigmagp_r=1.E-10*(0.0181)*exp(1.70*log(Wgp));
			break;
		default: cout<< "!!!  ERROR: Unidentified Vector Meson: "<< _particleType <<endl;
		}                                                                  
	return sigmagp_r;
}


//______________________________________________________________________________
double
photonNucleusCrossSection::sigma_A(const double sig_N)
{                                                         
	// Nuclear Cross Section
	// sig_N,sigma_A in (fm**2) 

	double sum;
	double b,bmax,Pint,arg,sigma_A_r;
  
	int NGAUSS;
  
	double xg[17]=
		{.0,
		 .0483076656877383162,.144471961582796493,
		 .239287362252137075, .331868602282127650,
		 .421351276130635345, .506899908932229390,
		 .587715757240762329, .663044266930215201,
		 .732182118740289680, .794483795967942407,
		 .849367613732569970, .896321155766052124,
		 .934906075937739689, .964762255587506430,
		 .985611511545268335, .997263861849481564
		};
  
	double ag[17]=
		{.0,
		 .0965400885147278006, .0956387200792748594,
		 .0938443990808045654, .0911738786957638847,
		 .0876520930044038111, .0833119242269467552,
		 .0781938957870703065, .0723457941088485062,
		 .0658222227763618468, .0586840934785355471,
		 .0509980592623761762, .0428358980222266807,
		 .0342738629130214331, .0253920653092620595,
		 .0162743947309056706, .00701861000947009660
		};
  
	NGAUSS=16;
  
	// CALCULATE P(int) FOR b=0.0 - bmax (fm)
	bmax = 25.0;
	sum  = 0.;
	for(int IB=1;IB<=NGAUSS;IB++){
    
		b = 0.5*bmax*xg[IB]+0.5*bmax;
    
		arg=-sig_N*_bbs.beam1().rho0()*_bbs.beam1().thickness(b);
    
		Pint=1.0-exp(arg);
		sum=sum+2.*pi*b*Pint*ag[IB];
    
		b = 0.5*bmax*(-xg[IB])+0.5*bmax;
		arg=-sig_N*_bbs.beam1().rho0()*_bbs.beam1().thickness(b);
		Pint=1.0-exp(arg);
		sum=sum+2.*pi*b*Pint*ag[IB];

	}

	sum=0.5*bmax*sum;
  
	sigma_A_r=sum;
 
	return sigma_A_r;
}


//______________________________________________________________________________
double
photonNucleusCrossSection::breitWigner(const double W,
                                       const double C)
{
	// use simple fixed-width s-wave Breit-Wigner without coherent backgorund for rho'
	// (PDG '08 eq. 38.56)
	if(_particleType==FOURPRONG) {
		if (W < 4.01 * pionChargedMass)
			return 0;
		const double termA  = _channelMass * _width;
		const double termA2 = termA * termA;
		const double termB  = W * W - _channelMass * _channelMass;
		return C * _ANORM * _ANORM * termA2 / (termB * termB + termA2);
	}

	// Relativistic Breit-Wigner according to J.D. Jackson,
	// Nuovo Cimento 34, 6692 (1964), with nonresonant term. A is the strength
	// of the resonant term and b the strength of the non-resonant
	// term. C is an overall normalization.

	double ppi=0.,ppi0=0.,GammaPrim,rat;
	double aa,bb,cc;
  
	double nrbw_r;

	// width depends on energy - Jackson Eq. A.2
	// if below threshold, then return 0.  Added 5/3/2001 SRK
	// 0.5% extra added for safety margin
	if( _particleType==RHO ||_particleType==RHOZEUS){  
		if (W < 2.01*pionChargedMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt( ((W/2.)*(W/2.)) - pionChargedMass * pionChargedMass);
		ppi0=0.358;
	}
  
	// handle phi-->K+K- properly
	if (_particleType  ==  PHI){
		if (W < 2.*kaonChargedMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt( ((W/2.)*(W/2.))- kaonChargedMass*kaonChargedMass);
		ppi0=sqrt( ((_channelMass/2.)*(_channelMass/2.))-kaonChargedMass*kaonChargedMass);
	}

	//handle J/Psi-->e+e- properly
	if (_particleType==JPSI || _particleType==JPSI2S){
		if(W<2.*mel){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-mel*mel);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-mel*mel);
	}
	if (_particleType==JPSI_ee){
		if(W<2.*mel){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-mel*mel);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-mel*mel);   
	}
	if (_particleType==JPSI_mumu){
		if(W<2.*muonMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-muonMass*muonMass);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-muonMass*muonMass);
	}
	if (_particleType==JPSI2S_ee){
		if(W<2.*mel){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-mel*mel);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-mel*mel);   
	}
	if (_particleType==JPSI2S_mumu){
		if(W<2.*muonMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-muonMass*muonMass);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-muonMass*muonMass);
	}

	if(_particleType==UPSILON || _particleType==UPSILON2S ||_particleType==UPSILON3S ){ 
		if (W<2.*muonMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-muonMass*muonMass);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-muonMass*muonMass);
	}
  
	if(_particleType==UPSILON_mumu || _particleType==UPSILON2S_mumu ||_particleType==UPSILON3S_mumu ){ 
		if (W<2.*muonMass){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-muonMass*muonMass);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-muonMass*muonMass);
	}
  
	if(_particleType==UPSILON_ee || _particleType==UPSILON2S_ee ||_particleType==UPSILON3S_ee ){ 
		if (W<2.*mel){
			nrbw_r=0.;
			return nrbw_r;
		}
		ppi=sqrt(((W/2.)*(W/2.))-mel*mel);
		ppi0=sqrt(((_channelMass/2.)*(_channelMass/2.))-mel*mel);
	}
  
	if(ppi==0.&&ppi0==0.) 
		cout<<"Improper Gammaacrosssection::breitwigner, ppi&ppi0=0."<<endl;
  
	rat=ppi/ppi0;
	GammaPrim=_width*(_channelMass/W)*rat*rat*rat;
  
	aa=_ANORM*sqrt(GammaPrim*_channelMass*W);
	bb=W*W-_channelMass*_channelMass;
	cc=_channelMass*GammaPrim;
  
	// First real part squared 
	nrbw_r = (( (aa*bb)/(bb*bb+cc*cc) + _BNORM)*( (aa*bb)/(bb*bb+cc*cc) + _BNORM));
  
	// Then imaginary part squared 
	nrbw_r = nrbw_r + (( (aa*cc)/(bb*bb+cc*cc) )*( (aa*cc)/(bb*bb+cc*cc) ));

	//  Alternative, a simple, no-background BW, following J. Breitweg et al.
	//  Eq. 15 of Eur. Phys. J. C2, 247 (1998).  SRK 11/10/2000
	//      nrbw_r = (_ANORM*_mass*GammaPrim/(bb*bb+cc*cc))**2
  
	nrbw_r = C*nrbw_r;
  
	return nrbw_r;    
}
