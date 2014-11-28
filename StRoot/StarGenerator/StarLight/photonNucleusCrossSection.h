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


#ifndef PHOTONNUCLEUSCROSSSECTION_H
#define PHOTONNUCLEUSCROSSSECTION_H


#include "starlightconstants.h"
#include "beambeamsystem.h"


class photonNucleusCrossSection {

public:

	photonNucleusCrossSection(const inputParameters& input,
	                          const beamBeamSystem&  bbsystem);
	~photonNucleusCrossSection();
  
	double         slopeParameter  () const { return _slopeParameter;   }  ///< returns slope of t-distribution [(GeV/c)^{-2}]
	double         getChannelMass  () const { return _channelMass;      }  ///< returns mass of the produced system [GeV/c^2]
	double         getBNORM        () const { return _BNORM;            }
	double         luminosity      () const { return _luminosity;       }  ///< returns luminosity [10^{26} cm^{-2} sec^{-1}]
	beamBeamSystem getbbs          () const { return _bbs;              }  ///< returns beamBeamSystem
	double         vmPhotonCoupling() const { return _vmPhotonCoupling; }  ///< vectormeson-photon coupling constant f_v / 4 pi (cf. Eq. 10 in KN PRC 60 (1999) 014903)
	double         getDefaultC     () const { return _defaultC;         }
	double         maxPhotonEnergy () const { return _maxPhotonEnergy;  }  ///< returns max photon energy in lab frame [GeV] (for vectormesons only)

	void crossSectionCalculation(const double bwnormsave);
	// Will think about it...For VMs we just calculate it
	// So just use the wide or narrow constructor to calculate it
	// wide/narrow will inherit this.
	double getcsgA(const double Egamma,
	               const double W);
	double photonFlux(const double Egamma);
	double sigmagp(const double Wgp);
	double sigma_A(const double sig_N);
	double breitWigner(const double W,
	                   const double C);

private:

	double nepoint(const double Egamma,
	               const double bmin);
  
	beamBeamSystem _bbs;
  
	// copied from inputParameters
	double                               _protonEnergy;
	double                               _beamLorentzGamma;    ///< Lorentz gamma factor of beams in collider frame
	starlightConstants::particleTypeEnum _particleType;
	int                                  _beamBreakupMode;     ///< breakup mode for beam particles
	bool                                 _coherentProduction;  ///< if true, production is coherent, else incoherent
	double                               _incoherentFactor;    ///< allows to scale the incoherent contribution in vector meson production
	int                                  _sigmaNucleus;

	// locally defined parameters
	double _slopeParameter;    ///< slope of t-distribution [(GeV/c)^{-2}]
	double _vmPhotonCoupling;  ///< vectormeson-photon coupling constant f_v / 4 pi (cf. Eq. 10 in KN PRC 60 (1999) 014903)
	double _ANORM;
	double _BNORM;
	double _defaultC;
	double _luminosity;       ///< luminosity [10^{26} cm^{-2} sec^{-1}]
	double _maxPhotonEnergy;  ///< max photon energy in lab frame [GeV] (for vectormesons only)
	double _width;            ///< width of the produced system  [GeV/c^2]
	double _channelMass;      ///< mass of the produced system  [GeV/c^2]
};


#endif  // PHOTONNUCLEUSCROSSSECTION_H
