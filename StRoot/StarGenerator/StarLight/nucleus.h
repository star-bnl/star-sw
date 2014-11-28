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


#ifndef NUCLEUS_H
#define NUCLEUS_H


#include <cmath>


//This class holds the information for a target nucleus
class nucleus
{

public:

	nucleus(const int    Z,
	        const int    A,
	        const double deuteronSlopePar,
	        const bool   dAuCoherentProduction);
	~nucleus();
 
	int    Z              () const { return _Z;                     }  ///< returns atomic number of nucleus
	int    A              () const { return _A;                     }  ///< returns nucleon number of nucleus
	double woodSaxonRadius() const { return 1.2 * pow(_A, 1. / 3.); }  ///< returns Wood-Saxon nuclear radius [fm] (Fermi model)
	double nuclearRadius  () const; ///< returns nuclear radius [fm]; except for some special nuclei this is the Wood-Saxon radius (Fermi model)

	double formFactor(const double t) const;  ///< computes form factor for given squared 4-momentum transfer
	double thickness (const double b) const;  ///< calculates nuclear thickness function for given distance b in impact parameter space (Eq. 4 in KN, PRC 60)

	double Q0  () const { return _Q0;   }
	double rho0() const { return _rho0; }

	
private:

	double woodSaxonSkinDepth() const { return 0.53;                   }  ///< returns surface (0.53 fm for Au)
	double fritiofR0         () const { return _r0 * pow(_A, (1./3.)); }  ///< Fritiof _r0 (rws)/formfactor

	double rws(const double r) const
	{ return 1.0 / (1. + exp((r - fritiofR0()) / woodSaxonSkinDepth())); } ///< Wood-Saxon nuclear density

	int    _Z;                      ///< atomic number of nucleus
	int    _A;                      ///< nucleon number of nucleus
	double _deuteronSlopePar;       ///< slope parameter for deuteron form factor [(GeV/c)^{-2}]
	bool   _dAuCoherentProduction;  ///< if true, production in d Au collisions is coherent, else incoherent

	double _r0;
	double _rho0;
	double _Q0;

};


#endif  // NUCLEUS_H
