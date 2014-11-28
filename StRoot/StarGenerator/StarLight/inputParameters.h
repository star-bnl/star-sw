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


#ifndef INPUTPARAMETERS_H
#define INPUTPARAMETERS_H


#include "starlightconstants.h"
//This is where we read in our input values.


class inputParameters {

public:

	inputParameters();
	~inputParameters();

	bool init(const std::string& configFileName = "./config/slight.in");
	bool init(std::ifstream &configStream );

	unsigned int beam1Z                () const { return _beam1Z;                 }  ///< returns atomic number of beam particle 1
	unsigned int beam1A                () const { return _beam1A;                 }  ///< returns atomic mass number of beam particle 1
	unsigned int beam2Z                () const { return _beam2Z;                 }  ///< returns atomic number of beam particle 2
	unsigned int beam2A                () const { return _beam2A;                 }  ///< returns atomic mass number of beam particle 2
	double       beamLorentzGamma      () const { return _beamLorentzGamma;       }  ///< returns Lorentz gamma factor of beams in collider frame
	double       maxW                  () const { return _maxW;                   }  ///< returns maximum mass W of produced hadronic system [GeV/c^2]
	double       minW                  () const { return _minW;                   }  ///< returns minimum mass W of produced hadronic system [GeV/c^2]
	unsigned int nmbWBins              () const { return _nmbWBins;               }  ///< returns number of W bins in lookup table
	double       maxRapidity           () const { return _maxRapidity;            }  ///< returns maximum absolute value of rapidity
	unsigned int nmbRapidityBins       () const { return _nmbRapidityBins;        }  ///< returns number of rapidity bins in lookup table
	bool         ptCutEnabled          () const { return _ptCutEnabled;           }  ///< returns cut in pt
	double       ptCutMin              () const { return _ptCutMin;               }  ///< returns minimum pt
	double       ptCutMax              () const { return _ptCutMax;               }  ///< returns maximum pt
	bool         etaCutEnabled         () const { return _etaCutEnabled;          }  ///< returns cut in eta
	double       etaCutMin             () const { return _etaCutMin;              }  ///< returns minimum eta
	double       etaCutMax             () const { return _etaCutMax;              }  ///< returns maximum eta
	int          productionMode        () const { return _productionMode;         }  ///< returns production mode
	unsigned int nmbEvents             () const { return _nmbEventsTot;           }  ///< returns total number of events to generate
	int          prodParticleId        () const { return _prodParticleId;         }  ///< returns PDG particle ID of produced particle
	int          randomSeed            () const { return _randomSeed;             }  ///< returns seed for random number generator
	int          outputFormat          () const { return _outputFormat;           }  ///< returns output format
	int          beamBreakupMode       () const { return _beamBreakupMode;        }  ///< returns breakup mode for beam particles
	bool         interferenceEnabled   () const { return _interferenceEnabled;    }  ///< returns whether interference is taken into account
	double       interferenceStrength  () const { return _interferenceStrength;   }  ///< returns percentage of interference
	bool         coherentProduction    () const { return _coherentProduction;     }  ///< returns whether production is coherent or incoherent
	double       incoherentFactor      () const { return _incoherentFactor;       }  ///< returns incoherent contribution in vector meson production
	double       deuteronSlopePar      () const { return _deuteronSlopePar;       }  ///< returns slope parameter for deuteron form factor [(GeV/c)^{-2}]
	double       maxPtInterference     () const { return _maxPtInterference;      }  ///< returns maximum p_T for interference calculation [GeV/c]
	int          nmbPtBinsInterference () const { return _nmbPtBinsInterference;  }  ///< returns number of p_T bins for interference calculation
	double       ptBinWidthInterference() const { return _ptBinWidthInterference; }  ///< returns width of p_T bins for interference calculation [GeV/c]

	starlightConstants::particleTypeEnum    prodParticleType     () const { return _particleType;    }  ///< returns type of produced particle
	starlightConstants::decayTypeEnum       prodParticleDecayType() const { return _decayType;       }  ///< returns decay type of produced particle
	starlightConstants::interactionTypeEnum interactionType      () const { return _interactionType; }  ///< returns interaction type
	// double vmPhotonCoupling();
	// double slopeParameter();
	double getProtonEnergy() const { return _protonEnergy; }

	std::ostream& print(std::ostream& out) const;  ///< prints parameter summary
	std::ostream& write(std::ostream& out) const;  ///< writes parameters back to an ostream

  
private:

	std::string _configFileName;  ///< path to configuration file (default = ./config/slight.in)

	// config file parameters
	unsigned int _beam1Z;                  ///< atomic number of beam particle 1
	unsigned int _beam1A;                  ///< atomic mass number of beam particle 1
	unsigned int _beam2Z;                  ///< atomic number of beam particle 2
	unsigned int _beam2A;                  ///< atomic mass number of beam particle 2
	double       _beamLorentzGamma;        ///< Lorentz gamma factor of beams in collider frame
	double       _maxW;                    ///< maximum mass W of produced hadronic system [GeV/c^2]
	double       _minW;                    ///< minimum mass W of produced hadronic system; if set to -1 default value is taken [GeV/c^2]
	unsigned int _nmbWBins;                ///< number of W bins in lookup table
	double       _maxRapidity;             ///< maximum absolute value of rapidity
	unsigned int _nmbRapidityBins;         ///< number of rapidity bins in lookup table
	bool         _ptCutEnabled;            ///< en/disables cut in pt
	double       _ptCutMin;                ///< minimum pt, if cut is enabled
	double       _ptCutMax;                ///< maximum pt, if cut is enabled
	bool         _etaCutEnabled;           ///< en/disables cut in eta
	double       _etaCutMin;               ///< minimum eta, if cut is enabled
	double       _etaCutMax;               ///< maximum eta, if cut is enabled
	int          _productionMode;          ///< \brief production mode
	                                       ///<
	                                       ///< 1 = photon-photon fusion,
	                                       ///< 2 = narrow vector meson resonance in photon-Pomeron fusion,
	                                       ///< 3 = Breit-Wigner vector meson resonance in photon-Pomeron fusion
	unsigned int _nmbEventsTot;            ///< total number of events to generate
	int          _prodParticleId;          ///< PDG particle ID of produced particle
	int          _randomSeed;              ///< seed for random number generator
	int          _outputFormat;            ///< \brief output format
	                                       ///<
	                                       ///< 1 = ASCII
	                                       ///< 2 = GSTARtext,
	                                       ///< 3 = PAW ntuple (not working)
	int          _beamBreakupMode;         ///< \brief breakup mode for beam particles
	                                       ///<
	                                       ///< 1 = hard sphere nuclei (b > 2R),
	                                       ///< 2 = both nuclei break up (XnXn),
	                                       ///< 3 = a single neutron from each nucleus (1n1n),
	                                       ///< 4 = neither nucleon breaks up (with b > 2R),
	                                       ///< 5 = no hadronic break up (similar to option 1, but with the actual hadronic interaction)
	bool         _interferenceEnabled;     ///< if true, interference is taken into account
	double       _interferenceStrength;    ///< percentage of interference: from 0 = none to 1 = full
	bool         _coherentProduction;      ///< if true, production is coherent, else incoherent
	double       _incoherentFactor;        ///< allows to scale the incoherent contribution in vector meson production
	double       _deuteronSlopePar;        ///< slope parameter for deuteron form factor [(GeV/c)^{-2}]
	double       _maxPtInterference;       ///< maximum p_T for interference calculation [GeV/c]
	int          _nmbPtBinsInterference;   ///< number of p_T bins for interference calculation
	double       _ptBinWidthInterference;  ///< width of p_T bins for interference calculation [GeV/c]
	double       _protonEnergy;

	starlightConstants::particleTypeEnum    _particleType;
	starlightConstants::decayTypeEnum       _decayType;
	starlightConstants::interactionTypeEnum _interactionType;

};


inline
std::ostream&
operator <<(std::ostream&          out,
            const inputParameters& par)
{
	return par.print(out);
}
 

#endif  // INPUTPARAMETERS_H
