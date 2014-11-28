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
#include <cstdlib>

#ifdef ENABLE_PYTHIA
#include "PythiaStarlight.h"
#endif

#include "reportingUtils.h"
#include "inputParameters.h"
#include "eventchannel.h"
#include "gammagammaleptonpair.h"
#include "gammagammasingle.h"
#include "gammaavm.h"
#include "psifamily.h"
#include "twophotonluminosity.h"
#include "gammaaluminosity.h"
#include "upcevent.h"
#include "eventfilewriter.h"
#include "starlight.h"


using namespace std;
using namespace starlightConstants;


starlight::starlight()
	:	_inputParameters       (0),
		_beam0                 (0),
		_beam1                 (0),
		_beamSystem            (0),
		_eventChannel          (0),
		_nmbEventsPerFile      (100),
		_nmbEventsToGenerate   (10),
		_configFileName        ("slight.in"),
		_eventDataFileName     ("slight.out"),
		_lumLookUpTableFileName("slight.txt"),
		_isInitialised         (false)
{ }


starlight::~starlight()
{ }


bool
starlight::init()
{
	cout << "##################################" << endl
	     << " initialising Starlight v" << Starlight_VERSION_MAJOR << "."
	     << Starlight_VERSION_MINOR << "..." << endl
	     << "##################################" << endl;

	_nmbEventsToGenerate = _inputParameters->nmbEvents();  // nmbEvents() gives only unsigned int
	_nmbEventsPerFile    = _nmbEventsToGenerate;           // for now we write only one file...

	_beamSystem = new beamBeamSystem(*_inputParameters);

	// streamsize precision(15);
	cout.setf(ios_base::fixed,ios_base::floatfield);
	cout.precision(15);
	const bool lumTableIsValid = luminosityTableIsValid();
	switch (_inputParameters->interactionType())	{
	case PHOTONPHOTON:
		if (!lumTableIsValid) {
			printInfo << "creating luminosity table for photon-photon channel" << endl;
			twoPhotonLuminosity(_beamSystem->beam1(), _beamSystem->beam2(),
			                    _inputParameters->beamBreakupMode(), *_inputParameters);
		}
		break;		
	case PHOTONPOMERONNARROW:  // narrow and wide resonances use
	case PHOTONPOMERONWIDE:    // the same luminosity function
		if (!lumTableIsValid) {
			printInfo << "creating luminosity table for photon-Pomeron channel" << endl;
			photonNucleusLuminosity(*_inputParameters, *_beamSystem);
		}
		break;
	default:
		{
			printWarn << "unknown interaction type '" << _inputParameters->interactionType() << "'."
			          << " cannot initialize starlight." << endl;
			return false;
		}
	}
	
	if (!createEventChannel())
		return false;

	_isInitialised = true;
	return true;
}


upcEvent
starlight::produceEvent()
{
	if (!_isInitialised) {
		printErr << "trying to generate event but Starlight is not initialised. aborting." << endl;
		exit(-1);
	}
	return _eventChannel->produceEvent();
}


bool
starlight::luminosityTableIsValid() const
{
	printInfo << "using random seed = " << _inputParameters->randomSeed() << endl;

	ifstream lumLookUpTableFile(_lumLookUpTableFileName.c_str());
	lumLookUpTableFile.precision(15);
	if ((!lumLookUpTableFile) || (!lumLookUpTableFile.good())) {
		// printWarn << "cannot open file '" << _lumLookUpTableFileName << "'" << endl;
		return false;
	}

	unsigned int beam1Z, beam1A, beam2Z, beam2A;
	double       beamLorentzGamma = 0;
	double       maxW = 0, minW = 0;
	unsigned int nmbWBins;
	double       maxRapidity = 0;
	unsigned int nmbRapidityBins;
	int          productionMode, beamBreakupMode;
	bool         interferenceEnabled = false;
	double       interferenceStrength = 0;
	bool         coherentProduction = false;
	double       incoherentFactor = 0, deuteronSlopePar = 0, maxPtInterference = 0;
	int          nmbPtBinsInterference;
	if (!(lumLookUpTableFile
	      >> beam1Z >> beam1A
	      >> beam2Z >> beam2A
	      >> beamLorentzGamma
	      >> maxW >> minW >> nmbWBins
	      >> maxRapidity >> nmbRapidityBins
	      >> productionMode
	      >> beamBreakupMode
	      >> interferenceEnabled >> interferenceStrength
	      >> coherentProduction >> incoherentFactor
	      >> deuteronSlopePar
	      >> maxPtInterference
	      >> nmbPtBinsInterference))
		// cannot read parameters from lookup table file
		return false;
	lumLookUpTableFile.close();

	if (!(   _inputParameters->beam1Z()                == beam1Z
	      && _inputParameters->beam1A()                == beam1A
	      && _inputParameters->beam2Z()                == beam2Z
	      && _inputParameters->beam2A()                == beam2A
	      && _inputParameters->beamLorentzGamma()      == beamLorentzGamma
	      //&& _inputParameters->maxW()                  == maxW
	      && _inputParameters->minW()                  == minW
	      && _inputParameters->nmbWBins()              == nmbWBins
	      && _inputParameters->maxRapidity()           == maxRapidity
	      && _inputParameters->nmbRapidityBins()       == nmbRapidityBins
	      && _inputParameters->productionMode()        == productionMode
	      && _inputParameters->beamBreakupMode()       == beamBreakupMode
	      && _inputParameters->interferenceEnabled()   == interferenceEnabled
	      && _inputParameters->interferenceStrength()  == interferenceStrength
	      && _inputParameters->deuteronSlopePar()      == deuteronSlopePar
	      && _inputParameters->coherentProduction()    == coherentProduction
	      && _inputParameters->incoherentFactor()      == incoherentFactor
	      && _inputParameters->maxPtInterference()     == maxPtInterference
	      && _inputParameters->nmbPtBinsInterference() == nmbPtBinsInterference))
		// parameters used to create luminosity lookup table are different than current parameters
		return false;

	return true;
}


bool
starlight::createEventChannel()
{
	switch (_inputParameters->prodParticleType()) {
	case ELECTRON:
	case MUON:
	case TAUON:
		{
			_eventChannel = new Gammagammaleptonpair(*_inputParameters, *_beamSystem);
			if (_eventChannel)
				return true;
			else {
				printWarn << "cannot construct Gammagammaleptonpair event channel." << endl;
				return false;
			}
		}
	case A2:        // jetset
	case ETA:       // jetset
	case ETAPRIME:  // jetset
	case ETAC:      // jetset
	case F0:        // jetset
		{
#ifdef ENABLE_PYTHIA
			// PythiaOutput = true;
 		        cout<<"Pythia is enabled!"<<endl;
			return true;
#endif
			printWarn << "Starlight is not compiled against Pythia8; "
			          << "jetset event channel cannot be used." << endl;
			return false;
		}
	case F2:
	case F2PRIME:
	case ZOVERZ03:
		{
		  //  #ifdef ENABLE_PYTHIA
	 	        cout<<" This is f2, f2prim, zoverz03 "<<endl; 
			_eventChannel= new Gammagammasingle(*_inputParameters, *_beamSystem);
			if (_eventChannel)
				return true;
			else {
				printWarn << "cannot construct Gammagammasingle event channel." << endl;
				return false;
			}
			// #endif
			//			printWarn << "Starlight is not compiled against Pythia8; "
			//          << "Gammagammasingle event channel cannot be used." << endl;
			// return false;
		}
	case RHO:
	case RHOZEUS:
	case FOURPRONG:
	case OMEGA:  // will probably be three body
	case PHI:
	case JPSI:
	case JPSI2S:
	case JPSI2S_ee:
	case JPSI2S_mumu:
	case JPSI_ee:
	case JPSI_mumu:
		//    {
		//        _eventChannel = new psiFamily(*_inputParameters, *_beamSystem);
		//        if (_eventChannel) return true;
		//        else return false;
		//    }
	case UPSILON:
	case UPSILON_ee:
	case UPSILON_mumu:
	case UPSILON2S:
	case UPSILON2S_ee:
	case UPSILON2S_mumu:
	case UPSILON3S:
	case UPSILON3S_ee:
	case UPSILON3S_mumu:
		{
			if (_inputParameters->interactionType() == PHOTONPOMERONNARROW) {
				_eventChannel = new Gammaanarrowvm(*_inputParameters, *_beamSystem);
				if (_eventChannel)
					return true;
				else {
					printWarn << "cannot construct Gammaanarrowvm event channel." << endl;
					return false;
				}
			}

			if (_inputParameters->interactionType() == PHOTONPOMERONWIDE) {
				_eventChannel = new Gammaawidevm(*_inputParameters, *_beamSystem);
				if (_eventChannel)
					return true;
				else {
					printWarn << "cannot construct Gammaawidevm event channel." << endl;
					return false;
				}
			}
			printWarn << "interaction type '" << _inputParameters->interactionType() << "' "
			          << "cannot be used with particle type '" << _inputParameters->prodParticleType() << "'. "
			          << "cannot create event channel." << endl;
			return false;
		}
	default:
		{
			printWarn << "unknown event channel '" << _inputParameters->prodParticleType() << "'."
			          << " cannot create event channel." << endl;
			return false;
		}
	}
}
