// ------------------------------------------------------------
//
//              CRMC interface to GEANT4
//              for more details on CRMC, see:
//              https://web.ikp.kit.edu/rulrich/crmc.html
//
//
// Author:      Andrii Tykhonov  (University of Geneva)
// Email:       andrii.tykhonov@cern.ch
// Created:     14.02.2018
//
// Modified by Andrii Kotenko (University of Geneva) on 03.09.2020
//
// ------------------------------------------------------------
#ifndef G4HadronPhysicsCRMC_h
#define G4HadronPhysicsCRMC_h 1

#include "G4VHadronPhysics.hh"
#include "globals.hh"

class G4HadronicInelasticModelCRMC;
class G4TrivialHadronicModel;


class G4VCrossSectionDataSet;
class G4VComponentCrossSection;

class G4HadronElastic;

class G4HadronPhysicsCRMC : public G4VHadronPhysics
{
public:

	//! [model]
	//!         0  :  EPOS LHC
	//!         1  :  EPOS 1.99
	//!         12 :  DPMJET3
	G4HadronPhysicsCRMC(int model);
	virtual ~G4HadronPhysicsCRMC();

	// This method will be invoked in the Construct() method.
	// each physics process will be instantiated and
	// registered to the process manager of each particle type
	void ConstructProcess();

	//! Energy threshold (in MeV - default Geant4 energy units) to switch between
	//! Geant4 and CRMC hadronic physics
	void SetEnergyThreshold(double thr);
	// makes a smooth linear transition between Geant4 and CRMC hadronic models in the specified range
	void SetEnergyThreshold(double E1, double E2);

	//! Set verbosity to debug level
	void SetPrintDebug(bool printdebug) {fPrintDebug = printdebug;}
private:

	void AddAllHadrons();
	void AddProcess(const G4String& name, G4ParticleDefinition* part, G4bool doElastic=true);

private:
	G4HadronicInelasticModelCRMC* fCRMC;

	//G4HadronElastic* fTrivial;

	//G4ComponentGGHadronNucleusXsc*   fDpmXS;
	static G4ThreadLocal G4VCrossSectionDataSet* theNuclNuclData;
	static G4ThreadLocal G4VComponentCrossSection* theGGNuclNuclXS;

	//! Energy threshould to switch between
	//! Geant4 and CRMC physics
	double fEthr;

	// variable which used when  user defines a transition region (using SetEnergyThreshold(double,double) method)
	// where hadronic physics changes from Geant4 to CRMC
	double fEthr_2;

	//! CRMC model id (0,1 - Epos, 12 - Dpmjet, etc.)
	int fModel;

	static const std::array<std::string, 14> fModelNames;

	//!
	bool fPrintDebug;
};

#endif
