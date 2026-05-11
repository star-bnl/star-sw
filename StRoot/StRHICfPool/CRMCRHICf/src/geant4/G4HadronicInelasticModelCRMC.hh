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
// ------------------------------------------------------------

#ifndef  G4HadronicInelasticModelCRMC_h
#define  G4HadronicInelasticModelCRMC_h

#include "G4HadronicInteraction.hh"
#include "G4HadFinalState.hh"
#include "G4SystemOfUnits.hh"

#include "CRMCinterface.h"
#include <string>


extern CRMCdata gCRMC_data;

class G4HadFinalState;
class G4ParticleTable;
class G4IonTable;


class  G4ParticleDefinition; //class G4DynamicParticle; 


class G4HadronicInelasticModelCRMC : public G4HadronicInteraction
{
public:
	//! model: 
	//!         0  :  EPOS LHC
	//!         1  :  EPOS 1.99
	//!         12 :  DPMJET3
	G4HadronicInelasticModelCRMC(int model, const G4String& modelName);
	~G4HadronicInelasticModelCRMC();

	G4HadFinalState * ApplyYourself (const G4HadProjectile &aTrack, G4Nucleus &targetNucleus);
	G4bool 	IsApplicable (const G4HadProjectile &, G4Nucleus &);

	void SetPrintDebug(bool printdebug) {fPrintDebug = printdebug;}
	G4ParticleDefinition* GetParticleDefinition(long particle_id,int& error_code); 
	void SplitMultiNeutrons(CRMCdata& CRMC_data);
	bool IsMultiNeutron(int Z, int A);

	virtual const std::pair<G4double, G4double> GetFatalEnergyCheckLevels() const {
		// possible energy non-coservations of up to 1 TeV are ignored
		return std::pair<G4double, G4double>( 10.0*perCent, 1000.0*GeV );
	}
private:
	CRMCinterface* fInterface;
	//CRMCdata fCRMCdata;
	int fTypeOutput;
	G4HadFinalState* finalState;
	G4ParticleTable* fParticleTable;
	G4IonTable*      fIonTable;

	//std::vector<G4ParticleDefinition*> fParticleDefinitions;
	//std::vector<G4DynamicParticle*>    fDynamicParticles;
	bool fPrintDebug;

	//
	std::string GetCrmcParamPath();

	// Utility function to convert a C string into a Fortran string
	// Added as a part of patch from Nicola Mori
	void ConvertToFortran(const char* cstring, std::size_t fstring_len,char* fstring);
};

#endif
