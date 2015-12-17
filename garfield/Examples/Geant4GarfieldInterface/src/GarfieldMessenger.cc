//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// $Id: GarfieldMessenger.cc 999995 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldMessenger.cc
/// \brief Implementation of the GarfieldMessenger class

#include "GarfieldMessenger.hh"
#include "GarfieldPhysics.hh"
#include "GarfieldDetectorConstruction.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithoutParameter.hh"

GarfieldMessenger::GarfieldMessenger(GarfieldDetectorConstruction * Det) :
		G4UImessenger(), fDetector(Det), fExampleDir(0), fAbsorberDir(0), fGarfieldPhysicsDir(
				0), fMaterialCmd(0), fIsotopeCmd(0), fIonizationModelCmd(0), fGeant4ParticleCmd(
				0) {
	fExampleDir = new G4UIdirectory("/exampleGarfield/");
	fExampleDir->SetGuidance("Commands specific to this example");

	G4bool broadcast = false;
	fAbsorberDir = new G4UIdirectory("/exampleGarfield/absorber/", broadcast);
	fAbsorberDir->SetGuidance("Absorber construction commands");

	fGarfieldPhysicsDir = new G4UIdirectory("/exampleGarfield/physics/",
			broadcast);
	fGarfieldPhysicsDir->SetGuidance(
			"Particle and energy ranges for Garfield++ physics model");

	fMaterialCmd = new G4UIcmdWithAString("/exampleGarfield/absorber/setMat",
			this);
	fMaterialCmd->SetGuidance("Select material of the absorber:");
	fMaterialCmd->SetParameterName("choice", false);
	fMaterialCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

	fIsotopeCmd = new G4UIcommand("/exampleGarfield/absorber/setIsotopeMat",
			this);
	fIsotopeCmd->SetGuidance("Build and select a material with single isotope");
	fIsotopeCmd->SetGuidance("  symbol of isotope, Z, A, density of material");
	//
	G4UIparameter* symbPrm = new G4UIparameter("isotope", 's', false);
	symbPrm->SetGuidance("isotope symbol");
	fIsotopeCmd->SetParameter(symbPrm);
	//
	G4UIparameter* ZPrm = new G4UIparameter("Z", 'i', false);
	ZPrm->SetGuidance("Z");
	ZPrm->SetParameterRange("Z>0");
	fIsotopeCmd->SetParameter(ZPrm);
	//
	G4UIparameter* APrm = new G4UIparameter("A", 'i', false);
	APrm->SetGuidance("A");
	APrm->SetParameterRange("A>0");
	fIsotopeCmd->SetParameter(APrm);
	//
	G4UIparameter* densityPrm = new G4UIparameter("density", 'd', false);
	densityPrm->SetGuidance("density of material");
	densityPrm->SetParameterRange("density>0.");
	fIsotopeCmd->SetParameter(densityPrm);
	//
	G4UIparameter* unitPrm = new G4UIparameter("unit", 's', false);
	unitPrm->SetGuidance("unit of density");
	G4String unitList = G4UIcommand::UnitsList(
			G4UIcommand::CategoryOf("g/cm3"));
	unitPrm->SetParameterCandidates(unitList);
	fIsotopeCmd->SetParameter(unitPrm);
	//
	fIsotopeCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

	fIonizationModelCmd = new G4UIcommand(
			"/exampleGarfield/physics/setIonizationModel", this);
	fIonizationModelCmd->SetGuidance("Select ionization model for Garfield++");
	fIonizationModelCmd->SetGuidance(
			"  and choose whether to use default particles");
	fIonizationModelCmd->SetGuidance("  and energy ranges for the chosen model");
	//
	G4UIparameter* ionizationModelPrm = new G4UIparameter("ionizationModel",
			's', false);
	ionizationModelPrm->SetGuidance("ionization model (1. PAIPhot, 2. PAI or 3. Heed)");
	ionizationModelPrm->SetGuidance("  1. PAIPhot model in Geant4, delta electrons transported by Heed");
	ionizationModelPrm->SetGuidance("  2. PAI model in Geant4, delta electrons transported by Heed");
	ionizationModelPrm->SetGuidance("  3. Use directly Heed");
	fIonizationModelCmd->SetParameter(ionizationModelPrm);
	//
	G4UIparameter* useDefaultsPrm = new G4UIparameter("useDefaults", 'b',
			false);
	useDefaultsPrm->SetGuidance(
			"true to use default, false to manually choose particles and energies");
	fIonizationModelCmd->SetParameter(useDefaultsPrm);
	//
	fIonizationModelCmd->AvailableForStates(G4State_PreInit);



	fGeant4ParticleCmd = new G4UIcommand(
			"/exampleGarfield/physics/setGeant4ParticleTypeAndEnergy", this);
	fGeant4ParticleCmd->SetGuidance(
			"Select particle types and energies for PAI and PAIPhot model");
	fGeant4ParticleCmd->SetGuidance("  in Geant4");
	//
	G4UIparameter* particleGeant4Prm = new G4UIparameter("particleName", 's', false);
	particleGeant4Prm->SetGuidance(
			"Particle name (e-, e+, mu-, mu+, proton, pi-, pi+, alpha, He3, GenericIon)");
	fGeant4ParticleCmd->SetParameter(particleGeant4Prm);
	//
	G4UIparameter* minEnergyGeant4Prm = new G4UIparameter("minimumEnergyGeant4", 'd',
			false);
	minEnergyGeant4Prm->SetGuidance("minimum energy");
	minEnergyGeant4Prm->SetParameterRange("minimumEnergyGeant4>=0");
	fGeant4ParticleCmd->SetParameter(minEnergyGeant4Prm);
	//
	G4UIparameter* maxEnergyGeant4Prm = new G4UIparameter("maximumEnergyGeant4", 'd',
			false);
	maxEnergyGeant4Prm->SetGuidance("maximum energy");
	maxEnergyGeant4Prm->SetParameterRange("maximumEnergyGeant4>=0");
	fGeant4ParticleCmd->SetParameter(maxEnergyGeant4Prm);
	//
	G4UIparameter* unitGeant4Prm = new G4UIparameter("unit", 's', false);
	unitGeant4Prm->SetGuidance("unit of energy");
	G4String unitListGeant4 = G4UIcommand::UnitsList(G4UIcommand::CategoryOf("MeV"));
	unitGeant4Prm->SetParameterCandidates(unitListGeant4);
	fGeant4ParticleCmd->SetParameter(unitGeant4Prm);
	//
	fGeant4ParticleCmd->AvailableForStates(G4State_PreInit);



	fGarfieldParticleCmd = new G4UIcommand(
			"/exampleGarfield/physics/setGarfieldParticleTypeAndEnergy", this);
	fGarfieldParticleCmd->SetGuidance(
			"Select particle types and energies for Heed model.");
	fGarfieldParticleCmd->SetGuidance(
				" For PAI and PAIPhot model choose at which energy electrons are");
	fGarfieldParticleCmd->SetGuidance(
						" transported as delta electrons by Heed, and treatment of gammas");
	//
	G4UIparameter* particleGarfieldPrm = new G4UIparameter("particleName", 's', false);
	particleGarfieldPrm->SetGuidance("Particle name (gamma, e-, e+, mu-, mu+, proton, anti_proton, pi-, pi+, kaon, kaon+, alpha, deuteron)");
	fGarfieldParticleCmd->SetParameter(particleGarfieldPrm);
	//
	G4UIparameter* minEnergyGarfieldPrm = new G4UIparameter("minimumEnergyGarfield", 'd',
			false);
	minEnergyGarfieldPrm->SetGuidance("minimum energy");
	minEnergyGarfieldPrm->SetParameterRange("minimumEnergyGarfield>=0");
	fGarfieldParticleCmd->SetParameter(minEnergyGarfieldPrm);
	//
	G4UIparameter* maxEnergyGarfieldPrm = new G4UIparameter("maximumEnergyGarfield", 'd',
			false);
	maxEnergyGarfieldPrm->SetGuidance("maximum energy");
	maxEnergyGarfieldPrm->SetParameterRange("maximumEnergyGarfield>=0");
	fGarfieldParticleCmd->SetParameter(maxEnergyGarfieldPrm);
	//
	G4UIparameter* unitGarfieldPrm = new G4UIparameter("unit", 's', false);
	unitGarfieldPrm->SetGuidance("unit of energy");
	G4String unitListGarfield = G4UIcommand::UnitsList(G4UIcommand::CategoryOf("MeV"));
	unitGarfieldPrm->SetParameterCandidates(unitListGarfield);
	fGarfieldParticleCmd->SetParameter(unitGarfieldPrm);
	//
	fGarfieldParticleCmd->AvailableForStates(G4State_PreInit);

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldMessenger::~GarfieldMessenger() {
	delete fMaterialCmd;
	delete fIsotopeCmd;
	delete fAbsorberDir;
	delete fExampleDir;
	delete fGarfieldPhysicsDir;
	delete fIonizationModelCmd;
	delete fGeant4ParticleCmd;
	delete fGarfieldParticleCmd;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldMessenger::SetNewValue(G4UIcommand* command, G4String newValue) {
	if (command == fMaterialCmd) {
		fDetector->SetAbsorberMaterial(newValue);
	} else if (command == fIsotopeCmd) {
		G4int Z;
		G4int A;
		G4double dens;
		G4String name, unt;
		std::istringstream is(newValue);
		is >> name >> Z >> A >> dens >> unt;
		dens *= G4UIcommand::ValueOf(unt);
		fDetector->AbsorberMaterialWithSingleIsotope(name, name, dens, Z, A);
		fDetector->SetAbsorberMaterial(name);
	} else if (command == fIonizationModelCmd) {
		GarfieldPhysics* garfieldPhysics = GarfieldPhysics::GetInstance();
		G4String modelName;
		G4bool useDefaults;
		std::istringstream is(newValue);
		is >> modelName >> std::boolalpha >> useDefaults;
		garfieldPhysics->SetIonizationModel(modelName, useDefaults);
	} else if (command == fGeant4ParticleCmd) {
		GarfieldPhysics* garfieldPhysics = GarfieldPhysics::GetInstance();
		G4String particleName, unit, programName;
		G4double minEnergy;
		G4double maxEnergy;
		std::istringstream is(newValue);
		is >> particleName >> minEnergy >> maxEnergy >> unit;
		minEnergy *= G4UIcommand::ValueOf(unit);
		maxEnergy *= G4UIcommand::ValueOf(unit);
		garfieldPhysics->AddParticleName(particleName, minEnergy, maxEnergy,
				"geant4");
	} else if (command == fGarfieldParticleCmd) {
		GarfieldPhysics* garfieldPhysics = GarfieldPhysics::GetInstance();
		G4String particleName, unit, programName;
		G4double minEnergy;
		G4double maxEnergy;
		std::istringstream is(newValue);
		is >> particleName >> minEnergy >> maxEnergy >> unit;
		minEnergy *= G4UIcommand::ValueOf(unit);
		maxEnergy *= G4UIcommand::ValueOf(unit);
		garfieldPhysics->AddParticleName(particleName, minEnergy, maxEnergy,
				"garfield");
	}
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
