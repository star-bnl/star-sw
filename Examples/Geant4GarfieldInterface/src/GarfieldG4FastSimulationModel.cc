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
// $Id: GarfieldG4FastSimulationModel.cc 999994 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldG4FastSimulationModel.cc
/// \brief Implementation of the GarfieldG4FastSimulationModel class

#include <iostream>
#include "GarfieldG4FastSimulationModel.hh"
#include "G4VPhysicalVolume.hh"
#include "G4GDMLParser.hh"
#include "G4Electron.hh"
#include "G4Gamma.hh"

#include "G4SystemOfUnits.hh"

GarfieldG4FastSimulationModel::GarfieldG4FastSimulationModel(G4String modelName,
		G4Region* envelope) :
		G4VFastSimulationModel(modelName, envelope) {
	fGarfieldPhysics = GarfieldPhysics::GetInstance();
	fGarfieldPhysics->InitializePhysics();

}

GarfieldG4FastSimulationModel::GarfieldG4FastSimulationModel(G4String modelName) :
		G4VFastSimulationModel(modelName) {
	fGarfieldPhysics = GarfieldPhysics::GetInstance();
	fGarfieldPhysics->InitializePhysics();
}

GarfieldG4FastSimulationModel::~GarfieldG4FastSimulationModel() {
}

void GarfieldG4FastSimulationModel::WriteGeometryToGDML(
		G4VPhysicalVolume* physicalVolume) {

	G4GDMLParser* parser = new G4GDMLParser();
	remove("garfieldGeometry.gdml");
	parser->Write("garfieldGeometry.gdml", physicalVolume, false);
	delete parser;
}

G4bool GarfieldG4FastSimulationModel::IsApplicable(
		const G4ParticleDefinition& particleType) {
	G4String particleName = particleType.GetParticleName();
	if (fGarfieldPhysics->FindParticleName(particleName, "garfield")) {
		return true;
	}
	return false;
}

G4bool GarfieldG4FastSimulationModel::ModelTrigger(
		const G4FastTrack& fastTrack) {
	double ekin_MeV = fastTrack.GetPrimaryTrack()->GetKineticEnergy() / MeV;
	G4String particleName =
			fastTrack.GetPrimaryTrack()->GetParticleDefinition()->GetParticleName();
	if (fGarfieldPhysics->FindParticleNameEnergy(particleName, ekin_MeV,
			"garfield")) {
		return true;
	}
	return false;
}

void GarfieldG4FastSimulationModel::DoIt(const G4FastTrack& fastTrack,
		G4FastStep& fastStep) {

	G4TouchableHandle theTouchable =
			fastTrack.GetPrimaryTrack()->GetTouchableHandle();
	G4String name = theTouchable->GetVolume()->GetName();

	G4ThreeVector pdirection =
			fastTrack.GetPrimaryTrack()->GetMomentum().unit();
	G4ThreeVector localdir = fastTrack.GetPrimaryTrackLocalDirection();

	G4ThreeVector worldPosition = fastTrack.GetPrimaryTrack()->GetPosition();
	G4ThreeVector localPosition = fastTrack.GetPrimaryTrackLocalPosition();

	double ekin_MeV = fastTrack.GetPrimaryTrack()->GetKineticEnergy() / MeV;
	G4double globalTime = fastTrack.GetPrimaryTrack()->GetGlobalTime();

	G4String particleName =
			fastTrack.GetPrimaryTrack()->GetParticleDefinition()->GetParticleName();

	fastStep.KillPrimaryTrack();
	fastStep.SetPrimaryTrackPathLength(0.0);

	if (particleName == "kaon+") {
		particleName = "K+";
	} else if (particleName == "kaon-") {
		particleName = "K-";
	} else if (particleName == "anti_proton") {
		particleName = "anti-proton";
	}

	fGarfieldPhysics->DoIt(particleName, ekin_MeV, globalTime,
			localPosition.x() / CLHEP::cm, localPosition.y() / CLHEP::cm,
			localPosition.z() / CLHEP::cm, localdir.x(), localdir.y(),
			localdir.z());

	fastStep.SetTotalEnergyDeposited(fGarfieldPhysics->GetEnergyDeposit_MeV());

	if (fGarfieldPhysics->GetCreateSecondariesInGeant4()) {
		std::vector<GarfieldParticle*>* secondaryParticles =
				fGarfieldPhysics->GetSecondaryParticles();

		if (secondaryParticles->size() > 0) {
			fastStep.SetNumberOfSecondaryTracks(secondaryParticles->size());

			G4double totalEnergySecondaries_MeV = 0;

			for (std::vector<GarfieldParticle*>::iterator it =
					secondaryParticles->begin();
					it != secondaryParticles->end(); ++it) {
				G4double x = (*it)->getX_mm();
				G4double y = (*it)->getY_mm();
				G4double z = (*it)->getZ_mm();
				G4double eKin_MeV = (*it)->getEkin_MeV();
				G4double dx = (*it)->getDX();
				G4double dy = (*it)->getDY();
				G4double dz = (*it)->getDZ();
				G4double time = (*it)->getTime();
				G4ThreeVector momentumDirection(dx, dy, dz);
				G4ThreeVector position(x, y, z);
				if ((*it)->getParticleName() == "e-") {
					G4DynamicParticle particle(G4Electron::ElectronDefinition(),
							momentumDirection, eKin_MeV);
					fastStep.CreateSecondaryTrack(particle, position, time,
							true);
					totalEnergySecondaries_MeV += eKin_MeV;
				} else if ((*it)->getParticleName() == "gamma") {
					G4DynamicParticle particle(G4Gamma::GammaDefinition(),
							momentumDirection, eKin_MeV);
					fastStep.CreateSecondaryTrack(particle, position, time,
							true);
					totalEnergySecondaries_MeV += eKin_MeV;
				}

			}
		}
	}
}
