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
// $Id: GarfieldEventAction.cc 999993 2015-12-11 14:47:43Z dpfeiffe $
// 
/// \file GarfieldEventAction.cc
/// \brief Implementation of the GarfieldEventAction class

#include "GarfieldPhysics.hh"

#include "GarfieldEventAction.hh"
#include "GarfieldRunAction.hh"
#include "GarfieldAnalysis.hh"

#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4UnitsTable.hh"

#include "Randomize.hh"
#include <iomanip>

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldEventAction::GarfieldEventAction() :
		G4UserEventAction(), fEnergyAbs(0.), fEnergyGas(0.), fTrackLAbs(0.) {
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldEventAction::~GarfieldEventAction() {
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldEventAction::BeginOfEventAction(const G4Event* /*event*/) {
	// initialisation per event
	fEnergyAbs = 0;
	fEnergyGas = 0;
	fTrackLAbs = 0;
	fAvalancheSize = 0;
	fGain = 0;

	GarfieldPhysics* garfieldPhysics = GarfieldPhysics::GetInstance();
	garfieldPhysics->Clear();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldEventAction::EndOfEventAction(const G4Event* event) {
	// Accumulate statistics
	//
	GarfieldPhysics* garfieldPhysics = GarfieldPhysics::GetInstance();

	// get analysis manager
	G4AnalysisManager* analysisManager = G4AnalysisManager::Instance();
	//fEnergyGas += garfieldPhysics->GetEnergyDeposit_MeV();
	fAvalancheSize = garfieldPhysics->GetAvalancheSize();
	fGain = garfieldPhysics->GetGain();

	// fill histograms
	analysisManager->FillH1(1, fEnergyAbs);
	analysisManager->FillH1(2, fTrackLAbs);
	analysisManager->FillH1(3, fEnergyGas);
	analysisManager->FillH1(4, fAvalancheSize);
	analysisManager->FillH1(5, fGain);



	// fill ntuple
	analysisManager->FillNtupleDColumn(0, fEnergyAbs);
	analysisManager->FillNtupleDColumn(1, fTrackLAbs);
	analysisManager->FillNtupleDColumn(2, fEnergyGas);
	analysisManager->FillNtupleDColumn(3, fAvalancheSize);
	analysisManager->FillNtupleDColumn(4, fGain);

	// Print per event (modulo n)
	//
	G4int eventID = event->GetEventID();
	G4int printModulo = G4RunManager::GetRunManager()->GetPrintProgress();
	if ((printModulo > 0) && (eventID % printModulo == 0)) {
		G4cout << "---> End of event: " << eventID << G4endl;

		G4cout << "   Absorber: total energy: " << std::setw(7)
				<< G4BestUnit(fEnergyAbs, "Energy")
				<< "       total track length: " << std::setw(7)
				<< G4BestUnit(fTrackLAbs, "Length") << G4endl;

		G4cout << "        Gas: total energy: " << std::setw(7)
				<< G4BestUnit(fEnergyGas, "Energy") <<  "       avalanche size: " << fAvalancheSize <<  "       gain: " << fGain << G4endl;


	}
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
