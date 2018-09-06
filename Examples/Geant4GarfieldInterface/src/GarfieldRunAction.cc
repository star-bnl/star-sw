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
// $Id: GarfieldRunAction.cc 999999 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldRunAction.cc
/// \brief Implementation of the GarfieldRunAction class


#include "GarfieldRunAction.hh"
#include "GarfieldAnalysis.hh"

#include "G4Run.hh"
#include "G4RunManager.hh"
#include "G4UnitsTable.hh"
#include "G4SystemOfUnits.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldRunAction::GarfieldRunAction() :
		G4UserRunAction() {
	// set printing event number per each event
	G4RunManager::GetRunManager()->SetPrintProgress(1);

	// Create analysis manager
	// The choice of analysis technology is done via selectin of a namespace
	// in Garfieldnalysis.hh
	G4AnalysisManager* analysisManager = G4AnalysisManager::Instance();
	G4cout << "Using " << analysisManager->GetType() << G4endl;

	// Create directories
	//analysisManager->SetHistoDirectoryName("histograms");
	//analysisManager->SetNtupleDirectoryName("ntuple");
	analysisManager->SetVerboseLevel(1);
	analysisManager->SetFirstHistoId(1);

	// Book histograms, ntuple
	//

	// Creating histograms
	analysisManager->CreateH1("1", "Edep in absorber", 100, 0., 800 * MeV);
	analysisManager->CreateH1("2", "trackL in absorber", 100, 0., 1 * m);
	analysisManager->CreateH1("3", "Edep in gas", 1000, 0., 100 * keV);

	analysisManager->CreateH1("4", "Avalanche size in gas", 10000,0, 10000);
	analysisManager->CreateH1("5", "gain", 1000, 0., 100);
	analysisManager->CreateH3("1", "Track position",200, -10*cm, 10*cm, 29, -1.45*cm, 1.45*cm, 29,-1.45*cm, 1.45*cm);

	// Creating ntuple
	//
	analysisManager->CreateNtuple("Garfield", "Edep and TrackL");
	analysisManager->CreateNtupleDColumn("Eabs");
	analysisManager->CreateNtupleDColumn("Labs");
	analysisManager->CreateNtupleDColumn("Egas");
	analysisManager->CreateNtupleDColumn("AvalancheSize");
	analysisManager->CreateNtupleDColumn("Gain");
	analysisManager->FinishNtuple();



}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldRunAction::~GarfieldRunAction() {
	delete G4AnalysisManager::Instance();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldRunAction::BeginOfRunAction(const G4Run* /*run*/) {
	//inform the runManager to save random number seed
	//G4RunManager::GetRunManager()->SetRandomNumberStore(true);

	// Get analysis manager
	G4AnalysisManager* analysisManager = G4AnalysisManager::Instance();

	// Open an output file
	//
	G4String fileName = "Garfield";
	analysisManager->OpenFile(fileName);
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldRunAction::EndOfRunAction(const G4Run* /*run*/) {
	// print histogram statistics
	//
	G4AnalysisManager* analysisManager = G4AnalysisManager::Instance();
	if (analysisManager->GetH1(1)) {
		G4cout << G4endl << " ----> print histograms statistic ";
		if (isMaster) {
			G4cout << "for the entire run " << G4endl << G4endl;
		} else {
			G4cout << "for the local thread " << G4endl << G4endl;
		}

		G4cout << " EAbs : mean = "
				<< G4BestUnit(analysisManager->GetH1(1)->mean(), "Energy")
				<< " rms = "
				<< G4BestUnit(analysisManager->GetH1(1)->rms(), "Energy")
				<< G4endl;

		G4cout << " LAbs : mean = "
				<< G4BestUnit(analysisManager->GetH1(2)->mean(), "Length")
				<< " rms = "
				<< G4BestUnit(analysisManager->GetH1(2)->rms(), "Length")
				<< G4endl;

		G4cout << " EGas : mean = "
				<< G4BestUnit(analysisManager->GetH1(3)->mean(), "Energy")
				<< " rms = "
				<< G4BestUnit(analysisManager->GetH1(3)->rms(), "Energy")
				<< G4endl;

		G4cout << " Avalanche size : mean = "
						<< analysisManager->GetH1(4)->mean()
						<< " rms = "
						<< analysisManager->GetH1(4)->rms()
						<< G4endl;

		G4cout << " Gain : mean = "
						<< analysisManager->GetH1(5)->mean()
						<< " rms = "
						<< analysisManager->GetH1(5)->rms()
						<< G4endl;
	}

	// save histograms & ntuple
	//
	analysisManager->Write();
	analysisManager->CloseFile();

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
