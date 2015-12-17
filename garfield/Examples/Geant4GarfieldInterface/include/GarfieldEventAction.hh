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
// $Id: GarfieldEventAction.hh 9999993 2015-12-11 14:47:43Z dpfeiffe $
// 
/// \file GarfieldEventAction.hh
/// \brief Definition of the GarfieldEventAction class

#ifndef GarfieldEventAction_h
#define GarfieldEventAction_h 1

#include "G4UserEventAction.hh"
#include "globals.hh"

/// Event action class
///
/// It defines data members to hold the energy deposit and track lengths
/// of charged particles in Absober and Gap layers:
/// - fEnergyAbs, fEnergyGap, fTrackLAbs, fTrackLGap
/// which are collected step by step via the functions
/// - AddAbs(), AddGap()

class GarfieldEventAction: public G4UserEventAction {
public:
	GarfieldEventAction();
	virtual ~GarfieldEventAction();

	virtual void BeginOfEventAction(const G4Event* event);
	virtual void EndOfEventAction(const G4Event* event);

	void AddAbs(G4double de, G4double dl);
	void AddGas(G4double de);

private:
	G4double fEnergyAbs;
	G4double fEnergyGas;
	G4double fTrackLAbs;
	G4double fAvalancheSize;
	G4double fGain;


};

// inline functions

inline void GarfieldEventAction::AddAbs(G4double de, G4double dl) {
	fEnergyAbs += de;
	fTrackLAbs += dl;
}

inline void GarfieldEventAction::AddGas(G4double de) {
	fEnergyGas += de;

}


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif

