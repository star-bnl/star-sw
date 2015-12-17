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
// $Id: GarfieldMessenger.hh 9999995 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldMessenger.hh
/// \brief Definition of the GarfieldMessenger class

#ifndef GarfieldMessenger_h
#define GarfieldMessenger_h 1

#include "G4UImessenger.hh"
#include "globals.hh"

class GarfieldDetectorConstruction;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithADoubleAndUnit;
class G4UIcmdWithoutParameter;

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

class GarfieldMessenger: public G4UImessenger {
public:

	GarfieldMessenger(GarfieldDetectorConstruction*);
	~GarfieldMessenger();

	virtual void SetNewValue(G4UIcommand*, G4String);

private:

	GarfieldDetectorConstruction* fDetector;

	G4UIdirectory* fExampleDir;
	G4UIdirectory* fAbsorberDir;
	G4UIdirectory*  fGarfieldPhysicsDir;

	G4UIcmdWithAString* fMaterialCmd;
	G4UIcommand* fIsotopeCmd;
	G4UIcommand* fIonizationModelCmd;
	G4UIcommand* fGeant4ParticleCmd;
	G4UIcommand* fGarfieldParticleCmd;
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif

