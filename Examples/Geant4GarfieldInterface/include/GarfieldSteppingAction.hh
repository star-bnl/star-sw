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
// $Id: GarfieldSteppingAction.hh 9999990 2015-12-11 14:47:43Z dpfeiffe $
// 
/// \file GarfieldSteppingAction.hh
/// \brief Definition of the GarfieldSteppingAction class

#ifndef GarfieldSteppingAction_h
#define GarfieldSteppingAction_h 1

#include "G4UserSteppingAction.hh"

class GarfieldDetectorConstruction;
class GarfieldEventAction;

/// Stepping action class.
///
/// In UserSteppingAction() there are collected the energy deposit and track 
/// lengths of charged particles in Absober and Gap layers and
/// updated in GarfieldEventAction.

class GarfieldSteppingAction : public G4UserSteppingAction
{
public:
  GarfieldSteppingAction(const GarfieldDetectorConstruction* detectorConstruction,
                    GarfieldEventAction* eventAction);
  virtual ~GarfieldSteppingAction();

  virtual void UserSteppingAction(const G4Step* step);
    
private:
  const GarfieldDetectorConstruction* fDetConstruction;
  GarfieldEventAction*  fEventAction;  
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
