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
// $Id: GarfieldDetectorConstruction.hh 99992 2015-12-11 14:47:43Z dpfeiffe $
// 
/// \file GarfieldDetectorConstruction.hh
/// \brief Definition of the GarfieldDetectorConstruction class

#ifndef GarfieldDetectorConstruction_h
#define GarfieldDetectorConstruction_h 1

#include "G4VUserDetectorConstruction.hh"
#include "G4UserLimits.hh"
#include "globals.hh"

class G4VPhysicalVolume;
class G4VLogicalVolume;
class G4Material;
class GarfieldG4FastSimulationModel;
class GarfieldMessenger;

/// Detector construction class to define materials and geometry.
/// The drift tube is a cylinder made of Al filled with Ar/CO2.
/// It has a thin window on the base that is adjacent to the absorber plate.
/// In the center of the cylinder is the anode wire made from Tungsten.
///
/// The primary particles interact in the absorber and produce secondary particles,
/// which enter the drift tube via the thin window.


class GarfieldDetectorConstruction : public G4VUserDetectorConstruction
{
  public:
    GarfieldDetectorConstruction();
    virtual ~GarfieldDetectorConstruction();

  public:
    virtual G4VPhysicalVolume* Construct();

    // get methods
    //
    const G4VPhysicalVolume* GetAbsorberPV() const;
    const G4VPhysicalVolume* GetDriftTubePV() const;
    const G4VPhysicalVolume* GetGasPV() const;
    const G4VPhysicalVolume* GetThinWindowPV() const;
    const G4VPhysicalVolume* GetWirePV() const;
    void SetAbsorberMaterial(G4String materialChoice);
    G4Material* AbsorberMaterialWithSingleIsotope( G4String name,
                               G4String symbol, G4double density, G4int Z, G4int A);
  private:
    // methods
    //
    void DefineMaterials();
    G4VPhysicalVolume* DefineVolumes();
  

    G4VPhysicalVolume*   fAbsorberPV; // the absorber physical volume
    G4VPhysicalVolume*   fTubePV;      // the cathode of the single wire physical volume
    G4VPhysicalVolume*   fGasPV;      // the gas physical volume
    G4VPhysicalVolume*   fWirePV;      // the wire physical volume
    
    G4Material* fAbsorberMaterial;
    G4LogicalVolume* fAbsorberLV;

    G4bool  fCheckOverlaps; // option to activate checking of volumes overlaps

    GarfieldG4FastSimulationModel* fGarfieldG4FastSimulationModel;
    GarfieldMessenger* fGarfieldMessenger;
};

// inline functions

inline const G4VPhysicalVolume* GarfieldDetectorConstruction::GetAbsorberPV() const { 
  return fAbsorberPV; 
}

inline const G4VPhysicalVolume* GarfieldDetectorConstruction::GetDriftTubePV() const  {
  return fTubePV;
}

inline const G4VPhysicalVolume* GarfieldDetectorConstruction::GetGasPV() const  {
  return fGasPV;
}


inline const G4VPhysicalVolume* GarfieldDetectorConstruction::GetWirePV() const  {
  return fWirePV;
}
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif

