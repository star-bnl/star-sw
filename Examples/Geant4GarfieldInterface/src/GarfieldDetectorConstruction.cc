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
// $Id: GarfieldDetectorConstruction.cc 999992 2015-12-11 14:47:43Z dpfeiffe $
// 
/// \file GarfieldDetectorConstruction.cc
/// \brief Implementation of the GarfieldDetectorConstruction class

#include "GarfieldG4FastSimulationModel.hh"
#include "GarfieldDetectorConstruction.hh"
#include "GarfieldMessenger.hh"
#include "G4Material.hh"
#include "G4NistManager.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4PVReplica.hh"
#include "G4GlobalMagFieldMessenger.hh"
#include "G4AutoDelete.hh"

#include "G4GeometryManager.hh"
#include "G4PhysicalVolumeStore.hh"
#include "G4LogicalVolumeStore.hh"
#include "G4SolidStore.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh"
#include "G4RunManager.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldDetectorConstruction::GarfieldDetectorConstruction() :
		G4VUserDetectorConstruction(), fAbsorberPV(0), fTubePV(0), fGasPV(0), fWirePV(
				0), fAbsorberMaterial(0), fAbsorberLV(0), fCheckOverlaps(true), fGarfieldG4FastSimulationModel(
				0), fGarfieldMessenger(0) {
	fGarfieldMessenger = new GarfieldMessenger(this);
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

GarfieldDetectorConstruction::~GarfieldDetectorConstruction() {
	delete fGarfieldMessenger;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4VPhysicalVolume* GarfieldDetectorConstruction::Construct() {
	// Define materials
	DefineMaterials();

	// Define volumes
	return DefineVolumes();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldDetectorConstruction::DefineMaterials() {
	G4bool isotopes = false;
	G4String name, symbol;
	G4int ncomponents, natoms;
	G4double density, fractionmass;

	// Lead material defined using NIST Manager
	G4NistManager* nistManager = G4NistManager::Instance();

	nistManager->FindOrBuildMaterial("G4_Pb");
	nistManager->FindOrBuildMaterial("G4_Cu");
	nistManager->FindOrBuildMaterial("G4_Al");
	nistManager->FindOrBuildMaterial("G4_Au");
	nistManager->FindOrBuildMaterial("G4_W");

	nistManager->FindOrBuildMaterial("G4_AIR");

	G4Element* H = nistManager->FindOrBuildElement("H", isotopes);
	G4Element* N = nistManager->FindOrBuildElement("N", isotopes);
	G4Element* C = nistManager->FindOrBuildElement("C", isotopes);
	G4Element* O = nistManager->FindOrBuildElement("O", isotopes);
	G4Element* Ar = nistManager->FindOrBuildElement("Ar", isotopes);

	G4Material* CO2 = new G4Material("CO2",
			density = 1.977 * CLHEP::mg / CLHEP::cm3, ncomponents = 2);
	CO2->AddElement(C, natoms = 1);
	CO2->AddElement(O, natoms = 2);

	G4Material* ArCO2_70_30 = new G4Material("ArCO2_70_30",
			density = 1.8223 * CLHEP::mg / CLHEP::cm3, ncomponents = 2,
			kStateGas);

	ArCO2_70_30->AddElement(Ar, fractionmass = 0.70);
	ArCO2_70_30->AddMaterial(CO2, fractionmass = 0.30);

	G4Material* ArCO2_93_7 = new G4Material("ArCO2_93_7",
			density = 1.8223 * CLHEP::mg / CLHEP::cm3, ncomponents = 2);
	ArCO2_93_7->AddElement(Ar, fractionmass = 0.93);
	ArCO2_93_7->AddMaterial(CO2, fractionmass = 0.07);

	density = 1.413 * CLHEP::g / CLHEP::cm3;
	G4Material* Kapton = new G4Material(name = "Kapton", density, ncomponents =
			4);
	Kapton->AddElement(O, 5);
	Kapton->AddElement(C, 22);
	Kapton->AddElement(N, 2);
	Kapton->AddElement(H, 10);

	// Print materials
	G4cout << *(G4Material::GetMaterialTable()) << G4endl;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4VPhysicalVolume* GarfieldDetectorConstruction::DefineVolumes() {
	// Geometry parameters
	G4double worldSizeXYZ = 1000 * mm;
	G4double absorberThicknessZ = 10. * mm;
	G4double absorberThicknessXY = 100. * mm;
	G4double wireRadius = 0.025 * mm;
	G4double tubeRadius = 15 * mm;
	G4double tubeHalfLength = 100 * mm;
	G4double tubeThickness = 500 * um;

	// Get materials
	G4Material* defaultMaterial = G4Material::GetMaterial("G4_AIR");
	fAbsorberMaterial = G4Material::GetMaterial("G4_Pb");

	G4Material* gasMaterial = G4Material::GetMaterial("ArCO2_70_30");
	G4Material* cathodeMaterial = G4Material::GetMaterial("G4_Al");
	G4Material* wireMaterial = G4Material::GetMaterial("G4_W");

	if (!defaultMaterial || !fAbsorberMaterial || !gasMaterial
			|| !cathodeMaterial || !wireMaterial) {
		G4ExceptionDescription msg;
		msg << "Cannot retrieve materials already defined.";
		G4Exception("GarfieldDetectorConstruction::DefineVolumes()",
				"exampleGarfield", FatalException, msg);
	}

	//
	// World
	//
	G4VSolid* worldS = new G4Box("World",           // its name
			0.5 * worldSizeXYZ, 0.5 * worldSizeXYZ, 0.5 * worldSizeXYZ); // its size

	G4LogicalVolume* worldLV = new G4LogicalVolume(worldS,          // its solid
			defaultMaterial,  // its material
			"World");         // its name

	G4VPhysicalVolume* worldPV = new G4PVPlacement(0,             // no rotation
			G4ThreeVector(),  // at (0,0,0)
			worldLV,          // its logical volume
			"World",          // its name
			0,                // its mother  volume
			false,            // no boolean operation
			0,                // copy number
			fCheckOverlaps);  // checking overlaps
	//
	// Absorber
	//
	G4VSolid* absorberS = new G4Box(
			"Absorber",            // its name
			0.5 * absorberThicknessXY, 0.5 * absorberThicknessXY,
			0.5 * absorberThicknessZ); // its size

	fAbsorberLV = new G4LogicalVolume(absorberS,    // its solid
			fAbsorberMaterial, // its material
			"Absorber");          // its name

	fAbsorberPV = new G4PVPlacement(0,                // no rotation
			G4ThreeVector(0., 0., absorberThicknessZ / 2), // its position
			fAbsorberLV,       // its logical volume
			"Absorber",           // its name
			worldLV,          // its mother  volume
			false,            // no boolean operation
			0,                // copy number
			fCheckOverlaps);  // checking overlaps

	//
	// Drift tube
	//
	G4VSolid* tubeS = new G4Tubs("Tube",     // its name
			0, tubeRadius, tubeHalfLength + tubeThickness, 0, 2 * pi); // its size

	G4LogicalVolume* tubeLV = new G4LogicalVolume(tubeS, // its solid
			cathodeMaterial, // its material
			"Tube");          // its name

	G4RotationMatrix* rotY = new G4RotationMatrix();
	rotY->rotateY(90. * CLHEP::degree);

	fTubePV = new G4PVPlacement(rotY,
			G4ThreeVector(0., -0.2 * tubeRadius,
					absorberThicknessZ + tubeRadius), // its position
			tubeLV,       // its logical volume
			"Tube",           // its name
			worldLV,          // its mother  volume
			false,            // no boolean operation
			0,                // copy number
			fCheckOverlaps);  // checking overlaps

	//
	// Drift Tube Gas
	//
	G4VSolid* gasS = new G4Tubs("Gas",            // its name
			wireRadius, tubeRadius - tubeThickness, tubeHalfLength, 0, 2 * pi); // its size

	G4LogicalVolume* gasLV = new G4LogicalVolume(gasS, // its solid
			gasMaterial, // its material
			"Gas");          // its name

	fGasPV = new G4PVPlacement(0,                // no rotation
			G4ThreeVector(0., 0., 0.), // its position
			gasLV,       // its logical volume
			"Gas",           // its name
			tubeLV,          // its mother  volume
			false,            // no boolean operation
			0,                // copy number
			fCheckOverlaps);  // checking overlaps

	//
	// Wire
	//
	G4VSolid* wireS = new G4Tubs("Wire",            // its name
			0, wireRadius, tubeHalfLength, 0, 2 * pi); // its size

	G4LogicalVolume* wireLV = new G4LogicalVolume(wireS, // its solid
			wireMaterial, // its material
			"Wire");          // its name

	fWirePV = new G4PVPlacement(0,                // no rotation
			G4ThreeVector(0., 0., 0.), // its position
			wireLV,       // its logical volume
			"Wire",           // its name
			tubeLV,      // its mother  volume
			false,            // no boolean operation
			0,                // copy number
			fCheckOverlaps);  // checking overlaps

	//
	// Visualization attributes
	//
	worldLV->SetVisAttributes(G4VisAttributes::Invisible);

	G4VisAttributes* VisAttBlue = new G4VisAttributes(G4Colour(0.0, 0.0, 1.0));
	G4VisAttributes* VisAttGreen = new G4VisAttributes(G4Colour(0.0, 1.0, 0.0));
	G4VisAttributes* VisAttRed = new G4VisAttributes(G4Colour(1.0, 0.0, 0.0));
	G4VisAttributes* VisAttWhite = new G4VisAttributes(G4Colour(1.0, 1.0, 1.0));

	wireLV->SetVisAttributes(VisAttRed);
	tubeLV->SetVisAttributes(VisAttGreen);
	gasLV->SetVisAttributes(VisAttWhite);
	fAbsorberLV->SetVisAttributes(VisAttBlue);

	G4Region* regionGarfield = new G4Region("RegionGarfield");
	regionGarfield->AddRootLogicalVolume(gasLV);

	G4Region* regionWire = new G4Region("RegionWire");
	regionWire->AddRootLogicalVolume(wireLV);

	fGarfieldG4FastSimulationModel = new GarfieldG4FastSimulationModel(
			"GarfieldG4FastSimulationModel", regionGarfield);

	fGarfieldG4FastSimulationModel->WriteGeometryToGDML(fGasPV);

	//
	// Always return the physical World
	//
	return worldPV;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4Material* GarfieldDetectorConstruction::AbsorberMaterialWithSingleIsotope(
		G4String name, G4String symbol, G4double density, G4int Z, G4int A) {
	// define a material from an isotope
	//
	G4int ncomponents;
	G4double abundance, massfraction;

	G4Isotope* isotope = new G4Isotope(symbol, Z, A);

	G4Element* element = new G4Element(name, symbol, ncomponents = 1);
	element->AddIsotope(isotope, abundance = 100. * perCent);

	G4Material* material = new G4Material(name, density, ncomponents = 1);
	material->AddElement(element, massfraction = 100. * perCent);

	return material;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void GarfieldDetectorConstruction::SetAbsorberMaterial(
		G4String materialChoice) {
	// search the material by its name
	G4Material* newMaterial = G4NistManager::Instance()->FindOrBuildMaterial(
			materialChoice);

	if (newMaterial) {
		if (fAbsorberMaterial != newMaterial) {
			fAbsorberMaterial = newMaterial;
			if (fAbsorberLV) {
				fAbsorberLV->SetMaterial(fAbsorberMaterial);
			}
			G4RunManager::GetRunManager()->PhysicsHasBeenModified();
		}
	} else {
		G4cout
				<< "\n--> warning from GarfieldDetectorConstruction::SetMaterial : "
				<< materialChoice << " not found" << G4endl;
	}
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
