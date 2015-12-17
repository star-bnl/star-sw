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
// $Id: GarfieldFastSimulationModel.hh 9999994 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldFastSimulationModel.hh
/// \brief Definition of the GarfieldFastSimulationModel class

#ifndef GarfieldFastSimulationModel_h
#define GarfieldFastSimulationModel_h 1

#include "G4VFastSimulationModel.hh"
#include "GarfieldPhysics.hh"

class G4VPhysicalVolume;

class GarfieldG4FastSimulationModel : public G4VFastSimulationModel
{
public:
  //-------------------------
  // Constructor, destructor
  //-------------------------
	GarfieldG4FastSimulationModel (G4String, G4Region*);
	GarfieldG4FastSimulationModel (G4String);
  ~GarfieldG4FastSimulationModel ();

  void SetPhysics(GarfieldPhysics* fGarfieldPhysics);
  void WriteGeometryToGDML(G4VPhysicalVolume* physicalVolume);

  virtual G4bool IsApplicable(const G4ParticleDefinition&);
  virtual G4bool ModelTrigger(const G4FastTrack &);
  virtual void DoIt(const G4FastTrack&, G4FastStep&);



private:

   GarfieldPhysics* fGarfieldPhysics;
};


#endif /* GARFIELDMODEL_H_ */
