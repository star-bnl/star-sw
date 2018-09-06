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
// $Id: GarfieldPhysics.hh 9999996 2015-12-11 14:47:43Z dpfeiffe $
//
/// \file GarfieldPhysics.hh
/// \brief Definition of the GarfieldPhysics class

#ifndef GarfieldPhysics_h
#define GarfieldPhysics_h 1

#include <map>
#include <vector>
#include <iostream>

#include "Sensor.hh"
#include "AvalancheMC.hh"
#include "AvalancheMicroscopic.hh"
#include "ComponentAnalyticField.hh"
#include "TrackHeed.hh"
#include "TrackSimple.hh"
#include "MediumMagboltz.hh"
#include "GeometryRoot.hh"
#include "GeometrySimple.hh"
#include "SolidTube.hh"

typedef std::pair<double, double> EnergyRange_MeV;
typedef std::map< const std::string, EnergyRange_MeV> MapParticlesEnergy;

class GarfieldParticle{
public:
	GarfieldParticle(std::string particleName, double ekin_eV,double time, double x_cm,double y_cm,double z_cm, double dx,double dy,double dz):fParticleName(particleName), fEkin_MeV(ekin_eV/1000000), fTime(time), fx_mm(10*x_cm),fy_mm(10*y_cm), fz_mm(10*z_cm),  fdx(dx), fdy(dy), fdz(dz){}
	~GarfieldParticle(){};

	std::string getParticleName(){return fParticleName;}
	double getX_mm() {return fx_mm;}
	double getY_mm(){return fy_mm;}
	double getZ_mm(){return fz_mm;}
	double getEkin_MeV(){return fEkin_MeV;}
	double getTime(){return fTime;}
	double getDX(){return fdx;}
	double getDY(){return fdy;}
	double getDZ(){return fdz;}


private:
	std::string fParticleName;
	double fEkin_MeV, fTime, fx_mm,fy_mm,fz_mm,fdx,fdy,fdz;


};


class GarfieldPhysics {
public:
	static GarfieldPhysics* GetInstance();
	static void Dispose();

	void InitializePhysics();
	void CreateGeometry();

	void DoIt(std::string particleName, double ekin_MeV,double time,
			double x_cm, double y_cm, double z_cm, double dx, double dy, double dz);

	void AddParticleName(const std::string particleName, double ekin_min_MeV, double ekin_max_MeV, std::string program);
	bool FindParticleName(const std::string name, std::string program = "garfield");
	bool FindParticleNameEnergy(std::string name, double ekin_MeV, std::string program = "garfield");
	double GetMinEnergyMeVParticle(std::string name, std::string program = "garfield");
	double GetMaxEnergyMeVParticle(std::string name, std::string program = "garfield");
	void SetIonizationModel(std::string model, bool useDefaults=true);
	std::string GetIonizationModel();
	std::vector<GarfieldParticle*>* GetSecondaryParticles();
	void DeleteSecondaryParticles();
	inline void EnableCreateSecondariesInGeant4(bool flag) {createSecondariesInGeant4 = flag;};
	inline bool GetCreateSecondariesInGeant4() {return createSecondariesInGeant4;};
	inline double GetEnergyDeposit_MeV() {return fEnergyDeposit/1000000;};
	inline double GetAvalancheSize() {return fAvalancheSize;};
	inline double GetGain() {return fGain;};
	inline void Clear() {fEnergyDeposit=0;fAvalancheSize=0;fGain=0;nsum=0;}

private:
	GarfieldPhysics();
	~GarfieldPhysics();

	std::string fIonizationModel;

	static GarfieldPhysics* fGarfieldPhysics;
	MapParticlesEnergy* fMapParticlesEnergyGeant4;
	MapParticlesEnergy* fMapParticlesEnergyGarfield;
	TGeoManager* fGeoManager;
	Garfield::MediumMagboltz* fMediumMagboltz;
	Garfield::Sensor* fSensor;
	Garfield::AvalancheMC* fDrift;
	Garfield::AvalancheMicroscopic* fAvalanche;
	Garfield::TrackHeed* fTrackHeed;
	Garfield::GeometryRoot* fGeometryRoot;
	Garfield::GeometrySimple* fGeometrySimple;
	Garfield::ComponentAnalyticField* fComponentAnalyticField;
	Garfield::SolidTube* fTube;

	std::vector<GarfieldParticle*>* fSecondaryParticles;

	bool createSecondariesInGeant4;
	double fEnergyDeposit;
	double fAvalancheSize;
	double fGain;
	int nsum;


};
#endif /* GARFIELDMODELCONFIG_HH_ */
