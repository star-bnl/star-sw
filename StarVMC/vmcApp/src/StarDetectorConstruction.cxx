// $Id: StarDetectorConstruction.cxx,v 1.1 2004/07/12 20:36:38 potekhin Exp $
//
 
#include <iostream.h>

#include <TVirtualMC.h>
#include "TGeoManager.h"

#include "StarDetectorConstruction.h"

#include "StarMaterial.h"
#include "StarMedium.h"
#include "StarVolume.h"
#include "StarRotation.h"

#include "StarParams.h"
// #include "TPCG.h"
// #include "TECW.h"

#include "StarCaveGeometry.h"
#include "StarChamberParametrization.h"

ClassImp(StarDetectorConstruction)

using namespace std;

//_____________________________________________________________________________
StarDetectorConstruction::StarDetectorConstruction()
  : TObject(),
    fWorldLength(0.),  
    fTargetLength(0.), 
    fTrackerLength(0.),
    fChamberWidth(0.),  
    fChamberSpacing(0.),
    fImedAir(0),
    fImedPb(0),
    fImedXe(0)
{
  cout<<"StarDetectorConstruction ctor called"<<endl;
  //fpMagField = new ExN02MagneticField();
}

//_____________________________________________________________________________
StarDetectorConstruction::~StarDetectorConstruction()
{
  //delete fpMagField;
}

//_____________________________________________________________________________
  // Int_t imatAir = 1;
  //  gMC->Mixture(imatAir, "Air", a2, z2, density, 2, w2); 

void StarDetectorConstruction::ConstructMaterials()
{

  cout<<"StarDetectorConstruction::ConstructMaterials called"<<endl;
  //  Test lines to look for the already created materials:
  //  StarMaterial* testmat = (StarMaterial*) StarMaterial::_materials.FindObject("Air");
  //  cout<<testmat->GetName()<<" "<<testmat->GetNumber()<<endl;

//--------- Material definition ---------

  Double_t a,z,density,radl,absl;
  Float_t* ubuf = 0;
 
  Float_t a2[2] = {14.01,16.00}; Float_t z2[2] = {7.0,8.0}; Float_t w2[2] = {0.7,0.3};
  density = 1.29e-03;  
  StarMaterial::Mixture("Air", a2, z2, density, 2, w2); 

  // experiment:
  TGeoMaterial* mat = gGeoManager->GetMaterial("Air");
  Int_t nn = mat->GetIndex();
  cout<<"NUMBER: "<<nn<<endl;

  /*     TPC default gas P10: Ar/methane 9:1 by volume
	 Component Ar    A=40  Z=18 W=9
	 Component C     A=12  Z=6  W=1
	 Component H     A=1   Z=1  W=4 */


  Float_t a3[3] = {40.0,12.0,1.0};
  Float_t z3[3] = {18.0,6.0, 1.0};
  Float_t w3[3] = {9,   1,   4};

  density=0.9*0.001782+0.1*0.000667; // temporary -- will use a better formula later
  StarMaterial::Mixture("P10",a3,z3,density,-3,w3); 

  /*      Component C5     A=12  Z=6  W=5
	  Component H4     A=1   Z=1  W=4
	  Component O2     A=16  Z=8  W=2
	  Mixture   Mylar  Dens=1.39 */

  a3[0] = 12.0; a3[1]= 1.0; a3[2] = 16.0;
  z3[0] =  6.0; z3[1]= 4.0; z3[2] =  8.0;
  w3[0] =  5.0; w3[1]= 4.0; w3[2] =  2.0;
  density=1.39;
  StarMaterial::Mixture("Mylar",a3,z3,density,-3,w3);

  a = 207.19;  z = 82.;  density = 11.35;  radl = 0.5612;  absl = 0.1;
  StarMaterial::Material("Pb", a,   z, density, radl, absl); 


  a = 26.98;  z = 13.;  density = 2.7;  radl = 8.9;  absl = 0.2;
  StarMaterial::Material("Al", a,   z, density, radl, absl); 


  //
  // Tracking media

  Int_t    ifield = 2;       // User defined magnetic field
  Double_t fieldm = 10.;     // Maximum field value (in kiloGauss)
  Double_t epsil  = .001;    // Tracking precision, 
  Double_t stemax = -0.01;   // Maximum displacement for multiple scat 
  Double_t tmaxfd = -20.;    // Maximum angle due to field deflection 
  Double_t deemax = -.3;     // Maximum fractional energy loss, DLS 
  Double_t stmin  = -.8;

  StarMedium::Medium("Air",  "Air",   0, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin);
  StarMedium::Medium("P10",  "P10",   StarMedium::Sensitive, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin);
  StarMedium::Medium("Mylar","Mylar", 0, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin);
  StarMedium::Medium("Al",   "Al",    0, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin);

  //  gMC->Medium(fImedXe, "XenonGas", imatXe, 0, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin, ubuf, 0); 

  cout<<"StarDetectorConstruction::ConstructMaterials done"<<endl;
}    

//_____________________________________________________________________________
void StarDetectorConstruction::ConstructGeometry()
{

      
//--------- Definitions of Solids, Logical Volumes, Physical Volumes ---------
  
  //------------------------------ 
  // World
  //------------------------------ 


  cout<<"StarDetectorConstruction::ConstructGeometry"<<endl;


  // Create the world volume once and for all:

  Double_t halfWorldLength = 0.5*fWorldLength;
  
  Float_t world[3];
  world[0] = WORLD_SIZE_X/2.0;
  world[1] = WORLD_SIZE_Y/2.0;
  world[2] = WORLD_SIZE_Z/2.0;

  StarVolume::Volume("WRLD","BOX","Air",world,3);

  //  new StarCaveGeometry();

	 
}

//_____________________________________________________________________________
void StarDetectorConstruction::SetTargetMaterial(const TString& materialName)
{
  Warning("SetTargetMaterial", "Not available in virtual Monte Carlo");
}
 
//_____________________________________________________________________________
void StarDetectorConstruction::SetChamberMaterial(const TString& materialName)
{
  Warning("SetTargetMaterial", "Not available in virtual Monte Carlo");
}
