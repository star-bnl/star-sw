#include "G4Material.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4PVReplica.hh"
#include "G4UniformMagField.hh"
#include "G4FieldManager.hh"
#include "G4GeometryManager.hh"
#include "G4PhysicalVolumeStore.hh"
#include "G4LogicalVolumeStore.hh"
#include "G4SolidStore.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4SDManager.hh"
#include "G4GDMLParser.hh"
#include "G4ProcessManager.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4LogicalBorderSurface.hh"
#include "G4OpticalSurface.hh"

#include <vector>
#include <string>

#include "RHICfDetectorConstruction.hh"
#include "RHICfField.hh"

#include "RHICfGSOplateSD.hh"
#include "RHICfGSObarSD.hh"
#include "RHICfFCSD.hh"
#include "RHICfTruthCounterSD.hh"
#include "RHICfZDCTruthCounterSD.hh"
#include "RHICfZDCSD.hh"
#include "RHICfSMDSD.hh"

#include "RHICfSimPar.hh"
#include "RHICfSimUtil.hh"
#include "RHICfSimOptions.hh"

RHICfDetectorConstruction::RHICfDetectorConstruction(): G4VUserDetectorConstruction()
{
}

RHICfDetectorConstruction::~RHICfDetectorConstruction()
{
}


G4VPhysicalVolume* RHICfDetectorConstruction::Construct()
{
  G4GeometryManager::GetInstance()->OpenGeometry();
  G4PhysicalVolumeStore::GetInstance()->Clean();
  G4LogicalVolumeStore::GetInstance()->Clean();
  G4SolidStore::GetInstance()->Clean();
  //  G4LogicalSkinSurface::CleanSurfaceTable();
  //  G4LogicalBorderSurface::CleanSurfaceTable();
  //  G4SurfaceProperty::CleanSurfacePropertyTable();

  RHICfSimUtil* simUtil = RHICfSimUtil::GetRHICfSimUtil();
	RHICfSimOptions* simOpt = simUtil -> GetOptions();
  TString geoPath = simOpt -> GetOptString("GEOMETRYDIR");
  TString tablePath = simOpt -> GetOptString("TABLEDIR");
  TString runType = simOpt -> GetOptString("RUNTYPE");
  G4String gdmlFile = (simUtil -> GetGDMLFile()).Data();
  
  G4VPhysicalVolume* fWorldPhysVol;
  parser.Read(gdmlFile);

  if(0) G4cout << *(G4Material::GetMaterialTable()) << G4endl;
  if(0) SetOpticalProperties();

  //  G4LogicalBorderSurface::DumpTableInfo();
  if(0) G4SurfaceProperty::DumpTableInfo();

  /// Get World Volume
  fWorldPhysVol = parser.GetWorldVolume();

  G4FieldManager* FieldMan
    =G4TransportationManager::GetTransportationManager()->GetFieldManager();
  /// Magnetic field (DX magnet)
  RHICfField* DXmagnetField=new RHICfField();
  FieldMan->SetDetectorField(DXmagnetField);
  FieldMan->CreateChordFinder(DXmagnetField);

  /// Set sensitive detector
  /// Set numbers for copy volumes
  int ifibr=0, igapf=0, izdc=0;
  int ismdh=0, ismdv=0;
  int ismdh_star=0, ismdv_star=0;
  int ilplate=0;
  int ilbar=0, ixysmall=0, ixylarge=0, ibarsmall=0, ibarlarge=0;
  G4PhysicalVolumeStore* pvs=G4PhysicalVolumeStore::GetInstance();
  for(G4PhysicalVolumeStore::iterator it=pvs->begin(); it!=pvs->end(); it++){
    if(0) G4cout << (*it)->GetName() << G4endl;

    if((*it)->GetName()=="Vol-holder-scintillator-gso-assembly_PV") (*it)->SetCopyNo(ilplate++);
    if((*it)->GetName()=="Vol-gsobar-holder-assembly_PV") (*it)->SetCopyNo(ilbar++);
    if((*it)->GetName()=="Vol-gsobelt-holder-assembly_PV") (*it)->SetCopyNo(ilbar++);
    if((*it)->GetName()=="Vol-gsobelt-small_PV") (*it)->SetCopyNo(ixysmall++); /// 0:x/1:y
    if((*it)->GetName()=="Vol-gsobar-small_PV") (*it)->SetCopyNo(ibarsmall++);
    if((*it)->GetName()=="Vol-gsobelt-large_PV") (*it)->SetCopyNo(ixylarge++);
    if((*it)->GetName()=="Vol-gsobar-large_PV") (*it)->SetCopyNo(ibarlarge++);
    if((*it)->GetName()=="Vol-frontcounter-small_PV") (*it)->SetCopyNo(0);
    if((*it)->GetName()=="Vol-frontcounter-large_PV") (*it)->SetCopyNo(1);

    if((*it)->GetName()=="Vol-fibr_PV") (*it)->SetCopyNo(ifibr++);
    if((*it)->GetName()=="Vol-gapf_PV") (*it)->SetCopyNo(igapf++);
    if((*it)->GetName()=="Vol-zdc_PV")  (*it)->SetCopyNo(izdc++);
    /// STAR SMD
    if((*it)->GetName()=="Vol-smdh-strip_PV") (*it)->SetCopyNo(ismdh_star++);
    if((*it)->GetName()=="Vol-smdv-strip_PV") (*it)->SetCopyNo(ismdv_star++);
    /// PHENIX SMD
    if((*it)->GetName()=="Vol-smdh_PV") (*it)->SetCopyNo(ismdh++);
    if((*it)->GetName()=="Vol-smdv_PV") (*it)->SetCopyNo(ismdv++);
    if((*it)->GetName()=="Vol-fcsc_PV") (*it)->SetCopyNo(0);
    if((*it)->GetName()=="Vol-rcsc_PV") (*it)->SetCopyNo(1);

    /// Truth Particle Counter (not real detector)
    if((*it)->GetName()=="Vol-TruthCounter-small_PV") (*it)->SetCopyNo(0);
    if((*it)->GetName()=="Vol-TruthCounter-large_PV") (*it)->SetCopyNo(1);
  }


  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  RHICfGSOplateSD* GSOplateSD = new RHICfGSOplateSD("GSOplate");
  GSOplateSD->SetTables(tablePath);
  SDman->AddNewDetector(GSOplateSD);
  RHICfGSObarSD* GSObarSD = new RHICfGSObarSD("GSObar");
  GSObarSD->SetTables(tablePath);
  SDman->AddNewDetector(GSObarSD);
  RHICfFCSD* FCSD = new RHICfFCSD("FC");
  SDman->AddNewDetector(FCSD);

  RHICfTruthCounterSD* truthCounterSD = new RHICfTruthCounterSD("TruthCounter");
  SDman->AddNewDetector(truthCounterSD);

  RHICfZDCSD* ZDCSD = new RHICfZDCSD("ZDC");
  ZDCSD->SetTables(tablePath);
  SDman->AddNewDetector(ZDCSD);
  const G4LogicalVolumeStore* lvs = G4LogicalVolumeStore::GetInstance();
  std::vector<G4LogicalVolume*>::const_iterator lvcite;
  for(lvcite=lvs->begin(); lvcite!=lvs->end(); lvcite++) {
    if((*lvcite)->GetSolid()->GetName()=="Log-fibr") {
      ZDCSD->SetFiberLength(dynamic_cast<G4Tubs*> ((*lvcite)->GetSolid())->GetZHalfLength());
      break;
    }
  }
  RHICfSMDSD* SMDSD = new RHICfSMDSD("SMD");
  SDman->AddNewDetector(SMDSD);

  RHICfZDCTruthCounterSD* zdcTruthCounterSD = new RHICfZDCTruthCounterSD("ZDCTruthCounter");
  SDman->AddNewDetector(zdcTruthCounterSD);


  //////////
  // Retrieve auxiliary information for sensitive detector
  const G4GDMLAuxMapType* auxmap = parser.GetAuxMap();
  if(0) {
    G4cout << "Found " << auxmap->size()
	   << " volume(s) with auxiliary information."
	   << G4endl << G4endl;
    for(G4GDMLAuxMapType::const_iterator iter=auxmap->begin(); iter!=auxmap->end(); iter++) {
      G4cout << "Volume " << ((*iter).first)->GetName()
	     << " has the following list of auxiliary information: "
	     << G4endl << G4endl;
      for(G4GDMLAuxListType::const_iterator vit=(*iter).second.begin(); vit!=(*iter).second.end(); vit++) {
	G4cout << "--> Type: " << (*vit).type
	       << " Value: " << (*vit).value << G4endl;
      }
    }
    G4cout << G4endl;
  }

  // The same as above, but now we are looking for
  // sensitive detectors setting them for the volumes
  for(G4GDMLAuxMapType::const_iterator iter=auxmap->begin(); iter!=auxmap->end(); iter++) {
    if(0) {
      G4cout << "Volume " << ((*iter).first)->GetName()
	     << " has the following list of auxiliary information: "
	     << G4endl << G4endl;
    }
    for(G4GDMLAuxListType::const_iterator vit=(*iter).second.begin(); vit!=(*iter).second.end();vit++) {
      if ((*vit).type=="SensDet") {
	if(0) {
	  G4cout << "Attaching sensitive detector " << (*vit).value
		 << " to volume " << ((*iter).first)->GetName()
		 <<  G4endl << G4endl;
	}
	G4VSensitiveDetector* mydet=SDman->FindSensitiveDetector((*vit).value);
	if(mydet) {
	  G4LogicalVolume* myvol = (*iter).first;
	  myvol->SetSensitiveDetector(mydet);
	}else{
	  G4cout << (*vit).value << " detector not found" << G4endl;
	}
      }
    }
  }

  //------------------------------------------------
  if(0) {
    G4VisAttributes* invisible = new G4VisAttributes();
    invisible->SetVisibility(false);

    // Go through every logical volume in the detector description.
    for(G4LogicalVolumeStore::iterator it=G4LogicalVolumeStore::GetInstance()->begin(); it!=G4LogicalVolumeStore::GetInstance()->end(); ++it) {
      G4LogicalVolume* volume = (*it);
      G4String name = volume->GetName();

      G4cerr << __FILE__ << ", line " << __LINE__
	     << ": checking '" << name << "'"
	     << G4endl;
    }
  }
  //------------------------------------------------

  return fWorldPhysVol;
}

void RHICfDetectorConstruction::SetOpticalProperties()
{
  // ------------ Generate Material Properties Table ------------
  const int NN = 50;
  G4double PP[NN]=
    {2.00*CLHEP::eV,2.03*CLHEP::eV,2.06*CLHEP::eV,2.09*CLHEP::eV,2.12*CLHEP::eV,
     2.15*CLHEP::eV,2.18*CLHEP::eV,2.21*CLHEP::eV,2.24*CLHEP::eV,2.27*CLHEP::eV,
     2.30*CLHEP::eV,2.33*CLHEP::eV,2.36*CLHEP::eV,2.39*CLHEP::eV,2.42*CLHEP::eV,
     2.45*CLHEP::eV,2.48*CLHEP::eV,2.51*CLHEP::eV,2.54*CLHEP::eV,2.57*CLHEP::eV,
     2.60*CLHEP::eV,2.63*CLHEP::eV,2.66*CLHEP::eV,2.69*CLHEP::eV,2.72*CLHEP::eV,
     2.75*CLHEP::eV,2.78*CLHEP::eV,2.81*CLHEP::eV,2.84*CLHEP::eV,2.87*CLHEP::eV,
     2.90*CLHEP::eV,2.93*CLHEP::eV,2.96*CLHEP::eV,2.99*CLHEP::eV,3.02*CLHEP::eV,
     3.05*CLHEP::eV,3.08*CLHEP::eV,3.11*CLHEP::eV,3.14*CLHEP::eV,3.17*CLHEP::eV,
     3.20*CLHEP::eV,3.23*CLHEP::eV,3.26*CLHEP::eV,3.29*CLHEP::eV,3.32*CLHEP::eV,
     3.35*CLHEP::eV,3.38*CLHEP::eV,3.41*CLHEP::eV,3.44*CLHEP::eV,3.47*CLHEP::eV};

  G4double refractiveIndex[NN]=
    {1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00,
     1.00, 1.00, 1.00, 1.00, 1.00};

  G4MaterialPropertiesTable* mptAir = new G4MaterialPropertiesTable();
  mptAir->AddProperty("RINDEX", PP, refractiveIndex, NN);

  (G4Material::GetMaterial("Air"))->SetMaterialPropertiesTable(mptAir);

  //--------------------------------------------------
  //  Polyethylene
  //--------------------------------------------------
  G4double refractiveIndexfiber[NN]=
  {1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49,
   1.49, 1.49, 1.49, 1.49, 1.49};

  G4double absfiber[NN] =
  {5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,
   5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,
   5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,
   5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,
   5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,
   5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,5.40*CLHEP::m,1.10*CLHEP::m,
   1.10*CLHEP::m,1.10*CLHEP::m,1.10*CLHEP::m,1.10*CLHEP::m,1.10*CLHEP::m,
   1.10*CLHEP::m,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,
   1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,
   1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm,1.0*CLHEP::mm};

  G4double emissionFib[NN] =
    {0.05, 0.10, 0.30, 0.50, 0.75,
     1.00, 1.50, 1.85, 2.30, 2.75,
     3.25, 3.80, 4.50, 5.20, 6.00,
     7.00, 8.50, 9.50, 11.1, 12.4,
     12.9, 13.0, 12.8, 12.3, 11.1,
     11.0, 12.0, 11.0, 17.0, 16.9,
     15.0, 9.00, 2.50, 1.00, 0.05,
     0.00, 0.00, 0.00, 0.00, 0.00,
     0.00, 0.00, 0.00, 0.00, 0.00,
     0.00, 0.00, 0.00, 0.00, 0.00};

  // Add entries into properties table
  G4MaterialPropertiesTable* mptfiber = new G4MaterialPropertiesTable();
  mptfiber->AddProperty("RINDEX",PP,refractiveIndexfiber,NN);
  mptfiber->AddProperty("ABSLENGTH",PP,absfiber,NN);
  mptfiber->AddProperty("COMPONENT",PP,emissionFib,NN);
  mptfiber->AddConstProperty("TIMECONSTANT", 0.5*CLHEP::ns);

  (G4Material::GetMaterial("PMMA"))->SetMaterialPropertiesTable(mptfiber);

  /*
  //--------------------------------------------------
  // Fluorinated Polyethylene
  //--------------------------------------------------
  G4double refractiveIndexClad2[NN]=
    {1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40,
     1.40, 1.40, 1.40, 1.40, 1.40};

  // Add entries into properties table
  G4MaterialPropertiesTable* mptClad2 = new G4MaterialPropertiesTable();
  mptClad2->AddProperty("RINDEX",PP,refractiveIndexClad2,NN);
  mptClad2->AddProperty("ABSLENGTH",PP,absClad,NN);

  fFPethylene->SetMaterialPropertiesTable(mptClad2);
  */
  //--------------------------------------------------
  // Fluorinated PMMA
  //--------------------------------------------------
   G4double refractiveIndexFPMMA[NN] =
     {1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40,
      1.40, 1.40, 1.40, 1.40, 1.40};

   G4double absClad[NN]=
     {20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,
      20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m,20.0*CLHEP::m};


  // Add entries into properties table
  G4MaterialPropertiesTable* mptFPMMA = new G4MaterialPropertiesTable();
  mptFPMMA->AddProperty("RINDEX",PP,refractiveIndexFPMMA,NN);
  mptFPMMA->AddProperty("ABSLENGTH",PP,absClad,NN);

  (G4Material::GetMaterial("FPMMA"))->SetMaterialPropertiesTable(mptFPMMA);

  /*
  //--------------------------------------------------
  // Silicone
  //--------------------------------------------------
   G4double refractiveIndexSilicone[NN] =
     {1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46,
      1.46, 1.46, 1.46, 1.46, 1.46};

  // Add entries into properties table
  G4MaterialPropertiesTable* mptSilicone = new G4MaterialPropertiesTable();
  mptSilicone->AddProperty("RINDEX",PP,refractiveIndexSilicone,NN);
  mptSilicone->AddProperty("ABSLENGTH",PP,absClad,NN);

  fSilicone->SetMaterialPropertiesTable(mptSilicone);
  */

  /*
  //--------------------------------------------------
  //  Polystyrene
  //--------------------------------------------------
  G4double refractiveIndexPS[NN] =
    {1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50,
     1.50, 1.50, 1.50, 1.50, 1.50};

  G4double absPS[NN] =
    {2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,
     2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm,2.*CLHEP::cm};

  G4double scintilFast[NN] =
    {0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0,
     1.0, 1.0, 1.0, 1.0, 1.0,
     1.0, 1.0, 1.0, 1.0, 1.0,
     1.0, 1.0, 1.0, 1.0, 1.0,
     1.0, 1.0, 1.0, 1.0, 1.0};
  
  // Add entries into properties table
  G4MaterialPropertiesTable* mptPL_SCINTI = new G4MaterialPropertiesTable();
  mptPL_SCINTI->AddProperty("RINDEX",PP,refractiveIndexPS,NN);
  mptPL_SCINTI->AddProperty("ABSLENGTH",PP,absPS,NN);
  mptPL_SCINTI->
               AddProperty("FASTCOMPONENT",PP, scintilFast,NN);
  mptPL_SCINTI->AddConstProperty("SCINTILLATIONYIELD",10./keV);
  mptPL_SCINTI->AddConstProperty("RESOLUTIONSCALE",1.0);
  mptPL_SCINTI->AddConstProperty("FASTTIMECONSTANT", 10.*ns);
 
  fPL_SCINTI->SetMaterialPropertiesTable(mptPL_SCINTI);

  // Set the Birks Constant for the PL scintillator

  fPL_SCINTI->GetIonisation()->SetBirksConstant(0.126*mm/MeV);
  */

  //  fFIBRLogical->SetUserLimits(new G4UserLimits(DBL_MAX, DBL_MAX, 10*ms));

  /*
  G4OpticalSurface* fBlackpaint = new G4OpticalSurface("GAP1/FIBRSurface", unified, polishedbackpainted, dielectric_dielectric, 1);

  G4double REFLECTIVITY[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  G4double EFFICIENCY[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  G4double RINDEX[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  G4double BACKSCATTER[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  G4double SPECULARSPIKE[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  G4double SPECULARLOBE[NN]=
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    
  fBlack=new G4MaterialPropertiesTable();
  fBlack->AddProperty("REFLECTIVITY", PP, REFLECTIVITY, NN);
  fBlack->AddProperty("RINDEX", PP, RINDEX, NN);
  fBlack->AddProperty("SPECULARSPIKE", PP, SPECULARSPIKE, NN);
  fBlack->AddProperty("SPECULARLOBE", PP, SPECULARLOBE, NN);
  fBlack->AddProperty("BACKSCATTER", PP, BACKSCATTER, NN);
  //fBlackpaint -> SetMaterialPropertiesTable(fBlack);
  */

  // Optical surface setting for Refrection and Reflection 
  fOpsurface1=new G4OpticalSurface("boundary1", unified, polished, dielectric_dielectric, 1);
  fOpsurface2=new G4OpticalSurface("boundary2", unified, polished, dielectric_dielectric, 1);
  //  G4VPhysicalVolume* PVworld=parser.GetWorldVolume();
  G4VPhysicalVolume* PVgapf=G4PhysicalVolumeStore::GetInstance()->GetVolume("Vol-gapf_PV");
  G4VPhysicalVolume* PVfibr=G4PhysicalVolumeStore::GetInstance()->GetVolume("Vol-fibr_PV");
  G4VPhysicalVolume* PVzdc=G4PhysicalVolumeStore::GetInstance()->GetVolume("Vol-zdc_PV");
  new G4LogicalBorderSurface("surface1In",  PVfibr, PVgapf, fOpsurface1);
  new G4LogicalBorderSurface("surface1Out", PVgapf, PVfibr, fOpsurface1);
  new G4LogicalBorderSurface("surface2In",  PVfibr, PVzdc, fOpsurface2);
  new G4LogicalBorderSurface("surface2Out", PVzdc, PVfibr, fOpsurface2);
  /*
  new G4LogicalBorderSurface("surfaceOut1_1", fPMT_1Physical, fGAPF_1Physical, fOpsurface);
  new G4LogicalBorderSurface("surfaceIn1_1",  fGAPF_1Physical, fPMT_1Physical, fOpsurface);
  new G4LogicalBorderSurface("surfaceOut2_1", fFIBRPhysical, fGAPF_1Physical, fOpsurface);
  new G4LogicalBorderSurface("surfaceIn2_1", fGAPF_1Physical, fFIBRPhysical, fOpsurface);
  */
}

void RHICfDetectorConstruction::SetCuts(G4double aetacut, G4double aecut, bool aopposite)
{
  etacut=aetacut;
  ecut=aecut;
  opposite=aopposite;
}
