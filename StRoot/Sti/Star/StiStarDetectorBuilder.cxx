#include <stdexcept>
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/Star/StiStarDetectorBuilder.h"
#include "Sti/StiElossCalculator.h"
#define __NO_SECTORS__
StiStarDetectorBuilder::StiStarDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("StarBuilder",active,inputFile)
{}

StiStarDetectorBuilder::~StiStarDetectorBuilder()
{}

void StiStarDetectorBuilder::buildDetectors(StMaker&s)
{
	string name;
  if ( StiVMCToolKit::GetVMC()) {useVMCGeometry(); return;}
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Started" << endl;
  setNRows(1);
  float dPhi=TMath::Pi()/6.;
  setNSectors(0,12);
  float radius = 4.;  // Kai - set this to 1.45
  float thickness = 0.076;
  float depth = 200.; // was 20...
  _pipeMaterial = add(new StiMaterial("Be", 4.,9.010, 1.848, 65.19, 4.*12.e-9) );
  _vacuumMaterial = add(new StiMaterial("Vaccum",0., 1., 0., 1e30, 0.)  );

  //Instantiate energy loss detector for beam pipe material
  //
  // Material is berylium
  //
  double ionization = _pipeMaterial->getIonization();
  StiElossCalculator * pipeElossCalculator = new StiElossCalculator(_pipeMaterial->getZOverA(), ionization*ionization);

  _beamPipeShape = new StiCylindricalShape;
  _beamPipeShape->setName("Star/pipe");
  _beamPipeShape->setThickness(thickness); // checked
  _beamPipeShape->setHalfDepth( depth );
  _beamPipeShape->setOpeningAngle( dPhi );
  _beamPipeShape->setOuterRadius(radius+thickness/2.);// checked
  add(_beamPipeShape);
  for(unsigned int sector = 0; sector<12; sector++)
    {
      StiPlacement *p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(radius);
      p->setLayerAngle(sector*dPhi);
      p->setNormalRep(sector*dPhi, radius, 0.);
      p->setRegion(StiPlacement::kMidRapidity);
      StiDetector *pipeVolume = _detectorFactory->getInstance();
      name="Star/Pipe/Sector_";
      name+=sector;
      pipeVolume->setName(name);
      pipeVolume->setIsOn(true);
      pipeVolume->setIsActive(new StiNeverActiveFunctor);
      pipeVolume->setIsContinuousMedium(false);
      pipeVolume->setIsDiscreteScatterer(true);
      pipeVolume->setShape(_beamPipeShape);
      pipeVolume->setPlacement(p);
      pipeVolume->setGas(_vacuumMaterial);
      pipeVolume->setMaterial(_pipeMaterial);
      pipeVolume->setElossCalculator(pipeElossCalculator);
      add(0,sector,pipeVolume);
    }
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}
//________________________________________________________________________________
void StiStarDetectorBuilder::useVMCGeometry() {
  string name;
  setNRows(1);
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  const VolumeMap_t PipeVolumes[] = { 
    {"PIPE","the STAR beam pipe mother volume","HALL_1/CAVE_1/PIPE_%d","",""},
    {"PIPC","the Central Beam PIPe Volume","HALL_1/CAVE_1/PIPE_%d/PIPC_1","",""},
    {"PVAC","the Vacuum Volume of Be section of pipe","HALL_1/CAVE_1/PIPE_%d/PIPC_1/PVAC_1","",""},
    {"PIPO","Steel pipe from Be to 1st flanges","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""},
    {"PVAO","its cavity","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""}
  };
#ifdef __NO_SECTORS__
  setNSectors(0,1);
  Double_t dPhi = 2*TMath::Pi();
#else
  setNSectors(0,12);
  Double_t dPhi = 2*TMath::Pi()/12;
#endif
  for (Int_t i = 1; i < 5; i += 2) {// loop over Be and Steel pipes
    TGeoVolume *pipe = gGeoManager->GetVolume(PipeVolumes[i  ].name);
    TGeoMaterial *pipeMaterial = pipe->GetMaterial();
    Double_t PotI = StiVMCToolKit::GetPotI(pipeMaterial);
    _pipeMaterial = add(new StiMaterial(pipeMaterial->GetName(),
					pipeMaterial->GetZ(),
					pipeMaterial->GetA(),
					pipeMaterial->GetDensity(),
					pipeMaterial->GetDensity()*pipeMaterial->GetRadLen(),
					PotI));
    Double_t ionization = _pipeMaterial->getIonization();
    StiElossCalculator * pipeElossCalculator = new StiElossCalculator(_pipeMaterial->getZOverA(), ionization*ionization);
    TGeoTube *pipeShape = (TGeoTube *) pipe->GetShape();
    Double_t Rmax = pipeShape->GetRmax();
    Double_t dZ   = pipeShape->GetDz();
    TGeoVolume *vac  = gGeoManager->GetVolume(PipeVolumes[i+1].name);
    TGeoTube *vacShape = (TGeoTube *) vac->GetShape();
    Double_t Rmin = vacShape->GetRmax();
    Double_t radius = (Rmin + Rmax)/2;
    _beamPipeShape = new StiCylindricalShape;
    _beamPipeShape->setName(PipeVolumes[i].comment);
    _beamPipeShape->setThickness(Rmax-Rmin);
    if (i == 1) _beamPipeShape->setHalfDepth( 2*dZ ); // merge two half of Be beam pipe
    else        _beamPipeShape->setHalfDepth( dZ );
    _beamPipeShape->setOpeningAngle( dPhi );
    _beamPipeShape->setOuterRadius(Rmax);
    add(_beamPipeShape);
    for (Int_t j = 0; j < 2; j++) { // placements
      if (i == 1 && j == 1) continue;
#ifdef __NO_SECTORS__
      unsigned int sector = 0; 
#else
      for(unsigned int sector = 0; sector<12; sector++) {
#endif
	StiPlacement *p = new StiPlacement;
	TString pathT = Form(PipeVolumes[i].path,j+1);
	if (i == 1) 	p->setZcenter(0); // merge two half of Be beam pipe
	else       	{
	  TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(pathT);
	  TGeoHMatrix  *hmat   = nodeP->GetMatrix();
	  Double_t *xyz = hmat->GetTranslation();
	  p->setZcenter(xyz[2]);
	}
	p->setLayerRadius(radius);
	p->setLayerAngle(sector*dPhi);
	p->setNormalRep(sector*dPhi, radius, 0.);
	p->setRegion(StiPlacement::kMidRapidity);
	StiDetector *pipeVolume = _detectorFactory->getInstance();
	TString nameP = pathT.Data();
	nameP += "_";
	nameP += sector;
	nameP.ReplaceAll("HALL_1/CAVE_1/","");
	nameP.Resize(30); nameP.Strip();
	pipeVolume->setName(nameP.Data());
	pipeVolume->setIsOn(true);
	pipeVolume->setIsActive(new StiNeverActiveFunctor);
	pipeVolume->setIsContinuousMedium(false);
	pipeVolume->setIsDiscreteScatterer(true);
	pipeVolume->setShape(_beamPipeShape);
	pipeVolume->setPlacement(p);
	pipeVolume->setGas(_vacuumMaterial);
	pipeVolume->setMaterial(_pipeMaterial);
	pipeVolume->setElossCalculator(pipeElossCalculator);
	add(0,sector,pipeVolume);
#ifndef __NO_SECTORS__
      }
#endif
    }
  }
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}
