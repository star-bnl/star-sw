#include <assert.h>
#include <stdexcept>
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
#include "TMath.h"
#include "TError.h"
//________________________________________________________________________________
void StiStarDetectorBuilder::buildDetectors(StMaker&s) {
  if (! _gasMat) 
    _gasMat    = add(new StiMaterial("Air",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  TGeoManager *geo = StiVMCToolKit::GetVMC();
  assert(geo);
  useVMCGeometry(); 
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}
//________________________________________________________________________________
void StiStarDetectorBuilder::useVMCGeometry() {
  string name;
  setNRows(1);
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t PipeVolumes[] = { 
    {"PIPE","the STAR beam pipe mother volume","HALL_1/CAVE_1/PIPE_%d","",""},
    {"PIPC","the Central Beam PIPe Volume","HALL_1/CAVE_1/PIPE_%d/PIPC_1","",""},
    {"PVAC","the Vacuum Volume of Be section of pipe","HALL_1/CAVE_1/PIPE_%d/PIPC_1/PVAC_1","",""},
    {"PIPO","Steel pipe from Be to 1st flanges","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""},
    {"PVAO","its cavity","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""},
    //    {"SCON", "Support cone mother","HALL_1/CAVE_1/SVTT_1/SCON_1-2/*","",""},
    {"SROD", "Support rod","HALL_1/CAVE_1/SVTT_1/SROD_1-2","",""},
    {"SBSP", "Beampipe support mother","HALL_1/CAVE_1/SVTT_1/SBSP_1-2","",""},
    //    {"FGTM", "Beampipe support mother","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCM", "FGT nylon and Al ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTH", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTD", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCN", "FGT nylon 1st ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCT", "FGT inner cooling tube","HALL_1/CAVE_1/FGTM_1","",""}
  };
  setNSectors(0,1);
  Double_t dPhi = 2*TMath::Pi();
  _vacuumMaterial = add(new StiMaterial("Vaccum",0., 1., 0., 1e30, 0.)  );
  for (Int_t i = 1; i < 5; i += 2) {// loop over Be and Steel pipes
    TGeoVolume *pipe = gGeoManager->GetVolume(PipeVolumes[i].name);
    if (!pipe) { //No volume (??)
      Warning("StiStarDetectorBuilder::useVMCGeometry","No %s volume\n",PipeVolumes[i].name);
      continue;
    }
    TGeoMaterial *pipeMaterial = pipe->GetMaterial();
    Double_t PotI = StiVMCToolKit::GetPotI(pipeMaterial);
    _pipeMaterial = add(new StiMaterial(pipeMaterial->GetName(),
					pipeMaterial->GetZ(),
					pipeMaterial->GetA(),
					pipeMaterial->GetDensity(),
					pipeMaterial->GetDensity()*pipeMaterial->GetRadLen(),
					PotI));
    Double_t ionization = _pipeMaterial->getIonization();
    StiElossCalculator * pipeElossCalculator = new StiElossCalculator(_pipeMaterial->getZOverA(), ionization*ionization, _pipeMaterial->getA(), _pipeMaterial->getZ(), _pipeMaterial->getDensity());
    TGeoTube *pipeShape = (TGeoTube *) pipe->GetShape();
    Double_t Rmax = pipeShape->GetRmax();
    Double_t dZ   = pipeShape->GetDz();
    TGeoVolume *vac  = gGeoManager->GetVolume(PipeVolumes[i+1].name);
    if (!vac) { //No volume (??)
      Warning("StiStarDetectorBuilder::useVMCGeometry","No %s volume\n",PipeVolumes[i+1].name);
      continue;
    }
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
      p->setLayerAngle(0);
      p->setNormalRep(0, radius, 0.);
      p->setRegion(StiPlacement::kMidRapidity);
      StiDetector *pipeVolume = _detectorFactory->getInstance();
      TString nameP = pathT.Data();
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
      Int_t layer = getNRows();
      add(layer+1,0,pipeVolume);
    }
  }
  Int_t NoExtraVols = sizeof(PipeVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  TString path("");
  for (Int_t i = 5; i < NoExtraVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;
    StiVMCToolKit::LoopOverNodes(nodeT, path, PipeVolumes[i].name, MakeAverageVolume);
  }
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}
