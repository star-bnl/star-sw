#include <assert.h>
#include <stdexcept>

#include "St_base/StMessMgr.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/Star/StiStarDetectorBuilder.h"
#include "TMath.h"
#include "TError.h"
#include "TSystem.h"
//________________________________________________________________________________
void StiStarDetectorBuilder::buildDetectors(StMaker&s) {
  TGeoManager *geo = StiVMCToolKit::GetVMC();
  assert(geo);
  useVMCGeometry(); 
  LOG_INFO << "StiStarDetectorBuilder::buildDetectors() : Done" << endm;
}
//________________________________________________________________________________
void StiStarDetectorBuilder::useVMCGeometry() {
  _TpcRefSys = (gGeoManager->GetVolume("TpcRefSys"  )!=0) 
            || (gGeoManager->GetVolume("TpcRefSys_1")!=0);
  TGeoVolume *pipc = gGeoManager->GetVolume("PIPC"); 
  if (!pipc) { //No volume (??)
    Warning("StiStarDetectorBuilder::useVMCGeometry","No PIPC volume\n");
    assert(0);
    return;
  }
  //  setNSectors(0,0);
  _vacuumMaterial = add(new StiMaterial("Vacuum",0., 1., 0., 1e30, 0.)  );
  _gasMat = (add(new StiMaterial("Air",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9)));
  if (pipc->GetShape()->TestShapeBit(TGeoShape::kGeoTube)) {
    OldBeamPipe();
  } else {                                                     
    HftBeamPipe();     
  }
  TGeoVolume *osca = gGeoManager->GetVolume("OSCA"); 
  if (osca) NewSuppCone();
  TGeoVolume *fgtm = gGeoManager->GetVolume("FGTM"); 
  if (fgtm) Fgt();
}
//________________________________________________________________________________
void StiStarDetectorBuilder::OldBeamPipe() {
  LOG_INFO << "StiStarDetectorBuilder::buildDetectors() : Use VMC old beam pipe geometry" << endm;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t PipeVolumes[] = { 
    {"PIPE","the STAR beam pipe mother volume","HALL_1/CAVE_1/PIPE_%d","",""}, // pcon
    // old beam pipe
    {"PIPC","the Central Beam PIPe Volume","HALL_1/CAVE_1/PIPE_%d/PIPC_1","",""}, // tube
    {"PVAC","the Vacuum Volume of Be section of pipe","HALL_1/CAVE_1/PIPE_%d/PIPC_1/PVAC_1","",""}, //tube
    {"PIPO","Steel pipe from Be to 1st flanges","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""}, //tube
    {"PVAO","its cavity","HALL_1/CAVE_1/PIPE_%d/PIPO_1/PVAO_1","",""}, // tube
  //{"SCON", "Support cone mother","HALL_1/CAVE_1/SVTT_1/SCON_1-2/*","",""},
    {"SROD", "Support rod","HALL_1/CAVE_1/SVTT_1/SROD_1-2","",""},
    {"SBSP", "Beampipe support mother","HALL_1/CAVE_1/SVTT_1/SBSP_1-2","",""},
#if 0    
  //{"FGTM", "Beampipe support mother","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCM", "FGT nylon and Al ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTH", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTD", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCN", "FGT nylon 1st ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCT", "FGT inner cooling tube","HALL_1/CAVE_1/FGTM_1","",""}
#endif
  };
  
  for (Int_t i = 1; i < 5; i += 2) {// loop over Be and Steel pipes
    MakePipe(i, &PipeVolumes[i],&PipeVolumes[i+1]);
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
}
//________________________________________________________________________________
void StiStarDetectorBuilder::HftBeamPipe() {
  LOG_INFO << "StiStarDetectorBuilder::buildDetectors() : Use VMC HFT beam pipe geometry" << endm;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t PipeVolumes[] = { 
    /* new beam pipe in IDSM
   <Content>

 /CAVE/TpcRefSys/IDSM/PIPE,         <!--Mother volume of the east section, pcon(4) --> 
              PIHE,         <!--Hole inside the beam pipeof east section, pcon(4)> 
	      PALQ,         <!--East end tube, pcon(6) -->
	      PALR,         <!--East transition tube, pcon(6)-->
              SSCG, *3      <!-- Stainless Steel conflat flanges (ID 3 inches), tube -->
              SSCF,         <!-- Stainless Steel conflat flanges (ID 2 cm), tube  -->
 /CAVE/TpcRefSys/IDSM/PIPI,         <!--Mother volume of the middle section Placed in IDSM, pcon(16)-->
	      PIHI,         <!--Hole inside the beam pipe of middle section, tube -->
	      PALS,         <!--East aluminium part, pcon(6) -->
              PBES,         <!--Berillium  part, tube -->
	      PALI,         <!--West aluminium part, pcon(6) -->
              SSCF, *2      <!-- Stainless Steel conflat flanges (ID 2 cm), tube --> 

 /CAVE/TpcRefSys/IDSM/PIPW,         <!--Mother volume of the west section, pcon(4)-->
 	      PIHW,         <!--Hole inside the beam pipe of west section, pcon(4) -->
	      PALJ,         <!--West transition tube, pcon(8)-->
	      PALK,          <!--West end tube, pcon(8) -->
              SSCG, *3      <!-- Stainless Steel conflat flanges (ID 3 inches), tube -->
      /CAVE/PIPA,          <!--outer East section, pcon(2) -->
              PIPB,          <!--outer east bellow section, tube -->
	        PVPB,          <!--vacuum part of bellow,   -->
              PIPC,          <!-- east outset steel section with cone and large radius, tube -->
                PVPC,          <!-- vacuum section, tube  -->
              PRIS,           <!-- Bellow - used both east and west taken from PipeGeo.cml (tube)
                PRID, *8    the Bellow Steel Rib Set (tube)
                  PRIB      Rib of Steel Bellows (tube)
              PFLO          is the 1nd set of flanges at ~4.2 m from IR, tube
              PFLT           <!-- Flanges for bellow section, tube  -->
      /CAVE/PIWA,          <!--outer West section, pcon(2) -->
              PIWB,          <!--outer West bellow section, tube -->
                PVWB,          <!--vacuum part of bellow, tube   -->
              PIWC,          <!-- east outset steel section with cone and large radius, pcon(4) -->
                PVWC,          <!-- vacuum section, con(4) -->
              PRWS            <!-- West bellow section, tube
                PRWD,*8         the Bellow Steel Rib Set, tube
              PFLO           is the 1nd set of flanges at ~4.2 m from IR, tube 
              PFLT           <!-- Flanges for bellow section, tube
================
    */
  //{"PIPI", "Mother volume of the middle section Placed in IDSM"  ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1"        ,"",""},
  //{"PIHI", "Hole inside the beam pipe of middle section"         ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1/PIHI_1" ,"",""},
    {"PALS", "East aluminium part"                                 ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1/PALS_1" ,"",""},
    {"PBES", "Berillium  part"                                     ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1/PBES_1" ,"",""},
    {"PALI", "West aluminium part"                                 ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1/PALI_1" ,"",""},
    {"SSCF", "Stainless Steel conflat flange"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPI_1/SSCF_%d","",""},
  //{"PIPE", "Mother volume of the east sect"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1"        ,"",""},
  //{"PIHE", "Hole inside the beam pipe of east sect"              ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1/PIHE_1" ,"",""},
    {"PALQ", "East end tube"                                       ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1/PALQ_1" ,"",""},
    {"PALR", "East transition tube"                                ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1/PALR_1" ,"",""},
    {"SSCG", "Stainless Steel conflat flange"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1/SSCG_%d","",""},
    {"SSCF", "Stainless Steel conflat flange"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPE_1/SSCF_1" ,"",""},
  //{"PIPW", "Mother volume of the west sect"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPW_1"        ,"",""},
  //{"PIHW", "Hole inside the beam pipe of west sect"              ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPW_1/PIHW_1" ,"",""},
    {"PALJ", "West transition tube"                                ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPW_1/PALJ_1" ,"",""},
    {"PALK", "West end tube"                                       ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPW_1/PALK_1" ,"",""},
    {"SSCG", "Stainless Steel conflat flange"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PIPW_1/SSCG_%d","",""},
    {"PIPA", "outer East section"                                  ,"/HALL_1/CAVE_1/PIPA_1"               ,"",""},
    {"PIPB", "outer east bellow section"                           ,"/HALL_1/CAVE_1/PIPA_1/PIPB_1"        ,"",""},
    {"PVPB", "vacuum part of bellow"                               ,"/HALL_1/CAVE_1/PIPA_1/PIPB_1/PVPB_1" ,"",""},
    {"PIPC", "east outset steel section"                           ,"/HALL_1/CAVE_1/PIPA_1/PIPC_1"        ,"",""},
    {"PVPC", "vacuum section"                                      ,"/HALL_1/CAVE_1/PIPA_1/PIPC_1/PVPC_1" ,"",""},
    {"PRIS", "Bellow"                                              ,"/HALL_1/CAVE_1/PIPA_1/PRIS_1"        ,"",""},
  //{"PRID", "the Bellow Steel Rib Set"                            ,"/HALL_1/CAVE_1/PIPA_1/PRIS_1/PRID_%d","",""},
    {"PFLO", "is the 1nd set of flanges"                           ,"/HALL_1/CAVE_1/PIPA_1/PFLO_1"        ,"",""},
    {"PFLT", "Flanges for bellow section "                         ,"/HALL_1/CAVE_1/PIPA_1/PFLT_1"        ,"",""},
    {"PIWA", "outer West section"                                  ,"/HALL_1/CAVE_1/PIWA_1"               ,"",""},
    {"PIWB", "outer West bellow section"                           ,"/HALL_1/CAVE_1/PIWA_1/PIWB_1"        ,"",""},
    {"PVWB", "vacuum part of bellow"                               ,"/HALL_1/CAVE_1/PIWA_1/PIWB_1/PVWB_1" ,"",""},
    {"PIWC", "east outset steel section"                           ,"/HALL_1/CAVE_1/PIWA_1/PIWC_1"        ,"",""},
    {"PVWC", "vacuum section"                                      ,"/HALL_1/CAVE_1/PIWA_1/PIWC_1/PVWC_1" ,"",""},
    {"PRWS", "West bellow section"                                 ,"/HALL_1/CAVE_1/PIWA_1/PRWS_1"        ,"",""},
  //{"PRWD", "the Bellow Steel Rib Set"                            ,"/HALL_1/CAVE_1/PIWA_1/PRWS_1/PRWD_%d","",""},
    {"PFLO", "is the 1nd set of flange"                            ,"/HALL_1/CAVE_1/PIWA_1/PFLO_1"        ,"",""},
    {"PFLT", "Flanges for bellow section"                          ,"/HALL_1/CAVE_1/PIWA_1/PFLT_1"        ,"",""},
#if 0
  //{"FGTM", "Beampipe support mother"                             ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGTM_1"        ,"",""},
    {"FGCM", "FGT nylon and Al ring"                               ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGTM_1/FGCM_1" ,"",""},
    {"FGTD", "mother volume for FGT disk"                          ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGTM_1/FGTD_1" ,"",""},
    {"FGCN", "FGT nylon 1st ring"                                  ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGTM_1/FGCN_1" ,"",""},
    {"FGCT", "FGT inner cooling tube"                              ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGTM_1/FGCT_1" ,"",""}
#endif
  }; 
  Int_t NVol = sizeof(PipeVolumes)/sizeof(VolumeMap_t);
  for (Int_t i = 0;  i < NVol; i++) {
    TGeoVolume *pipe = gGeoManager->GetVolume(PipeVolumes[i].name);
    if (!pipe) { //No volume (??)
      Warning("StiStarDetectorBuilder::useVMCGeometry","No %s volume\n",PipeVolumes[i].name);
      continue;
    }
    TString pathT(gSystem->DirName(PipeVolumes[i].path));
    TString path("");
    if (pipe->GetShape()->TestShapeBit(TGeoShape::kGeoTube)) {
      TString pm(PipeVolumes[i].path);
      TString pv;
      if (i < NVol - 1 && PipeVolumes[i+1].name) {
	pv = pm; pv += "/"; pv += PipeVolumes[i+1].name;pv += "_1";
      }
      if (i < NVol - 1 && pv == TString(PipeVolumes[i+1].path)) {// case tube with vacuum daughter
	MakePipe(0, &PipeVolumes[i],&PipeVolumes[i+1]);
	i++;
      } else {
	gGeoManager->RestoreMasterVolume(); 
	gGeoManager->CdTop();
	if (! _TpcRefSys) {
	  pathT.ReplaceAll("/TpcRefSys_1","");
	  pathT.ReplaceAll("/TpcRefSys",  "");
        }
	gGeoManager->cd(pathT); path = pathT;
	TGeoNode *nodeT = gGeoManager->GetCurrentNode();
	if (! nodeT) continue;
	StiVMCToolKit::LoopOverNodes(nodeT, path, PipeVolumes[i].name, MakeAverageVolume);
      }
      continue;
    }
    if (! pipe->GetShape()->TestShapeBit(TGeoShape::kGeoPcon)) {
      Warning("StiStarDetectorBuilder::useVMCGeometry","Shape for %s volume is not recognized\n",PipeVolumes[i].name);
      assert(0);
      continue;
    }
    TGeoPcon *pcon = (TGeoPcon*) pipe->GetShape();
    TGeoMaterial *pipeMaterial = pipe->GetMaterial();
    Double_t PotI = StiVMCToolKit::GetPotI(pipeMaterial);
    Double_t density = pipeMaterial->GetDensity();
    _pipeMaterial = add(new StiMaterial(pipeMaterial->GetName(),
					pipeMaterial->GetZ(),
					pipeMaterial->GetA(),
					density,
					density*pipeMaterial->GetRadLen(),
					PotI));
    StiMaterial *_pipeMaterial2 = 0;
    Double_t Rmax, Rmin, radius, dZ, Z;
    Int_t Nz = pcon->GetNz();
#if 0
    LOG_INFO << pathT.Data() << "\t" << PipeVolumes[i].name << endm;
    for (Int_t k = 0; k < Nz; k++) {
      LOG_INFO << k << "\tZ\t" << pcon->GetZ(k) << "\tRmin\t" << pcon->GetRmin(k) << "\tRmax\t" << pcon->GetRmax(k) <<
	 endm;
    }    
#endif
    Int_t section = 0;
    for (Int_t k = 0; k < Nz-1; k++) {// replace pcon by tubes
      dZ  = TMath::Abs(pcon->GetZ(k) - pcon->GetZ(k+1))/2;
      if (dZ <  1e-7) continue;
      Z   =           (pcon->GetZ(k) + pcon->GetZ(k+1))/2;
      _pipeMaterial2 = _pipeMaterial;
      if (pcon->GetRmin(k) == pcon->GetRmin(k+1) &&
	  pcon->GetRmax(k) == pcon->GetRmax(k+1)) {
	Rmax = pcon->GetRmax(k);
	Rmin = pcon->GetRmin(k);
	radius = (Rmin + Rmax)/2;
      } else {
	Rmax = TMath::Max(pcon->GetRmax(k),pcon->GetRmax(k+1));
	Rmin = TMath::Min(pcon->GetRmin(k),pcon->GetRmin(k+1));
	Double_t scale = (pcon->GetRmax(k+1)*pcon->GetRmax(k+1) - pcon->GetRmin(k+1)*pcon->GetRmin(k+1) + 
	         pcon->GetRmax(k  )*pcon->GetRmax(k  ) - pcon->GetRmin(k  )*pcon->GetRmin(k  ))/
	  (Rmax*Rmax - Rmin*Rmin);
	Double_t dens = density*scale;
	_pipeMaterial2 = add(new StiMaterial(pipeMaterial->GetName(),
					pipeMaterial->GetZ(),
					pipeMaterial->GetA(),
					dens,
					density*pipeMaterial->GetRadLen(),
					PotI));
      }
      radius = (Rmin + Rmax)/2;
      _beamPipeShape = new StiCylindricalShape;
      TString comment(PipeVolumes[i].comment,15);
      _beamPipeShape->setName(comment.Data());
      _beamPipeShape->setThickness(Rmax-Rmin);
      _beamPipeShape->setHalfDepth( dZ );
      _beamPipeShape->setOpeningAngle( TMath::TwoPi() );
      _beamPipeShape->setOuterRadius(Rmax);
      
      add(_beamPipeShape);
      StiPlacement *p = new StiPlacement;
      p->setZcenter(Z);
      p->setLayerRadius(radius);
      p->setLayerAngle(0);
      p->setNormalRep(0, radius, 0.);
      p->setRegion(StiPlacement::kMidRapidity);
      TString nameP = PipeVolumes[i].name; nameP += "#"; section++; nameP += section;
      nameP.ReplaceAll("HALL_1/CAVE_1/","");
      nameP.ReplaceAll("/TpcRefSys_1","");
      nameP.ReplaceAll("/TpcRefSys","");
      nameP.ReplaceAll("/IDSM_1","");
      nameP.Resize(30); nameP.Strip();
      StiDetector *pipeVolume = _detectorFactory->getInstance();
      pipeVolume->setName(nameP.Data());
      pipeVolume->setIsOn(true);
      pipeVolume->setIsActive(new StiNeverActiveFunctor);
      pipeVolume->setIsContinuousMedium(false);
      pipeVolume->setIsDiscreteScatterer(true);
      pipeVolume->setShape(_beamPipeShape);
      pipeVolume->setPlacement(p);
      pipeVolume->setGas(_vacuumMaterial);
      pipeVolume->setMaterial(_pipeMaterial2);
      Int_t layer = getNRows();
      add(layer,0,pipeVolume);
    }
  }
}
//________________________________________________________________________________
void StiStarDetectorBuilder::NewSuppCone() {
  LOG_INFO << "StiStarDetectorBuilder::buildDetectors() : Use VMC Support Cone geometry" << endm;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t PipeVolumes[] = { 
    // cone 
    {"OSCA", "central CFiber tube"                     ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/OSCA_1" ,"",""},
    {"SUCB", "small Alu ring"                          ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCB_%d","",""},
    {"SUCC", "CFiber cone section"                     ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCC_%d","",""},
    {"SUCD", "large Al ring at cone"                   ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCD_%d","",""},
    {"SUCE", "CFiber tube"                             ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCE_%d","",""},
    {"SUCF", "large Alu ring"                          ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCF_%d","",""},
    {"SUCG", "large Alu end disk"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SUCG_%d","",""},
    {"FGRL", "FGT rail"                                ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGRL_%d","",""},
  //{"FGHV", "FGT cables mixture"                      ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/FGHV_%d","",""},
    {"EFSA", "Electrostatic shroud"                    ,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/EFSA_%d","",""}
  }; 
  Int_t NVol = sizeof(PipeVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  TString path("");
  for (Int_t i = 0;  i < NVol; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    if (! _TpcRefSys) {
      pathT.ReplaceAll("/TpcRefSys_1","");
      pathT.ReplaceAll("/TpcRefSys"  ,"");
    }
   gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;
    StiVMCToolKit::LoopOverNodes(nodeT, path, PipeVolumes[i].name, MakeAverageVolume);
  }
}
//________________________________________________________________________________
void StiStarDetectorBuilder::MakePipe(Int_t iflag, const VolumeMap_t *ptube,const VolumeMap_t *pvacu) {
  // iflag = 0 a signle volume, iflag = 1 merge west and east sides, iflag > 1 keep west and east volume separate
  if (! ptube || ! pvacu) return;
  TGeoVolume *pipe = gGeoManager->GetVolume(ptube->name);
  if (!pipe) { //No volume (??)
    Warning("StiStarDetectorBuilder::useVMCGeometry","No %s volume\n",ptube->name);
    assert(0);
   return;
  }
  TGeoMaterial *pipeMaterial = pipe->GetMaterial();
  Double_t PotI = StiVMCToolKit::GetPotI(pipeMaterial);
  _pipeMaterial = add(new StiMaterial(pipeMaterial->GetName(),
				      pipeMaterial->GetZ(),
				      pipeMaterial->GetA(),
				      pipeMaterial->GetDensity(),
				      pipeMaterial->GetDensity()*pipeMaterial->GetRadLen(),
				      PotI));
  TGeoTube *pipeShape = (TGeoTube *) pipe->GetShape();
  Double_t Rmax = pipeShape->GetRmax();
  Double_t Rmin = pipeShape->GetRmin();
  Double_t dZ   = pipeShape->GetDz();
  if (Rmin < 1e-3 && pvacu->name) {
    TGeoVolume *vac  = gGeoManager->GetVolume(pvacu->name);
    if (!vac) { //No volume (??)
      Warning("StiStarDetectorBuilder::useVMCGeometry","No %s volume\n",pvacu->name);
      assert(0);
      return;
    }
    TGeoTube *vacShape = (TGeoTube *) vac->GetShape();
    Rmin = vacShape->GetRmax();
  }
  Double_t radius = (Rmin + Rmax)/2;
  _beamPipeShape = new StiCylindricalShape;
  
  TString comment(ptube->comment,15);
  _beamPipeShape->setName(comment.Data());
  _beamPipeShape->setThickness(Rmax-Rmin);
  if (iflag == 1) _beamPipeShape->setHalfDepth( 2*dZ ); // merge two half of Be beam pipe
  else            _beamPipeShape->setHalfDepth( dZ );
  _beamPipeShape->setOpeningAngle( TMath::TwoPi() );
  _beamPipeShape->setOuterRadius(Rmax);
  add(_beamPipeShape);
  for (Int_t j = 0; j < 2; j++) { // placements
    if (iflag <= 1 && j == 1) continue;
    StiPlacement *p = new StiPlacement;
    TString pathT = ptube->path;
    if (pathT.Contains("%d")) pathT = Form(ptube->path,j+1);
    TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(pathT);
    if (! nodeP) continue;
    if (iflag == 1) 	p->setZcenter(0); // merge two half of Be beam pipe
    else       	{
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
    nameP.ReplaceAll("/TpcRefSys_1","");
    nameP.ReplaceAll("/TpcRefSys","");
    nameP.ReplaceAll("/IDSM","");
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
    Int_t layer = getNRows();
    add(layer,0,pipeVolume);
  }
}
//________________________________________________________________________________
void StiStarDetectorBuilder::Fgt() {
  LOG_INFO << "StiStarDetectorBuilder::buildDetectors() : Use VMC old beam pipe geometry" << endm;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t FgtVolumes[] = { 
    //{"FGTM", "Beampipe support mother","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCM", "FGT nylon and Al ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTH", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGTD", "mother volume for FGT disk","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCN", "FGT nylon 1st ring","HALL_1/CAVE_1/FGTM_1","",""},
    {"FGCT", "FGT inner cooling tube","HALL_1/CAVE_1/FGTM_1","",""}
  };
  Int_t NoExtraVols = sizeof(FgtVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  TString path("");
  for (Int_t i = 0; i < NoExtraVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;
    StiVMCToolKit::LoopOverNodes(nodeT, path, FgtVolumes[i].name, MakeAverageVolume);
  }
}
