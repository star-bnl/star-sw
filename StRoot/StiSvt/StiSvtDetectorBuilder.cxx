#include <stdexcept>
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StiSvt/StiSvtDetectorBuilder.h" 
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSvtDbMaker/St_SvtDb_Reader.hh"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSvt/StiSvtIsActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include <stdio.h>
#include "tables/St_HitError_Table.h"

/*
  Geant names: SVTT the mother of all SVT volumes
               SFMO: is the mother of all Silicon Strip Detector volumes
	       SOUM: Outer shileding structure
	       SXR[L,1,2]: Circular water feeds
	       SCBM: Mother of All Cables
	       SALM: aluminum shield mesh
	       SOSH: SVT outer shield "
	       SISH: SVT inner shield "
	       SLY[D,1,2,3,4,5]: layer mother
	       SROD: Support rod
	       SBSP: Beampipe support mother
	       SCON: Support cone mother
	       SBWC: water manifold to support cone bracket mother
	       SWMM: water manifold mother
	       SIES: Volume to hold inner endring screws
	       SOES: Volume to hold outer endring screws
	       SBRG: Bracket joining the end rungs
	       SOER: outer end ring
	       SIRT: inner end ring tube piece 
               SIRP: inner end ring polygon piece 

  SVT Layer and Ladder Naming Convention
  Hardware       StiSvtBuilder
  ----------------------------------
  Layer Ladder   Layer Ladder
    1     2        0     0
    1     4        0     1
    1     6        0     2
    1     8        0     3
    2     1        1     0
    2     3        1     1
    2     5        1     2
    2     7        1     3

    3     2        2     0
    3     4        2     1
    3     6        2     2
    3     8        2     3
    3     10       2     4
    3     12       2     5
    4     1        3     0
    4     3        3     1
    4     5        3     2
    4     7        3     3
    4     9        3     4
    4     11       3     5

    5     2        4     0
    5     4        4     1
    5     6        4     2
    5     8        4     3
    5     10       4     4
    5     12       4     5
    5     14       4     6
    5     16       4     7
    6     1        5     0
    6     3        5     1
    6     5        5     2
    6     7        5     3
    6     9        5     4
    6     11       5     5
    6     13       5     6
    6     15       5     7
 */

StiSvtDetectorBuilder::StiSvtDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Svt",active,inputFile), _siMat(0), _hybridMat(0)
{
  _trackingParameters.setName("svtTrackingParameters");
  _calc.setName("svtHitError");
}

StiSvtDetectorBuilder::~StiSvtDetectorBuilder()
{}

void StiSvtDetectorBuilder::buildDetectors(StMaker & source)
{
  char name[50];  
	int nRows;
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Started" << endl;
	load(_inputFile, source);

	St_DataSet *dataSet = NULL;
  dataSet = source.GetDataSet("StSvtConfig");
	if (!dataSet)	throw runtime_error("StiSvtDetectorBuilder::loadDb() -E- dataSet==0 while getting StSvtConfig");
  _config = static_cast<StSvtConfig*>(dataSet->GetObject());
  if (!_config) throw runtime_error("StiSvtDetectorBuilder::loadDb() -E- _config==0");

  dataSet = source.GetDataSet("StSvtGeometry");
	if (!dataSet)	throw runtime_error("StiSvtDetectorBuilder::loadDb() -E- dataSet==0 while getting StSvtGeometry");
  _geometry = static_cast<StSvtGeometry*>(dataSet->GetObject());
  if (!_geometry) throw runtime_error("StiSvtDetectorBuilder::loadDb() -E- _geometry==0");
  nRows = 2* _config->getNumberOfBarrels();
  setNRows(nRows);
  if (StiVMCToolKit::GetVMC()) {useVMCGeometry();}
  
  cout << "SVT Number of  Rows : "<<2* _config->getNumberOfBarrels()<<endl
			 << "  Layer#  numberOfLadders      Radius" << endl;
  for (int layer=0;layer<nRows;layer++)
		cout << "  "<<layer<<"     "<<_config->getNumberOfLadders(1+layer/2)/2 << "   " 
				 << _geometry->getBarrelRadius(layer+1) << endl;

  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Materials" << endl;
  if (! _gasMat) 
    _gasMat    = add(new StiMaterial("Air",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  if (! _siMat)
    _siMat     = add(new StiMaterial("Si",     14.,      28.0855,   2.33,     21.82,           14.*12.*1e-9) );
  if (! _hybridMat)
    _hybridMat = add(new StiMaterial("Hybrid", 14.,      28.0855,   2.33,     21.82,           14.*12.*1e-9) );
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Shapes" << endl;

  double ionization = _siMat->getIonization();
  StiElossCalculator * siElossCalculator = new StiElossCalculator(_siMat->getZOverA(), ionization*ionization, _siMat->getA(), _siMat->getZ(), _siMat->getDensity());
  int nLayers = nRows;
  for(int layer = 0; layer<nLayers; layer++)
    {
      int nWafers = _config->getNumberOfWafers(1+layer/2);
      // Si wafer
      sprintf(name, "Svt/Layer_%d/Wafers", layer);
      _waferShape[layer] = new StiPlanarShape(name,
					      nWafers*_geometry->getWaferLength(),
					      2.*_geometry->getWaferThickness(),
					      _geometry->getWaferWidth() );
      add(_waferShape[layer]);
#if 0
      // Hybrids
      sprintf(name, "Svt/Layer_%d/Hybrids", layer);
      _hybridShape[layer] = new StiPlanarShape(name,nWafers*_geometry->getWaferLength(),0.1, 1.);
      add(_hybridShape[layer]);
#endif
    } // for layer
  
  for (int layer=0;layer<nRows;layer++)
    {
      int nSectors = _config->getNumberOfLadders(1+layer/2)/2;
      setNSectors(layer,nSectors); 
      // calculate generic params for this layer
      // number of ladders per layer (not barrel) & phi increment between ladders
      float fDeltaPhi = M_PI/nSectors;
      // width of gap between the edges of 2 adjacent ladders:
      //   first, the angle subtended by 1/2 of the ladder
      float fLadderRadius  = _geometry->getBarrelRadius(layer+1);
      if (fLadderRadius<=0)	throw runtime_error("StiSvtDetectorBuilder::buildDetectors() - FATAL - fLadderRadius<=0");
      float fHalfLadderPhi = atan2(_waferShape[layer]->getHalfWidth(),fLadderRadius);
      float fHalfGapPhi    = fDeltaPhi - fHalfLadderPhi;    
      // then the distance from the origin to  the gap center
      // != the layer radius bc the layer width differs from the gap width,
      // i.e. not a regular polygon.
      float fGapRadius = fLadderRadius*cos(fHalfGapPhi)/cos(fHalfLadderPhi);
      //   finally half the gap distance
      //      float fHalfGap = fGapRadius*tan(fHalfGapPhi);
      // determine the radius for the detector (ladders + hybrids)
      float fLayerRadius = (fLadderRadius + fGapRadius)/2.;

      double x,y,z,rc,rn, nx,ny,nz,dx,dy,dz;
      StSvtWaferGeometry* waferGeom;
      float fLadderPhi;
      float phiC, phiN, dPhi;
      for(unsigned int ladder = 0; ladder<getNSectors(layer); ladder++)
	{
	  //detector wafers placement 
	  if (layer==0 || layer==2 || layer==4)
	    waferGeom = (StSvtWaferGeometry*) _geometry->at(_geometry->getWaferIndex(1+layer/2,2*ladder+2,1));		
	  else
	    waferGeom = (StSvtWaferGeometry*) _geometry->at(_geometry->getWaferIndex(1+layer/2,2*ladder+1,1));		
	  x = waferGeom->x(0);
	  y = waferGeom->x(1);
	  z = waferGeom->x(2);
	  nx = waferGeom->n(0);
	  ny = waferGeom->n(1);
	  nz = waferGeom->n(2);
	  double nt = sqrt(nx*nx+ny*ny);
	  dx = waferGeom->d(0);
	  dy = waferGeom->d(1);
	  dz = waferGeom->d(2);
	  rc = sqrt(x*x+y*y);
	  rn = (x*nx+y*ny)/nt;
	  dPhi = acos((x*nx+y*ny)/(rc*nt));
	  phiC = fLadderPhi = atan2(y,x);
	  phiN = atan2(ny,nx);
	  dPhi = phiC-phiN;
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  pPlacement->setLayerRadius(fLayerRadius);
	  pPlacement->setLayerAngle(fLadderPhi);
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  pPlacement->setCenterRep(phiC, rc, -dPhi); 
	  sprintf(name, "Svt/Layer_%d/Ladder_%d/Wafers", layer, ladder);
	  StiDetector *pLadder = _detectorFactory->getInstance();
	  pLadder->setName(name);
	  pLadder->setIsOn(true);
	  pLadder->setIsActive(new StiIsActiveFunctor(_active));
	  pLadder->setIsContinuousMedium(true);
	  pLadder->setIsDiscreteScatterer(true);
	  pLadder->setGas(_gasMat);
	  pLadder->setMaterial(_siMat);
	  pLadder->setShape(_waferShape[layer]);
	  pLadder->setPlacement(pPlacement); 
	  pLadder->setHitErrorCalculator(&_calc);
	  pLadder->setKey(1,layer);
	  pLadder->setKey(2,ladder);
	  pLadder->setElossCalculator(siElossCalculator);

	  add(layer,ladder,pLadder);
	} // for ladder
    } // for layer
}

void StiSvtDetectorBuilder::loadFS(ifstream& iFile)
{
	cout << "StiSvtDetectorBuilder::loadFS(ifstream& iFile) -I- Started" << endl;
	_trackingParameters.loadFS(iFile);
	_calc.loadFS(iFile);
	cout << "StiSvtDetectorBuilder::loadFS(ifstream& iFile) -I- Started" << endl;
}

void StiSvtDetectorBuilder::loadDS(TDataSet& ds)
{
	cout << "StiSvtDetectorBuilder::loadDS(TDataSet* ds) -I- Started" << endl;
	_trackingParameters.loadDS(ds);
	_calc.loadDS(ds);
	cout << "StiSvtDetectorBuilder::loadDS(TDataSet* ds) -I- Done" << endl;
}


void StiSvtDetectorBuilder::setDefaults()
{
  cout << "StiSvtDetectorBuilder::setDefaults() -I- Started" << endl;
  _trackingParameters.setMaxChi2ForSelection(5.);
  _trackingParameters.setMinSearchWindow(1.);
  _trackingParameters.setMaxSearchWindow(4.);
  _trackingParameters.setSearchWindowScaling(10.);
  _calc.set(.0009,0.004,0.04,.0009,0.0032,0.09); 
  cout << _trackingParameters << endl;
  cout << _calc<<endl;
  cout << "StiSvtDetectorBuilder::setDefaults() -I- Done" << endl;
}
//________________________________________________________________________________
void StiSvtDetectorBuilder::useVMCGeometry() {
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  struct Material_t {
    Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"SILICON", &_siMat},
    {"SILICON", &_hybridMat}
  };
  Int_t M = sizeof(map)/sizeof(Material_t);
  for (Int_t i = 0; i < M; i++) {
    const TGeoMaterial *mat =  gGeoManager->GetMaterial(map[i].name); 
    if (! mat) continue;
    Double_t PotI = StiVMCToolKit::GetPotI(mat);
    *map[i].p = add(new StiMaterial(mat->GetName(),
				    mat->GetZ(),
				    mat->GetA(),
				    mat->GetDensity(),
				    mat->GetDensity()*mat->GetRadLen(),
				    PotI));
  }
  const VolumeMap_t SvtVolumes[] = { 
    {"SOUM", "Outer shileding structure","HALL_1/CAVE_1/SVTT_1/SOUM_1/*","",""},
    {"SXRL", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXRL_1-2/*","",""}, 
    {"SXR1", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXR1_3-4/*","",""}, 
    {"SXR2", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXR2_5-6/*","",""},
    //    {"SCBM", "Mother of All Cables","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/*","",""},
    {"SCBL", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCBL_1","",""},
    {"SCB1", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB1_2","",""},
    {"SCB2", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB2_3","",""},
    {"SCB3", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB3_4","",""},
    {"SFED", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFED_1","",""},
    {"SFE1", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFE1_2","",""},
    {"SFE2", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFE2_3","",""},
    {"SPLS", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPLS_1","",""},
    {"SPL1", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPL1_2","",""},
    {"SPL2", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPL2_3","",""},
    {"SALM", "aluminum shield mesh","HALL_1/CAVE_1/SVTT_1/SALM_1-2","",""},
    {"SOSH", "SVT outer shield","HALL_1/CAVE_1/SVTT_1/SOSH_1","",""},
    {"SISH", "SVT inner shield","HALL_1/CAVE_1/SVTT_1/SISH_1","",""},
    {"SLYD", "layer mother","HALL_1/CAVE_1/SVTT_1/SLYD_1/*","",""},
    {"SLY1", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY1_2/*","",""},
    {"SLY2", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY2_3/*","",""},
    {"SLY3", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY3_4/*","",""},
    {"SLY4", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY4_5/*","",""},
    {"SLY5", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY5_6/*","",""},
    //  {"SVTD", "an active wafer volume","HALL_1/CAVE_1/SVTT_1/SLY*/SLS*/SLD*/STL*/STS*/SVTD_1","svt","SVTD"}, // <+++
    {"SROD", "Support rod","HALL_1/CAVE_1/SVTT_1/SROD_1-2","",""},
    {"SBSP", "Beampipe support mother","HALL_1/CAVE_1/SVTT_1/SBSP_1-2","",""},
    //  {"SCON", "Support cone mother","HALL_1/CAVE_1/SVTT_1/SCON_1-2/*","",""},
    {"SBWC", "water manifold to support cone bracket mother","HALL_1/CAVE_1/SVTT_1/SBWC_1-2/*","",""},
    {"SWMM", "water manifold mother","HALL_1/CAVE_1/SVTT_1/SWMM_1-2/*","",""},
    {"SIES", "Volume to hold inner endring screws","HALL_1/CAVE_1/SVTT_1/SIES_1-2/*","",""},
    {"SOES", "Volume to hold outer endring screws","HALL_1/CAVE_1/SVTT_1/SOES_1-2/*","",""},
    {"SBRG", "Bracket joining the end rungs","HALL_1/CAVE_1/SVTT_1/SBRG_1-2/*","",""},
    {"SOER", "outer end ring","HALL_1/CAVE_1/SVTT_1/SOER_1-2/*","",""},
    {"SIRT", "inner end ring tube piece ","HALL_1/CAVE_1/SVTT_1/SIRT_1-2","",""},
    {"SIRP", "inner end ring polygon piece ","HALL_1/CAVE_1/SVTT_1/SIRP_1-2","",""},
#ifndef __SsdInChain__
    // SSD
    //  {"SFMO", "the mother of all Silicon Strip Detector volumes","HALL_1/CAVE_1/SVTT_1/SFMO_1","",""},
    {"SCMP","SSD mounting plate inserted in the cone","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCMP_1-8","",""},
    {"SCVM","SSD V-shape mouting piece","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCVM_1-8/*","",""},
    {"SSLT","the linking (sector to the cone) tube","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLT_1-8","",""},
    {"SSLB","the linking (sector to the cone)","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLB_1-8","",""},
    {"SSRS","the side of the small rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRS_1-4","",""},
    {"SSRT","the top of the side rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRT_1-4","",""},
    {"SSSS","Side parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSSS_1-4","",""},
    {"SSST","Top parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSST_1-4","",""},
    //  {"SFLM","the mother of the ladder","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/*","",""}, 
    {"SFSM","the structure mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFSM_1/*","",""},
    {"SFDM","the detectors and adcs mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/*","",""},
    {"SFSD","the strip detector",      "HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/SFSW_1-16/SFSD_1","ssd",""},// <+++
#endif
    {"STAC", "twinax cable approximation, copper","HALL_1/CAVE_1/SVTT_1/SCON_1/STAC_1-2","",""}
  };
  Int_t NoSvtVols = sizeof(SvtVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1/SVTT_1");
  TString path("");
  for (Int_t i = 0; i < NoSvtVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;;
    StiVMCToolKit::LoopOverNodes(nodeT, path, SvtVolumes[i].name, MakeAverageVolume);
  }
}
