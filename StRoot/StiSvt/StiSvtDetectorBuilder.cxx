#include <stdexcept>
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StiSvtDetectorBuilder.h" 
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StDbUtilities/St_svtRDOstrippedC.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiIsSvtActiveFunctor.h"
//#include "Sti/StiElossCalculator.h"
#include "StDetectorDbMaker/StiSvtHitErrorCalculator.h"
#include <stdio.h>
#include "tables/St_HitError_Table.h"
#include "StThreeVectorD.hh"
#include "StiSvtHitLoader.h"
/*
  Geant names: SVTT the mother of all SVT volumes
                 SFMO: is the mother of all Silicon Strip Detector volumes
	         SOUM: Outer shielding structure
	         SXR[L,1,2]: Circular water feeds
	         SCBM: Mother of All Cables
	         SALM: aluminum shield mesh
	         SOSH: SVT outer shield "
	         SISH: SVT inner shield "
	         SLY[D,1,2,3,4,5]: layer mother
	            SLS[D,1,2,3,4,5]: ladder mother
                      SELE: electronics mother volume
                      SLDI: a ladder volume
		        SPCB: the G10 PCB
			SRHC: roha cell wafer supports
			STAB: the Berrillium tabs and the ends of the wafer carriers
			SBER: the Berillium wafer carrier rails
			STLI: the wafer pack container
			  STSI:  a single waver container
			    SVTD: an active wafer volume
			      SSIR: a non-sensitive up-down border of the wafer
			      SSID: a non-sensitive left-right border of the wafer
			      STRA: a trapezoid of triangular shape
			        Total epoxy: ~10 pounds = 4.57 kG ==> 4.57 / 216 = 21.16 g/wafer *7 => 148.1 g/big ladder
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
//static Int_t _debug = 0;
StiSvtDetectorBuilder::StiSvtDetectorBuilder(bool active)
  : StiDetectorBuilder("Svt",active), _siMat(0), _hybridMat(0){}

StiSvtDetectorBuilder::~StiSvtDetectorBuilder() {}

void StiSvtDetectorBuilder::buildDetectors(StMaker & source)
{
  char name[50];  
  int nRows;
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Started" << endl;

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
  
  cout << "SVT Number of  Rows : "<<2* _config->getNumberOfBarrels()<<endl
			 << "  Layer#  numberOfLadders      Radius" << endl;
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Materials" << endl;
  if (! _gasMat) 
    _gasMat    = add(new StiMaterial("Air",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  if (! _siMat)
    _siMat     = add(new StiMaterial("Si",     14.,      28.0855,   2.33,     21.82,           14.*12.*1e-9) );
  if (! _hybridMat)
    _hybridMat = add(new StiMaterial("Hybrid", 14.,      28.0855,   2.33,     21.82,           14.*12.*1e-9) );
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Shapes" << endl;

//  double ionization = _siMat->getIonization();
//   StiElossCalculator * siElossCalculator = 
//     new StiElossCalculator(_siMat->getZOverA(), ionization*ionization, _siMat->getA(), _siMat->getZ(), _siMat->getDensity());
  for (int layer=0;layer<nRows;layer++)    {
    cout << "  "<<layer<<"     "<<_config->getNumberOfLadders(1+layer/2)/2 << "   " 
	 << _geometry->getBarrelRadius(layer+1) << endl;
    Int_t svtLayer = layer+1;
    Int_t svtBarrel = StiSvtHitLoader::getSvtBarrel(svtLayer);
    int nWafers = _config->getNumberOfWafers(svtBarrel);
    // Si wafer
    sprintf(name, "Svt/Layer_%d/Wafers", layer);
    _waferShape[layer] = new StiPlanarShape(name,
					    nWafers*3.15, //_geometry->getWaferLength(),
					    2.*_geometry->getWaferThickness(),
					    3.15);        //_geometry->getWaferWidth() );
    add(_waferShape[layer]);
    int nSectors = _config->getNumberOfLadders(svtBarrel)/2;
    setNSectors(layer,nSectors); 
    // calculate generic params for this layer
    // number of ladders per layer (not barrel) & phi increment between ladders
    //    float fDeltaPhi = M_PI/nSectors;
    // width of gap between the edges of 2 adjacent ladders:
    //   first, the angle subtended by 1/2 of the ladder
    float fLadderRadius  = _geometry->getBarrelRadius(svtLayer);
    if (fLadderRadius<=0)	throw runtime_error("StiSvtDetectorBuilder::buildDetectors() - FATAL - fLadderRadius<=0");
    StSvtWaferGeometry* waferGeom;
    Int_t index1, index2;
    StSvtWaferGeometry* waferGeom2;
    for(unsigned int ladder = 0; ladder<getNSectors(layer); ladder++)	{
      Int_t svtLadder = 2*(ladder+1) - (svtLayer-1)%2;
      Int_t wafer = nWafers/2+1;
      index1 = _geometry->getWaferIndex(svtBarrel,svtLadder,wafer);
      assert (index1 >= 0);
      waferGeom = (StSvtWaferGeometry*) _geometry->at(index1);
      if (_debug) waferGeom->print();
      StThreeVectorD centerVector(waferGeom->x(0), waferGeom->x(1), waferGeom->x(2) );
      if ( nWafers%2 == 0) {
	index2 = _geometry->getWaferIndex(svtBarrel,svtLadder,wafer-1);
	assert(index2 >= 0);
	waferGeom2 = (StSvtWaferGeometry*) _geometry->at(index2);
	StThreeVectorD centerVector2(waferGeom2->x(0), waferGeom2->x(1), waferGeom2->x(2) );
	if (_debug) waferGeom2->print();
	centerVector += centerVector2;
	centerVector *= 0.5;
      }
      StThreeVectorD normalVector(waferGeom->n(0), waferGeom->n(1), waferGeom->n(2) );
      Double_t prod = centerVector.x()*normalVector.x() + centerVector.y()*normalVector.y();
      if (prod < 0) normalVector *= -1;
      double phi  = centerVector.phi();
      double phiD = normalVector.phi();
      double r = centerVector.perp();
      cout <<"Det Id = "<<waferGeom->getID()<<"\tcv\t:"<<centerVector<<"\tphi:\t"<<phi<<"\tr:\t"<<r<<"\tz:\t" << centerVector.z() << endl;
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(centerVector.z());
      pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
      pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
      sprintf(name, "Svt/Layer_%d/Ladder_%d/Wafers", layer, ladder);
      StiDetector *pLadder = _detectorFactory->getInstance();
      pLadder->setName(name);
      pLadder->setIsOn(true);
      pLadder->setIsActive(new StiIsSvtActiveFunctor(svtBarrel,svtLadder, nWafers, _geometry->getWaferWidth(), _geometry->getWaferLength(), _active));
      pLadder->setIsContinuousMedium(true);
      pLadder->setIsDiscreteScatterer(true);
      pLadder->setGas(_gasMat);
      pLadder->setMaterial(_siMat);
      pLadder->setShape(_waferShape[layer]);
      pLadder->setPlacement(pPlacement); 
      pLadder->setHitErrorCalculator(StiSvtHitErrorCalculator::instance());
      pLadder->setKey(1,layer);
      pLadder->setKey(2,ladder);
//      pLadder->setElossCalculator(siElossCalculator);
      add(layer,ladder,pLadder);
    } // for ladder
  } // for layer
  if (StiVMCToolKit::GetVMC()) {useVMCGeometry();}
  if (debug()) {
    cout << "StiSvtDetectorBuilder::buildDetectors list of built detectors" << endl;
    Int_t nlayers = _detectors.size(); 
    for (Int_t layer = 0; layer < nlayers; layer++) {
      Int_t nLadders = _detectors[layer].size();
      for (Int_t ladder = 0; ladder < nLadders; ladder++) {
	cout << "layer " << layer << "\tladder = " << ladder << "\t" << _detectors[layer][ladder]->getName() << endl;
      }
    }
  }
}
//________________________________________________________________________________
void StiSvtDetectorBuilder::useVMCGeometry() {
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  struct Material_t {
    const Char_t *name;
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
//     {"SLYD", "layer mother","HALL_1/CAVE_1/SVTT_1/SLYD_1/*","",""},
//     {"SLY1", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY1_2/*","",""},
//     {"SLY2", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY2_3/*","",""},
//     {"SLY3", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY3_4/*","",""},
//     {"SLY4", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY4_5/*","",""},
//     {"SLY5", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY5_6/*","",""},
    {"SELE","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_1/SLSD_1/SELE_1","",""},//	Weight = 0.0422223[kG]
#if 1
    {"SEL1","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_2/SLSD_1/SELE_1","",""},//	Weight = 0.0422223[kG]
    {"SEL2","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_3/SLSD_1/SELE_1","",""},//	Weight = 0.048099[kG]
    {"SEL3","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_4/SLSD_1/SELE_1","",""},//	Weight = 0.048099[kG]
    {"SEL4","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_5/SLSD_1/SELE_1","",""},//	Weight = 0.0510373[kG]
    {"SEL5","electronics mother volume","HALL_1/CAVE_1/SVTT_1/SLYD_6/SLSD_1/SELE_1","",""},//	Weight = 0.0510373[kG]
#endif
    //  {"SVTD", "an active wafer volume","HALL_1/CAVE_1/SVTT_1/SLY*/SLS*/SLD*/STL*/STS*/SVTD_1","svt","SVTD"}, // <+++
    {"SBWC", "water manifold to support cone bracket mother","HALL_1/CAVE_1/SVTT_1/SBWC_1-2/*","",""},
    {"SWMM", "water manifold mother","HALL_1/CAVE_1/SVTT_1/SWMM_1-2/*","",""},
    {"SIES", "Volume to hold inner endring screws","HALL_1/CAVE_1/SVTT_1/SIES_1-2/*","",""},
    {"SOES", "Volume to hold outer endring screws","HALL_1/CAVE_1/SVTT_1/SOES_1-2/*","",""},
    {"SBRG", "Bracket joining the end rungs","HALL_1/CAVE_1/SVTT_1/SBRG_1-2/*","",""},
    {"SOER", "outer end ring","HALL_1/CAVE_1/SVTT_1/SOER_1-2/*","",""},
    {"SIRT", "inner end ring tube piece ","HALL_1/CAVE_1/SVTT_1/SIRT_1-2","",""},
    {"SIRP", "inner end ring polygon piece ","HALL_1/CAVE_1/SVTT_1/SIRP_1-2","",""}
    //?    {"STAC", "twinax cable approximation, copper","HALL_1/CAVE_1/SVTT_1/SCON_1/STAC_1-2","",""} 
    // StiDetectorVolume	SVTT_1_SCON_1_STAC_1	StiDetector OBJ: TTUBE	STAC	StiCylindricalShape Rmin 21.935 Rmax 37.77 dz 86.9 
  };
  Int_t NoSvtVols = sizeof(SvtVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  TString path("");
  for (Int_t i = 0; i < NoSvtVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;
    StiVMCToolKit::LoopOverNodes(nodeT, path, SvtVolumes[i].name, MakeAverageVolume);
  }
}
