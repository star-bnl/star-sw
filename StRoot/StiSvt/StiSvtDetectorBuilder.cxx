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
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiSvt/StiSvtIsActiveFunctor.h"
#include <stdio.h>
#include "tables/St_HitError_Table.h"

/*
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
  : StiDetectorBuilder("Svt",active,inputFile)
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
  cout << "SVT Number of  Rows : "<<2* _config->getNumberOfBarrels()<<endl
			 << "  Layer#  numberOfLadders      Radius" << endl;
  for (unsigned int layer=0;layer<nRows;layer++)
		cout << "  "<<layer<<"     "<<_config->getNumberOfLadders(1+layer/2)/2 << "   " 
				 << _geometry->getBarrelRadius(layer+1) << endl;

  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Materials" << endl;
  _gasMat    = add(new StiMaterial("Air",     0.49919,  1.,       0.001205, 30420.*0.001205, 5.) );
  _siMat     = add(new StiMaterial("Si",     14.,      28.0855,   2.33,     21.82,           5.) );
  _hybridMat = add(new StiMaterial("Hybrid", 14.,      28.0855,   2.33,     21.82,           5.) );
  cout << "StiSvtDetectorBuilder::buildDetectors() -I- Define Svt Shapes" << endl;
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
      // Hybrids
      sprintf(name, "Svt/Layer_%d/Hybrids", layer);
      _hybridShape[layer] = new StiPlanarShape(name,nWafers*_geometry->getWaferLength(),0.1, 1.);
      add(_hybridShape[layer]);
    } // for layer

	for (unsigned int layer=0;layer<nRows;layer++)
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
      float fHalfGap = fGapRadius*tan(fHalfGapPhi);
      // determine the radius for the detector (ladders + hybrids)
      float fLayerRadius = (fLadderRadius + fGapRadius)/2.;

			double x,y,z,rc,rn, nx,ny,nz,dx,dy,dz,yOff;
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
					dx = waferGeom->d(0);
					dy = waferGeom->d(1);
					dz = waferGeom->d(2);
					rc = sqrt(x*x+y*y);
					rn = x*nx+y*ny;
					dPhi = acos((x*nx+y*ny)/rc);
					phiC = fLadderPhi = atan2(y,x);
					phiN = atan2(ny,nx);
					dPhi = phiC-phiN;
					yOff = sqrt(rc*rc-rn*rn);
					StiPlacement *pPlacement = new StiPlacement;
					pPlacement->setZcenter(0.);
					pPlacement->setLayerRadius(fLayerRadius);
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
					add(layer,ladder,pLadder);
					
					double offset = _hybridShape[layer]->getHalfWidth() - fHalfGap;
					// hybrid 1
					pPlacement = new StiPlacement;
					pPlacement->setZcenter(0.);
					pPlacement->setLayerRadius(fLayerRadius);
					pPlacement->setRegion(StiPlacement::kMidRapidity);
					pPlacement->setNormalRep(fLadderPhi + fDeltaPhi, 
																	 fGapRadius,
																	 -offset);
					sprintf(name, "Svt/Layer_%d/Ladder_%d/Hybrids_1", layer, ladder);
					StiDetector *pHybrid1 = _detectorFactory->getInstance();
					pHybrid1->setName(name);
					pHybrid1->setIsOn(true);
					pHybrid1->setIsActive(new StiNeverActiveFunctor);
					pHybrid1->setIsContinuousMedium(true);
					pHybrid1->setIsDiscreteScatterer(true);
					pHybrid1->setGas(_gasMat);
					pHybrid1->setMaterial(_hybridMat);
					pHybrid1->setShape(_hybridShape[layer]);
					pHybrid1->setPlacement(pPlacement);
					add(pHybrid1);
					// hybrid 2
					pPlacement = new StiPlacement;
					pPlacement->setZcenter(0.);
					pPlacement->setLayerRadius(fLayerRadius);
					pPlacement->setRegion(StiPlacement::kMidRapidity);
					pPlacement->setNormalRep(fLadderPhi - fDeltaPhi, 
																	 fGapRadius,
																	 offset);
					sprintf(name, "Svt/Layer_%d/Ladder_%d/Hybrids_2", layer, ladder);
					StiDetector *pHybrid2 = _detectorFactory->getInstance();
					pHybrid2->copy(*pHybrid1);
					pHybrid2->setName(name);
					pHybrid2->setPlacement(pPlacement);
					add(pHybrid2);
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
