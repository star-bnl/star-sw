#include <stdexcept>
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StiSvt/StiSvtDetectorBuilder.h" 
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
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
#include "Sti/StiHitErrorCalculator.h"

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
StiSvtDetectorBuilder::StiSvtDetectorBuilder(bool active)
  : StiDetectorBuilder("Svt",active)
{
  _calc = new StiDefaultHitErrorCalculator();
  _calc->set(0.25,0.,0.,0.25,0.,0.);  
  StiTrackingParameters * trackingPars = getTrackingParameters();
  trackingPars->setMaxChi2ForSelection(5.);
  trackingPars->setMinSearchWindow(1.);
  trackingPars->setMaxSearchWindow(2.);
  trackingPars->setSearchWindowScaling(4.);
}

StiSvtDetectorBuilder::~StiSvtDetectorBuilder()
{
  delete _calc;

}

void StiSvtDetectorBuilder::buildMaterials()
{
  _messenger << "StiSvtDetectorBuilder::buildMaterials() - INFO - Started" << endl;
  _gasMat    = add(new StiMaterial("Air",     0.49919,  1.,       0.001205, 30420.*0.001205, 5.) );
  _siMat     = add(new StiMaterial("Si",     14.,      28.0855,   2.33,     21.82,           5.) );
  _hybridMat = add(new StiMaterial("Hybrid", 14.,      28.0855,   2.33,     21.82,           5.) );
  _messenger << "StiSvtDetectorBuilder::buildMaterials() - INFO - Done" << endl;
}

void StiSvtDetectorBuilder::buildShapes()
{
  char name[50];  
  int nLayers = getNRows();
  for(int layer = 0; layer<nLayers; layer++)
    {
      int nWafers = _config->getNumberOfWafers(1+layer/2);
      cout << " layer:"<<layer<<" nWafrers:"<<nWafers<<endl;
      // Si wafer
      sprintf(name, "Svt/Layer_%d/Wafers", layer);

      cout <<"StiSvtDetectorBuilder::buildShapes() -I- layer:"<<layer<<"thickness:"<< _geometry->getWaferThickness()<<endl;
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
}

void StiSvtDetectorBuilder::buildDetectors()
{
  char name[50];  
  cout << "StiSvtDetectorBuilder::buildDetectors() - INFO - Starting"<<endl;

  cout << "SVT 0:"<< _geometry->getBarrelRadius(0)<<endl;
  cout << "SVT 1:"<< _geometry->getBarrelRadius(1)<<endl;
  cout << "SVT 2:"<< _geometry->getBarrelRadius(2)<<endl;
  cout << "SVT 3:"<< _geometry->getBarrelRadius(3)<<endl;
  cout << "SVT 4:"<< _geometry->getBarrelRadius(4)<<endl;
  cout << "SVT 5:"<< _geometry->getBarrelRadius(5)<<endl;
  cout << "SVT 6:"<< _geometry->getBarrelRadius(6)<<endl;


  for(unsigned int layer = 0; layer<getNRows(); layer++)
    {
      // calculate generic params for this layer
      // number of ladders per layer (not barrel) & phi increment between ladders
      float fDeltaPhi = M_PI/getNSectors(layer);
      // width of gap between the edges of 2 adjacent ladders:
      //   first, the angle subtended by 1/2 of the ladder
      float fLadderRadius  = _geometry->getBarrelRadius(layer+1);
      cout << "SVT layer:"<<layer<<" Radius "<< fLadderRadius<<endl;
      if (fLadderRadius<=0)
	throw runtime_error("StiSvtDetectorBuilder::buildDetectors() - FATAL - fLadderRadius<=0");
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
      for(unsigned int ladder = 0; ladder<getNSectors(layer); ladder++)
	{
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  pPlacement->setLayerRadius(fLayerRadius);
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  float fLadderPhi = phiForSvtBarrelLadder(layer, ladder);
	  pPlacement->setCenterRep(fLadderPhi, fLadderRadius, 0.); 
	  sprintf(name, "Svt/Layer_%d/Ladder_%d/Wafers", layer, ladder);
	  StiDetector *pLadder = _detectorFactory->getInstance();
	  pLadder->setName(name);
	  pLadder->setIsOn(true);
	  if (_active)
	    pLadder->setIsActive(new StiSvtIsActiveFunctor);
	  else
	    pLadder->setIsActive(new StiNeverActiveFunctor);
	  pLadder->setIsContinuousMedium(true);
	  pLadder->setIsDiscreteScatterer(true);
	  pLadder->setGas(_gasMat);
	  pLadder->setMaterial(_siMat);
	  pLadder->setShape(_waferShape[layer]);
	  pLadder->setPlacement(pPlacement); 
	  pLadder->setHitErrorCalculator(_calc);
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

void StiSvtDetectorBuilder::loadDb()
{
  _transform = new StSvtCoordinateTransform;
  if (!_transform)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - Null _transform");
  if (!gStSvtDbMaker)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - gStSvtDbMaker==0");
  St_SvtDb_Reader *pSvtDb_Reader = gStSvtDbMaker->get_SvtDb_Reader();
  if (!pSvtDb_Reader)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - pSvtDb_Reader==0");
  _config = pSvtDb_Reader->getConfiguration();
  if (!_config)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - _config_Reader==0");
  _geometry = pSvtDb_Reader->getGeometry();
  if (!_geometry)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - _geometry==0");
  StSvtHybridCollection *pDriftVelocity = pSvtDb_Reader->getDriftVelocity();
  if (!pDriftVelocity)
    throw runtime_error("StiSvtDetectorBuilder::loadDb() - ERROR - _pDriftVelocity==0");
  _transform->setParamPointers(_geometry, _config,pDriftVelocity);
  setNRows(2* _config->getNumberOfBarrels());
  cout << "SVT N Rows:"<<2* _config->getNumberOfBarrels()<<endl;
  for (unsigned int layer=0;layer<getNRows();layer++)
    {
      cout << "SVT Layer"<<layer<<" N Ladders:"<<_config->getNumberOfLadders(layer/2)/2<<endl;
      setNSectors(layer, _config->getNumberOfLadders(1+layer/2)/2);
    }
}


double StiSvtDetectorBuilder::phiForSvtBarrelLadder(unsigned int layer, 
						    unsigned int ladder) const
{
  double angle;
  switch (layer)
    {
    case 0:
      switch (ladder)
	{
	case 0: angle =    0.; break;
	case 1: angle =  270.; break;
	case 2: angle =  180.; break;
	case 3: angle =   90.; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    case 1:
      switch (ladder)
	{
	case 0: angle =   45.; break;
	case 1: angle =  315.; break;
	case 2: angle =  225.; break;
	case 3: angle =  135.; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    case 2:
      switch (ladder)
	{
	case 0: angle =   30.; break;
	case 1: angle =  330.; break;
	case 2: angle =  270.; break;
	case 3: angle =  210.; break;
	case 4: angle =  150.; break;
	case 5: angle =   90.; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    case 3:
      switch (ladder)
	{
	case 0: angle =   60.; break;
	case 1: angle =    0.; break;
	case 2: angle =  300.; break;
	case 3: angle =  240.; break;
	case 4: angle =  180.; break;
	case 5: angle =  120.; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    case 4:
      switch (ladder)
	{
	case 0: angle =   45.; break;
	case 1: angle =    0.; break;
	case 2: angle =  315.; break;
	case 3: angle =  270.; break;
	case 4: angle =  225.; break;
	case 5: angle =  180.; break;
	case 6: angle =  135.; break;
	case 7: angle =   90.; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    case 5:
      switch (ladder)
	{
	case 0: angle =   67.5; break;
	case 1: angle =   22.5; break;
	case 2: angle =  337.5; break;
	case 3: angle =  292.5; break;
	case 4: angle =  247.5; break;
	case 5: angle =  202.5; break;
	case 6: angle =  157.5; break;
	case 7: angle =  112.5; break;
	default: throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
	};
      break;
    default:   throw runtime_error("StiSvtDetectorBuilder::phiForSvtBarrelLadder() -E- Arg out of bound");
    }
  cout << "  Layer:"<<layer<<" LADDER:"<<ladder<<" ANGLE PHI:"<< angle*M_PI/180.;
  return  angle*M_PI/180.;
} // phiForSvtLayerLadder
