//Std
#include <iostream.h>
#include <float.h>
using std::cout;
using std::endl;
#include <string>
using std::string;
//#include <sstream>
//using std::stringstream;

//StDb
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"
#include "StTpcDb/StTpcDimensionsI.h"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSvtDbMaker/St_SvtDb_Reader.hh"

//Sti
#include "StiCoordinateTransform.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiMaterial.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "Messenger.h"
#include "StiToolkit.h"

#include "StiCodedDetectorBuilder.h"

StiCodedDetectorBuilder::StiCodedDetectorBuilder(){
    cout <<"StiCodedDetectorBuilder::StiCodedDetectorBuilder()"<<endl;

    m_pCoordinateTransform = StiToolkit::instance()->getCoordinateTransform();
    init();

    m_messenger << 
	"################################StiCodedDetectorBuilder()" << endl;
}

StiCodedDetectorBuilder::~StiCodedDetectorBuilder(){
  m_messenger << 
      "################################~StiCodedDetectorBuilder()" << endl;
}

void StiCodedDetectorBuilder::init(){
    
    cout <<"StiCodedDetectorBuilder::init()"<<endl;
    cout <<"Build Materials"<<endl;
    buildMaterials();
    cout <<"Done building materials"<<endl;
    cout <<"Build Shapes"<<endl;
    buildShapes();
    cout <<"Done Building shapes"<<endl;
    cout <<"Build Detector"<<endl;
    buildDetectors();
    cout <<"Done Building Detectors"<<endl;
    
    mDetectorIterator = mDetectorMap.begin();
  
} // init()



// build material objects and add them to map by name
void StiCodedDetectorBuilder::buildMaterials(){
  
  int nMaterials = 8;
  const char* aNames[] = {"Air", "Al", "Hybrid", "IfcAdhesive", 
                          "Nomex", "P10", "Si", "Vacuum"};
  float aRadLengths[] = {30420., 8.9, 8.3, 33.3, 625., 12850., 9.36, DBL_MAX};
  float aDensities[] = {.00119, 2.7, 2.5, 1.2, .064, .00156, 2.33, 0.};

  for(int iMaterial = 0; iMaterial<nMaterials; iMaterial++){
    StiMaterial *pMaterial = new StiMaterial();
    pMaterial->setName(aNames[iMaterial]);
    pMaterial->setRadLength(aRadLengths[iMaterial]);
    pMaterial->setDensity(aDensities[iMaterial]);
    
    NameMapKey key(pMaterial->getName());
    mMaterialMap.insert( materialMapValType(key, pMaterial) );

  }

}

// builds all the unique shapes needed to describe our detectors, then
// adds them to map by name
void StiCodedDetectorBuilder::buildShapes()
{
    m_messenger <<"StiCodedDetectorBuilder::buildShapes()"<<endl;
    cout <<"StiCodedDetectorBuilder::buildShapes()"<<endl;
    char szName[100];
    
    //---------------------------------------------------------------------------
    // tpc
    //---------------------------------------------------------------------------
    cout <<"Build Tpc Shapes"<<endl;
    if (!gStTpcDb) {
	cout <<"StiCodedDetectorBuilder::buildShapes().  ERROR:\t"
	     <<"null gStTpcDb.  Abort"<<endl;
	return;
    }
    
    StTpcPadPlaneI *pPadPlane = gStTpcDb->PadPlaneGeometry();
    StTpcDimensionsI *pDimensions = gStTpcDb->Dimensions();
	
    int nPadrows = pPadPlane->numberOfRows();
    int nInnerPadrows = pPadPlane->numberOfInnerRows();
    for(int iPadrow = 1; iPadrow<=nPadrows; iPadrow++){
	StiPlanarShape *pShape = new StiPlanarShape;
    
    if(iPadrow<nInnerPadrows){
      pShape->setThickness(pPadPlane->innerSectorPadLength());
    }else{
      pShape->setThickness(pPadPlane->outerSectorPadLength());
    }
          
    pShape->setHalfDepth(pDimensions->tpcTotalLength()/2.);

    pShape->setHalfWidth(pPadPlane->PadPitchAtRow(iPadrow) *
                         pPadPlane->numberOfPadsAtRow(iPadrow) / 2.);

    sprintf(szName, "Tpc/Padrow_%d", iPadrow);
    NameMapKey key(szName);
    mShapeMap.insert( shapeMapValType(key, pShape) );

  } // for iPadrow
    cout <<"Done Building Tpc Shapes"<<endl;
  
  //---------------------------------------------------------------------------
  // svt
  //---------------------------------------------------------------------------
    cout <<"Build Svt Shapes"<<endl;
  // get the configuration & geometry tables
  StSvtConfig *pSvtConfig = 
      gStSvtDbMaker->get_SvtDb_Reader()->getConfiguration();
  StSvtGeometry *pSvtGeometry = gStSvtDbMaker->get_SvtDb_Reader()->getGeometry();

  Int_t nLayers = 2*pSvtConfig->getNumberOfBarrels();
  for(Int_t iLayer = 1; iLayer<=nLayers; iLayer++){
    StiPlanarShape *pShape = new StiPlanarShape;

    Int_t nWafers = pSvtConfig->getNumberOfWafers((iLayer + 1)/2);

    pShape->setHalfWidth(pSvtGeometry->getWaferWidth());
    pShape->setHalfDepth(nWafers*pSvtGeometry->getWaferLength());
    pShape->setThickness(2.*pSvtGeometry->getWaferThickness());

    sprintf(szName, "Svt/Layer_%d/Wafers", iLayer);
    NameMapKey key(szName);
    mShapeMap.insert( shapeMapValType(key, pShape) );

    // now do hybrids

    pShape = new StiPlanarShape(nWafers*pSvtGeometry->getWaferLength(),
                                0.1, 1.);
    sprintf(szName, "Svt/Layer_%d/Hybrids", iLayer);
    NameMapKey key2(szName);
    mShapeMap.insert( shapeMapValType(key2, pShape) );

  } // for iLayer
  cout <<"Done Building Svt Shapes"<<endl;
  //---------------------------------------------------------------------------
  // ifc
  //---------------------------------------------------------------------------

  // we build the Ifc in 12 sections, each of which spans a 30 degree arc
  // in xy and runs the whole length of the IFC.  This makes for easy
  // (we hope) transition from padrow 1.

  cout <<"Build IFC Shapes"<<endl;
  StiCylindricalShape *pShape = new StiCylindricalShape;

  pShape->setThickness(1.27); 
  pShape->setHalfDepth( pDimensions->tpcTotalLength()/2. );
  pShape->setOpeningAngle( M_PI/6. );
  pShape->setOuterRadius(pDimensions->ifcRadius() + pShape->getThickness()/2.);

  NameMapKey key("Ifc/Nomex");
  mShapeMap.insert( shapeMapValType(key, pShape) );

  cout <<"Done building IFC Shapes"<<endl;
} // buildShapes()

void StiCodedDetectorBuilder::buildDetectors(){

  char szName[100];

  //---------------------------------------------------------------------------
  // tpc
  //---------------------------------------------------------------------------

  StTpcPadPlaneI *pPadPlane = gStTpcDb->PadPlaneGeometry();

  // create detector properties common to all TPC
  StiMaterial *pGas = findMaterial("P10");

  int nPadrows = pPadPlane->numberOfRows();
  for(int iPadrow = 1; iPadrow<=nPadrows; iPadrow++){

    // create properties shared by all sectors in this padrow
    float fRadius = pPadPlane->radialDistanceAtRow(iPadrow);
    sprintf(szName, "Tpc/Padrow_%d", iPadrow);
    StiPlanarShape *pShape = (StiPlanarShape *)findShape(szName);

    for(int iSector = 1; iSector<=12; iSector++){

      // create unique detector properties (placement & name)
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setNormalRep(
          m_pCoordinateTransform->phiForTpcSector(iSector), fRadius, 0.); 
      pPlacement->setLayerRadius(fRadius);

      sprintf(szName, "Tpc/Padrow_%d/Sector_%d", iPadrow, iSector);

      // now fill in the detector object and save it in our vector
      StiDetector *pDetector = new StiDetector;

      pDetector->setName(szName);
      
      pDetector->setIsOn(true);
      pDetector->setIsActive(true);
      pDetector->setIsContinuousMedium(true);
      pDetector->setIsDiscreteScatterer(false);

      pDetector->setMaterial(pGas);
      pDetector->setGas(pGas);

      pDetector->setShape(pShape);
      pDetector->setPlacement(pPlacement);

      NameMapKey key(szName);
      mDetectorMap.insert( detectorMapValType(key, pDetector) );

    }// for iSector

  }// for iPadrow

  //---------------------------------------------------------------------------
  // svt
  //---------------------------------------------------------------------------
  
  // get the configuration & geometry tables
  StSvtConfig *pSvtConfig = 
      gStSvtDbMaker->get_SvtDb_Reader()->getConfiguration();
  StSvtGeometry *pSvtGeometry = gStSvtDbMaker->get_SvtDb_Reader()->getGeometry();

  pGas = findMaterial("Air");
  StiMaterial *pLadderMaterial = findMaterial("Si");
  StiMaterial *pHybridMaterial = findMaterial("Hybrid");

  int nLayers = 2*pSvtConfig->getNumberOfBarrels();
  for(int iLayer = 1; iLayer<=nLayers; iLayer++){
    int iBarrel = (iLayer + 1)/2;

    //-----------------------------------------
    // calculate generic params for this layer
 
    sprintf(szName, "Svt/Layer_%d/Wafers", iLayer);
    StiPlanarShape *pLadderShape = (StiPlanarShape *)findShape(szName);
    sprintf(szName, "Svt/Layer_%d/Hybrids", iLayer);
    StiPlanarShape *pHybridShape = (StiPlanarShape *)findShape(szName);

    // number of ladders per layer (not barrel) & phi increment between ladders
    int nLadders = pSvtConfig->getNumberOfLadders((iLayer + 1)/2)/2;
    float fDeltaPhi = 2.*M_PI/nLadders;

    // width of gap between the edges of 2 adjacent ladders:
    //   first, the angle subtended by 1/2 of the ladder
    float fLadderRadius = pSvtGeometry->getBarrelRadius(iLayer);
    float fHalfLadderPhi = atan(pLadderShape->getHalfWidth()/fLadderRadius);
    float fHalfGapPhi = fDeltaPhi/2. - fHalfLadderPhi;    

    //   then the distance from the origin to the gap center
    //     != the layer radius bc the layer width differs from the gap width,
    //     i.e. not a regular polygon.
    float fGapRadius = fLadderRadius/cos(fHalfLadderPhi)*cos(fHalfGapPhi);
    //   finally half the gap distance
    float fHalfGap = fGapRadius*tan(fHalfGapPhi);

    // determine the radius for the detector (ladders + hybrids)
    float fLayerRadius = (fLadderRadius + fGapRadius)/2.;

    for(Int_t iLadder = 1; iLadder<=nLadders; iLadder++){

      // formal ladder number within barrel (odd or even, depending on layer)
      int jLadder = 2*(iLadder) - (iLayer + 1)%2; // count by 2s in svt

      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setLayerRadius(fLayerRadius);
      float fLadderPhi = 
          m_pCoordinateTransform->phiForSvtBarrelLadder(iBarrel, jLadder);
      pPlacement->setCenterRep(fLadderPhi, fLadderRadius, 0.); 
      sprintf(szName, "Svt/Layer_%d/Ladder_%d/Wafers", iLayer, jLadder);

      StiDetector *pLadder = new StiDetector;
      pLadder->setName(szName);

      pLadder->setIsOn(true);
      pLadder->setIsActive(true);
      pLadder->setIsContinuousMedium(true);
      pLadder->setIsDiscreteScatterer(true);

      pLadder->setGas(pGas);
      pLadder->setMaterial(pLadderMaterial);

      pLadder->setShape(pLadderShape);
      pLadder->setPlacement(pPlacement);

      NameMapKey key(szName);
      mDetectorMap.insert( detectorMapValType(key, pLadder) );

      // hybrid 1
      pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setLayerRadius(fLayerRadius);
      pPlacement->setNormalRep(fLadderPhi + fDeltaPhi/2., fGapRadius,
                               pHybridShape->getHalfWidth() - fHalfGap);
      
      sprintf(szName, "Svt/Layer_%d/Ladder_%d/Hybrids_1", iLayer, jLadder);

      StiDetector *pHybrid1 = new StiDetector;
      pHybrid1->setName(szName);
     
      pHybrid1->setIsOn(true);
      pHybrid1->setIsActive(false);
      pHybrid1->setIsContinuousMedium(true);
      pHybrid1->setIsDiscreteScatterer(true);

      pHybrid1->setGas(pGas);
      pHybrid1->setMaterial(pHybridMaterial);

      pHybrid1->setShape(pHybridShape);
      pHybrid1->setPlacement(pPlacement);

      NameMapKey key1(szName);
      mDetectorMap.insert( detectorMapValType(key1, pHybrid1) );

      // hybrid 2

      pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setLayerRadius(fLayerRadius);
      pPlacement->setNormalRep(fLadderPhi - fDeltaPhi/2., fGapRadius,
                               fHalfGap - pHybridShape->getHalfWidth());
      
      sprintf(szName, "Svt/Layer_%d/Ladder_%d/Hybrids_2", iLayer, jLadder);

      StiDetector *pHybrid2 = new StiDetector;
      pHybrid2->copy(*pHybrid1);
      pHybrid2->setName(szName);
      pHybrid2->setPlacement(pPlacement);

      NameMapKey key2(szName);
      mDetectorMap.insert( detectorMapValType(key2, pHybrid2) );
    } // for iLadder
  } // for iLayer
      
  //---------------------------------------------------------------------------
  // ifc
  //---------------------------------------------------------------------------

  // we build the Ifc in 12 sections, each of which spans a 30 degree arc
  // in xy and runs the whole length of the IFC.  This makes for easy
  // (we hope) transition from padrow 1.

  float fIfcRadius = gStTpcDb->Dimensions()->ifcRadius();   
  
  pGas = findMaterial("P10");
  StiMaterial *pMaterial = findMaterial("Nomex");
  StiShape *pShape = findShape("Ifc/Nomex");

  for(int iSector = 1; iSector<=12; iSector++){

    StiPlacement *pPlacement = new StiPlacement;
    pPlacement->setZcenter(0.);
    pPlacement->setLayerRadius(fIfcRadius);
    pPlacement->setNormalRep(
        m_pCoordinateTransform->phiForTpcSector(iSector), fIfcRadius, 0.);
    
    sprintf(szName, "Ifc/Sector_%d", iSector);
    
    StiDetector *pDetector = new StiDetector;
    pDetector->setName(szName);
    
    pDetector->setIsOn(true);
    pDetector->setIsActive(false);
    pDetector->setIsContinuousMedium(false);
    pDetector->setIsDiscreteScatterer(true);
    
    pDetector->setShape(pShape);
    pDetector->setPlacement(pPlacement);
    
    pDetector->setGas(pGas);
    pDetector->setMaterial(pMaterial);
    
    NameMapKey key(szName);
    mDetectorMap.insert( detectorMapValType(key, pDetector) );
    
  } // for iSector

} // buildDetectors()
