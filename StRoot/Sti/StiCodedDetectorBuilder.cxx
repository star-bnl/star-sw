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
#include "tables/St_svg_config_Table.h"
#include "tables/St_svg_shape_Table.h"

//Sti
#include "StiGeometryTransform.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiMaterial.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "Messenger.h"

#include "StiCodedDetectorBuilder.h"

StiCodedDetectorBuilder::StiCodedDetectorBuilder(){
  init();
  m_messenger << 
      "################################StiCodedDetectorBuilder()" << endl;
}

StiCodedDetectorBuilder::~StiCodedDetectorBuilder(){
  m_messenger << 
      "################################~StiCodedDetectorBuilder()" << endl;
}

void StiCodedDetectorBuilder::init(){

  buildMaterials();
  buildShapes();
  buildDetectors();

  mDetectorIterator = mDetectorMap.begin();

} // init()



// build material objects and add them to map by name
void StiCodedDetectorBuilder::buildMaterials(){
  
  int nMaterials = 8;
  char* aNames[] = {"Air", "Al", "Hybrid", "IfcAdhesive", 
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
  char szName[100];

  //---------------------------------------------------------------------------
  // tpc
  //---------------------------------------------------------------------------
  StTpcPadPlaneI *pPadPlane = gStTpcDb->PadPlaneGeometry();
  StTpcDimensionsI *pDimensions = gStTpcDb->Dimensions();

  int nPadrows = pPadPlane->numberOfRows();
  int nInnerPadrows = pPadPlane->numberOfInnerRows();
  for(int iPadrow = 0; iPadrow<nPadrows; iPadrow++){
    StiPlanarShape *pShape = new StiPlanarShape;
    
    if(iPadrow<nInnerPadrows){
      pShape->setThickness(pPadPlane->innerSectorPadLength());
    }else{
      pShape->setThickness(pPadPlane->outerSectorPadLength());
    }
          
    pShape->setHalfDepth(pDimensions->tpcTotalLength()/2.);

    pShape->setHalfWidth(pPadPlane->PadPitchAtRow(iPadrow + 1) *
                         pPadPlane->numberOfPadsAtRow(iPadrow + 1) / 2.);

    sprintf(szName, "Tpc/Padrow_%d", iPadrow + 1);

    //string theName = "Tpc/Padrow_";
    //stringstream stemp("");
    //stemp << iPadrow + 1;
    //theName += stemp.str();
    //NameMapKey key(theName);    
    //m_messenger <<"szName: "<<szName<<"\ttheName: "<<theName<<endl;
    
    NameMapKey key(szName);
    mShapeMap.insert( shapeMapValType(key, pShape) );

  } // for iPadrow
  
  //---------------------------------------------------------------------------
  // svg (svt + ssd)
  //---------------------------------------------------------------------------
  
  // get the configuration & geometry tables
  StiGeometryTransform *pGeometryTransform = StiGeometryTransform::instance();
  svg_config_st svgConfig = pGeometryTransform->getSvgConfig();
  svg_shape_st *pSvgShape = pGeometryTransform->getSvgShape();

  // extract the wafer shape parameters by looking up the shape code used by
  // the first wafer (assume SVT) and last wafer (assume SSD).
  svg_shape_st svtWaferShape = pSvgShape[ 0 ];
  svg_shape_st ssdWaferShape = pSvgShape[ 1 ];

  Int_t nLayers = svgConfig.n_layer; // last layer is ssd
  for(Int_t iLayer = 0; iLayer<nLayers; iLayer++){
    StiPlanarShape *pShape = new StiPlanarShape;

    Int_t nWafers = svgConfig.n_wafer[iLayer];

    if (iLayer < nLayers - 1){ // svt
      pShape->setHalfWidth(svtWaferShape.shape[0]);
      pShape->setHalfDepth(svtWaferShape.shape[1] * nWafers);
      pShape->setThickness(2.*svtWaferShape.shape[2]);
    } else { // ssd
      pShape->setHalfWidth(ssdWaferShape.shape[0]);
      pShape->setHalfDepth(ssdWaferShape.shape[1] * nWafers);
      pShape->setThickness(2.*ssdWaferShape.shape[2]);
    }

    sprintf(szName, "Svg/Layer_%d/Ladder", iLayer + 1);
    NameMapKey key(szName);
    mShapeMap.insert( shapeMapValType(key, pShape) );

    // now do hybrids
    if(iLayer == nLayers - 1){ continue; }

    pShape = new StiPlanarShape( ssdWaferShape.shape[1] * nWafers, 0.1, 1.);
    sprintf(szName, "Svg/Layer_%d/Hybrid", iLayer + 1);
    NameMapKey key2(szName);
    mShapeMap.insert( shapeMapValType(key2, pShape) );

  } // for iLayer
  
  //---------------------------------------------------------------------------
  // ifc
  //---------------------------------------------------------------------------

  // we build the Ifc in 12 sections, each of which spans a 30 degree arc
  // in xy and runs the whole length of the IFC.  This makes for easy
  // (we hope) transition from padrow 1.

  StiCylindricalShape *pShape = new StiCylindricalShape;

  pShape->setThickness(1.27); 
  pShape->setHalfDepth( pDimensions->tpcTotalLength()/2. );
  pShape->setOpeningAngle( M_PI/6. );
  pShape->setOuterRadius(pDimensions->ifcRadius() + pShape->getThickness()/2.);

  NameMapKey key("Ifc/Nomex");
  mShapeMap.insert( shapeMapValType(key, pShape) );

} // buildShapes()

void StiCodedDetectorBuilder::buildDetectors(){

  char szName[100];

  //---------------------------------------------------------------------------
  // tpc
  //---------------------------------------------------------------------------

  StiGeometryTransform *pGeometryTransform = StiGeometryTransform::instance();
  StTpcPadPlaneI *pPadPlane = gStTpcDb->PadPlaneGeometry();

  // create detector properties common to all TPC
  StiMaterial *pGas = findMaterial("P10");

  int nPadrows = pPadPlane->numberOfRows();
  for(int iPadrow = 0; iPadrow<nPadrows; iPadrow++){

    // create properties shared by all sectors in this padrow
    float fRadius = pPadPlane->radialDistanceAtRow(iPadrow + 1);
    sprintf(szName, "Tpc/Padrow_%d", iPadrow + 1);
    StiPlanarShape *pShape = (StiPlanarShape *)findShape(szName);

    for(int iSector = 0; iSector<12; iSector++){

      // create unique detector properties (placement & name)
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setNormalRep(
          pGeometryTransform->phiForSector(iSector + 1, 12), fRadius, 0.); 

      sprintf(szName, "Tpc/Padrow_%d/Sector_%d", iPadrow + 1, iSector + 1);

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
  // svg (svt + ssd)
  //---------------------------------------------------------------------------
  
  // get the configuration & geometry tables
  svg_config_st svgConfig = pGeometryTransform->getSvgConfig();

  pGas = findMaterial("Air");
  StiMaterial *pLadderMaterial = findMaterial("Si");
  StiMaterial *pHybridMaterial = findMaterial("Hybrid");

  int nLayers = svgConfig.n_layer; // last layer is ssd
  for(int iLayer = 0; iLayer<nLayers; iLayer++){
 
    //-----------------------------------------
    // calculate generic params for this layer
 
    sprintf(szName, "Svg/Layer_%d/Ladder", iLayer + 1);
    StiPlanarShape *pLadderShape = (StiPlanarShape *)findShape(szName);
    sprintf(szName, "Svg/Layer_%d/Hybrid", iLayer + 1);
    StiPlanarShape *pHybridShape = (StiPlanarShape *)findShape(szName);

    // phi increment between ladders
    int nLadders = svgConfig.n_ladder[iLayer];
    float fDeltaPhi = 2.*M_PI/nLadders;

    // width of gap between the edges of 2 adjacent ladders:
    //   first, the angle subtended by 1/2 of the ladder
    float fLadderRadius = svgConfig.layer_radius[iLayer];
    float fHalfLadderPhi = atan(pLadderShape->getHalfWidth()/fLadderRadius);
    float fHalfGapPhi = fDeltaPhi/2. - fHalfLadderPhi;    

    //   then the distance from the origin to the gap center
    //     != the layer radius bc the layer width differs from the gap width,
    //     i.e. not a regular polygon.
    float fGapRadius = fLadderRadius/cos(fHalfLadderPhi)*cos(fHalfGapPhi);
    //   finally half the gap distance
    float fHalfGap = fGapRadius*tan(fHalfGapPhi);

    for(Int_t iLadder = 0; iLadder<nLadders; iLadder++){

      // ladder
      StiPlacement *pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      float fLadderPhi = pGeometryTransform->phiForSector(
          2*(iLadder + 1) - iLayer%2, 2*nLadders);

      if(iLayer < nLayers - 1){
        pPlacement->setCenterRep(fLadderPhi, fLadderRadius, 0.); 
      }else{ // svt ladders have slight tilt
        pPlacement->setCenterRep(fLadderPhi, fLadderRadius, 0.09); 
      }
      sprintf(szName, "Svg/Layer_%d/Ladder_%d/Ladder", 
              iLayer + 1, iLadder + 1);

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

      if(iLayer == nLayers - 1){ continue; }

      // hybrid 1
      pPlacement = new StiPlacement;
      pPlacement->setZcenter(0.);
      pPlacement->setNormalRep(fLadderPhi + fDeltaPhi/2., fGapRadius,
                               pHybridShape->getHalfWidth() - fHalfGap);
      
      sprintf(szName, "Svg/Layer_%d/Ladder_%d/Hybrid_1", 
              iLayer + 1, iLadder + 1);

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
      pPlacement->setNormalRep(fLadderPhi - fDeltaPhi/2., fGapRadius,
                               fHalfGap - pHybridShape->getHalfWidth());
      
      sprintf(szName, "Svg/Layer_%d/Ladder_%d/Hybrid_2", 
              iLayer + 1, iLadder + 1);

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
    pPlacement->setNormalRep(pGeometryTransform->phiForSector(iSector, 12),
                             fIfcRadius, 0.);
    
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

