//  StiGeometryGenerator.cxx
// M.L. Miller
// 5/00

#include <iostream.h>
#include <math.h>
#include <string>

// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"

// StRoot
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "StiDetector.h"
#include "StiMaterial.h" 
#include "StiGeometryTransform.h"

#include "StiGeometryGenerator.h"

ClassImp(StiGeometryGenerator)
 
StiGeometryGenerator::StiGeometryGenerator(const Char_t *name) : StMaker(name)
{
//  m_szGeomDirectory = "/star/u/bnorman/Detectors";
    //m_szGeomDirectory = "/scr20/ittf/StiGeometryParameters/Detectors";
  m_szGeomDirectory = "/scr20/TempIttf/StiGeometryParameters/Detectors";
  m_polydirectory = "/scr20/TempIttf/StiGeometryParameters/Polygons";
}

StiGeometryGenerator::~StiGeometryGenerator() 
{
}

void StiGeometryGenerator::Clear(const char*)
{
  StMaker::Clear();
}

Int_t StiGeometryGenerator::Finish()
{
  return StMaker::Finish();
}

Int_t StiGeometryGenerator::Init()
{
  return StMaker::Init();
}

Int_t StiGeometryGenerator::Make()
{
  cout <<"\n\n ---------------------- StiGeometryGenerator::Make() ---------------- \n\n"<<endl;

  mpoly_vec.clear();
  
  // load Sti geometry routines
  mGeometryTransform = new StiGeometryTransform();


  // load svt & ssd geometry
  St_DataSetIter local(GetInputDB("svt"));
  m_svg_config = (St_svg_config *)local("svgpars/config");
  m_svg_geom = (St_svg_geom *)local("svgpars/geom");
  m_svg_shape = (St_svg_shape *)local("svgpars/shape");

  cout << "  building SVT && SSD." << endl;
  buildSvg();

  cout << "  building TPC." << endl;
  buildTpc();

  buildPolygons();
  
  cout << "  done." << endl;

  return kStOK;

}

void StiGeometryGenerator::buildPolygons()
{
    char outfile[500];
    for (unsigned int i=0; i<mpoly_vec.size(); ++i) {
	sprintf(outfile, "%s/Layer_%i.txt", m_polydirectory, i);
	mpoly_vec[i].write(outfile);
	cout <<"Writing File:\t"<<outfile<<endl;
		
    }
    return;
}
// warning:  the ladder numbers used here are NOT the same as those shown in
// CSN 229A
void StiGeometryGenerator::buildSvg(){

  // get the configuration & geometry tables
  svg_config_st svgConfig = m_svg_config->GetTable()[0];
  svg_geom_st *pSvgGeom = m_svg_geom->GetTable();
  svg_shape_st *pSvgShape = m_svg_shape->GetTable();

  char szOutfile[500];

  // extract the wafer shape parameters by looking up the shape code used by
  // the first wafer (assume SVT) and last wafer (assume SSD).
  svg_shape_st svtWaferShape = pSvgShape[ pSvgGeom[0].id_shape - 1 ];
  svg_shape_st ssdWaferShape = pSvgShape[ 
      pSvgGeom[m_svg_geom->GetTableSize() - 1].id_shape - 1 ];

  // the smallest item we will write out is a ladder, since that will be our
  // r-phi element, just as we stop at the padrow level, not writing individual
  // pads.

  Int_t nLayers = svgConfig.n_layer; // last layer is ssd
  for(Int_t nLayer = 0; nLayer < nLayers; nLayer++){

    //-----------------------------------------
    // calculate generic params for this layer

    // phi increment between ladders
    Int_t nLadders = svgConfig.n_ladder[nLayer];
    Double_t dDeltaPhi = 2.*M_PI/nLadders;

    // wafer size
    Double_t dWaferHalfWidth, dWaferHalfDepth, dWaferHalfThickness,
        dOrientationAngle;
    if (nLayer < nLayers - 1){ // svt
      dWaferHalfWidth = svtWaferShape.shape[0];
      dWaferHalfDepth = svtWaferShape.shape[1];
      dWaferHalfThickness = svtWaferShape.shape[2];
      dOrientationAngle = 0.;
    } else { // ssd
      dWaferHalfWidth = ssdWaferShape.shape[0];
      dWaferHalfDepth = ssdWaferShape.shape[1];
      dWaferHalfThickness = ssdWaferShape.shape[2];
      dOrientationAngle = 0.09; // or so...
    }
  

    // width of gap between the edges of 2 adjacent ladders:
    //   first, the angle subtended by 1/2 of the ladder
    Double_t dLadderRadius = svgConfig.layer_radius[nLayer];

    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
/*
    Double_t dHalfLadderPhi = asin(dWaferHalfWidth/dLadderRadius);
    Double_t dHalfGapPhi = dDeltaPhi/2. - dHalfLadderPhi;
    //   then the distance from the origin to the gap center
    Double_t dGapRadius = dLadderRadius/cos(dHalfLadderPhi)*cos(dHalfGapPhi);
    //   finally half the gap distance
    Double_t dHalfGap = dGapRadius*tan(dHalfGapPhi);
*/
    //MLM (6/5/01)
    double phi0forpoly=999.;
    
    for(Int_t iLadder = 0; iLadder < nLadders; iLadder++){

      StiDetector detector;
      StiMaterial gas; // dummy for holding gas name
      gas.setName("Air");
      detector.setGas(&gas);
      StiMaterial material;
      material.setName("Si");
      detector.setMaterial(&material);

      // first write out a file for the ladder.
      // we assume pure silicon.

      detector.setIsOn(true);
      detector.setIsActive(true);
      detector.setIsContinuousMedium(false);
      detector.setIsDiscreteScatterer(true);

      detector.setShapeCode(StiDetector::kPlanar);

      int nLadder = 2*(iLadder + 1) - nLayer%2; // formal ladder number in svt
      if(nLayer == nLayers - 1){ nLadder = iLadder + 1; } // ssd

      
      double thephi0 =  mGeometryTransform->phiForSector(nLadder, 
							 (2 - nLayer/(nLayers-1))*nLadders);
      if (thephi0<phi0forpoly && thephi0>=0.) { //first ladder in given layer
	  phi0forpoly = thephi0;
      }
      
      detector.setCenterRep(dLadderRadius, thephi0, dOrientationAngle, dWaferHalfWidth);

      detector.setActivePosition(0.);
      detector.setHalfDepth(dWaferHalfDepth*svgConfig.n_wafer[nLayer]);
      detector.setZCenter(0.);
      detector.setThickness(2.*dWaferHalfThickness);

      detector.setSector(nLadder);
      detector.setPadrow(nLayer + 1);

      //Each ladder gets its own file, currently
      sprintf(szOutfile, "Svg/Layer_%i/Ladder_%i", 
              nLayer + 1, nLadder);
      detector.setName(szOutfile);
      sprintf(szOutfile, "%s/%s.txt", m_szGeomDirectory, detector.getName());
      detector.write(szOutfile);

      cout << "wrote file '"<< szOutfile << "'." << endl;

      // now we need to write out a file for the hybrids on each side of the
      // ladder:
      //                     ladder
      //                   ---------
      //      hybrid A   /           \  hybrid B
      //               /               \                    
          
      // need dimensions, rad length & density estimate of hybrid

      // We assume the hybrid is on a plane perpendicular to a vector from
      // the origin which bisects the angle between the two adjacent ladders

      // this is the hybrid clockwise of the ladder (I dub this 'B' for now)
/*
      params.refAngle -= dDeltaPhi/2.;
      params.active = false;
      params.halfDepth = STI_HYBRID_DEPTH/2.;
      params.thickness = STI_HYBRID_THICKNESS;
      params.density = STI_HYBRID_DENSITY;
      params.radLength = STI_HYBRID_RADLENGTH;
      params.yMin = dHalfGap - STI_HYBRID_WIDTH;
      params.yMax = dHalfGap;
      params.position = dGapRadius;

      sprintf(szOutfile, "Svt/Layer_%i/Ladder_%i_hybridB", 
              nLayer + 1, iLadder + 1);
      WriteGeomFile(params, szOutfile);

      // this is the hybrid counterclockwise of the ladder (dubbed 'A')
      params.refAngle += dDeltaPhi;
      params.yMin = -dHalfGap + STI_HYBRID_WIDTH;
      params.yMax = -dHalfGap;

      sprintf(szOutfile, "Svt/Layer_%i/Ladder_%i_hybridA", 
              nLayer + 1, iLadder + 1);
      WriteGeomFile(params, szOutfile);
*/
    } // for iLadder

    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //MLM (6/5/01)
    
    StiPolygon poly(nLadders, phi0forpoly, dLadderRadius);
    mpoly_vec.push_back(poly);

  } // for nLayer

} // buildSvg()

void StiGeometryGenerator::buildTpc()
{
  char outfile[500];

  int numSectors = gStTpcDb->Dimensions()->numberOfSectors();
  int numPadrows = gStTpcDb->PadPlaneGeometry()->numberOfRows();

  for (int padrow=1; padrow<=numPadrows; ++padrow) {
      
      double center = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
      double phi0forpoly;
      
      for (int sector=1; sector<=12; ++sector) {

	  //MLM (6/6/01) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Forget about sectors 13-24.  Treat TPC as 12 sectors of -200<z<200
	  // for (int sector=1; sector<=numSectors; ++sector) {
	  
      
      StiDetector detector;
      StiMaterial gas; // dummy for holding gas name
      gas.setName("P10");
      detector.setGas(&gas);
      detector.setMaterial(NULL);

      int numberOfPads = 
          gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(padrow);
      if (padrow<14){
        detector.setThickness(
            gStTpcDb->PadPlaneGeometry()->innerSectorPadLength() );
      }else{
        detector.setThickness(
            gStTpcDb->PadPlaneGeometry()->outerSectorPadLength() );
      }    

      double rowWidth = gStTpcDb->PadPlaneGeometry()->PadPitchAtRow(padrow) *
          static_cast<double>(numberOfPads);
	    

      detector.setIsOn(true);
      detector.setIsActive(true);
      detector.setIsContinuousMedium(true);
      detector.setIsDiscreteScatterer(false);

      double thephi0 = mGeometryTransform->phiForSector(sector, numSectors/2);
      
      if (sector==3) { //on x-axis
	  phi0forpoly = thephi0;
      }
      
      detector.setCenterRep(
          center, thephi0,
          0., rowWidth/2.);

      detector.setActivePosition(0.);
      //detector.setZCenter( sector>12 ? -100. : 100. );
      //detector.setHalfDepth(100.);

      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Temp Kludge to get a reasonable z-extent (MLM 6/6/01)
      detector.setZCenter(0.);
      detector.setHalfDepth(208.);

      detector.setSector(sector);
      detector.setPadrow(padrow);
      detector.setShapeCode(StiDetector::kPlanar);

      //Each padrow gets its own file, currently
      sprintf(outfile, "Tpc/Sector_%i/Padrow_%i", sector, padrow);
      detector.setName(outfile);
      sprintf(outfile, "%s/%s.txt", m_szGeomDirectory, detector.getName());
      detector.write(outfile);

      cout << "wrote file '"<< outfile << "'." << endl;

      }
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //M.L.M. (6/5/01)
      StiPolygon poly(12, phi0forpoly, center);
      mpoly_vec.push_back(poly);
  }
    
  return;
}
