/*
 * $Id: StiPixelDetectorBuilder.cxx,v 1.23 2008/04/03 20:04:20 fisyak Exp $
 *
 * $Log: StiPixelDetectorBuilder.cxx,v $
 * Revision 1.23  2008/04/03 20:04:20  fisyak
 * Straighten out DB access via chairs
 *
 * Revision 1.22  2007/10/20 00:16:27  fisyak
 * Active hit errors from DB
 *
 * Revision 1.21  2007/10/16 19:50:24  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.20  2007/05/16 15:02:57  andrewar
 * Removed couts in favor of LOG_INFO.
 *
 * Revision 1.19  2007/05/03 06:14:56  andrewar
 * Geometry fix to conform to StiHit:setGlobal() test.
 *
 * Revision 1.18  2007/03/30 02:14:19  andrewar
 * Removed some debug output.
 *
 * Revision 1.17  2006/11/30 16:37:19  andrewar
 * Removed call to dbase for tracking parameter loading for the review. Dynamic
 * access will be debugged and restored after the STAR review. Hit errors are
 * forced to 60um.
 *
 * Revision 1.16  2006/11/29 04:02:01  andrewar
 * Make use of pre-existing STAR DB inteface.
 *
 * Revision 1.15  2006/11/29 00:44:04  andrewar
 * Added call to get tracking parameters from DBase.
 *
 * Revision 1.14  2006/11/17 15:39:03  wleight
 * Changes to make PXL hits work with UPGR05 geometry
 *
 * Revision 1.13  2006/04/19 19:49:47  andrewar
 * Added call to setLayerAngle, needed for detector container sort.
 *
 * Revision 1.12  2006/02/23 00:22:54  andrewar
 * Set Detector Id to kPxlId, corrected Ist*pars -> Pixel*pars
 *
 * Revision 1.11  2006/02/17 21:39:32  andrewar
 * Added calls to StiDetector::setKey(key,val)
 *
 *
 */

#include <stdio.h>
#include <stdexcept>
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "StiPixelIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "StiPixelDetectorBuilder.h" 
#include "StiPixelIsActiveFunctor.h"
#include "StiPixelHitErrorCalculator.h"
#include "StiPixelTrackingParameters.h"
#include "TDataSetIter.h"
#include "tables/St_HitError_Table.h"
#include "StEvent.h"
#include "StEventTypes.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active,
						 const string & inputFile)
  : StiDetectorBuilder("Pixel",active,inputFile)
{
	//Parameterized hit error calculator.  Given a track (dip, cross, pt, etc)
        //returns average error once you actually want to do tracking, the results
        //depend strongly on the numbers below. 
}

StiPixelDetectorBuilder::~StiPixelDetectorBuilder()
{}

/// Build all detector components of the Pixel detector.
void StiPixelDetectorBuilder::buildDetectors(StMaker &source)
{

  char name[50];
  LOG_INFO << "StiPixelDetectorBuilder::buildDetectors() -I- Started" << endm;


  unsigned int nRows=1;

  // 2 real rows, but we have detector elements and support elements. 
  setNRows(2);
  
  if (StiVMCToolKit::GetVMC()) {useVMCGeometry(); return;}


  //_gas is the gas that the pixel detector lives in
  _gasMat            = add(new StiMaterial("PixelAir",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
  StiMaterial * material = add(new StiMaterial("PixelSi", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );


  //Instantiate energy loss detector for si material  
  //const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  double ionization = material->getIonization();
  StiElossCalculator * siElossCalculator = new StiElossCalculator(material->getZOverA(),
								  ionization*ionization,
								  material->getA(),
								  material->getZ(),
								  material->getDensity());
  _trackingParameters = (StiTrackingParameters *) StiPixelTrackingParameters::instance();
  StiPlanarShape *pShape;
  for (unsigned int row=0; row<nRows; row++) 
    {
      pShape = new StiPlanarShape;
      if (!pShape) throw runtime_error("StiPixelDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");
      sprintf(name, "Pixel/Layer_%d", row);
      pShape->setName(name);
      pShape->setThickness(0.0280); //cm 
      pShape->setHalfDepth( 20./2. );
      pShape->setHalfWidth(1.0);
      for(unsigned int sector = 0; sector<24; sector++)	
	{      
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  double phi = phiForPixelSector(sector) + psiForPixelSector(sector);
	  double r = radiusForPixelSector(sector)* cos(psiForPixelSector(sector)) - 0.0040; // note 40 microns offset
	  double dY = radiusForPixelSector(sector)*sin(psiForPixelSector(sector));
	  /*printf(" sector: %g phi: %g radius: %g normal: %g dY: %g\n",sector
	       ,phi*180/3.1415
	       << " radius:"<<radiusForPixelSector(sector)
	       << " normal r:"<<r
	       << "     dY:"<<dY<<endl;*/
	  pPlacement->setNormalRep(phi, r, dY); 
	  pPlacement->setLayerRadius(r);
	  pPlacement->setLayerAngle(phi);
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  sprintf(name, "Pixel/Layer_%d/Ladder_%d", row, sector);
	  StiDetector *pDetector = _detectorFactory->getInstance();
	  pDetector->setName(name);
	  pDetector->setIsOn(true);
	  pDetector->setIsActive(new StiPixelIsActiveFunctor);
	  pDetector->setIsContinuousMedium(true);
	  pDetector->setIsDiscreteScatterer(false);
	  pDetector->setMaterial(material);
	  pDetector->setGas(_gasMat);
	  pDetector->setGroupId(kPxlId);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  pDetector->setHitErrorCalculator(StiPixelHitErrorCalculator::instance());
	  pDetector->setElossCalculator(siElossCalculator);
	  if (sector<18)
	    {
	      pDetector->setKey(1,1);
	      pDetector->setKey(2,sector);
	      add(1,sector,pDetector);
	    }
	  else
	    {
	      pDetector->setKey(1,0);
	      pDetector->setKey(2,sector-18);
	      add(0,(sector-18),pDetector);
	    }

	  //cout << "Setting detector: " << name << " with key values: "
	  //     << pDetector->getKey(1) << " "  << pDetector->getKey(2) << endl;
	}
    }
  LOG_INFO << " -I- Done" << endl;
}

void StiPixelDetectorBuilder::useVMCGeometry() {
  LOG_INFO << "StiPixelDetectorBuilder::buildDetectors() -I- Use VMC geometry" 
       << endm;
  SetCurrentDetectorBuilder(this);

  // Build materials. In the Pixel detector we have two: Air for the mother volume,
  // silicon for both the detector and the ladder support. This will be updated with
  // more detailed support structures at the appropriate time.
  struct Material_t {
    Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"SILICON", &_fcMaterial} 
  };
  Int_t M = sizeof(map)/sizeof(Material_t);
  for (Int_t i = 0; i < M; i++) 
    {
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

  //
  // Build volumes. Will be done from GEANT tables.
  //

  // Set volume name tree. Inactive volumes only here. Active volumes are declared in ::AverageVolume, called
  // through loop over StiDetectorBuilder::AverageVolume
  const VolumeMap_t PxlVolumes[] = 
    { 
      // PXL - only active volumes for now.
 
      //{"PXMO","the structure mother volume","HALL_1/CAVE_1/PXMO_1","",""}
      //{"PSEC","Ladder group mother","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/*","",""}
      //{"PLMO","Ladder Mother Volume","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/PLMO_1-11/*","",""}
      {"PLAC","Active ladder volume","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/PLMO_1-11/PLAC_1/*","",""}
    };

  Int_t NoPxlVols = sizeof(PxlVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1");
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  TString path("");
  for (Int_t i = 0; i < NoPxlVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    if (gGeoManager->cd(pathT)) 
      {
	path = pathT;
	TGeoNode *nodeT = gGeoManager->GetCurrentNode();
	if (! nodeT) continue;;
	StiVMCToolKit::LoopOverNodes(nodeT, path, PxlVolumes[i].name, MakeAverageVolume);
      } 
    else {LOG_INFO << "StiPixelDetectorBuilder::useVMCGeometry skip node " 
		   << pathT.Data() << endm;}
  }
}


void StiPixelDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP)
{
  // Handle pathalogical input
  if (!nodeP) 
    {
      LOG_INFO << "StiPixelDetectorBuilder::AverageVolume -E- no TGeoPhysicalNode. "
		       << " Perhaps Pixel is turned on in tracking, but not present in simulation. Returning."
		       << endm;
     
      return;
    }


  // Note:
  // Volumes are currently all planes. I am coding this routine appropriately. Other
  // GEANT shapes (cylinder/sphere) are handled differently, and would require adding cases
  // if such are added to the PXL geometry.
  // AAR - Oct 31, 2006 

    TString nameP(nodeP->GetName());


  //Material definitions
  TGeoMaterial *matP   = nodeP->GetVolume()->GetMaterial(); 
  Double_t PotI = StiVMCToolKit::GetPotI(matP);
  static StiMaterial *matS = 0;
  if (! matS) matS = add(new StiMaterial(matP->GetName(),
					 matP->GetZ(),
					 matP->GetA(),
					 matP->GetDensity(),
					 matP->GetDensity()*matP->GetRadLen(),
					 PotI));
  Double_t ionization = matS->getIonization();
  StiElossCalculator *ElossCalculator = new StiElossCalculator(matS->getZOverA(), 
							       ionization*ionization, 
							       matS->getA(), 
							       matS->getZ(),
							       matS->getDensity());

  //Extract volume geometry from TGeoPhysicalNode
  TGeoVolume   *volP   = nodeP->GetVolume();
  TGeoShape    *shapeP = nodeP->GetShape();  
  TGeoBBox     *box    = (TGeoBBox *) shapeP;
  StiShape     *sh     = new StiPlanarShape(volP->GetName(),        // Name
					    box->GetDZ(),   // halfDepth
					    box->GetDY(),           // thickness
					    box->GetDX());          // halfWidth


  // position information
    TGeoHMatrix  *hmat   = nodeP->GetMatrix();  if (debug()) hmat->Print("");
    Double_t     *xyz    = hmat->GetTranslation();
    Double_t     *rot    = hmat->GetRotationMatrix();
    StThreeVectorD centerVector(xyz[0],xyz[1],xyz[2]);
    StThreeVectorD normalVector(rot[1],rot[4],rot[7]);

    //In ittf parlance, normalVector is the vector normal to the detector plane that passes through 0,0
    if( normalVector.magnitude() != 1. ) 
      {
	//printf("StiPixelDetectorBuilder -I- Normal vector is not a unit vector!\n");
	normalVector/=normalVector.magnitude();
      }
    normalVector *= centerVector.magnitude()*cos(normalVector.phi()-centerVector.phi());
    LOG_INFO <<"Setting detector normal angle: "
	     << normalVector.phi()<<endm;

    double r = normalVector.perp();
    //Set the offset - this will be the difference between the two vectors. Set magnitude from vector difference...
    double dY = (normalVector - centerVector).magnitude();
    // set direction (+/-) from vector direction...
    // Remove auto-correction for change of ladder tilt. Assume dY is positive.
    // if ( (normalVector-centerVector).phi() < 0 ) dY=-1.*dY;


    StiPlacement *pPlacement = new StiPlacement;
    pPlacement->setZcenter(0);
    pPlacement->setLayerRadius(centerVector.perp()); //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(centerVector.phi()); //this is only used for ordering in detector container...
    LOG_INFO << " -I- Setting detector center angle: " << centerVector.phi()
	     << " offset: " << dY << " Ist style offset: "
	     << centerVector.magnitude()*TMath::Sin(normalVector.phi() - centerVector.phi()) << endm;

    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(normalVector.phi(), r, dY); 


    // Add detector only if preceeding steps were successful

    if( sh ) add(sh);
    else 
    {
      LOG_INFO <<"StiPixelDetectorBuilder::AverageVolume() -E- StiPlanarShape build unsuccessful. Returning."
	       <<endm;
      return;
    }
    if( pPlacement ) {}
    else
      {
        LOG_INFO <<"StiPixelDetectorBuilder::AverageVolume() -E- StiPlacement unsuccessful. "
		 << "Volume: "<<nameP.Data()<<". Returning."
		 <<endm;
        return;
      }

    //Build final detector object
    StiDetector *p =getDetectorFactory()->getInstance();
    if ( !p ) 
      {
	LOG_INFO <<"StiPixelDetectorBuilder::AverageVolume() -E- StiDetector pointer invalid." <<endm;
	return;
      }

    //p->setIsOn(true); //assume the detector is on if the detector builder is called
    p->setIsOn(false);
    p->setIsActive(new StiPixelIsActiveFunctor);
    p->setIsContinuousMedium(false);
    p->setIsDiscreteScatterer(true);
    p->setShape(sh);
    p->setPlacement(pPlacement);
    p->setGas(GetCurrentDetectorBuilder()->getGasMat());
    if(!p->getGas()) LOG_INFO <<"gas not there!"<<endm;
    p->setMaterial(matS);
    p->setElossCalculator(ElossCalculator);
    p->setHitErrorCalculator(StiPixelHitErrorCalculator::instance());

    int startMoth   = nameP.Index("PSEC_",5) + 5;
    int startLadder = nameP.Index("PLMO_",5) + 5;
    TString mother(nameP);
    mother.Remove(0,startMoth);
    mother.Remove(1,mother.Length());
    int motherN=mother.Atoi();
 
    TString ladderNme(nameP);
    ladderNme.Remove(0, startLadder);
    ladderNme.Remove(2, ladderNme.Length());
    if( ! ladderNme.IsDigit() ) ladderNme.Remove(1, ladderNme.Length());
    int ladder = ladderNme.Atoi();

    int layer=0;
    if(ladder>3)
      {
	layer =1;
	ladder = (motherN-1)*8 + (ladder-3);
      }
    else
      {
	layer=0;
	ladder = (motherN-1)*3 + ladder;
      }
    
    
    //ladder=ladder-1; //go from FORTRAN to C++

    //now, correct for screw up in ladder numbering:
    if(layer == 0)
      ladder= ( 2*(motherN-1)+1)*3 - ladder;
    else
      ladder= ( 2*(motherN-1)+1)*8 - ladder;
    
    char name[50];
    sprintf(name, "Pixel/Layer_%d/Ladder_%d", layer, ladder);
    p->setName(name);
    LOG_INFO <<"StiPixelDetectorBuilder: -I- built detector "
	     << p->getName()
	     << " from " << nameP.Data()<<" center: "
	     <<centerVector.phi() <<" normal: "<<normalVector.phi()<<endm;
  
    p->setKey(1, layer);
    p->setKey(2, ladder);
    add(layer,ladder, p);
}
