/*
 * $Id: StiPixelDetectorBuilder.cxx,v 1.14 2006/11/17 15:39:03 wleight Exp $
 *
 * $Log: StiPixelDetectorBuilder.cxx,v $
 * Revision 1.14  2006/11/17 15:39:03  wleight
 * Changes to make HFT hits work with UPGR05 geometry
 *
 * Revision 1.13  2006/04/19 19:49:47  andrewar
 * Added call to setLayerAngle, needed for detector container sort.
 *
 * Revision 1.12  2006/02/23 00:22:54  andrewar
 * Set Detector Id to kHftId, corrected Ist*pars -> Pixel*pars
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

#include "StEvent.h"
#include "StEventTypes.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active,
						 const string & inputFile)
  : StiDetectorBuilder("Pixel",active,inputFile)
{
	//Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
	//once you actually want to do tracking, the results depend strongly on the numbers below.
	//here I plug in 4micron resolution in both local x and y coordinates
	//I also put no dependence on either crossing angle or dip angle of track
    _trackingParameters.setName("PixelTrackingParameters");
    _calculator.setName("PixelHitErrors");

    //_calculator = new StiDefaultHitErrorCalculator();
    _calculator.set(6e-5, 0., 0., 6e-5, 0., 0.);
  
    ifstream inF("PixelBuilder_pars.txt");
    if (inF)
      {
	_trackingParameters.loadFS(inF);
	cout << "StiPixelDetectorBuilder:: -I-  New tracking parameters from file" << endl;
      }
    else
      {

	//StiTrackingParameters * trackingPars = getTrackingParameters();
	_trackingParameters.setMaxChi2ForSelection(100.);
	_trackingParameters.setMinSearchWindow(0.01);
	_trackingParameters.setMaxSearchWindow(.5);
	_trackingParameters.setSearchWindowScaling(10.);
      }
}

StiPixelDetectorBuilder::~StiPixelDetectorBuilder()
{}

/// Build all detector components of the Pixel detector.
void StiPixelDetectorBuilder::buildDetectors(StMaker &source)
{
  char name[50];
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Started" << endl;
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
	  cout << " sector:"<<sector
	       << "    phi:"<<phi*180/3.1415
	       << " radius:"<<radiusForPixelSector(sector)
	       << " normal r:"<<r
	       << "     dY:"<<dY<<endl;
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
	  pDetector->setGroupId(kHftId);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  pDetector->setHitErrorCalculator(&_calculator);
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

	  cout << "Setting detector: " << name << " with key values: "
	       << pDetector->getKey(1) << " "  << pDetector->getKey(2) << endl;
	}
    }
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Done" << endl;
}

void StiPixelDetectorBuilder::useVMCGeometry() {
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Use VMC geometry" 
       << endl;
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
  const VolumeMap_t HftVolumes[] = 
    { 
      // HFT - only active volumes for now.
 
      //{"PXMO","the structure mother volume","HALL_1/CAVE_1/PXMO_1","",""}
      //{"PSEC","Ladder group mother","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/*","",""}
      //{"PLMO","Ladder Mother Volume","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/PLMO_1-11/*","",""}
      {"PLAC","Active ladder volume","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/PLMO_1-11/PLAC_1/*","",""}
      //{"PLPS","Passive ladder volume","HALL_1/CAVE_1/PXMO_1/PSEC_1-3/PLMO_1-11/PLPS_1/*","",""}
    };

  Int_t NoHftVols = sizeof(HftVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1/PXMO_1");
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  if (! gGeoManager->cd(pathT)) {
    pathT = "HALL_1/CAVE_1/PXMO_1";
  }
  TString path("");
  for (Int_t i = 0; i < NoHftVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    if (gGeoManager->cd(pathT)) 
      {
	path = pathT;
	TGeoNode *nodeT = gGeoManager->GetCurrentNode();
	if (! nodeT) continue;;
	StiVMCToolKit::LoopOverNodes(nodeT, path, HftVolumes[i].name, MakeAverageVolume);
      } 
    else gMessMgr->Info() << "StiPixelDetectorBuilder::useVMCGeometry skip node " 
			  << pathT.Data() << endm;
  }
}


void StiPixelDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP)
{
  // Handle pathalogical input
  if (!nodeP) 
    {
      gMessMgr->Info() << "StiPixelDetectorBuilder::AverageVolume -E- no TGeoPhysicalNode. "
		       << " Perhaps Pixel is turned on in tracking, but not present in simulation. Returning."
		       << endl;
      printf("StiPixelDetectorBuilder::AverageVolume -E- no TGeoPhysicalNode. \n");
      return;
    }

  gMessMgr->Info() << "StiPixelDetectorBuilder::AverageVolume -I- TGeoPhysicalNode: " 
		   << nodeP->GetName()
		   << endl;
  printf("StiPixelDetectorBuilder::AverageVolume -I- TGeoPhysicalNode: %s\n",nodeP->GetName());

  // Note:
  // Volumes are currently all planes. I am coding this routine appropriately. Other
  // GEANT shapes (cylinder/sphere) are handled differently, and would require adding cases
  // if such are added to the HFT geometry.
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
	printf("StiPixelDetectorBuilder -I- Normal vector is not a unit vector!\n");
	normalVector/=normalVector.magnitude();
      }
    normalVector *= centerVector.magnitude()*cos(normalVector.phi()-centerVector.phi());

    double r = normalVector.perp();
    //Set the offset 
    double dY = (normalVector - centerVector).magnitude();
    if (normalVector.phi()>centerVector.phi()) dY=-1.*dY;


    StiPlacement *pPlacement = new StiPlacement;
    pPlacement->setZcenter(0);
    pPlacement->setLayerRadius(centerVector.perp()); //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(centerVector.phi()); //this is only used for ordering in detector container...
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(normalVector.phi(), r, dY); 


    // Add detector only if preceeding steps were successful

    if( sh ) add(sh);
    else 
    {
      gMessMgr->Info() <<"StiPixelDetectorBuilder::AverageVolume() -E- StiPlanarShape build unsuccessful. Returning."
		       <<endl;
      return;
    }
    if( pPlacement ) {}
    else
      {
      gMessMgr->Info() <<"StiPixelDetectorBuilder::AverageVolume() -E- StiPlacement unsuccessful. "
		       << "Volume: "<<nameP.Data()<<". Returning."
		       <<endl;
      return;
    }

    //Build final detector object
    StiDetector *p =getDetectorFactory()->getInstance();
    if ( !p ) 
      {
	gMessMgr->Info() <<"StiPixelDetectorBuilder::AverageVolume() -E- StiDetector pointer invalid." <<endl;
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
    if(!p->getGas()) cout<<"gas not there!"<<endl;
    p->setMaterial(matS);
    p->setElossCalculator(ElossCalculator);
    p->setHitErrorCalculator(&_calculator);

    int startMoth   = nameP.Index("PSEC_",5) + 5;
    int startLadder = nameP.Index("PLMO_",5) + 5;
    TString mother(nameP);
    mother.Remove(0,startMoth);
    mother.Remove(1,mother.Length());
    int motherN=mother.Atoi();
    printf("Mother Volume: %i for %s and %s\n", motherN,nameP.Data(), mother.Data());

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
    gMessMgr->Info() <<"StiPixelDetectorBuilder: -I- built detector "
		     << p->getName()
		     << " from " << nameP.Data()<<endl;
    printf("StiPixelDetectorBuilder: -I- built detector %s from %s\n", 
	   p->getName().c_str(), nameP.Data());


    p->setKey(1, layer);
    p->setKey(2, ladder);
    add(layer,ladder, p);
}
