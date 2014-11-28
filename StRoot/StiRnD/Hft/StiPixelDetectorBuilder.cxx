// 12/12/2012 : modification of the builder to take into account the new geometry path names
// backward compatibility with upgr15 geometry is lost
/*
 * $Id: StiPixelDetectorBuilder.cxx,v 1.32 2014/08/22 17:47:53 perev Exp $
 *
 * $Log: StiPixelDetectorBuilder.cxx,v $
 * Revision 1.32  2014/08/22 17:47:53  perev
 * Remove never used input file
 *
 * Revision 1.31  2014/01/30 16:50:59  didenko
 * get back to previous revision
 *
 * Revision 1.29  2013/03/11 17:24:08  bouchet
 * StiRnD for Y2013
 *
 * Revision 1.28  2012/12/18 20:52:32  bouchet
 * update for DEV13 geometry
 *
 * Revision 1.27  2011/04/22 22:00:18  fisyak
 * warn off
 *
 * Revision 1.26  2010/08/25 21:57:41  fisyak
 * Get rid off access to specfic detector tracking parameters which usage has been  disable since 2008/06/11
 *
 * Revision 1.25  2009/03/16 13:51:00  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 1.24  2009/02/09 02:47:19  andrewar
 * UPGR15 update. Will break backward compatibility with older geometries.
 *
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

/* 
   numbering should be the following :
   hardware : sector ladder   ITTF : layer  ladder
   1      1                          1      0                
   1      2                          1      1
   1      3                          1      2
   1      4                          0      0
   
   2      1                          1      3                
   2      2                          1      4
   2      3                          1      5
   2      4                          0      1
   (...)
   10     1                          1     27               
   10     2                          1     28
   10     3                          1     29
   10     4                          0     9
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
//#include "Sti/StiElossCalculator.h"
#include "StiPixelDetectorBuilder.h" 
#include "StiPixelIsActiveFunctor.h"
#include "StDetectorDbMaker/StiPixelHitErrorCalculator.h"
#include "TDataSetIter.h"
#include "tables/St_HitError_Table.h"
#include "StEvent.h"
#include "StEventTypes.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active)
  : StiDetectorBuilder("Pixel",active)
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


  unsigned int nRows=2;

  // 2 real rows, but we have detector elements and support elements. 
  setNRows(nRows);
  
  if (StiVMCToolKit::GetVMC()) {useVMCGeometry(); return;}

  //_gas is the gas that the pixel detector lives in
  _gasMat            = add(new StiMaterial("PixelAir",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
  //StiMaterial * material = add(new StiMaterial("PixelSi", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );
  _siMat     = add(new StiMaterial("PixelSi", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );
  _hybridMat = add(new StiMaterial("PixelHyb", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );

  //Instantiate energy loss detector for si material  
  //const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  //double ionization = material->getIonization();
//  double ionization = _siMat->getIonization();

//   StiElossCalculator * siElossCalculator = new StiElossCalculator(_siMat->getZOverA(),
// 								  ionization*ionization,
// 								  _siMat->getA(),
// 								  _siMat->getZ(),
// 								  _siMat->getDensity());
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
	  pDetector->setMaterial(_siMat);
	  pDetector->setGas(_gasMat);
	  pDetector->setGroupId(kPxlId);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  pDetector->setHitErrorCalculator(StiPixelHitErrorCalculator::instance());
//	  pDetector->setElossCalculator(siElossCalculator);
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
    const Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"SILICON", &_siMat},
    {"SILICON", &_hybridMat} 
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
      {"PLAC","Active ladder volume",     "HALL_1/CAVE_1/IDSM_1/PXMO_1","",""}
      //{"PXSI","InActive ladder volume",   "HALL_1/CAVE_1/IDSM_1/PXMO_1","",""}
      //{"SIFR","InActive ladder volume",   "HALL_1/CAVE_1/IDSM_1/PXMO_1","",""}
    };
  
  Int_t NoPxlVols = sizeof(PxlVolumes)/sizeof(VolumeMap_t);
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  for (Int_t i = 0; i < NoPxlVols; i++) {
    gGeoManager->cd(PxlVolumes[i].path); 
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;;
    LOG_DEBUG <<" current node : " << i <<"/" << NoPxlVols <<" path is : " << PxlVolumes[i].name << endm;
    StiVMCToolKit::LoopOverNodes(nodeT, PxlVolumes[i].path, PxlVolumes[i].name, MakeAverageVolume);
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
  
  TString temp=nameP;
  temp.ReplaceAll("HALL_1/CAVE_1/IDSM_1/PXMO_1/","");
  int q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString numsec=temp(0,2);
  int sector=numsec.Atoi();
  
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString numlad=temp(0,2);
  if(!numlad.IsDigit()) numlad=temp(0,1);
  int ladder=numlad.Atoi();
  
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString numsens=temp(0,2);
  if(!numsens.IsDigit()) numsens=temp(0,1);
  int sensor=numsens.Atoi();
  
  if(sensor!=1) return;

  // Check whether this is an active volume
  Bool_t ActiveVolume = kFALSE;
  if (nodeP->GetVolume()->GetMedium()->GetParam(0) == 1) {
  ActiveVolume = kTRUE;
  LOG_DEBUG <<" this node is active " << endm;
  }
  
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
//  Double_t ionization = matS->getIonization();
//   StiElossCalculator *ElossCalculator = new StiElossCalculator(matS->getZOverA(), 
// 							       ionization*ionization, 
// 							       matS->getA(), 
// 							       matS->getZ(),
// 							       matS->getDensity());

// Extract volume geometry for this node
  TGeoBBox *box = (TGeoBBox *) nodeP->GetShape();
  StiShape *sh  = new StiPlanarShape(nodeP->GetVolume()->GetName(), // Name
			             10*box->GetDZ(),          // halfDepth
			             box->GetDY(),                  // thickness
			             box->GetDX());                 // halfWidth
  add(sh);

  // position information
  TGeoHMatrix  *hmat   = nodeP->GetMatrix();  if (debug()) hmat->Print("");
  Double_t     *xyz    = hmat->GetTranslation();
  Double_t     *rot    = hmat->GetRotationMatrix();
  //StThreeVectorD centerVector(xyz[0],xyz[1],0.0);
  StThreeVectorD centerVector(xyz[0],xyz[1],xyz[2]);
  StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
  
  Double_t prod = centerVector*normalVector;
  if (prod < 0) normalVector *= -1;
  // Normalize normal vector, just in case....
  normalVector /= normalVector.magnitude();
  
  // Volume positioning
  StiPlacement *pPlacement = new StiPlacement;
  Double_t phi  = centerVector.phi();
  Double_t phiD = normalVector.phi();
  Double_t r    = centerVector.perp();
  pPlacement->setZcenter(0);
  pPlacement->setLayerRadius(r); 
  
  //if(nameP.Contains("PLAC")) 
  pPlacement->setLayerAngle(phi);
  pPlacement->setRegion(StiPlacement::kMidRapidity);
  pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
  assert(pPlacement);
  
  //Build final detector object
  
  StiDetector *p =getDetectorFactory()->getInstance();
  if ( !p ) 
    {
      LOG_INFO <<"StiPixelDetectorBuilder::AverageVolume() -E- StiDetector pointer invalid." <<endm;
      return;
    }
  p->setName(nameP.Data());
  
  p->setIsOn(false);
  if (ActiveVolume) {
    p->setIsActive(new StiPixelIsActiveFunctor);
  }
  else {
    p->setIsActive(new StiNeverActiveFunctor);
  }
  //layer=layer+10;
  //if(nameP.Contains("SIFL")) {layer=layer+10;}
  //if(nameP.Contains("SIFR")) {layer=layer+20;}
  //}
  
  p->setIsContinuousMedium(false);
  p->setIsDiscreteScatterer(true);
  p->setShape(sh);
  p->setPlacement(pPlacement);
  p->setGas(GetCurrentDetectorBuilder()->getGasMat());
  if(!p->getGas()) LOG_INFO <<"gas not there!"<<endm;
  p->setMaterial(matS);
//  p->setElossCalculator(ElossCalculator);
  p->setHitErrorCalculator(StiPixelHitErrorCalculator::instance());
  
  Int_t ROW    = 0;
  Int_t SECTOR = 0;
  
  /* numbering is : 
     ladder = 0-1- ...9 for inner layer --> ROW =0
     ladder = 0-1-2 for sector 0 of outer layer, then 3-4-5 for the second sector until 29 for the last sectro
     ladder=4 is the inner ladder
  */
  if(ladder==4)
    {
      ROW =0 ;
      SECTOR = sector-1;
    }
  else {
    ROW = 1;
    SECTOR = (sector-1)*3 + (ladder -1);
  }
  p->setKey(1, ROW);
  p->setKey(2, SECTOR);
  add(ROW,SECTOR, p);

  // Whole bunch of debugging information
  
  Float_t rad2deg = 180.0/3.1415927;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:Name               = " << p->getName()                               << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:sector             = " << sector                                     << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:Ladder             = " << ladder                                     << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:sensor             = " << sensor                                     << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:row/sector (ITTF)  = " << ROW <<" / " << SECTOR                      << endm;
  LOG_DEBUG << "===>NEW:PIXEL:pDetector:Active?            = " << p->isActive()                              << endm;
  
}
