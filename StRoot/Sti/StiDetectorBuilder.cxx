#include "Stiostream.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "Sti/StiDefaultTrackingParameters.h"
#include "StThreeVector.hh"
#include "StMaker.h"
#include "StThreeVectorD.hh"
StiDetectorBuilder* StiDetectorBuilder::fCurrentDetectorBuilder = 0;
int StiDetectorBuilder::_debug = 0;
StiDetectorBuilder::StiDetectorBuilder(const string & name,bool active, const string & inputFile)
  : Named(name+"Builder"),
    _groupId(-1),
    _active(active),
    _detectorFactory( StiToolkit::instance()->getDetectorFactory() ),
    _trackingParameters(0),
    _inputFile(inputFile),
    _gasMat(0)
{
  cout << "StiDetectorBuilder::StiDetectorBuilder() - INFO - Instantiating builder named:"<<name<<endl;
  fCurrentDetectorBuilder = this;
}

StiDetectorBuilder::~StiDetectorBuilder()
{}

bool StiDetectorBuilder::hasMore() const 
{
  //cout<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  return mDetectorIterator != mDetectorMap.end();
} // hasMore()

StiDetector * StiDetectorBuilder::next()
{
  //cout<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  if (mDetectorIterator != mDetectorMap.end())
    return (mDetectorIterator++)->second;
  else 
    return 0;
} // next()

StiMaterial* StiDetectorBuilder::findMaterial(const string& szName) const
{
  materialMap::const_iterator where = mMaterialMap.find(NameMapKey(szName));
  return (where!= mMaterialMap.end()) ? (*where).second : 0;
} // findMaterial()

StiShape* StiDetectorBuilder::findShape(const string& szName) const
{
  shapeMap::const_iterator where = mShapeMap.find(NameMapKey(szName));
  return (where!=mShapeMap.end()) ? (*where).second: 0;
} // findShape()

StiDetector* StiDetectorBuilder::findDetector(const string& szName) const
{
  detectorMap::const_iterator where = mDetectorMap.find(NameMapKey(szName));
  return (where!=mDetectorMap.end()) ? (*where).second: 0;
} // findDetector()

StiMaterial * StiDetectorBuilder::add(StiMaterial *material)
{  
  NameMapKey key(material->getName());
  mMaterialMap.insert( materialMapValType(key,material) );
  return material;
}

StiShape * StiDetectorBuilder::add(StiShape *shape)
{
  NameMapKey key(shape->getName());
  mShapeMap.insert( shapeMapValType(key, shape) );
	return shape;
}

StiDetector * StiDetectorBuilder::add(unsigned int row, unsigned int sector, StiDetector *detector)
{
  setNSectors(row,sector+1);
  _detectors[row][sector] = detector;
  if (_debug ) {
    cout << "StiDetectorBuilder::add(" << row << "," << sector << ") detector ";
    if (detector) cout << detector->getName();
    else          cout << " NULL ??";
    cout <<endl;
  }
  return add(detector);
}

/*! Add the given detector to the list of detectors known to this builder.
    Complete the "build" of this detector. 
 */
StiDetector * StiDetectorBuilder::add(StiDetector *detector)
{
  NameMapKey key(detector->getName());
  mDetectorMap.insert( detectorMapValType(key, detector) );
  //complete the building of this detector element
  // in the base class nothing is actually done
  // but ROOT stuff is built in the drawable version of this class.
  detector->setGroupId(_groupId);
  if (_trackingParameters) 
    detector->setTrackingParameters(_trackingParameters);
  else 
    detector->setTrackingParameters(StiDefaultTrackingParameters::instance());
  return detector;
}

void StiDetectorBuilder::build(StMaker& source)
{
  buildDetectors(source);
  mDetectorIterator = mDetectorMap.begin();
}

void StiDetectorBuilder::buildDetectors(StMaker& source)
{}

//________________________________________________________________________________
void StiDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP) {
  if (debug()) {cout << "StiDetectorBuilder::AverageVolume -I TGeoPhysicalNode\t" << nodeP->GetName() << endl;}
  TGeoVolume   *volP   = nodeP->GetVolume();
  TGeoMaterial *matP   = volP->GetMaterial(); if (debug()) matP->Print("");
  TGeoShape    *shapeP = nodeP->GetShape();   if (debug()) {cout << "New Shape\t"; StiVMCToolKit::PrintShape(shapeP);}
  TGeoHMatrix  *hmat   = nodeP->GetMatrix();  if (debug()) hmat->Print("");
  Double_t PotI = StiVMCToolKit::GetPotI(matP);
  StiMaterial *matS = add(new StiMaterial(matP->GetName(),
						matP->GetZ(),
						matP->GetA(),
						matP->GetDensity(),
						matP->GetDensity()*matP->GetRadLen(),
						PotI));
  Double_t ionization = matS->getIonization();
  StiElossCalculator *ElossCalculator = new StiElossCalculator(matS->getZOverA(), ionization*ionization, matS->getA(), matS->getZ(),matS->getDensity());
  StiShape     *sh     = findShape(volP->GetName());
  Double_t     *xyz    = hmat->GetTranslation();
  Double_t     *rot    = hmat->GetRotationMatrix();
  Double_t      Phi    = 0;
  //  Double_t xc,yc,zc,rc,rn, nx,ny,nz,yOff;
  StiPlacement *pPlacement = 0;
  if (xyz[0]*xyz[0] + xyz[1]*xyz[1] < 1.e-3 && 
      TMath::Abs(rot[0]*rot[0] + rot[4]*rot[4] + rot[8]*rot[8] - 3) < 1e-5 &&
      (shapeP->TestShapeBit(TGeoShape::kGeoTubeSeg) ||
       shapeP->TestShapeBit(TGeoShape::kGeoTube))) {
    Double_t paramsBC[3];
    shapeP->GetBoundingCylinder(paramsBC);
    TGeoTube *shapeC = (TGeoTube *) shapeP;
    Double_t Rmax = shapeC->GetRmax();
    Double_t Rmin = shapeC->GetRmin();
    Double_t dZ   = shapeC->GetDz();
    Double_t radius = (Rmin + Rmax)/2;
    Double_t dPhi = 2*TMath::Pi();
    Double_t dR   = Rmax - Rmin;
    dR = TMath::Min(0.2*dZ, dR);
    if (dR < 0.1) dR = 0.1;
    int Nr = (int) ((Rmax - Rmin)/dR);
    if (Nr <= 0) Nr = 1;
    dR = (Rmax - Rmin)/Nr;
    if (shapeP->TestShapeBit(TGeoShape::kGeoTubeSeg)) {
      TGeoTubeSeg *shapeS = (TGeoTubeSeg *) shapeP;
      Phi =  TMath::DegToRad()*(shapeS->GetPhi2() + shapeS->GetPhi1())/2;
      Phi =  StiVMCToolKit::Nice(Phi);
      dPhi = TMath::DegToRad()*(shapeS->GetPhi2() - shapeS->GetPhi1());
    }
    for (Int_t ir = 0; ir < Nr; ir++) {
      TString Name(volP->GetName());
      if (ir > 0) {
	Name += "__";
	Name += ir;
      }
      sh     = findShape(Name.Data());
      if (! sh) {// I assume that the shape name is unique
	sh = new StiCylindricalShape(volP->GetName(), // Name
				     dZ,              // halfDepth
				     dR,              // thickness
				     Rmin + (ir+1)*dR,// outerRadius
				     dPhi);           // openingAngle
	add(sh);
      }
      pPlacement = new StiPlacement;
      pPlacement->setZcenter(xyz[2]);
      pPlacement->setLayerRadius(Rmin + (ir+0.5)*dR);
      pPlacement->setLayerAngle(Phi);
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      pPlacement->setNormalRep(Phi,radius, 0); 
    }
  }
  else {// BBox
    shapeP->ComputeBBox();
    TGeoBBox *box = (TGeoBBox *) shapeP;
    if (! sh) {
      sh = new StiPlanarShape(volP->GetName(),// Name
			      box->GetDZ(),   // halfDepth
			      box->GetDX(),   // thickness
			      box->GetDY());  // halfWidth
      add(sh);
    }
    StThreeVectorD centerVector(xyz[0],xyz[1],xyz[2]);
    StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
    Double_t prod = centerVector*normalVector;
    if (prod < 0) normalVector *= -1;
    Double_t phi  = centerVector.phi();
    Double_t phiD = normalVector.phi();
    Double_t r = centerVector.perp();
    pPlacement = new StiPlacement;
    pPlacement->setZcenter(xyz[2]);
    pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
 }
  assert(pPlacement);
  StiDetector *pDetector = getDetectorFactory()->getInstance();
  TString nameP(nodeP->GetName());
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  nameP.Resize(30); nameP.Strip();
  pDetector->setName(nameP.Data());
  pDetector->setIsOn(false);
  pDetector->setIsActive(new StiNeverActiveFunctor);
  pDetector->setIsContinuousMedium(false);
  pDetector->setIsDiscreteScatterer(true);
  pDetector->setShape(sh);
  pDetector->setPlacement(pPlacement); 
  pDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
  pDetector->setMaterial(matS);
  pDetector->setElossCalculator(ElossCalculator);
  Int_t layer = getNRows();
  add(layer+1,0,pDetector);
}

