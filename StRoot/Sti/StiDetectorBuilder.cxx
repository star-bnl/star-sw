#include "Stiostream.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiUtilities/StiDebug.h"
#include "Sti/StiElossCalculator.h"
#include "StDetectorDbMaker/StiDefaultTrackingParameters.h"
#include "StThreeVector.hh"
#include "StMaker.h"
#include "StThreeVectorD.hh"
#include "TMath.h"
#include "TVector3.h"

StiDetectorBuilder* StiDetectorBuilder::fCurrentDetectorBuilder = 0;
int StiDetectorBuilder::_debug = 0;
//________________________________________________________________________________
StiDetectorBuilder::StiDetectorBuilder(const string & name,bool active)
  : Named(name+"Builder"),
    mThkSplit(0.2),
    mMaxSplit( 20),
    _groupId(-1),
    _active(active),
    _detectorFactory( StiToolkit::instance()->getDetectorFactory() ),
    _trackingParameters(0),
    _gasMat(0)
{
  cout << "StiDetectorBuilder::StiDetectorBuilder() - INFO - Instantiating builder named:"<<name<<endl;
  fCurrentDetectorBuilder = this;
}

//________________________________________________________________________________
StiDetectorBuilder::~StiDetectorBuilder()
{}

//________________________________________________________________________________
bool StiDetectorBuilder::hasMore() const 
{
  //cout<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  return mDetectorIterator != mDetectorMap.end();
} // hasMore()

//________________________________________________________________________________
StiDetector * StiDetectorBuilder::next()
{
  //cout<<"StiDetectorBuilder::hasMore() - INFO - Started"<<endl;
  if (mDetectorIterator != mDetectorMap.end())
    return (mDetectorIterator++)->second;
  else 
    return 0;
} // next()

//________________________________________________________________________________
StiMaterial* StiDetectorBuilder::findMaterial(const string& szName) const
{
  materialMap::const_iterator where = mMaterialMap.find(NameMapKey(szName));
  return (where!= mMaterialMap.end()) ? (*where).second : 0;
} // findMaterial()

//________________________________________________________________________________
StiShape* StiDetectorBuilder::findShape(const string& szName) const
{
  shapeMap::const_iterator where = mShapeMap.find(NameMapKey(szName));
  return (where!=mShapeMap.end()) ? (*where).second: 0;
} // findShape()

//________________________________________________________________________________
StiDetector* StiDetectorBuilder::findDetector(const string& szName) const
{
  detectorMap::const_iterator where = mDetectorMap.find(NameMapKey(szName));
  return (where!=mDetectorMap.end()) ? (*where).second: 0;
} // findDetector()

//________________________________________________________________________________
StiMaterial * StiDetectorBuilder::add(StiMaterial *material)
{  
  NameMapKey key(material->getName());
  mMaterialMap.insert( materialMapValType(key,material) );
  return material;
}

//________________________________________________________________________________
StiShape * StiDetectorBuilder::add(StiShape *shape)
{
  NameMapKey key(shape->getName());
  mShapeMap.insert( shapeMapValType(key, shape) );
	return shape;
}

//________________________________________________________________________________
StiDetector * StiDetectorBuilder::add(UInt_t row, UInt_t sector, StiDetector *detector)
{
  setNSectors(row,sector+1);
  if (_detectors[row][sector]) {
    printf("***ERROR*** StiDetectorBuilder::add(%d,%d,\"%s\") "
          ,row,sector,detector->getName().c_str());
    printf("  is replacing %s\n",_detectors[row][sector]->getName().c_str());
    assert( !_detectors[row][sector]);
  }  
  _detectors[row][sector] = detector;
  if (_debug || sector == 0) {
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
//________________________________________________________________________________
StiDetector * StiDetectorBuilder::add(StiDetector *detector)
{
  NameMapKey key(detector->getName());
  StiDetector *old = findDetector(detector->getName());
  if (old ) {
    cout << "StiDetectorBuilder::add(" << detector << old << "existing with the same name " << detector->getName() <<endl;
    assert(0);
  }
  mDetectorMap.insert( detectorMapValType(key, detector) );
  //complete the building of this detector element
  // in the base class nothing is actually done
  // but ROOT stuff is built in the drawable version of this class.
  detector->setGroupId(_groupId);
  if (! detector->getTrackingParameters())
  detector->setTrackingParameters(StiDefaultTrackingParameters::instance());
  return detector;
}
//________________________________________________________________________________
void StiDetectorBuilder::del(UInt_t row, UInt_t sector)
{
// * Completely removes previously added Sti detector/volume at a given row and
// * sector. Returnstrue if removal was successful orfalse otherwise.
//

 assert(row < _detectors.size());
 assert(sector < _detectors[row].size());

 StiDetector* stiDetector = getDetector(row, sector);

 assert(stiDetector);
 cout << "StiDetectorBuilder::del(" << row << "," << sector << ") detector "  << stiDetector->getName() <<endl;

 mDetectorMap.erase(stiDetector->getName());

//delete stiDetector;
 getDetectorFactory()->free(stiDetector);

 _detectors[row][sector] = 0;
}


//________________________________________________________________________________
void StiDetectorBuilder::build(StMaker& source)
{
  buildDetectors(source);

  mDetectorIterator = mDetectorMap.begin();
}

//________________________________________________________________________________
void StiDetectorBuilder::buildDetectors(StMaker& source)
{}
//________________________________________________________________________________
void StiDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP) 
{
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
//  Double_t ionization = matS->getIonization();
//   StiElossCalculator *ElossCalculator = 
//     new StiElossCalculator(matS->getZOverA(), ionization, matS->getA(), matS->getZ(),matS->getDensity());
  StiShape     *sh     = findShape(volP->GetName());
  Double_t     *xyz    = hmat->GetTranslation();
//  Double_t     *rot    = hmat->GetRotationMatrix();
  Double_t      Phi    = 0;
  //  Double_t xc,yc,zc,rc,rn, nx,ny,nz,yOff;
  StiPlacement *pPlacement = 0;
  do {//only once
    if (!shapeP->TestShapeBit(TGeoShape::kGeoTube)) 	break;
    TGeoTube *shapeC = (TGeoTube *)shapeP;
    Double_t Rmax = shapeC->GetRmax();
    Double_t Rmin = shapeC->GetRmin();
    Double_t delta=fabs(xyz[0])+fabs(xyz[1]);
    if (delta>0.1*Rmin) 				break;
    Double_t dZ   = shapeC->GetDz();
    Double_t dPhi = 2*TMath::Pi();
    Double_t dR   = Rmax - Rmin;

    if (shapeP->TestShapeBit(TGeoShape::kGeoTubeSeg)) {
      TGeoTubeSeg *shapeS = (TGeoTubeSeg *) shapeP;
      Double_t gloV[3];
      Double_t Phi1 = TMath::DegToRad()*shapeS->GetPhi1();
      Double_t Phi2 = TMath::DegToRad()*shapeS->GetPhi2();
      if (Phi2<Phi1) Phi2+=M_PI*2;
      Double_t PhiM = (Phi2+Phi1)/2;
      dPhi        = (Phi2-Phi1);
      Double_t locV[3]={cos(PhiM),sin(PhiM),0};
      hmat->LocalToMaster(locV,gloV);	       	       
      Phi = atan2(gloV[1],gloV[0]);
    }
    
    TString Name(nodeP->GetName());
    sh = new StiCylindricalShape(Name.Data(),   // Name
				   dZ,     	// halfDepth
				   dR,         	// thickness
				   Rmax,	// outerRadius
				   dPhi);    	// openingAngle
    add(sh);
    pPlacement = new StiPlacement;
    pPlacement->setZcenter(xyz[2]);
    pPlacement->setLayerRadius((Rmin+Rmax)*0.5);
    pPlacement->setLayerAngle(Phi);
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(Phi,0.5*(Rmin+Rmax), 0); 
  } while(0);

  if (!pPlacement)  {// BBox

    shapeP->ComputeBBox();

    TGeoBBox *box = (TGeoBBox*) shapeP;
    TGeoRotation geoRotation(*hmat);

    // Sti geometry deals only with simple object rotated about the z axis
    double euler_phi = geoRotation.GetPhiRotation()/180*M_PI;

    // Define "center" and normal vectors for the considered volume
    TVector3 centerVec(xyz);

    double halfThickness = box->GetDX();
    double halfWidth     = box->GetDY();
    double dz            = box->GetDZ();
    double r             = centerVec.Perp();
    double phi           = centerVec.Phi();

    // Consider two normal vectors, i.e. along the x and y axes in the local coordinate system
    TVector3 normVec(cos(euler_phi), sin(euler_phi), 0);
    TVector3 normVecPerp(-sin(euler_phi), cos(euler_phi), 0);

    double centerOrient     = centerVec.DeltaPhi(normVec);
//    double centerOrient2    = normVec.DeltaPhi(centerVec);
//    double centerOrientPerp = centerVec.DeltaPhi(normVecPerp);

    // First, select the normal vector closest to the central vector
    if ( fabs(centerVec.Dot(normVecPerp)) > fabs(centerVec.Dot(normVec)) )
    {
       halfThickness = box->GetDY();
       halfWidth     = box->GetDX();
       normVec       = normVecPerp;
    }

    // Then make sure the normal is pointing outwards
    if (normVec.Dot(centerVec) < 0) {
       normVec     *= -1;
       normVecPerp *= -1;
    }

    if (!sh) {
       // name, halfDepth, thickness, halfWidth
       sh = new StiPlanarShape(volP->GetName(), dz, 2*halfThickness, halfWidth);
       add(sh);
    }

    centerOrient = centerVec.DeltaPhi(normVec);

    double normVecMag = fabs(r*cos(centerOrient));
    double normVecOffset = r*sin(centerOrient);

    pPlacement = new StiPlacement;
    pPlacement->setZcenter(xyz[2]);
    pPlacement->setLayerRadius(r);  //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(normVec.Phi(), normVecMag, normVecOffset);
  }
  assert(pPlacement);
  StiDetector *pDetector = getDetectorFactory()->getInstance();
  TString nameP(nodeP->GetName());
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  nameP.Strip(); // GVB: Do not truncate the name: it needs to be unique
  pDetector->setName(nameP.Data());
  pDetector->setIsActive(new StiNeverActiveFunctor);
  pDetector->setShape(sh);
  pDetector->setPlacement(pPlacement); 
  pDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
  pDetector->setMaterial(matS);

  if (mThkSplit>0 && mMaxSplit>1) {//	try to split 
    StiDetVect dv;
    pDetector->splitIt(dv,mThkSplit,mMaxSplit);
    for (int i=0;i<(int)dv.size();i++) {
      int layer = getNRows();
      add(layer,0,dv[i]); 
      cout << "StiDetectorBuilder::AverageVolume build detector " << dv[i]->getName() << " at layer " << layer << endl;
    } }
  else {  
   int layer = getNRows();
   add(layer,0,pDetector); 
   cout << "StiDetectorBuilder::AverageVolume build detector " << pDetector->getName() << " at layer " << layer << endl;
  }

}

///Returns the number of sectors (or segments) in a the
///given row. Sector are expected to be azimuthally
///distributed.
//________________________________________________________________________________
UInt_t  StiDetectorBuilder::getNSectors(UInt_t row) const
{
  assert(row<_detectors.size());
  return _detectors[row].size();
}


//________________________________________________________________________________
StiDetector * StiDetectorBuilder::getDetector(UInt_t row, UInt_t sector) const
{
  assert(row<_detectors.size());
  assert(sector<_detectors[row].size());
  return _detectors[row][sector];
}

//________________________________________________________________________________
void StiDetectorBuilder::setDetector(UInt_t row, UInt_t sector, StiDetector *detector)
{
  setNSectors(row+1,sector+1);
assert(!_detectors[row][sector]);
   _detectors[row][sector] = detector;
}


void StiDetectorBuilder::Print() const
{
   std::cout << "StiDetectorBuilder::Print(): " << getName() << std::endl;
   std::copy(mDetectorMap.begin(), mDetectorMap.end(), std::ostream_iterator<DetectorMapPair>(std::cout, "\n"));
}


ostream& operator<<(ostream& os, const DetectorMapPair& detMapEntry)
{
   return os << *detMapEntry.second;
}
