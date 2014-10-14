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
#include "StDetectorDbMaker/StiDefaultTrackingParameters.h"
#include "StThreeVector.hh"
#include "StMaker.h"
#include "StThreeVectorD.hh"
#include "TMath.h"
#include "TVector3.h"
#include "TGeoCone.h"

StiDetectorBuilder* StiDetectorBuilder::fCurrentDetectorBuilder = 0;

class StiAuxMat {
public: 
double A,Z,X0,Dens,Wt;
};

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
  assert(detector);
  setNSectors(row,sector+1);
  if (_detectors[row][sector]) {
  Error("StiDetectorBuilder::add()","%s(%d,%d) == %s"
       ,getName().c_str(),row,sector,detector->getName().c_str());

  Error("StiDetectorBuilder::add()","is replacing by %s"
       ,detector->getName().c_str());
//    assert( !_detectors[row][sector]);
  }  
  _detectors[row][sector] = detector;
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
  Error("StiDetectorBuilder::add()","Clash of the name %s",detector->getName().c_str());
//    assert(0);
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
//      cout << "StiDetectorBuilder::AverageVolume build detector " << dv[i]->getName() << " at layer " << layer << endl;
    } }
  else {  
   int layer = getNRows();
   add(layer,0,pDetector); 
   cout << "StiDetectorBuilder::AverageVolume build detector " << pDetector->getName() << " at layer " << layer << endl;
  }

}
static void eigen2(double G[3], double lam[2], double eig[2]);
//________________________________________________________________________________
int StiDetectorBuilder::AverageVolume(const char *fullPath) 
{
static int nCall=0; nCall++;
int botOho =(strstr(fullPath,"PXRB") || strstr(fullPath,"PXLB"));
if (botOho) StiDebug::Break(-1946);   
  gGeoManager->cd(fullPath);
  TGeoNode* node = gGeoManager->GetCurrentNode();
  assert(node);
  TGeoVolume *volu = node->GetVolume();
  assert(volu);

  TGeoShape *shapeG = volu->GetShape();
  assert(shapeG);
  StiShape *shapeS = 0; StiPlacement *placeS=new StiPlacement;

  StiAuxMat aux;
  int iAns =  AveMate(volu,aux);
//  assert(iAns<1000);
  TVector3 local,global;
  double zMin,zMax,rMin,rMax,phiMin,phiMax;
  shapeG->GetAxisRange(3,zMin,zMax);
  local[2] = 0.5*(zMin+zMax);
  gGeoManager->LocalToMaster(&local[0],&global[0]);
  do {//only once
    if (!shapeG->IsCylType())		break;
    double zLoc[3]={0,0,1},zGlo[3];
//	Is it the same direction of z axis?
    gGeoManager->LocalToMasterVect(zLoc,zGlo);
    if (fabs(zGlo[0]) > 1e-3) 		break;
    if (fabs(zGlo[1]) > 1e-3) 		break;
    double par[4];
//    shapeG->GetAxisRange(1,rMin,rMax);
    shapeG->GetBoundingCylinder(par);
    rMin = sqrt(par[0]);
    rMax = sqrt(par[1]);
    if (fabs(global[0]) > rMax*1e-3) 	break;
    

    if (fabs(global[1]) > rMax*1e-3) 	break;
//    shapeG->GetAxisRange(2,phiMin,phiMax);
    phiMin = par[2];
    phiMax = par[3];

    double dPhi = phiMax-phiMin; 
    if (dPhi<  0) dPhi+=360;
    if (dPhi>360) dPhi-=360;

    dPhi*=TMath::DegToRad();
    double phi = (phiMax+phiMin)/2*TMath::DegToRad();


    shapeS = new StiCylindricalShape(node->GetName(),   	// Name
				   (zMax-zMin)/2,     	// halfDepth
				   (rMax-rMin),         // thickness
				   rMax,		// outerRadius
				   dPhi);    		// openingAngle

    placeS->setZcenter(global[2]);
    placeS->setLayerRadius((rMin+rMax)/2);
    placeS->setLayerAngle(phi);
    placeS->setRegion(StiPlacement::kMidRapidity);
    placeS->setNormalRep(phi,(rMin+rMax)/2, 0); 
    

  } while(0);



  if (!shapeS)  {// BBox
    shapeG->ComputeBBox();

    TGeoBBox *box = (TGeoBBox*)shapeG;
    gGeoManager->LocalToMaster(box->GetOrigin(),&global[0]);
    double D[3] = {box->GetDX(),box->GetDY(),box->GetDZ()};

    double corner[3];
    TVector3 gorner[8];
    for (int msk=0;msk<8;msk++) {
      for (int ix=0,im=1;ix<3;ix++,im*=2) {
        corner[ix] = (msk&im)? -D[ix]:D[ix];
      }
      gGeoManager->LocalToMasterVect(corner,&(gorner[msk])[0]);
    }

    double G[3]={0};
    for (int i=0;i<8;i++) {
      G[0]+=gorner[i][0]*gorner[i][0];
      G[1]+=gorner[i][0]*gorner[i][1];
      G[2]+=gorner[i][1]*gorner[i][1];
    }

    TVector3 eig[3]; eig[2][2]=1;
    double lam[2];
    eigen2(G, lam,&(eig[1][0]));	// lam[0] related to Y, and lam[1] to X

    if (fabs(lam[0]-lam[1]) <= 0.1*(lam[0]+lam[1])) {// no special direction
      eig[1][0] = global[1]; eig[1][1] = -global[0]; eig[1].SetMag(1.);
    }

    eig[0][0] = -eig[1][1];eig[0][1] = eig[1][0];

    double offset,rNorm;
    TVector3 dca,swp;
    for (int jk=0;jk<2;jk++) {
      if ( eig[0].Dot(global)<0) {eig[0]*=-1.; eig[1]*=-1.;} 
      offset = global.Dot(eig[1]);
      dca  = global-eig[1]*offset;
      rNorm  = dca.Perp();
      if (rNorm*rNorm > lam[1]/4) break;
      assert(!jk);
      swp   = eig[0]; eig[0]=eig[1]; eig[1]=swp;
      swp[0]= lam[0]; lam[0]=lam[1]; lam[1]=swp[0];
    }



    double miMax[2][3]={{1e11,1e11,1e11},{-1e11,-1e11,-1e11}};
    for (int i=0;i<8;i++) {
      for (int j=0;j<3;j++) {
        double dot = gorner[i].Dot(eig[j]);
        if (miMax[0][j]>dot) miMax[0][j]=dot;
        if (miMax[1][j]<dot) miMax[1][j]=dot;
    } }

    double dZ = (miMax[1][2]-miMax[0][2]); assert(fabs(dZ)<1e3);
    double dY = (miMax[1][1]-miMax[0][1]); assert(fabs(dY)<1e3);
    double dX = (miMax[1][0]-miMax[0][0]); assert(fabs(dX)<1e3);

    shapeS = new StiPlanarShape(volu->GetName(), 
                           dZ/2,	//dZ/2
			   dX,		//thickness(dX)
                           dY/2);	//dY/2

    placeS->setZcenter(global[2]);
    placeS->setLayerRadius(global.Perp());
    placeS->setLayerAngle(global.Phi());
    placeS->setRegion(StiPlacement::kMidRapidity);
    placeS->setNormalRep(dca.Phi(),rNorm, offset); 
  }

  assert(placeS);
 
  double gVolu = shapeG->Capacity();
  double sVolu = shapeS->getVolume();
  assert(sVolu > 0.99*gVolu);
  assert(sVolu <  1e3 *gVolu);

  TGeoMaterial *mateP = volu->GetMaterial();
  StiMaterial *mateS = new StiMaterial(mateP->GetName(),
					      aux.Z,
					      aux.A,
					      aux.Dens*gVolu/sVolu,
					      aux.X0  *sVolu/gVolu);



  StiDetector *pDetector = getDetectorFactory()->getInstance();

  TString nameP(fullPath);
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  nameP.Strip(); 			// GVB: Do not truncate the name: it needs to be unique
  pDetector->setName(nameP.Data());
  pDetector->setIsActive(new StiNeverActiveFunctor);
  pDetector->setShape(shapeS);
  pDetector->setPlacement(placeS); 
  pDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
  pDetector->setMaterial(mateS);

  if (mThkSplit>0 && mMaxSplit>1) {//	try to split 
    StiDetVect dv;
    pDetector->splitIt(dv,mThkSplit,mMaxSplit);
    for (int i=0;i<(int)dv.size();i++) {
      int layer = getNRows();
      add(layer,0,dv[i]); 
//      cout << "StiDetectorBuilder::AverageVolume build detector " << dv[i]->getName() << " at layer " << layer << endl;
    } }
  else {  
   int layer = getNRows();
   add(layer,0,pDetector); 
   cout << "StiDetectorBuilder::AverageVolume build detector " << pDetector->getName() << " at layer " << layer << endl;
  }
  return iAns;

}

//_____________________________________________________________________________
static void eigen2(double G[3], double lam[2], double eig[2])
{
  double spur = G[0]+G[2];
//double det  = G[0]*G[2]-G[1]*G[1];
  double dis  = (G[0]-G[2])*(G[0]-G[2])+4*G[1]*G[1];
  dis = sqrt(dis);
  if (lam) {
    lam[0] = 0.5*(spur+dis);
    lam[1] = 0.5*(spur-dis);
  }
  if (!eig) return;
  double g[3]={G[0]-G[2]-dis,2*G[1],G[2]-G[0]-dis};
  int kase =0;
  for (int i=1;i<3;i++) { if (fabs(g[i])> 1.001*fabs(g[kase])) kase = i;}
  if (fabs(g[kase])<1e-11) kase = 3;

  switch(kase) {
    case 0: eig[0] = g[1]/g[0]; eig[1]=-1; break;
    case 1: eig[1] = g[0]/g[1]; eig[0]=-1; break;
    case 2: eig[1] = g[1]/g[2]; eig[0]=-1; break;
    case 3: eig[0] = 1        ; eig[1]= 0; 
  }
  double nor = sqrt(eig[0]*eig[0]+eig[1]*eig[1]);
  if (nor>1e-11) {
    int j=(fabs(eig[0])>fabs(eig[1]))? 0:1;
    if(eig[j]<0) nor = -nor;
    eig[0]/=nor;eig[1]/=nor;}
  else {
    eig[0]=1;eig[1]=0;
  }
  double lam0=0;
  if (fabs(eig[0])>=fabs(eig[1])) 	{lam0 = G[0] + G[1]*eig[1]/eig[0];}
  else 					{lam0 = G[2] + G[1]*eig[0]/eig[1];}
  assert (fabs(lam[0]-lam0)<=1e-6*fabs(lam[0]+lam0));



}
///Returns the number of sectors (or segments) in a the
///given row. Sector are expected to be azimuthally
///distributed.
//_____________________________________________________________________________
 UInt_t  StiDetectorBuilder::getNSectors(UInt_t row) const
{
  assert(row<_detectors.size());
  return _detectors[row].size();
}


//_____________________________________________________________________________
 StiDetector * StiDetectorBuilder::getDetector(UInt_t row, UInt_t sector) const
{
  assert(row<_detectors.size());
  assert(sector<_detectors[row].size());
  return _detectors[row][sector];
}

//_____________________________________________________________________________
 void StiDetectorBuilder::setDetector(UInt_t row, UInt_t sector, StiDetector *detector)
{
  setNSectors(row+1,sector+1);
assert(!_detectors[row][sector]);
   _detectors[row][sector] = detector;
}
//_____________________________________________________________________________

static int IsSensitive(const TGeoVolume *volu)
{
  const TGeoMedium *tm = volu->GetMedium();
  if (!tm) return 0;
  return (tm->GetParam(0)>0.);
}



//_____________________________________________________________________________
int StiDetectorBuilder::AveMate(TGeoVolume *vol,StiAuxMat &aux) 
{
// Analytical computation of the average material.
   double capacity = vol->Capacity();
   double capaOnly = capacity;
   int nd = vol->GetNdaughters();
   TGeoVolume *daughterV;
   TGeoNode   *daughterN;
   double sumA=0,sumZ=0,sumX0=0,sumWt=0;
   int iAns = 0;
   for (int i=0; i<nd; i++) {
      daughterN = vol->GetNode(i);
      daughterV = daughterN->GetVolume();
      iAns++;
      if (IsSensitive(daughterV)) 	iAns+=1000;
      if (daughterN->IsOverlapping()) 	iAns+=100000;
      double dauCapa = daughterV->Capacity();
      capaOnly -= dauCapa;
      StiAuxMat dauMat;
      iAns+=AveMate(daughterV,dauMat);
      if (dauMat.Z<=0) continue;
      sumWt += dauMat.Wt;
      sumA  += dauMat.Wt/dauMat.A;
      sumZ  += dauMat.Wt/dauMat.A*dauMat.Z;
      sumX0 += dauCapa/dauMat.X0;
   }
   double density = 0.0;
   TGeoMaterial* tgMat = vol->GetMaterial();
   double Aonly = 1e-11,Zonly=0,X0only=1e11;

   if (!vol->IsAssembly() && tgMat) { 
     density = tgMat->GetDensity();
     Aonly  = tgMat->GetA();
     Zonly  = tgMat->GetZ();
     X0only = tgMat->GetRadLen();
   }
   double wtOnly = capaOnly * density;
   aux.Wt = sumWt + wtOnly;
   aux.Dens = aux.Wt/capacity;
   aux.A    = aux.Wt/(wtOnly/Aonly+sumA);
   aux.Z  = (wtOnly*(Zonly/Aonly)+sumZ)/aux.Wt *aux.A;
   aux.X0 = capacity/(capaOnly/X0only+sumX0); 
   return iAns;
}

