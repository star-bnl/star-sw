// $Id: StTGeoHelper.cxx,v 1.1 2009/06/07 02:28:36 perev Exp $
//
//
// Class StTGeoHelper
// ------------------



#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "TROOT.h"
#include "TObjArray.h"
#include "TGeoManager.h"
#include "TGeoNavigator.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "StTGeoHelper.h"

static StTGeoHelper *gStTGeoHelper=0;

ClassImp(StTGeoHelper)
enum EMEDIUM {kISVOL =0,kIFIELD=1,kFIELDM=2,kTMAXFD=3
             ,kSTEMAX=4,kDEEMAX=5,kEPSIL =6,kSTMIN =7};
//_____________________________________________________________________________
StTGeoHelper::StTGeoHelper()
{
  memset(fBeg,0,fEnd-fBeg);  
  fExtArr = new TObjArray();
  fModuLev = 1;
}
//_____________________________________________________________________________
void StTGeoHelper::Init()
{
  StTGeoIterator it;
  it.Print("StTGeoHelper_Init");
//	Create HitShape
  const TGeoVolume *vol= *it;
  TGeoBBox *bb = (TGeoBBox*)vol->GetShape();
  bb->ComputeBBox();
  const double *ori = bb->GetOrigin();
  double z1 = ori[2]-bb->GetDZ();
  double z2 = ori[2]+bb->GetDZ();
//  double rxy = sqrt(pow(fabs(ori[0])+bb->GetDX(),2)
//                   +pow(fabs(ori[1])+bb->GetDY(),2));
  fHitShape = new StTGeoHitShape(z1,z2);

  int nHits=0;
  StVoluExt *myModu=0;
  for (;(vol=*it);++it) {
//	Check for MODULE    
    int volId = vol->GetNumber();
//		First visit
    if (it.IsFirst()) {		//First visit
//			Recognize module
      do {
	if (IsModule(vol)) 		break;
	if (fModuLev != it.GetLev()) 	break;
	StVoluExt *ext = GetExt(volId);
	if (!ext) { ext = new StVoluExt(volId); SetExt(ext);}
	ext->SetModule();
        myModu = ext; nHits=0;
 	printf("Module = %s(%d) ",vol->GetName(),volId); it.Print(0);
      } while(0);

      if (IsSensitive(vol)) {//Update HitShape
        nHits++;
        double global[8][3],local[3],D[3];
        TGeoBBox *bb = (TGeoBBox*)vol->GetShape();
        bb->ComputeBBox();
        ori = bb->GetOrigin();
        D[0] = bb->GetDX();
        D[1] = bb->GetDY();
        D[2] = bb->GetDZ();
        for (int jk=0;jk<8;jk++) {
          for (int ix=0,im=1; ix<3; ix++,im<<=1) {
	    local[ix] = ori[ix] + D[ix]* ((!!(jk&im))*2 -1);}
          it.LocalToMaster(local,global[jk]);
	} 
        z1 = 3e33; z2 = -3e33; 
        double rMax = 0;
        for (int jk=0;jk<8;jk++) {
          if (z1 > global[jk][2]) z1 = global[jk][2];
          if (z2 < global[jk][2]) z2 = global[jk][2];
          double r2 = pow(global[jk][0],2)+pow(global[jk][1],2);
          if (rMax<r2) rMax=r2;
	}
	rMax = sqrt(rMax);
        fHitShape->Update(z1,z2,rMax);
      }

    } else { 		//Last visit 

        if (!myModu) 		continue;
	if (!IsModule(vol)) 	continue;
        if (nHits) {
	  myModu->SetHitModul();
 	  printf("HitModu = %s(%d) hits=%d",vol->GetName(),volId,nHits);
	  it.Print(0);
        }  
      myModu=0; nHits=0;
    }

  }


}
//_____________________________________________________________________________
StTGeoHelper::~StTGeoHelper()
{
  delete fExtArr;
}
//_____________________________________________________________________________
StTGeoHelper *StTGeoHelper::Instance()
{
  if (!gStTGeoHelper) gStTGeoHelper = new StTGeoHelper;
  return gStTGeoHelper;
}
//_____________________________________________________________________________
StVoluExt *StTGeoHelper::GetExt(int idx) const
{
  return (StVoluExt*)((idx<fExtArr->GetSize())? (*fExtArr)[idx]:0);
}    
//_____________________________________________________________________________
void StTGeoHelper::SetExt(StVoluExt* ext)
{
  fExtArr->AddAtAndExpand(ext,ext->GetNumber());
}    
//_____________________________________________________________________________
int StTGeoHelper::IsModule(const TGeoVolume *volu)  const
{
  StVoluExt *ext = GetExt(volu->GetNumber());
  if (!ext) return 0;
  return ext->IsModule();
}    
//_____________________________________________________________________________
int StTGeoHelper::IsModule(const TGeoNode   *node)  const
{ return IsModule(node->GetVolume()); }
//_____________________________________________________________________________
int StTGeoHelper::IsHitPlane(const TGeoVolume *volu)  const
{
  StVoluExt *ext = GetExt(volu->GetNumber());
  if (!ext) return 0;
  return ext->IsHitPlane();
}    
//_____________________________________________________________________________
int StTGeoHelper::IsHitPlane(const TGeoNode   *node)  const
{ return IsHitPlane(node->GetVolume()); }

//_____________________________________________________________________________
int StTGeoHelper::IsSensitive(const TGeoVolume *volu)
{
   return  (volu->GetMedium()->GetParam(kISVOL)>0.);
}
//_____________________________________________________________________________
const TGeoVolume *StTGeoHelper::GetModule() const
{
   for (int i=0;1;i++) {
     TGeoNode *n = gGeoManager->GetMother(i); 
     if(!n) 				break;
     TGeoVolume *v=n->GetVolume();
     if (!v) 				continue;
     if (IsModule(v))  			return v;
   }
   return 0;
}
//_____________________________________________________________________________
void StTGeoHelper::Print(const char *tit) const
{
  TGeoVolume *v=0;
  if (tit) printf("StTGeoHelpe::Print(%s)\n\n",tit);   
   for (int i=gGeoManager->GetLevel();i>=0;i--) {
     TGeoNode *n = gGeoManager->GetMother(i); 
     if(!n) 				break;
     v=n->GetVolume();
     if (!v) 				continue;
     printf("/%s#%d",v->GetName(),n->GetNumber());
   }
  if (v) {
    printf("(%s)",v->GetMaterial()->GetName());
    if (IsSensitive(v)) printf("*");
  }
  printf("\n");    

}
//_____________________________________________________________________________
void StTGeoHelper::Test()
{
   gROOT->Macro("$STAR/StarDb/VmcGeometry/y2009.h");
   StTGeoHelper hlp;
   hlp.Init();


//    StTGeoIterator it;  
//    const TGeoVolume *volu=0;
//    int n = 0;
//    for (; volu=it.GetVolu();++it) 
//    {
//      if (it.IsLast()) continue;
//      n++;
//      printf("%4d - ",n); it.Print(0);
//    }

}

//_____________________________________________________________________________
//_____________________________________________________________________________
  enum {kEND,kDOWN,kNEXT,kUPP};
//_____________________________________________________________________________
//_____________________________________________________________________________
  StTGeoIterator::StTGeoIterator()
{
  fLev = 0;
  fStk[0]=0;
  fNavi = new TGeoNavigator(gGeoManager);
  fNavi->BuildCache();
  fNavi->CdTop();
  fVolu = fNavi->GetCurrentVolume();
  fNow = 1;
  fKase = kDOWN;  
  assert(fLev == fNavi->GetLevel());
}
//_____________________________________________________________________________
  StTGeoIterator::~StTGeoIterator()
{ delete fNavi; fNavi = 0;}

//_____________________________________________________________________________
StTGeoIterator &StTGeoIterator::Down()
{
  fKase = kDOWN; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIterator &StTGeoIterator::Next()
{
  fKase = kNEXT; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIterator &StTGeoIterator::Upp()
{
  fKase = kUPP; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIterator &StTGeoIterator::operator++()
{
  int nKids;
CASE:

  switch (fKase) {

  case kEND: fVolu=0;				fKase=kEND ; fNow=0; break;
    
  case kDOWN: {
      nKids = fVolu->GetNdaughters();
      if (!nKids) { 				fKase=kNEXT; fNow=2; break;}
      fNavi->CdDown(0); fLev++;
      fStk[fLev]=0;  
  assert(fLev == fNavi->GetLevel());
      fVolu = fNavi->GetCurrentVolume();
      						fKase=kDOWN; fNow=1; break;}

  case kNEXT: {
      if (!fLev) { fVolu=0; 			fKase=kEND ; fNow=0; break;}
      fNavi->CdUp(); fLev--; 
  assert(fLev == fNavi->GetLevel());
      fVolu = fNavi->GetCurrentVolume();
      nKids = fVolu->GetNdaughters();
      fStk[fLev]++; 
      if (fStk[fLev]>=nKids)  {			fKase=kNEXT ; fNow=2; break;}
      fNavi->CdDown(fStk[fLev]);fLev++; ; 
  assert(fLev == fNavi->GetLevel());
      fVolu = fNavi->GetCurrentVolume();	fKase=kDOWN; fNow=1; break;}  

  case kUPP: {
    if (!fLev) {fVolu=0; 			fKase=kEND ; fNow=0; break;}
    fNavi->CdUp(); fLev--;
    fVolu = fNavi->GetCurrentVolume();	 
  assert(fLev == fNavi->GetLevel());
    						fKase=kNEXT; goto CASE;}
  }

  return *this;
}
//_____________________________________________________________________________
const TGeoNode *StTGeoIterator::GetNode(int idx) const
{
 if (!fLev) return 0;
 if (fLev<idx) return 0;
 return fNavi->GetMother(idx);
}

//_____________________________________________________________________________
const TGeoVolume *StTGeoIterator::GetVolu(int idx) const
{
  if (!idx) return fVolu;
  const TGeoNode *node = GetNode(idx);
  if (!node) return 0;
  return node->GetVolume();
}
//_____________________________________________________________________________
void StTGeoIterator::Print(const char *tit) const
{
  const TGeoVolume *v =0;
  if (tit) printf("\nStTGeoIterator::Print(%s)\n\n",tit);   
  printf("%d ",fNow);
  for (int l=0;l<=fLev;l++) {
    v = GetVolu(fLev-l);
    int iocc = (l) ? fStk[l-1]:0;
    printf("/%s#%d",v->GetName(),iocc);
  }
  printf("(%s)",v->GetMaterial()->GetName());
  if (v->GetMedium()->GetParam(kISVOL)>0.)printf("*");  
  printf("\n");    

}
//_____________________________________________________________________________
void  StTGeoIterator::LocalToMaster(const double* local, double* master) const
{ fNavi->LocalToMaster(local,master);}

//_____________________________________________________________________________
//_____________________________________________________________________________
StTGeoHitShape::StTGeoHitShape(double zMin,double zMax)
{
  fZMin = zMin; fZMax = zMax;
  memset(fRxy,0,sizeof(fRxy));
}
//_____________________________________________________________________________
void StTGeoHitShape::Update(double z1, double z2, double rxy)
{
   assert(z1>fZMin && z1<fZMax);
   assert(z2>fZMin && z2<fZMax);

   rxy*=1.1;
   int jl = (int)((z1-fZMin)/(fZMax-fZMin)*kNZ);
   int jr = (int)((z2-fZMin)/(fZMax-fZMin)*kNZ);
   for (int jj=jl;jj<=jr;jj++) {if(fRxy[jj]<rxy) fRxy[jj]=rxy;}
   double zz = (fabs(z1) < fabs(z1))? z1:z2;

   jl = (int)((0 -fZMin)/(fZMax-fZMin)*kNZ);
   jr = (int)((zz-fZMin)/(fZMax-fZMin)*kNZ);
   int js = (jl<jr)? 1:-1;
   if (jl==jr) return;
   for (int jj=jl;jj!=jr;jj+=js) {
     double rrr = ((jj-jl)*rxy)/(jr-jl);
     if(fRxy[jj]<rrr) fRxy[jj]=rrr;
   }

}
//_____________________________________________________________________________
int  StTGeoHitShape::Inside(double z,double rxy) const
{
   if (z<=fZMin) return 0;
   if (z>=fZMax) return 0;
   int jj = (int)((z-fZMin)/(fZMax-fZMin)*kNZ);
   if (rxy >=fRxy[jj]) return 0;
   return 1;
}
