// $Id: StTGeoProxy.cxx,v 1.13 2016/11/29 19:09:30 perev Exp $
//
//
// Class StTGeoProxy
// ------------------



#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string>
#include <map>
#include <assert.h>
#include <set>
#include "TROOT.h"
#include "TString.h"
#include "TMath.h"
#include "TObjArray.h"
#include "TGeoManager.h"
#include "TGeoNavigator.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoTube.h"
#include "TGeoMatrix.h"
#include "TCernLib.h"
#include "TVector3.h"

#include "StTGeoProxy.h"
#include "StMultiKeyMap.h"

int StTGeoProxy::StTGeoProxy::fgKount[3] = {0};


enum {kMaxVolId = 1000000};

//_____________________________________________________________________________
int GetVoluId(const TGeoVolume *vol) 
{
   return vol->GetNumber()+kMaxVolId*vol->GetUniqueID();
}

class myTVector3 : public TVector3 {

public:


   myTVector3(Double_t x = 0.0, Double_t y = 0.0, Double_t z = 0.0):TVector3(x,y,z){;}
   // The constructor.

   myTVector3(const Double_t *d):TVector3(d){;}
   myTVector3(const Float_t *f):TVector3(f){;}
   				// Constructors from an array

   operator TVector3 &() {return *(TVector3*)this;}
   operator const TVector3 &() const {return *(TVector3*)this;}
   				// The copy constructor.
   myTVector3(const TVector3 &v):TVector3(v){;}
   				// Assignment
   myTVector3 & operator = (const   TVector3 &v){ *((TVector3*)this)=v; return *this;}
   myTVector3 & operator = (const myTVector3 &v){ *((TVector3*)this)=v; return *this;}

   myTVector3 & operator = (const  double *d){ SetXYZ(d[0],d[1],d[2]); return *this;}
   myTVector3 & operator = (const   float *f){ SetXYZ(f[0],f[1],f[2]); return *this;}

   void Set(const Double_t *d){SetXYZ(d[0],d[1],d[2]);}
   void Set(const Float_t  *f){SetXYZ(f[0],f[1],f[2]);}
   void Get(      Double_t *d){d[0]=X();d[1]=Y();d[2]=Z();}
   void Get(      Float_t  *f){f[0]=X();f[1]=Y();f[2]=Z();}



const double* GetArrD() const { fD[0]=X();fD[0]=Y();fD[0]=Z(); return fD;}
const  float* GetArrF() const { fF[0]=X();fF[0]=Y();fF[0]=Z(); return fF;}

protected:
mutable double fD[3];
mutable float  fF[3];

};

#define DOT(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])

static StTGeoProxy *gStTGeoProxy=0;
typedef std::map<const TGeoVolume*,int> myVoluMap ;

ClassImp(StTGeoProxy)
ClassImp(StVoluInfo)
ClassImp(StHitPlaneInfo)
ClassImp(StHitPlane)
ClassImp(StHitTube )

enum EMEDIUM {kISVOL =0,kIFIELD=1,kFIELDM=2,kTMAXFD=3
             ,kSTEMAX=4,kDEEMAX=5,kEPSIL =6,kSTMIN =7};

struct myMap {int id; const char *name;};
static myMap gMyMod[] = {
{kUnknownId             ,""	},
{kTpcId       		,"TPCE"	},
{kSvtId       		,"SVTT"	},
{kRichId      		,"RICH"	},
{kFtpcWestId  		,"FTPC"	},
{kFtpcEastId  		,"FTPC"	},
{kTofId       		,"BTOF"	},
{kCtbId       		,"Ctb"	},
{kSsdId       		,"SFMO"	},
{kBarrelEmcTowerId     	,"CALB"	},
{kBarrelEmcPreShowerId 	,"CALB"	},
{kBarrelSmdEtaStripId  	,"CALB"	},
{kBarrelSmdPhiStripId  	,"CALB"	},
{kEndcapEmcTowerId     	,"ECAL"	},
{kEndcapEmcPreShowerId 	,"ECAL"	},
{kEndcapSmdUStripId    	,"ECAL"	},
{kEndcapSmdVStripId    	,"ECAL"	},
{kZdcWestId   		,""	},
{kZdcEastId   		,""	},
{kMwpcWestId  		,""	},
{kMwpcEastId  		,""	},
{kPhmdCpvId   		,"PHMD"	},
{kPhmdId      		,"PHMD"	},
{kPxlId       		,"PXMO"	},
{kIstId       		,"IBMO"	},
{kFgtId       		,"FGTM"	},
{kEtrId       		,"ETRV"	},
{kFpdWestId   		,"FBOX"	},
{kFpdEastId   		,"FBOX"	},
{kFmsId       		,""	},
{kRpsId       		,""	},
{kMtdId       		,"MMBL"	},
#ifdef kFtsIdentifier
{kFtsId       		,"FTSM"	},
#endif
{0,0				}};
  		  
void myBreak(int i) 
{ 
static int iCatch=-999;
  if (iCatch!=i) return;
  printf("myBreak:iCatch=%d\n",i);
}
//_____________________________________________________________________________
StTGeoProxy::StTGeoProxy()
{
  assert(!gStTGeoProxy);
  gStTGeoProxy=this;
  memset(fBeg,0,fEnd-fBeg);  
  fVoluInfoArr = new StVoluInfoMap;
  fHitPlaneArr = new TObjArray();
  fOpt = 1;
  fSeedHits =        new StVoidArr();
  fAllHits  =        new StVoidArr();
}
//_____________________________________________________________________________
int StTGeoProxy::Load(const char *geo)
{
  if (gGeoManager) { // Geom already there
    Warning("Load","TGeoManager(%s,%s) is already there"
           ,gGeoManager->GetName(),gGeoManager->GetTitle());
    return -1; 
  }
  TString ts("$STAR/StarDb/AgiGeometry/");
  ts+=geo; ts+=".h";
  int ierr=0;
  Long_t ans = gROOT->Macro(ts, &ierr); if (ans){};
  assert(!ierr);
  return ierr;
}
//_____________________________________________________________________________
void StTGeoProxy::Init(int mode)
{
  fMode = mode;
  InitInfo();
  if (fMode&2) InitHitPlane();
//  if (fMode&1) InitHitShape();
}
//_____________________________________________________________________________
void StTGeoProxy::InitLayers(StDetectorId did)
{
  StvSetLayer sl;
  Edit(did,&sl);
}
//_____________________________________________________________________________
void StTGeoProxy::Finish()
{
// Avoid deleting of TGeoManager
  gGeoManager = 0;
}
//_____________________________________________________________________________
void StTGeoProxy::InitInfo()
{
  gGeoManager->CdTop();
  std::map <int,int> moduMap;  
  StTGeoIter it;
  it.Print("StTGeoProxy_Init");
  const TGeoVolume *vol= *it;
  StVoluInfo *myModu=0;
  for (;(vol=*it);++it) {
    vol->GetShape()->ComputeBBox();
    int volId = ::GetVoluId(vol);
//		First visit
    if (it.IsFirst()) {		//First visit
//			Recognize module
//			Check for MODULE    
      do {
        myModu = 0;
	if (!IsModule(vol) && !it.IsOmule())	break;
	StVoluInfo *ext = SetFlag(vol,StVoluInfo::kModule);
        if (DetId(vol->GetName())) ext->SetMODULE();
        assert(IsModule(vol));
        myModu = ext; 
//	printf("Module = %s(%d) ",vol->GetName(),volId); it.Print(0);
      } while(0);

      if (myModu) 		continue;
      if (!IsSensitive(vol)) 	continue;
      for (int idx=1;1;idx++) {
        const TGeoVolume *myVolu = it.GetVolu(idx);
        if (!myVolu) 	break;
        StVoluInfo *inf = IsModule(myVolu);
        if (!inf)    	continue;
        inf->AddSens(); break;
      }
  }


    if (it.IsLast()) { 		//Last visit 

//		Define for MODULE (capital letters, module with hits)   
        myModu = IsModule(vol); 
        if (!myModu) continue;
        if (!myModu->GetSens()) 	{// Module without hits or not active now
          if (!(moduMap[volId])) 	{//first time 
 	     printf("Module = %s(%d) ",vol->GetName(),volId); it.Print(0);
	     moduMap[volId]++;
	  }
        } else 		{// Module with hits. It is MODULE
	  SetFlag(vol,StVoluInfo::kHitted);
          TString ts = (IsActive(vol))? "*":" ";
 	  printf("MODULE%s= %s(%d) Sens=%d",ts.Data(),vol->GetName(),volId,myModu->GetSens());
	  it.Print(0);
        }  
      
    }
  }
  




}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::SetModule (const char *voluName, int akt)
{
  const char *volName = voluName;
  StDetectorId did = DetId(volName);
  if (did) volName = ModName(did); 
  TGeoVolume *vol = gGeoManager->FindVolumeFast(volName);
  if (!vol) { Warning("SetModule","Volume %s Not Found",voluName);return 0;}

  StVoluInfo *info = SetFlag(vol,StVoluInfo::kModule,akt);
  if (did) info->SetMODULE();
  return info;
}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::SetActive (const char *voluName, int akt,StActorFunctor *af)
{
  StVoluInfo *inf = SetModule(voluName,1);
  if (!inf) return 0;
  inf->SetBit(StVoluInfo::kActive,akt);
  if (!akt) {inf->SetActiveFunctor(0); return inf;}
  inf->SetActiveFunctor(af);

// 		Now care about all parents. They should be active as well
  StTGeoIter it;
  const TGeoVolume *vol=0;
  for (;(vol=*it);++it) {
    if (strcmp(voluName,vol->GetName()))continue;
    if (!it.IsFirst()) 			continue; //Not a First visit
    const TGeoVolume *myVol ;
    for ( int idx=1; (myVol = it.GetVolu(idx));idx++) {
      if (IsActive(myVol)) continue;
      SetActive(myVol->GetName(),1,0);
    }
  }  
  return inf;
}
//_____________________________________________________________________________
StVoluInfo * StTGeoProxy::SetActive (StDetectorId did,int akt,StActorFunctor *af)
{
  const char *modu = ModName(did);
  if (!*modu)  { Warning("SetActive","DetId %d Unknown",did);return 0;}
  if (af) af->SetDetId(did);
  StVoluInfo *vi = SetActive(modu,akt,af); 
  if (!vi) return 0;
  Long64_t mask = 1; mask = mask<<(int)did;
  if (akt) { fActiveModu |=  mask; }
  else     { fActiveModu &= ~mask; }
  return vi;
}
//_____________________________________________________________________________
int StTGeoProxy::IsActive (StDetectorId did) const
{
  return ((fActiveModu & ((Long64_t)1)<<did)!=0);
}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::IsActive (const TGeoVolume *volu) const
{
 if (volu) {
   return (IsFlag(volu,StVoluInfo::kActive));
 } else {
   const TGeoVolume *modu = GetModu();  
   if (!modu) return 0;
   return (IsFlag(modu,StVoluInfo::kActive));
 }
}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::SetFlag(const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg,int act)
{
  int volId = ::GetVoluId(volu);
  StVoluInfo *inf = GetInfo(volId);
  if (!inf && !act) return 0;
  int kase = 0;
  if (inf) kase = inf->Kind();
  if (flg >= StVoluInfo::kHitPlane) kase += 10;
  switch (kase) {
    case 0: 			   inf = new StVoluInfo(    volId); break;
    case 10:  ; 
    case StVoluInfo::kVoluInfo+10: inf = new StHitPlaneInfo(volId); break;
  }
  SetInfo(inf); inf->SetBit(flg,act);
  return inf;
}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::IsFlag(const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg) const
{
  int volId = ::GetVoluId(volu);
  StVoluInfo *inf = GetInfo(volId);
  if (!inf ) return 0;

  if(inf->TestBit(flg)==0) return 0;
  return inf;
}
//_____________________________________________________________________________
int StTGeoProxy::Edit(StDetectorId did,StActorFunctor *af)
{
  
  StTGeoIter it;
  StDetectorId detId=kUnknownId;
  int nEdit=0;
  const TGeoVolume *vol=0,*myModu=0;
  for (;(vol=*it);++it) {

    if ( it.IsLast() && (vol == myModu)) myModu=0;
    if (!it.IsFirst()) {continue;}		//First visit only
    StVoluInfo *inf = IsModule(vol);
    if (inf) {
      if (!IsActive(vol)) 	{myModu=0; it.Skip();	continue;}
      detId = DetId(vol->GetName());
      if (!detId) 		{			continue;}
      if (!did	) 		{myModu =vol;		continue;}
      if (did != detId) 	{myModu=0; it.Skip();	continue;}
      myModu =vol;
    }
    if (!myModu) 	continue;
    const char *path = it.GetPath();
    gGeoManager->cd(path);
    af->SetDetId(detId);
    int ans = (*af)();
    if (ans>0) nEdit++;
  }
  return nEdit;
}
//_____________________________________________________________________________
void StTGeoProxy::InitHitShape()
{
  
  fHitShape = new StTGeoHitShape();
  for (int pass=0;pass<2;pass++) {
  StTGeoIter it;
    for (const TGeoVolume *vol=0;(vol=*it);++it) {
      vol->GetShape()->ComputeBBox();
  //		First visit
      if (!it.IsFirst()) continue;	//First visit only

      if(IsModule(vol) && !IsActive(vol)) {it.Skip();continue;}
  //			Update HitShape
      if (!IsSensitive(vol)) continue;		
      if (!IsHitPlane(vol) ) continue;		

      double global[8][3],local[3],D[3];
      TGeoBBox *bb = (TGeoBBox*)vol->GetShape();
      bb->ComputeBBox();
      const double *ori = bb->GetOrigin();
      D[0] = bb->GetDX();
      D[1] = bb->GetDY();
      D[2] = bb->GetDZ();
      for (int jk=0;jk<8;jk++) {
	for (int ix=0,im=1; ix<3; ix++,im<<=1) {
	  local[ix] = ori[ix] + D[ix]* ((!!(jk&im))*2 -1);}
	it.LocalToMaster(local,global[jk]);
      } 
      double z1 = 3e33, z2 = -3e33; 
      double rMax = 0;
      for (int jk=0;jk<8;jk++) {
	if (z1 > global[jk][2]) z1 = global[jk][2];
	if (z2 < global[jk][2]) z2 = global[jk][2];
	double r2 = pow(global[jk][0],2)+pow(global[jk][1],2);
	if (rMax<r2) rMax=r2;
      }
           
      rMax = sqrt(rMax);
      if (!pass) {
      		//calculation only zMin & zMax
        fHitShape->Update(z1,z2,0);}
      else {
        fHitShape->Update(z1,z2,rMax);
        fHitShape->Update(z1,z2,0.1);// It is a HACK (VP)
      }
    }
  }
  fHitShape->Smooth(-100,100);
  fHitShape->Print();

}
//_____________________________________________________________________________
void StTGeoProxy::InitHitPlane(StActorFunctor *act)
{  
  StTGeoIter it;
  StDetectorId detId=kUnknownId;
  const TGeoVolume *vol=0;
  for (;(vol=*it);++it) {	//Loop to create StHitPlaneInfo for sensitive & active volumes
    if (!it.IsFirst()) {continue;}		//First visit only
    if (IsModule(vol)) {
      if (!IsActive(vol)) {it.Skip();continue;}
      detId = DetId(vol->GetName());
      if (!detId) 	continue;;
    }
    vol->GetShape()->ComputeBBox();
//		First visit
//			try to make HitPlaneInfo  
    if(!IsSensitive(vol)) continue;
    StHitPlaneInfo *hpi = MakeHitPlaneInfo(it);
    hpi->SetDetId(detId);
  }

  it.Reset();
  detId=kUnknownId;
  for (;(vol=*it);++it) {
    if (!it.IsFirst()) {continue;}		//First visit only
    if (IsModule(vol)) {
      if (!IsActive(vol)) {it.Skip();continue;}
      detId = DetId(vol->GetName());
      if(!detId) 	continue;
    }
    if(!IsSensitive(vol)) continue;
    StHitPlaneInfo *hpi = IsHitPlane(vol);
    if (!hpi) 		continue;
    StHitPlane *hp = hpi->MakeHitPlane(it,act);
    if(!hp) 		continue;
    hp->SetDetId(detId);
    
  }

}
//_____________________________________________________________________________
int StTGeoProxy::SetHitErrCalc(StDetectorId modId,TNamed *hitErrCalc
                               ,StActorFunctor *sel)

{
  assert(IsActive(modId));
  const char *modu = ModName(modId);
  assert(modu);
  int kount=0;
  TGeoVolume *moduVol = gGeoManager->FindVolumeFast(modu);
  if (!moduVol) return 0;
  StTGeoIter it;
  const TGeoVolume *vol=0;

  for (;(vol=*it);++it) { if (vol==moduVol) break;}

  if (!vol) 		return 0;
  if (!IsMODULE(vol))	return 0; 
  if (!IsActive(vol))	return 0; 
  ++it;

  for (;(vol=*it);++it) {
    if (vol==moduVol && (it.IsFirst() || it.IsLast())) 	break;
    if (!it.IsFirst()) 		continue;
    if (!IsSensitive(vol))	continue;
    StHitPlaneInfo* hpi = IsHitPlane(vol);
    if (!hpi) 			continue;
    StHitPlane* hp = hpi->GetHitPlane (it.GetPath()); 
    if (!hp) 			continue;
    if (hp->GetHitErrCalc()==hitErrCalc) 	continue;

    if (sel) {//check selector
      gGeoManager->cd(it.GetPath());
      sel->SetDetId(modId);
      if (!sel->Operator(hp->GetPnt()))	continue;
    }
    hp->SetHitErrCalc(hitErrCalc);
    kount++;
  }
  return kount;
}
//_____________________________________________________________________________
StTGeoProxy::~StTGeoProxy()
{
  delete fVoluInfoArr;
}
//_____________________________________________________________________________
StTGeoProxy *StTGeoProxy::Instance()
{
  if (!gStTGeoProxy) gStTGeoProxy = new StTGeoProxy;
  return gStTGeoProxy;
}
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::GetInfo(int idx) const
{
  StVoluInfoMapIter it = fVoluInfoArr->find(idx);
  if (it == fVoluInfoArr->end()) return 0;
  return (*it).second;
}    
//_____________________________________________________________________________
StVoluInfo *&StTGeoProxy::GetINFO(int idx) 
{
  StVoluInfo *&inf = (*fVoluInfoArr)[idx];
  if (inf) return inf;
  inf = new StVoluInfo(idx);
  return inf;
}    
//_____________________________________________________________________________
void StTGeoProxy::SetInfo(StVoluInfo* ext)
{
  int volId = ext->GetVoluId();
  StVoluInfo *&inf = (*fVoluInfoArr)[volId];
  if (inf==ext) return;
  if (inf) delete inf;
  inf = ext;
}    
//_____________________________________________________________________________
void StTGeoProxy::AddHitPlane(StHitPlane* pla)
{
  fHitPlaneArr->Add(pla);
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::IsModule(const TGeoVolume *volu)  const
{
  int id = ::GetVoluId(volu);
  StVoluInfo *ext = GetInfo(id);
  if (!ext) return 0;
  if (!ext->IsModule()) return 0;
  return ext;
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::IsMODULE(const TGeoVolume *volu)  const
{
  int id = ::GetVoluId(volu);
  StVoluInfo *ext = GetInfo(id);
  if (!ext) return 0;
  if (!ext->IsMODULE()) return 0;
  return ext;
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoProxy::IsModule(const TGeoNode   *node)  const
{ return IsModule(node->GetVolume()); }
//_____________________________________________________________________________
StHitPlaneInfo* StTGeoProxy::IsHitPlane(const TGeoVolume *volu) const
{
  int id = ::GetVoluId(volu);
  StVoluInfo *ext = GetInfo(id);
  if (!ext) 			return 0;
  if (!ext->IsHitPlane())	return 0;
  return (StHitPlaneInfo*)ext;
}    
//_____________________________________________________________________________
StHitPlaneInfo* StTGeoProxy::IsHitPlane(const TGeoNode   *node) const
{ return IsHitPlane(node->GetVolume()); }

//_____________________________________________________________________________
int StTGeoProxy::MayHitPlane(const TGeoVolume *volu)  const
{
  enum {kHow=4};

  const TGeoShape* sh=volu->GetShape() ;
  if (!sh) 			return -1;
  if (!sh->IsValidBox()) 	return -1;     
  int myKode=0;
  int kase = sh->IsCylType() ;
  switch (kase) {
    case 0://Plane case
    {
      const TGeoBBox *bb = (const TGeoBBox*)sh;
      double dd[3]={bb->GetDX(),bb->GetDY(),bb->GetDZ()};
      int jMin =0,jMax=0,jMed=0;
      for (int j=1;j<3;j++) { if (dd[j]<dd[jMin]) jMin=j;}
      for (int j=1;j<3;j++) { if (dd[j]>dd[jMax]) jMax=j;} 
      for (int j=1;j<3;j++) { if (j==jMin) continue;if (j==jMax) continue;jMed=j;}
      myKode = (jMin+1)+ 10*(jMed+1) + 100*(jMax+1);
      if (dd[jMin]*kHow>dd[jMed]) myKode = 0;
      break;
    }
    case 1: //Tube case
    {
      double par[9];
      sh->GetBoundingCylinder(par);
      const TGeoBBox *bb = (const TGeoBBox*)sh;
      par[4] = bb->GetDZ();
      par[0] = sqrt(par[0]);
      par[1] = sqrt(par[1]);
      if ((par[1]-par[0])*kHow <      (2*par[4])) myKode =   4;		//thin walls
      if ((par[1]-par[0])      > kHow*(2*par[4])) myKode = 123;		//disk
    }
  }
  return myKode;
}
//_____________________________________________________________________________
StHitPlaneInfo *StTGeoProxy::MakeHitPlaneInfo(const StTGeoIter &it) 
{
static int nCall=0; nCall++;   

   const TGeoVolume *myVolu = *it;
   if (!IsSensitive(myVolu)) 			return 0;
   const TGeoVolume *volu=0;
   StHitPlaneInfo *bhp=0;
   for (int up=0;(volu=it.GetVolu(up));up++) {

     int ax = MayHitPlane(volu);
     if (ax<0) 	continue;

     const TGeoShape *sh = volu->GetShape();
     const TGeoBBox *bb = (const TGeoBBox*)sh;
     int iv = ::GetVoluId(volu);
     StVoluInfo *ext = GetInfo(iv);
     int kase = 0;
     if (ext) kase = ext->Kind();
     switch(kase) {

       case 0: //no extention
       case 1: {//basic extention
	 bhp = new StHitPlaneInfo(iv);
	 if (ext) *bhp = *ext;
	 bhp->SetHitPlane();
         bhp->SetAxis(ax);
	 TCL::ucopy(bb->GetOrigin(),bhp->fOrg,3);
         MakeDir(ax,bhp->fDir);
	 SetInfo(bhp); 
	 break;}

       case 2: //HitPlane  extention
	 bhp = (StHitPlaneInfo*)ext; 
	 break;
       default: assert(0 && " Wrong kind of StVoluInfo");
     }
     break;
  }
  if (bhp) return bhp;
  printf(" ***Warning: HitPlane not found for:"); it.Print(0);
  return 0;
}
//_____________________________________________________________________________
int StTGeoProxy::MakeDir(int kode, float dir[3][3])
{
  memset(dir[0],0,sizeof(dir[0][0])*3*3);

  int ax1 = (kode%10);
  if (ax1>0 && ax1<=3) {
    myTVector3 myDir[3];
    myDir[0][((kode    )%10)-1]=1;
    myDir[2][((kode/100)%10)-1]=1;
    myDir[1] = myDir[2].Cross(myDir[0]);
    for (int i=0;i<3;i++) {myDir[i].Get(dir[i]);}
  }else if(ax1==4) {
    for (int jk=0;jk<3;jk++){dir[jk][jk]=1;}

  }else {;}

  return 0;
}
//_____________________________________________________________________________
void StTGeoProxy::Summary()
{
   int np = StTGeoProxy::fgKount[2];
   int nh = StTGeoProxy::fgKount[1];
   int ni = StTGeoProxy::fgKount[0]-nh;
   printf("/nStTGeoProxy::Summary() Info=%d HitPlaneInfo=%d HitPlane=%d created\n\n",ni,nh,np);
}

//_____________________________________________________________________________
void StHitPlane::SetLayer()
{
 static StTGeoProxy *tg = StTGeoProxy::Inst();
 const float *pnt = GetPnt();
 double myPnd[3]={pnt[0],pnt[1],pnt[2]};
 const float *dir = GetDir(pnt)[0];
 double myDir[3] = {-dir[0],-dir[1],-dir[2]};
 assert(DOT(myPnd,myDir)<0);

 fNex = tg->Look(100,myPnd,myDir);
 assert(fNex>1e-2);
}
//_____________________________________________________________________________
int StTGeoProxy::IsSensitive(const TGeoVolume *volu)
{
  if (!volu) volu = gGeoManager->GetCurrentVolume();
  const TGeoMedium *tm = volu->GetMedium();
  if (!tm) return 0;
  return (tm->GetParam(kISVOL)>0.);
}
//_____________________________________________________________________________
const TGeoVolume *StTGeoProxy::GetModu() const
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
const TGeoVolume *StTGeoProxy::GetVolu() const
{
   return gGeoManager->GetCurrentVolume();
}
//_____________________________________________________________________________
void StTGeoProxy::Print(const char *tit) const
{
  TGeoVolume *v=0;
  if (tit) printf("StTGeoProxy::Print(%s)\n\n",tit);   
   for (int i=gGeoManager->GetLevel();i>=0;i--) {
     TGeoNode *n = gGeoManager->GetMother(i); 
     if(!n) 				break;
     v=n->GetVolume();
     if (!v) 				continue;
     printf("/%s#%d",v->GetName(),n->GetNumber());
   }
  if (v && v->GetMaterial()) {
    printf("(%s)",v->GetMaterial()->GetName());
    if (IsSensitive(v)) printf("*");
  }
  printf("\n");    

}
//_____________________________________________________________________________
void StTGeoProxy::ls(const char *opt) const
{
static int nCall=0;nCall++;
static const char *types[]={"Dead","MODU","Modu","HitP","Sens"};
  int opta = strstr(opt,"a")!=0;
  int optm = strstr(opt,"m")!=0;
  int optM = strstr(opt,"M")!=0;
  int opts = strstr(opt,"s")!=0;
  int optp = strstr(opt,"p")!=0;

  StTGeoIter it;
  const TGeoVolume *vol= *it;
  int num=0;
  for (;(vol=*it);++it) {
    if (!it.IsFirst()) continue;
//    int volId = ::GetVoluId(vol);
    int jk= (opta)? 0:-1;
    StHitPlaneInfo *ghp=0;
    do {
      if (optM && IsMODULE   (vol)) 	{jk=1;break;}
      if (optm && IsModule   (vol)) 	{jk=2;break;}
      if (optp && (ghp=IsHitPlane(vol))){jk=3;break;}
      if (opts && IsSensitive(vol)) 	{jk=4;break;}
    } while (0);
    if (jk<0) continue;
    num++;
//Break(num);
    TString path(it.GetPath());
    const TGeoShape    *sh = vol->GetShape();
    const TGeoMedium *me = vol->GetMedium();
    const TGeoMaterial *ma = (me)? me->GetMaterial():0;
    const char *maName = (ma)? ma->GetName(): "";
    printf("%3d - %s(%s",num,vol->GetName(),types[jk]);
    if (ghp) {
      StHitPlane *hp = ghp->GetHitPlane(path);
      if (!hp) {
        path+=" Not Found"; ghp->Print(path.Data());}
      assert(hp);    
      printf("#%d",hp->GetNHits());
    }


    printf(",%s,%s)\t %s"
           ,sh->ClassName()+4
	   ,maName,path.Data());
    
    printf("\n");
  }
}
//_____________________________________________________________________________
const char *StTGeoProxy::GetPath() const     
{
  return gGeoManager->GetPath();
}
//_____________________________________________________________________________
void StTGeoProxy::Test()
{
   gROOT->Macro("$STAR/StarDb/AgiGeometry/y2009.h");
   StTGeoProxy &hlp = *StTGeoProxy::Instance();
   hlp.Init(1+2);
//   hlp.ls("p");
  StTGeoIter it;
  const TGeoVolume *vol= 0;
  int num=0;
  hlp.ClearHits();
  for (;(vol=*it);++it) {
    if (!it.IsFirst())       continue;
    StHitPlaneInfo *hpi = hlp.IsHitPlane(vol);
    if (!hpi) continue;
    TString path(it.GetPath());
    StHitPlane *hp = hpi->GetHitPlane(path);
    assert(hp);
    num++;
    hlp.AddHit((void*)hp,hp->GetDetId(),hp->GetPnt(),num,1);
  }
  hlp.InitHits();
  hlp.ls("p");
}
//_____________________________________________________________________________
void StTGeoProxy::Break(int kase) 
{
static int myKase = -2009;
if (kase!=myKase) return;
printf("Break(%d)\n",kase); 
}
//_____________________________________________________________________________
const StHitPlane *StTGeoProxy::AddHit(void *hit,StDetectorId detId,const float xyz[3],unsigned int hardw,int seed)
{
static int nCall = 0;  nCall++;  
//		Test of hardware id assignment quality
enum 		{kMaxTest=1000,kMaxFail=100};
//
//   Break(nCall);
  fDetId = detId;
  StHitPlane *hp=0,*hpGeo=0;
  fGoodHit = 0;
  if (fHitLoadActor) fHitLoadActor->SetHit(hit);
  hpGeo = FindHitPlane(xyz,fGoodHit);
  if (!hpGeo) {
    if (fGoodHit) return 0; 	//Hit rejected
    double pnt[3]={xyz[0],xyz[1],xyz[2]};
    gGeoManager->SetCurrentPoint(pnt);
    Warning("AddHit","Hit(%g,%g,%g) in %s Not Found",pnt[0],pnt[1],pnt[2],GetPath());
    return 0;
  }
  hp = hpGeo;
  assert(hp);
  assert(hp->GetDetId() == detId);

   fAllHits->push_back(hit);
   if (seed) {//add to seed hit collection
     fSeedHits->push_back(hit);
  } 
  hp->AddHit(hit,xyz);
//printf("Hit(%g %g %g) added to %s\n",xyz[0],xyz[1],xyz[2],hp->GetName());

  if (hp->GetNHits()==1) AddHitPlane(hp);


  return hp;
}
//_____________________________________________________________________________
StHitPlane *StTGeoProxy::FindHitPlane(const float xyz[3],int &sure)
{
// cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)
// sin(a+b) = cos(a)*sin(b) + sin(a)*cos(b)
static const float dirs[][3] =  {
{ 0.0000,        0.0000,         1.0000 },      // 1
{ 0.5000,        0.0000,         0.8660 },      // 2
{ 0.2500,        0.4330,         0.8660 },      // 3
{-0.2500,        0.4330,         0.8660 },      // 4
{-0.5000,        0.0000,         0.8660 },      // 5
{-0.2500,       -0.4330,         0.8660 },      // 6
{ 0.2500,       -0.4330,         0.8660 },      // 7
{ 0.8660,        0.0000,         0.5000 },      // 8
{ 0.7344,        0.4589,         0.5000 },      // 9
{ 0.3796,        0.7784,         0.5000 },      //10
{-0.0905,        0.8613,         0.5000 },      //11
{-0.5332,        0.6824,         0.5000 },      //12
{-0.8138,        0.2962,         0.5000 },      //13
{-0.8471,       -0.1801,         0.5000 },      //14
{-0.6230,       -0.6016,         0.5000 },      //15
{-0.2095,       -0.8403,         0.5000 },      //16
{ 0.2676,       -0.8236,         0.5000 },      //17
{ 0.6634,       -0.5567,         0.5000 },      //18
{ 0.8576,       -0.1205,         0.5000 },      //19
{ 1.0000,        0.0000,         0.0000 },      //20
{ 0.8660,        0.5000,         0.0000 },      //21
{ 0.5000,        0.8660,         0.0000 },      //22
{ 0.0000,        1.0000,         0.0000 },      //23
{-0.5000,        0.8660,         0.0000 },      //24
{-0.8660,        0.5000,         0.0000 },      //25
{-1.0000,        0.0000,         0.0000 },      //26
{-0.8660,       -0.5000,         0.0000 },      //27
{-0.5000,       -0.8660,         0.0000 },      //28
{-0.0000,       -1.0000,         0.0000 },      //29
{ 0.5000,       -0.8660,         0.0000 },      //30
{ 0.8660,       -0.5000,         0.0000 },      //31
{ 0.8660,        0.0000,        -0.5000 },      //32
{ 0.7344,        0.4589,        -0.5000 },      //33
{ 0.3796,        0.7784,        -0.5000 },      //34
{-0.0905,        0.8613,        -0.5000 },      //35
{-0.5332,        0.6824,        -0.5000 },      //36
{-0.8138,        0.2962,        -0.5000 },      //37
{-0.8471,       -0.1801,        -0.5000 },      //38
{-0.6230,       -0.6016,        -0.5000 },      //39
{-0.2095,       -0.8403,        -0.5000 },      //40
{ 0.2676,       -0.8236,        -0.5000 },      //41
{ 0.6634,       -0.5567,        -0.5000 },      //42
{ 0.8576,       -0.1205,        -0.5000 },      //43
{ 0.5000,        0.0000,        -0.8660 },      //44
{ 0.2500,        0.4330,        -0.8660 },      //45
{-0.2500,        0.4330,        -0.8660 },      //46
{-0.5000,        0.0000,        -0.8660 },      //47
{-0.2500,       -0.4330,        -0.8660 },      //48
{ 0.2500,       -0.4330,        -0.8660 },      //49
{ 0.0000,        0.0000,        -1.0000 }};     //50

enum {kNDIRS = sizeof(dirs)/(3*sizeof(**dirs))}; 

static int nCall=0; nCall++;
  double pnt[3]={xyz[0],xyz[1],xyz[2]};
  const TGeoNode *node = gGeoManager->FindNode(pnt[0],pnt[1],pnt[2]);
  if (fHitLoadActor) {
    int act = (*fHitLoadActor)(pnt);
    if (act) node = gGeoManager->GetCurrentNode();
  }
  assert(node);
  StHitPlane *hp = GetCurrentHitPlane();     
  sure = 1;
  if (hp  && hp->GetHitErrCalc() && IsHitted(pnt)) 	return hp;

//	volume is sensitive but not active but still sure
  if (hp) 						return 0;
  sure = 0;
  double Rxy = sqrt(pnt[0]*pnt[0]+pnt[1]*pnt[1]);
  double myCos= pnt[0]/Rxy,mySin=pnt[1]/Rxy;
  double myDir[3];
  double minDist=(fabs(xyz[0])+fabs(xyz[1]))/10+5;
  StHitPlane *minHitPlane=0;
  for (int idir=0;idir<kNDIRS; idir++) {
    gGeoManager->SetCurrentPoint(pnt);
    node = gGeoManager->FindNode();
    myDir[0] = dirs[idir][0]*myCos - dirs[idir][1]*mySin; 
    myDir[1] = dirs[idir][0]*mySin + dirs[idir][1]*myCos; 
    myDir[2] = dirs[idir][2];
    double myStep = Look(minDist,pnt,myDir);
    if (myStep > minDist) 			continue;
    hp = GetCurrentHitPlane();     
    if (!hp ) 		  			continue;
    if (!hp->GetHitErrCalc())			continue;	
    if (!IsHitted(gGeoManager->GetLastPoint()))	continue;
    if (fDetId != hp->GetDetId())		continue;
    minDist = myStep; minHitPlane = hp; 	
  }
  return minHitPlane;  
}       
       
//_____________________________________________________________________________
StHitPlane *StTGeoProxy::GetCurrentHitPlane ()
{
  const TGeoNode *node = 0;
  const StHitPlaneInfo *inf=0;
  TString path(gGeoManager->GetPath());
  int inc=0;
  for (; inc<=3; inc++) {
    node = gGeoManager->GetMother(inc);
    if (!node) return 0;
    inf = IsHitPlane(node);
    if (inf) break;
  }
  if (!inf) return 0;
  for (int jnc = 1; jnc<=inc; jnc++) {gGeoManager->CdUp();}
  path=gGeoManager->GetPath();
  StHitPlane *hp= inf->GetHitPlane(path);
  return hp;
}
//_____________________________________________________________________________
void StTGeoProxy::ClearHits()
{
  fAllHits->clear();
  fSeedHits->clear();
  int n = fHitPlaneArr->GetLast()+1;
  for (int i=0;i<n;i++) {
    StHitPlane *hp = (StHitPlane *)fHitPlaneArr->At(i);
    hp->Clear();
  }
  fHitPlaneArr->Clear();
}    
//_____________________________________________________________________________
void StTGeoProxy::Clear(const char *)
{
  ClearHits();
}
//_____________________________________________________________________________
int StTGeoProxy::InitHits()
{

  int n = fHitPlaneArr->GetLast()+1;
  int nHits = 0;
  for (int i=0;i<n;i++) {
    StHitPlane *hp = (StHitPlane *)fHitPlaneArr->At(i);
    nHits += hp->InitHits();
  }
  return nHits;
}    
//_____________________________________________________________________________
StDetectorId StTGeoProxy::DetId(const char *detName) 
{
   StDetectorId id = detectorIdByName(detName); 
   if (id) return id;
   TString tDetName(detName);
   for (int i = 0;gMyMod[i].name; i++) {
    if (!tDetName.CompareTo(gMyMod[i].name,TString::kIgnoreCase)) 
        return (StDetectorId)gMyMod[i].id;}
   return (StDetectorId)0;
}
//_____________________________________________________________________________
const char *StTGeoProxy::DetName(StDetectorId detId)
{
  const char *det =detectorNameById(detId);
  return det;
}
//_____________________________________________________________________________
const char *StTGeoProxy::ModName(StDetectorId detId)
{
   for (int i = 0;gMyMod[i].name; i++) 
   { if (gMyMod[i].id==detId)  return gMyMod[i].name;}
   return "";
}

//_____________________________________________________________________________
int StTGeoProxy::IsHitted(const double X[3]) const
{
  if (!IsSensitive()) return 0;
  const TGeoVolume *mo = GetModu();
  if (!IsActive(mo)) return 0;
  StVoluInfo *vi = GetInfo(::GetVoluId(mo));
  StActorFunctor *af = vi->GetActiveFunctor();
  if (!af) return 1;
  return (*af)(X);
}
//_____________________________________________________________________________
//_____________________________________________________________________________
const char *StVoluInfo::GetName() const
{
 int volId = GetUniqueID();
 return (volId) ? gGeoManager->GetVolume(volId)->GetName():"NoName";
} 
 
//_____________________________________________________________________________
//_____________________________________________________________________________
StHitPlaneInfo::StHitPlaneInfo(int volId) : StVoluInfo(volId)	
{
   fAxi=0;
   memset(fOrg,0,sizeof(fOrg)+sizeof(fDir));
   StTGeoProxy::fgKount[1]++;
}
//_____________________________________________________________________________
StHitPlaneInfo::~StHitPlaneInfo()	
{
   StTGeoProxy::fgKount[1]--;
}
//_____________________________________________________________________________
StHitPlane *StHitPlaneInfo::MakeHitPlane(const StTGeoIter &it,StActorFunctor *act)
{
static int nCall=0; nCall++;
  const TGeoVolume *volu = *it;
  const TGeoShape  *sh = volu->GetShape();
  StHitPlane *hp=0;
  StHitTube  *ht=0;
  TString path(it.GetPath());
  hp= (StHitPlane *)GetHitPlane(path);
  if (hp) return 0;

  float  gPos[3],lDir[3][3];
  it.LocalToMaster(fOrg, gPos);
  memcpy(lDir[0],fDir[0],sizeof(fDir));
  int myAxi = fAxi;
  if (act) { 	//Try to use functor
    gGeoManager->cd(path);
    act->SetDetId(GetDetId());
    int ax = act->Operator(gPos);
    if (ax>0) {
      myAxi = ax;
      StTGeoProxy::MakeDir(ax,lDir);
  } }
  if (!myAxi) return 0;

  if ((myAxi%10)<=3) {
    hp = new StHitPlane(path,::GetVoluId(volu));
  } else if ((myAxi%10)==4) {
    StHitTube *ht = new StHitTube(path,::GetVoluId(volu)); hp = ht;
  }

  fHitPlanePathMap[path] = hp;
  const char *pa = (*(fHitPlanePathMap.find(path))).first;
  hp->SetPath(pa);
  
  memcpy(hp->fOrg,gPos,sizeof(gPos));
  myTVector3 Vd[3],Vo(hp->fOrg);
  int upd = 0;
  for (int i=0;i<3;i++) {
    it.LocalToMasterVect(lDir[i], hp->fDir[i]); Vd[i]=hp->fDir[i];}
// Check signature and try to fix
    for(int i=0;i<3;i++){Vd[i]= hp->fDir[i];}
    for(int iTry=0;iTry<2;iTry++) {
      int iTst=0;
      for(int i=0;i<3;i++){
	int j = (i+1)%3,k = (j+1)%3;
	myTVector3 res = Vd[i].Cross(Vd[j]);
	iTst+= ((Vd[k]-res).Mag()>1e-4);
      }
      if (!iTst) break;
      assert(!iTry);
      upd++; Vd[1]*=(-1.);
    }

  if (Vd[0].Dot(Vo)<0) 	{upd++; 	// Rotate around local Z
    for(int i=0;i<2;i++){Vd[i].Rotate(M_PI,Vd[2]);}}

  if (Vd[2][2]<0) 		{upd++; 	// Rotate around local X
    for(int i=1;i<3;i++)	{Vd[i].Rotate(M_PI,Vd[0]);}}
  if (upd) 			{  		// Update
    for(int i=0;i<3;i++){Vd[i].Get(hp->fDir[i]);}}
  
  if (ht) {
    double par[9];float pnt[3]={0};
    sh->GetBoundingCylinder(par);
    double rMed = (sqrt(par[0])+sqrt(par[1]))*0.5;
    double pMed = 0.5*(par[2]+par[3])*M_PI/180;
    pnt[0] = rMed*cos(pMed); 
    pnt[1] = rMed*sin(pMed); 
    it.LocalToMaster(pnt, ht->fPnt);
    ht->fRmed = rMed;
  }
  
// Check
    for(int i=0;i<3;i++){for(int j=0;j<3;j++) {Vd[i][j]= hp->fDir[i][j];}}
    for(int i=0;i<3;i++){
      int j = (i+1)%3,k = (j+1)%3;
      myTVector3 res = Vd[i].Cross(Vd[j]);
      assert( (Vd[k]-res).Mag()<1e-4);
    }

  return hp;
}
//_____________________________________________________________________________
StHitPlane *StHitPlaneInfo::GetHitPlane (const TString &path) const	
{
static int nCall=0; nCall++;
   StHitPlanePathMapIter it = fHitPlanePathMap.find(path);
   if (it ==  fHitPlanePathMap.end()) {return 0;}
   return (*it).second;
}
//_____________________________________________________________________________
StHitPlane *StHitPlaneInfo::RemHitPlane (const TString &path) 	
{
   StHitPlanePathMap::iterator it = fHitPlanePathMap.find(path);
   if (it ==  fHitPlanePathMap.end()) {return 0;}
   StHitPlane *hp = (*it).second;
   fHitPlanePathMap.erase(it);
   return hp;
}
//_____________________________________________________________________________
void StHitPlaneInfo::Print(const char* tit ) const
{
  printf("\nStHitPlaneInfo(%s) %s\n",GetName(),tit);
  printf("fOrg:     %g %g %g\n",fOrg[0],fOrg[1],fOrg[2]);
  for (int i=0;i<3;i++) {;
    printf("fDir[%d]: %g %g %g\n",i,fDir[i][0],fDir[i][1],fDir[i][2]);}
  int njk = fHitPlanePathMap.size();
  printf("HitPlanes %d:\n",njk);
  int j=0;
  for (StHitPlanePathMapIter it =fHitPlanePathMap.begin();
       it!=fHitPlanePathMap.end();++it) {printf(" %3d - %s\n",j,(*it).first.Data());j++;}
  printf("\n");
   
}
//_____________________________________________________________________________
StHitPlane::StHitPlane(const char *path,int voluId): TNamed(path,"") 	
{
   memset(fBeg,0,fEnd-fBeg+1);
   SetUniqueID(voluId);
   fHitMap = new StMultiKeyMap(2);
   StTGeoProxy::fgKount[2]++;
}
//_____________________________________________________________________________
StHitPlane::~StHitPlane() 	
{
   StTGeoProxy::fgKount[2]--;
   delete fHitMap;fHitMap = 0;
}
//_____________________________________________________________________________
void StHitPlane::AddHit(void *hit,const float xyz[3])
{
  float uv[3];
  ToLocal(xyz,uv);
  fHitMap->Add(hit,uv+1);

}
//_____________________________________________________________________________
void StHitPlane::ToLocal(const float xyz[3],float uv[3]) const
{
  float x[3];
  TCL::vsub(xyz, GetOrg(xyz), x,3);
  TCL::vmatl(GetDir(xyz)[0], x, uv, 3, 3);
}
//_____________________________________________________________________________
int StHitPlane::GetNHits() const	
{
return (fHitMap)?fHitMap->Size():0;
}
//_____________________________________________________________________________
int StHitPlane::InitHits()
{
  return fHitMap->MakeTree();
}
//_____________________________________________________________________________
void StHitPlane::Clear(const char*)
{
  fHitMap->Clear();
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
StHitTube::StHitTube(const char *path,int voluId): StHitPlane(path,voluId) 	
{
   memset(fBeg,0,fEnd-fBeg+1);
}
//_____________________________________________________________________________
const Mtx33F_t &StHitTube::GetDir(const float *hit) const
{
static float myDir[3][3];
  TCL::ucopy(fDir[2],myDir[2],3);
  TCL::vsub(hit,fOrg,myDir[0],3);
  float fak = TCL::vdot(myDir[0],fDir[2],3);
  TCL::vlinco(myDir[0],1.,fDir[2],-fak,myDir[0],3);
  fak = TCL::vdot(myDir[0],myDir[0],3);
  fak = sqrt(fak);
  TCL::vscale(myDir[0],1./fak,myDir[0],3);
  TMath::Cross(fDir[2],myDir[0],myDir[1]);
  return myDir;
}
//_____________________________________________________________________________
const float *StHitTube::GetOrg(const float *hit) const 
{
static float myOrg[3];
  TCL::vsub(hit,fOrg,myOrg,3);
  float fak = TCL::vdot(myOrg,myOrg,3);
  TCL::vlinco(myOrg,fRmed/fak,fOrg,1.,myOrg,3);
  return myOrg;
}
  

//_____________________________________________________________________________
  enum {kEND,kDOWN,kNEXT,kUPP};
//_____________________________________________________________________________
//_____________________________________________________________________________
  StTGeoIter::StTGeoIter()
{
  fNavi = new TGeoNavigator(gGeoManager);
  fNavi->BuildCache();
  Reset();
}
//_____________________________________________________________________________
void StTGeoIter::Reset()
{
  fLev = 0;
  fStk[0]=0;
  fNavi->CdTop();
  fVolu = fNavi->GetCurrentVolume();
  fNow = 1;
  fKase = kDOWN;  
  assert(fLev == fNavi->GetLevel());
}
//_____________________________________________________________________________
  StTGeoIter::~StTGeoIter()
{ delete fNavi; fNavi = 0;}

//_____________________________________________________________________________
StTGeoIter &StTGeoIter::Down()
{
  fKase = kDOWN; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIter &StTGeoIter::Next()
{
  fKase = kNEXT; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIter &StTGeoIter::Skip()
{
  fKase = kNEXT; return (*this);
}
//_____________________________________________________________________________
StTGeoIter &StTGeoIter::Upp()
{
  fKase = kUPP; return ++(*this);
}
//_____________________________________________________________________________
StTGeoIter &StTGeoIter::operator++()
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
      if (fStk[fLev]>=nKids)  { 		fKase=kNEXT ; fNow=2; break;}
      fNavi->CdDown(fStk[fLev]);fLev++; fStk[fLev]=0; 
  assert(fLev == fNavi->GetLevel());
      fVolu = fNavi->GetCurrentVolume();	fKase=kDOWN ; fNow=1; break;}  

  case kUPP: {
    if (!fLev) {fVolu=0; 			fKase=kEND  ; fNow=0; break;}
//  TGeoNode *node = fNavi->GetCurrentNode();
    fNavi->CdUp(); fLev--;
    fVolu = fNavi->GetCurrentVolume();	 
  assert(fLev == fNavi->GetLevel());
    						fKase=kNEXT; goto CASE;}
  }

  return *this;
}
//_____________________________________________________________________________
const TGeoNode *StTGeoIter::GetNode(int idx) const
{
 return fNavi->GetMother(idx);
}
//_____________________________________________________________________________
const TGeoHMatrix *StTGeoIter::GetMatr(int idx) const
{
 return fNavi->GetMotherMatrix(idx);
}

//_____________________________________________________________________________
const TGeoVolume *StTGeoIter::GetVolu(int idx) const
{
  if (!idx) return fVolu;
  const TGeoNode *node = GetNode(idx);
  if (!node) return 0;
  return node->GetVolume();
}
//_____________________________________________________________________________
void StTGeoIter::Print(const char *tit) const
{
  const TGeoVolume *v =0;
  if (tit) printf("\nStTGeoIter::Print(%s)\n\n",tit);   
  printf("%d ",fNow);
  for (int l=0;l<=fLev;l++) {
    v = GetVolu(fLev-l);
    int iocc = (l) ? fStk[l-1]:0;
    printf("/%s#%d",v->GetName(),iocc);
  }
  const TGeoMedium *tm = v->GetMedium();
  const TGeoMaterial *mat = (tm)? tm->GetMaterial():0;
  const char* maName = (mat)? mat->GetName():"";
  printf("(%s)",maName);
  if (tm && tm->GetParam(kISVOL)>0.)printf("*");  
    
  v = GetVolu(0);
  const TGeoShape* sh=v->GetShape() ;
  if (sh && sh->IsCylType()) {   
    double dd[10];
    sh->GetBoundingCylinder(dd);dd[4] = ((const TGeoBBox*)sh)->GetDZ();
    printf(" // shape=%s R1=%g R2=%g Phi1=%g,Phi2=%g,dz=%g",sh->ClassName()
          ,sqrt(dd[0]),sqrt(dd[1]),dd[2],dd[3],dd[4]);
        
  } else if (sh && sh->IsValidBox()) {   
    const TGeoBBox *bb = (const TGeoBBox*)sh;
    double dd[3]={bb->GetDX(),bb->GetDY(),bb->GetDZ()};
    printf(" // shape=%s dx=%g dy=%g dz=%g",sh->ClassName(),dd[0],dd[1],dd[2]);
  }
  printf("\n");    

}
//_____________________________________________________________________________
const char *StTGeoIter::GetPath() const  
{
  return fNavi->GetPath();
}
//_____________________________________________________________________________
void  StTGeoIter::LocalToMaster(const double* local, double* master) const
{ fNavi->LocalToMaster(local,master);}
//_____________________________________________________________________________
void  StTGeoIter::LocalToMasterVect(const double* local, double* master) const
{ fNavi->LocalToMasterVect(local,master);}
//_____________________________________________________________________________
void  StTGeoIter::LocalToMaster(const float* local, float* master) const
{ 
  double d[6];
  TCL::ucopy(local,d,3);
  fNavi->LocalToMaster(d,d+3);
  TCL::ucopy(d+3,master,3);
}  
//_____________________________________________________________________________
void  StTGeoIter::LocalToMasterVect(const float* local, float* master) const
{ 
  double d[6];
  TCL::ucopy(local,d,3);
  fNavi->LocalToMasterVect(d,d+3);
  TCL::ucopy(d+3,master,3);
}  
//_____________________________________________________________________________
int StTGeoIter::IsOmule()  const //candidate to module
{
   assert(fVolu);
   const TGeoVolume *vol=0;
   int jk=0;
   for (int ipar=1; (vol=GetVolu(ipar));ipar++) {
     int nDau = vol->GetNdaughters();
     if (ipar==1) {jk |= (nDau==1)? 1:2;}
     else         {jk |= (nDau==1)? 4:8;}
   }
   return (jk==6);
}
//_____________________________________________________________________________
const TGeoVolume *StTGeoProxy::FindModule(const char *patt) 
{
   
  const TGeoVolume *vol=0,*bestVolu=0;
  int bestQua = -1000000;
  for (StTGeoIter it;(vol=*it);++it) {
    const char *name = vol->GetName();
    if (patt[0]!=name[0]) continue;
    if (patt[1]!=name[1]) continue;
    int n = 0;
    for (;patt[n];n++) { if (name[n]!=patt[n]) break;}
    if (n<3) continue;
    if (it.GetLev()>5) continue;
    int qua = n*100 - it.GetLev();
    if (qua <= bestQua) continue;
    bestQua = qua; bestVolu = vol;
  }
  return bestVolu;
}
//_____________________________________________________________________________
double StTGeoProxy::Look(double maxDist,const double pnt[3],const double dir[3])
{
//  Search nearest sensitive volume in given direction.
//  returns distance
  double myPnt[3]={ pnt[0], pnt[1], pnt[2]};
  double myDir[3]={ dir[0], dir[1], dir[2]};

  gGeoManager->SetCurrentPoint(myPnt);
  const TGeoNode *node = gGeoManager->FindNode();
  if (!node) return 0;
  gGeoManager->SetCurrentDirection(myDir);
  TString prevPath(gGeoManager->GetPath());
  double myStep = 0,epsStp = 1e-4+maxDist*1e-3,minStp = epsStp,stp=0;
  for (int istep=0;istep<100 ;istep++) {
    node = gGeoManager->FindNextBoundaryAndStep(1.001*(maxDist-myStep));
    if (!node) 				break;
    TString currPath(gGeoManager->GetPath());
    stp = gGeoManager->GetStep();
    int same = (currPath == prevPath);
    if (stp<epsStp && same) {//Same volume
      stp=minStp; minStp*=2;
      const double *x = gGeoManager->GetCurrentPoint();
      for (int j=0;j<3;j++) { myPnt[j]=x[j]+myDir[j]*stp;}
      gGeoManager->SetCurrentPoint(myPnt);
      node = gGeoManager->FindNode();
      if (!node) 			break;
    } 
    myStep +=stp; if (myStep>=maxDist) 	break; 
    if (same) 				continue;

    prevPath=currPath; minStp = epsStp; 
    if (!IsSensitive()) 		continue;
    StHitPlaneInfo* inf = IsHitPlane(node) ;
    if (!inf) 				continue;
    StHitPlane *hp = inf->GetHitPlane(currPath);
    if (hp) 				break;
  }
  return myStep;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
StVoluInfo::StVoluInfo(int voluNumber)     
{ SetUniqueID(voluNumber);
  fActiveFunctor=0;
  fNSens = 0;
  StTGeoProxy::fgKount[0]++;
}
//_____________________________________________________________________________
StVoluInfo::~StVoluInfo()     
{
  StTGeoProxy::fgKount[0]--;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
StTGeoHitShape::StTGeoHitShape()
{
  fZMin = 1e11; fZMax = -1e11; fRMin=1e11; fRMax = 0;
  for (int i=0;i<kNZ;i++) {fRxy[i][0]=1e11;fRxy[i][1]=0;};
}
//_____________________________________________________________________________
void StTGeoHitShape::Update(double z1, double z2, double rxy)
{
   if (rxy<=0) {//Only zMin & zMax evaluation
     
     if (fZMin>z1) fZMin=z1;
     if (fZMax<z2) fZMax=z2;
     return;
   }
   fZStp = (fZMax-fZMin)/kNZ;
   assert(z1>=fZMin && z1<=fZMax);
   assert(z2>=fZMin && z2<=fZMax);
   if (fRMin>rxy) fRMin=rxy;
   if (fRMax<rxy) fRMax=rxy;
   int jl = (int)(z1-fZMin)/fZStp;
   int jr = (int)(z2-fZMin)/fZStp;
   for (int jj=jl;jj<=jr;jj++) {if(fRxy[jj][0]>rxy) fRxy[jj][0]=rxy; 
                                if(fRxy[jj][1]<rxy) fRxy[jj][1]=rxy;}
}
//_____________________________________________________________________________
void StTGeoHitShape::Smooth(double zl, double zr)
{

   double zVtx[2]={zl,zr};;
   for (int iVtx=0;iVtx<2;iVtx++) {
     int jVtx = (int)(zVtx[iVtx] -fZMin)/fZStp;
     for (int jE=0; jE <kNZ; jE++) {
       if (abs(jVtx-jE)<2) continue;
       double rE = fRxy[jE][1];
       if (rE<=0) continue;
       int jl,jr;
       if (jE>jVtx) {jl = jVtx+1;jr = jE  -1;}
       else         {jl = jE  +1;jr = jVtx-1;}

       if (jl>=jr) {jl = jE+1; jr = jVtx-1;}
       assert(jl<kNZ && jr<kNZ);
       for (int j=jl; j<=jr; j++) {
         double r = rE/(jE-jVtx)*(j-jVtx);
         if (r>fRxy[j][1]) fRxy[j][1] = r;
       }
       rE = fRxy[jE][0];
       if (jE>jVtx) {jl = jE+1;jr = kNZ-1;}
       else         {jl = 0   ;jr = jE -1;}
       for (int j=jl; j<=jr; j++) {
         double r = rE/(jE-jVtx)*(j-jVtx);
         if (r<fRxy[j][0]) fRxy[j][0] = r;
       }
     }
   }
//	Increase volume, do not lose the boundary hits
   fZMin-=fabs(fZMin)*kSafe/100.;
   fZMax+=fabs(fZMax)*kSafe/100.;
   fRMin*=100./kSafe;
   fRMax*=kSafe/100.;

   for (int j=0;j<kNZ;j++) {
     fRxy[j][0]*=100./kSafe;
     fRxy[j][1]*=kSafe/100.;
   }

// 	eval RMaxMin & RMinMax
  fRMinMax= fRMax;  fRMaxMin= fRMin; 
  for (int j=0;j<kNZ;j++) {
    if (fRMaxMin<fRxy[j][0]) fRMaxMin=fRxy[j][0];
    if (fRMinMax>fRxy[j][1]) fRMinMax=fRxy[j][1];
  }


}
//_____________________________________________________________________________
int  StTGeoHitShape::Outside(double z,double rxy) const
{
   if (z<fZMin) 		return  1;
   if (z>fZMax) 		return  2;
   if (rxy>fRMinMax && rxy<fRMaxMin ) 	return 0;
return 3;//??????????????????????????????????????????????????????????
   int jj = (int)(z-fZMin)/fZStp;
   if (jj<   0) jj=0;
   if (jj>=kNZ) jj=kNZ-1;
   if (rxy > fRxy[jj][1]) 	return  5;
   if (rxy < fRxy[jj][0]) 	return -6;
   return 0;
}
//_____________________________________________________________________________
void StTGeoHitShape::Get(double &zMin,double &zMax,double &rMax) const
{
  zMin = fZMin; zMax = fZMax; zMax = fRMax; 
}
//_____________________________________________________________________________
void StTGeoHitShape::Print(const char *) const
{
printf("StTGeoHitShape::Print() Zmin = %g Zmax = %g  Rmax = %g\n",fZMin,fZMax,fRMax);
  for (int j=0;j<kNZ;j++) {
    if (fRxy[j][1]<=0) continue;
    double zL=fZMin+fZStp*j, zR = zL+fZStp;
    printf ("Zl = %g Zr = %g Rxy = %g %g\n",zL,zR,fRxy[j][0],fRxy[j][1]);
  }
  printf ("\n");
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StActorFunctor)
//_____________________________________________________________________________
int StActorFunctor::GetIPath(int nLev,int *copyNums,const char **voluNams) const
{
  for (int iLev=0;iLev<nLev;iLev++) {
    TGeoNode *node = gGeoManager->GetMother(iLev);
    copyNums[iLev] = 0;
    if (!node) 		return 1; 
    copyNums[iLev] = node->GetNumber();
    if (!voluNams) 	continue;
    voluNams[iLev] = node->GetVolume()->GetName();
  }  
  return 0;
}
//_____________________________________________________________________________
const char* StActorFunctor::GetPath()       const
{
  return gGeoManager->GetPath();
}
//_____________________________________________________________________________
const TGeoVolume *StActorFunctor::GetVolu() const
{
  return gGeoManager->GetCurrentVolume();
}
//_____________________________________________________________________________
const TGeoNode *StActorFunctor::GetNode() const
{
  return gGeoManager->GetCurrentNode();
}
//_____________________________________________________________________________
StVoluInfo *StActorFunctor::GetInfo() const
{
static StTGeoProxy *tgp = StTGeoProxy::Inst();
const TGeoVolume *v = tgp->GetVolu();
      int voluId = ::GetVoluId(v);
      return tgp->GetInfo(voluId);
}
//_____________________________________________________________________________
StHitPlaneInfo *StActorFunctor::GetHitPlaneInfo() const
{
static StTGeoProxy *tgp = StTGeoProxy::Inst();
  const TGeoVolume *v = tgp->GetVolu();
  return tgp->IsHitPlane(v);
}
//_____________________________________________________________________________
StHitPlane *StActorFunctor::GetHitPlane()     const
{
static StTGeoProxy *tgp = StTGeoProxy::Inst();
  return tgp->GetCurrentHitPlane();
}
//______________________________________________________________________________
StvSetLayer::StvSetLayer():StActorFunctor("SetLayer")
{
}
//______________________________________________________________________________
int StvSetLayer::operator()( const double *)
{
static StTGeoProxy *tg = StTGeoProxy::Inst();
  if (!tg->IsSensitive()) 	return 0;
  StHitPlane  *hp =tg->GetCurrentHitPlane();
  if (!hp)			return 0;
  hp->SetLayer();
  return 1;
}





















//_____________________________________________________________________________
//_____________________________________________________________________________
