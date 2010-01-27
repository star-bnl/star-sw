
// $Id: StTGeoHelper.cxx,v 1.7 2010/01/27 23:02:57 perev Exp $
//
//
// Class StTGeoHelper
// ------------------



#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string>
#include <assert.h>
#include <set>
#include "TROOT.h"
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
#include "StTGeoHelper.h"
#include "StMultiKeyMap.h"

static StTGeoHelper *gStTGeoHelper=0;

ClassImp(StTGeoHelper)
ClassImp(StVoluInfo)
ClassImp(StHitPlaneInfo)
ClassImp(StHitPlane)
ClassImp(StHitTube )

enum EMEDIUM {kISVOL =0,kIFIELD=1,kFIELDM=2,kTMAXFD=3
             ,kSTEMAX=4,kDEEMAX=5,kEPSIL =6,kSTMIN =7};
static const char *gDetName[]={
  "Unknown",
  "Tpc",
  "Svt",
  "Rich",
  "FtpcWest",
  "FtpcEast",
  "Tof",
  "Ctb",
  "Ssd",
  "BarrelEmcTower",
  "BarrelEmcPreShower",
  "BarrelSmdEtaStrip",
  "BarrelSmdPhiStrip",
  "EndcapEmcTower",
  "EndcapEmcPreShower",
  "EndcapSmdUStrip",
  "EndcapSmdVStrip",
  "ZdcWest",
  "ZdcEast",
  "MwpcWest",
  "MwpcEast",
  "TpcSsd",
  "TpcSvt",
  "TpcSsdSvt",
  "SsdSvt",
  "PhmdCpv",
  "Phmd",
  "Pxl",
  "Ist",
  "Fgt",
  "FpdWest",
  "FpdEast",
  "Fms",
  "Rps",
   0};

static const char *gModName[]={
  "UNKNOWN", 		//Unknown
  "TPCE",		//Tpc
  "SVTT",		//Svt
  "RICH",		//Rich
  "FTPC",		//FtpcWest
  "FTPC",		//FtpcWest
  "BTOF",		//Tof
  "",			//Ctb
  "SFMO",		//Ssd
  "CALB",		//BarrelEmcTower
  "CALB",		//"BarrelEmcPreShower
  "CALB",		//BarrelSmdEtaStrip
  "CALB",		//BarrelSmdPhiStrip
  "ECAL",		//EndcapEmcTower
  "ECAL", 		//EndcapEmcPreShowe
  "ECAL",		//EndcapSmdUStrip
  "ECAL",		//EndcapSmdVStrip
  "",			//ZdcWest
  "",			//ZdcEast
  "",			//MwpcWest
  "MWPCEAST",		//MwpcEast
  "",			//TpcSsd
  "",			//TpcSvt
  "",			//TpcSsdSvt
  "",			//SsdSvt
  "PHMDCPV",		//PhmdCpv
  "PHMD",		//Phmd
  "PXMO",		//Pxl
  "IBMO",		//Ist
  "FGMO",		//Fgt
  "FBOX",		//FpdWest
  "FBOX",		//FpdEast
  "FBOX",		//Fms
  "",			//Rps
  };		
  		  
void myBreak(int i) 
{ 
static int iCatch=-999;
  if (iCatch!=i) return;
  printf("myBreak:iCatch=%d\n",i);
}
//_____________________________________________________________________________
StTGeoHelper::StTGeoHelper()
{
  assert(!gStTGeoHelper);
  gStTGeoHelper=this;
  memset(fBeg,0,fEnd-fBeg);  
  fVoluInfoArr = new TObjArray();
  fHitPlaneArr = new TObjArray();
  fModuLev = 2;
}
//_____________________________________________________________________________
void StTGeoHelper::Init(int mode)
{
  fMode = mode;
  InitInfo();
  if (fMode&1) InitHitShape();
  if (fMode&2) InitHitPlane();
}
//_____________________________________________________________________________
void StTGeoHelper::InitInfo()
{
  gGeoManager->CdTop();
  InitModuLev();
  
  StTGeoIter it;
  it.Print("StTGeoHelper_Init");
  const TGeoVolume *vol= *it;

  int nHits=0;
  StVoluInfo *myModu=0;
  for (;(vol=*it);++it) {
    vol->GetShape()->ComputeBBox();
    int volId = vol->GetNumber();
//		First visit
    if (it.IsFirst()) {		//First visit
//			Recognize module
//			Check for MODULE    
      do {
	if (IsModule(vol)) 		break;
	if (fModuLev != it.GetLev()) 	break;
	StVoluInfo *ext = SetFlag(vol,StVoluInfo::kModule);
        myModu = ext; nHits=0;
 	printf("Module = %s(%d) ",vol->GetName(),volId); it.Print(0);
      } while(0);

//			Update HitShape
      if (IsSensitive(vol)) nHits++;

    } else if (it.IsLast()) { 		//Last visit 

//		Define for MODULE (capital letters, module with hits)   
        if (!myModu) 		continue;
	if (!IsModule(vol)) 	continue;
	if ( IsMODULE(vol)) 	continue;
        if (nHits) {
	  myModu->SetMODULE();
 	  printf("MODULE = %s(%d) hits=%d",vol->GetName(),volId,nHits);
	  it.Print(0);
        }  
      myModu=0; nHits=0;
    }
  }

}
//_____________________________________________________________________________
void StTGeoHelper::InitModuLev()
{
  const TGeoVolume *vol = gGeoManager->GetTopVolume();
  int lev = 1;
  while (1) {
    if (vol->GetNdaughters()!=1) break;
    lev++;
    const TGeoNode *node = vol->GetNode(0);
    vol = node->GetVolume();
  }  
  fModuLev = lev;
  Info("InitModuLev","fModuLev=%d",fModuLev);
}
//_____________________________________________________________________________
void StTGeoHelper::SetModule (const char *voluName, int akt)
{
  TGeoVolume *vol = gGeoManager->FindVolumeFast(voluName);
  if (!vol) { Warning("SetModule","Volume %s Not Found",voluName);return;}

  SetFlag(vol,StVoluInfo::kModule,akt);
}
//_____________________________________________________________________________
void StTGeoHelper::SetActive (const char *voluName, int akt)
{
  TGeoVolume *vol = gGeoManager->FindVolumeFast(voluName);
  if (!vol) { Warning("SetActive","Volume %s Not Found",voluName);return;}

  SetFlag(vol,StVoluInfo::kActive,akt);
}
//_____________________________________________________________________________
void StTGeoHelper::SetActive (StDetectorId did,int akt)
{
  const char *modu = ModName(did);
  if (!*modu)  { Warning("SetActive","DetId %d Unknown",did);return;}

  TGeoVolume *vol = gGeoManager->FindVolumeFast(modu);
  if (!vol) { Warning("SetActive","Volume %s Not Found",modu);return;}
  SetFlag(vol,StVoluInfo::kActive,akt);
  Long64_t mask = 1; mask = mask<<(int)did;
  if (akt) { fActiveModu |=  mask; }
  else     { fActiveModu &= ~mask; }

}
//_____________________________________________________________________________
int StTGeoHelper::IsActive (StDetectorId did) const
{
  return ((fActiveModu & ((Long64_t)1)<<did)!=0);
}
//_____________________________________________________________________________
int StTGeoHelper::IsActive () const
{
 const TGeoVolume *mod = GetModu();  
 if (!mod) return 0;
 return (IsFlag(mod,StVoluInfo::kActive));
}
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::SetFlag(const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg,int act)
{
  int volId = volu->GetNumber();
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
int  StTGeoHelper::IsFlag(const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg) const
{
  int volId = volu->GetNumber();
  StVoluInfo *inf = GetInfo(volId);
  if (!inf ) return 0;
  return (inf->TestBit(flg)!=0);
}
//_____________________________________________________________________________
void StTGeoHelper::InitHitShape()
{
  
  StTGeoIter it;
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

  for (;(vol=*it);++it) {
    vol->GetShape()->ComputeBBox();
//		First visit
    if (!it.IsFirst()) continue;	//First visit only
//			Update HitShape
    if (!IsSensitive(vol)) continue;		

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

}
//_____________________________________________________________________________
void StTGeoHelper::InitHitPlane()
{
  fHitPlaneHardMap = new StHitPlaneHardMap;
  fSeedHits =        new StVoidArr();
  
  StTGeoIter it;
  const TGeoVolume *vol=0;
  for (;(vol=*it);++it) {
    vol->GetShape()->ComputeBBox();
//		First visit
    if (!it.IsFirst()) continue;		//First visit only
//			try to make HitPlaneInfo  
    MakeHitPlaneInfo(it);

  }

  it.Reset();
  for (;(vol=*it);++it) {
    if (!it.IsFirst()) 	continue;	//First visit only
    StHitPlaneInfo *ghp = IsHitPlane(vol);
    if (!ghp) 		continue;
    StHitPlane *hp = ghp->MakeHitPlane(it); if(hp){;}
    
  }

}
//_____________________________________________________________________________
StTGeoHelper::~StTGeoHelper()
{
  delete fVoluInfoArr;
}
//_____________________________________________________________________________
StTGeoHelper *StTGeoHelper::Instance()
{
  if (!gStTGeoHelper) gStTGeoHelper = new StTGeoHelper;
  return gStTGeoHelper;
}
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::GetInfo(int idx) const
{
  return (StVoluInfo*)((idx<fVoluInfoArr->GetSize())? (*fVoluInfoArr)[idx]:0);
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::GetINFO(int idx) 
{
  StVoluInfo *inf = GetInfo(idx);
  if (inf) return inf;
  inf = new StVoluInfo(idx);
  SetInfo(inf);
  return inf;
}    
//_____________________________________________________________________________
void StTGeoHelper::SetInfo(StVoluInfo* ext)
{
  int volId = ext->GetNumber();
  StVoluInfo* extOld = (StVoluInfo*)((fVoluInfoArr->GetSize()>volId)? (*fVoluInfoArr)[volId]:0);
  if (extOld==ext) return;
  if (extOld) {*extOld = *ext; delete extOld;}
  fVoluInfoArr->AddAtAndExpand(ext,volId);
}    
//_____________________________________________________________________________
void StTGeoHelper::AddHitPlane(StHitPlane* pla)
{
  fHitPlaneArr->Add(pla);
}    
//_____________________________________________________________________________
int StTGeoHelper::IsModule(const TGeoVolume *volu)  const
{
  StVoluInfo *ext = GetInfo(volu->GetNumber());
  if (!ext) return 0;
  return ext->IsModule();
}    
//_____________________________________________________________________________
int StTGeoHelper::IsMODULE(const TGeoVolume *volu)  const
{
  StVoluInfo *ext = GetInfo(volu->GetNumber());
  if (!ext) return 0;
  return ext->IsMODULE();
}    
//_____________________________________________________________________________
int StTGeoHelper::IsModule(const TGeoNode   *node)  const
{ return IsModule(node->GetVolume()); }
//_____________________________________________________________________________
StHitPlaneInfo* StTGeoHelper::IsHitPlane(const TGeoVolume *volu) const
{
  StVoluInfo *ext = GetInfo(volu->GetNumber());
  if (!ext) 			return 0;
  if (!ext->IsHitPlane())	return 0;
  return (StHitPlaneInfo*)ext;
}    
//_____________________________________________________________________________
StHitPlaneInfo* StTGeoHelper::IsHitPlane(const TGeoNode   *node) const
{ return IsHitPlane(node->GetVolume()); }

//_____________________________________________________________________________
int StTGeoHelper::MayHitPlane(const TGeoVolume *volu)  const
{
  enum {kHow=4};

  const TGeoShape* sh=volu->GetShape() ;
  if (!sh) 			return 0;
  if (!sh->IsValidBox()) 	return 0;     
  
  int kase = sh->IsCylType() ;
  switch (kase) {
    case 0://Plane case
    {
     const TGeoBBox *bb = (const TGeoBBox*)sh;
     double dd[3]={bb->GetDX(),bb->GetDY(),bb->GetDZ()};
     int jMin = 0;
     for (int j=1;j<3;j++) { if (dd[j]<dd[jMin]) jMin=j;}
     for (int j=0;j<3;j++) { if (j==jMin) continue;  if (dd[j]<kHow*dd[jMin]) return 0;} 
     return jMin+1;
    }
    case 1: //Tube case
     {
       double par[9];
       sh->GetBoundingCylinder(par);
       if ((par[1]-par[0])*kHow > (par[1]+par[0])) return 0;
       return 4;
    }
  }
  return 0;
}
//_____________________________________________________________________________
int StTGeoHelper::MakeHitPlaneInfo(const StTGeoIter &it) 
{
static int nCall=0; nCall++;   

   const TGeoVolume *myVolu = *it;
   if (!IsSensitive(myVolu)) 			return 0;
   const TGeoVolume *volu=0;
   StHitPlaneInfo *bhp=0;
   for (int up=0;(volu=it.GetVolu(up));up++) {

     int ax = MayHitPlane(volu);
     if (!ax) 	continue;

     const TGeoShape *sh = volu->GetShape();
     const TGeoBBox *bb = (const TGeoBBox*)sh;
     int iv = volu->GetNumber();
     StVoluInfo *ext = GetInfo(iv);
     int kase = 0;
     if (ext) kase = ext->Kind();
     switch(kase) {

       case 0: //no extention
       case 1: //basic extention
	 bhp = new StHitPlaneInfo(iv);
	 if (ext) *bhp = *ext;
	 bhp->SetHitPlane();
         bhp->SetAxis(ax);
         if (ax<=3) {
           for (int jk=0;jk<3;jk++){bhp->fDir[jk][(ax+jk-1)%3]=1;}
	   memcpy(bhp->fOrg,bb->GetOrigin(),3*sizeof(double));

	 }else if(ax==4) {
           for (int jk=0;jk<3;jk++){bhp->fDir[jk][jk]=1;}

         }else { assert(0 && "Wrong axis number"); }

	 SetInfo(bhp);
	 delete ext;
	 break;

       case 2: //HitPlane  extention
	 bhp = (StHitPlaneInfo*)ext; 
	 break;
       default: assert(0 && " Wrong kind of StVoluInfo");
     }
     break;
  }
  if (bhp) return 0;
  printf(" ***Warning: HitPlane not found for:"); it.Print(0);
  return 0;
}
//_____________________________________________________________________________
int StTGeoHelper::IsSensitive(const TGeoVolume *volu)
{
  const TGeoMedium *tm = volu->GetMedium();
  if (!tm) return 0;
  return (tm->GetParam(kISVOL)>0.);
}
//_____________________________________________________________________________
const TGeoVolume *StTGeoHelper::GetModu() const
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
  if (v && v->GetMaterial()) {
    printf("(%s)",v->GetMaterial()->GetName());
    if (IsSensitive(v)) printf("*");
  }
  printf("\n");    

}
//_____________________________________________________________________________
void StTGeoHelper::ls(const char *opt) const
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
//    int volId = vol->GetNumber();
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
    const TGeoMaterial *ma = vol->GetMaterial();
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
const char *StTGeoHelper::GetPath() const     
{
  return gGeoManager->GetPath();
}
//_____________________________________________________________________________
void StTGeoHelper::Test()
{
   gROOT->Macro("$STAR/StarDb/VmcGeometry/y2009.h");
   StTGeoHelper &hlp = *StTGeoHelper::Instance();
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
    hlp.AddHit((void*)hp,hp->GetPnt(),num,1);
  }
  hlp.InitHits();
  hlp.ls("p");
}
//_____________________________________________________________________________
void StTGeoHelper::Break(int kase) 
{
static int myKase = -2009;
if (kase!=myKase) return;
printf("Break(%d)\n",kase); 
}
//_____________________________________________________________________________
const StHitPlane *StTGeoHelper::AddHit(void *hit,const float xyz[3],unsigned int hardw,int seed)
{
static int nCall = 0;  nCall++;  
//   Break(nCall);
   if (seed) {//add to seed hit collection
     fSeedHits->push_back(hit);
  } 
  StHitPlaneHardMapIter it(fHitPlaneHardMap->find(hardw));
  StHitPlane *hp=0;
  TGeoNode   *node;
  if (it !=  fHitPlaneHardMap->end()) { //HitPlane found
     hp = (*it).second;
  } else {   
     node = gGeoManager->FindNode(double(xyz[0]),double(xyz[1]),double(xyz[2]));
     assert(node);
     hp = GetCurrentHitPlane();     
//     assert(hp);
     if (!hp) {
       Warning("AddHit","Hit in %s Not Found",GetPath());
       return 0;
     }
     (*fHitPlaneHardMap)[hardw]=hp;

  }
  assert(hp);
  hp->AddHit(hit,xyz);
  if (hp->GetNHits()==1) AddHitPlane(hp);


  return hp;
}
//_____________________________________________________________________________
StHitPlane *StTGeoHelper::GetCurrentHitPlane ()
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
void StTGeoHelper::ClearHits()
{
  fSeedHits->clear();
  int n = fHitPlaneArr->GetLast()+1;
  for (int i=0;i<n;i++) {
    StHitPlane *hp = (StHitPlane *)fHitPlaneArr->At(i);
    hp->Clear();
  }
  fHitPlaneArr->Clear();
}    
//_____________________________________________________________________________
void StTGeoHelper::InitHits()
{

  int n = fHitPlaneArr->GetLast()+1;
  for (int i=0;i<n;i++) {
    StHitPlane *hp = (StHitPlane *)fHitPlaneArr->At(i);
    hp->InitHits();
  }
}    
//_____________________________________________________________________________
int StTGeoHelper::DetId(const char *detName) 
{
   for (int i = 0;gDetName[i]; i++) 
   { if (strcmp(detName,gDetName[i])==0) return i;}
   return 0;
}
//_____________________________________________________________________________
const char *StTGeoHelper::DetName(int detId)
{
 if (detId >= int(sizeof(gDetName)/sizeof(char*))) detId=0;
 return gDetName[detId];
}
//_____________________________________________________________________________
const char *StTGeoHelper::ModName(int detId)
{
 if (detId >= int(sizeof(gModName)/sizeof(char*))) detId=0;
 return gModName[detId];
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
}
//_____________________________________________________________________________
StHitPlane *StHitPlaneInfo::MakeHitPlane(const StTGeoIter &it)

{
  const TGeoVolume *volu = *it;
  const TGeoShape  *sh = volu->GetShape();
  StHitPlane *hp=0;
  StHitTube  *ht=0;
  TString path(it.GetPath());
  hp= (StHitPlane *)GetHitPlane(path);
  if (hp) return 0;
  if (fAxi<=3) {
    hp = new StHitPlane(path,volu->GetNumber());
  } else if (fAxi==4) {
    StHitTube *ht = new StHitTube(path,volu->GetNumber()); hp = ht;
  }
  fHitPlanePathMap[path] = hp;
  it.LocalToMaster(fOrg, hp->fOrg);
  for (int i=0;i<3;i++) {it.LocalToMasterVect(fDir[i], hp->fDir[i]);}
  if (ht) {
    double par[9];float pnt[3]={0};
    sh->GetBoundingCylinder(par);
    double rMed = (sqrt(par[0])+sqrt(par[1]))*0.5;
    double pMed = 0.5*(par[2]+par[3])*M_PI/180;
    pnt[0] = rMed*cos(pMed); 
    pnt[1] = rMed*sin(pMed); 
    it.LocalToMaster(pnt, ht->fPnt);
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
   memset(fOrg,0,sizeof(fOrg)+sizeof(fDir));
   SetUniqueID(voluId);
   fHitMap = new StMultiKeyMap(2);
}
//_____________________________________________________________________________
StHitPlane::~StHitPlane() 	
{
   delete fHitMap;fHitMap = 0;
}
//_____________________________________________________________________________
void StHitPlane::AddHit(void *hit,const float xyz[3])
{
  float x[3],xx[3];
  TCL::vsub(xyz, fOrg, x,3);
  TCL::vmatl(GetDir(xyz)[0], x, xx, 3, 3);
  fHitMap->Add(hit,xx+1);

}
//_____________________________________________________________________________
int StHitPlane::GetNHits() const	
{
return (fHitMap)?fHitMap->Size():0;
}
//_____________________________________________________________________________
void StHitPlane::InitHits()
{
  fHitMap->MakeTree();
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
 memset(fPnt,0,sizeof(fPnt));
}
//_____________________________________________________________________________
const Mtx33_t &StHitTube::GetDir(const float *hit) const
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
  const TGeoMaterial *mat = v->GetMaterial();
  const char* maName = (mat)? mat->GetName():"";
  printf("(%s)",maName);
  const TGeoMedium *tm = v->GetMedium();
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
//_____________________________________________________________________________
void  StTGeoHelper::ShootZR(double z,double rxy) 
{
    typedef std::set<std::string> MySet_t ;
    MySet_t  mySet;

    double step = rxy*0.01; 
    if (step>1.0) step = 1;
    if (step<0.1) step = 0.1;
    int nStep = (int)(2*M_PI*rxy/step);
    double dAng = (2*M_PI)/nStep;
    
    for (int iStep=0;iStep<nStep;iStep++) {
      double xyz[3];xyz[2]=z;
      xyz[0]= rxy*cos(iStep*dAng);
      xyz[1]= rxy*sin(iStep*dAng);
      
      gGeoManager->FindNode(xyz[0],xyz[1],xyz[2]);
      TString tPath(gGeoManager->GetPath()); 
//      printf("path=%s\n",tPath.Data());
      if (gGeoManager->GetLevel()<2) 	continue;
      tPath.Replace(0,15,"");
      int j = tPath.Length()-1;
      if (j<0) 				continue;
      for(;tPath[j]!='_'&&j>0;j--){}
      if (j<0) 				continue;
      tPath.Replace(j,999,"");
      tPath.Replace(4,tPath.Length()-8,"...");
//      printf("path=%s\n",tPath.Data());
      std::string path(tPath.Data()); 
      mySet.insert(path);
//      printf("path=%s\n",path.c_str());
   }
   MySet_t::iterator it; int cnt=0;
   for (it=mySet.begin();it != mySet.end();++it) {
     cnt++; printf("%4d - %s\n",cnt,(*it).c_str());
   }

}


//_____________________________________________________________________________
//_____________________________________________________________________________
#if 0
StTGeoPath::StTGeoPath(const int *path)
{
  mPath = new int[path[0]+2];
  for (int j=0;j<path[0]+2;j++) {mPath[j]=path[j];}
}

//_____________________________________________________________________________
StTGeoPath::StTGeoPath(const StTGeoPath &fr)
{
  mPath = new int[fr.mPath[0]+2];
  for (int j=0;j<fr.mPath[0]+2;j++) {mPath[j]=fr.mPath[j];}
}

//_____________________________________________________________________________
bool StTGeoPath::operator<(const StTGeoPath &other) const
{
   if (mPath[0]!=other.mPath[0])
     return (mPath[0]<other.mPath[0])? true:false;
   for (int j= mPath[0]+1;j>0;j--) {
     if (mPath[j]==other.mPath[j]) continue;
     return (mPath[j]<other.mPath[j])? true:false;
   }
   return false;
}

//_____________________________________________________________________________
void StTGeoPath::Print() const
{
  for (int i=0;i<=mPath[0];i++) {printf("%d\t ",mPath[i]);}
  printf("\n");
}
#endif



