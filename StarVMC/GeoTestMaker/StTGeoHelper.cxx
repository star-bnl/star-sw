
// $Id: StTGeoHelper.cxx,v 1.27 2013/04/04 21:28:04 perev Exp $
//
//
// Class StTGeoHelper
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
#include "StTGeoHelper.h"
#include "StMultiKeyMap.h"

static StTGeoHelper *gStTGeoHelper=0;
typedef std::map<const TGeoVolume*,int> myVoluMap ;

ClassImp(StTGeoHelper)
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
{0,0				}};
  		  
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
  fOpt = 1;
}
//_____________________________________________________________________________
int StTGeoHelper::Load(const char *geo)
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
void StTGeoHelper::Init(int mode)
{
  fMode = mode;
  InitInfo();
  if (fMode&2) InitHitPlane();
  if (fMode&1) InitHitShape();
}
//_____________________________________________________________________________
void StTGeoHelper::Finish()
{
// Avoid deleting of TGeoManager
  gGeoManager = 0;
}
//_____________________________________________________________________________
void StTGeoHelper::InitInfo()
{
  gGeoManager->CdTop();
  std::map <int,int> moduMap;  
  StTGeoIter it;
  it.Print("StTGeoHelper_Init");
  const TGeoVolume *vol= *it;

  StVoluInfo *myModu=0;
  for (;(vol=*it);++it) {
    vol->GetShape()->ComputeBBox();
    int volId = vol->GetNumber();
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
 	  printf("MODULE%s= %s(%d) hits=%d",ts.Data(),vol->GetName(),volId,myModu->GetSens());
	  it.Print(0);
        }  
      
    }
  }

}
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::SetModule (const char *voluName, int akt)
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
StVoluInfo *StTGeoHelper::SetActive (const char *voluName, int akt,StActiveFunctor *af)
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
StVoluInfo * StTGeoHelper::SetActive (StDetectorId did,int akt,StActiveFunctor *af)
{
  const char *modu = ModName(did);
  if (!*modu)  { Warning("SetActive","DetId %d Unknown",did);return 0;}
  StVoluInfo *vi = SetActive(modu,akt,af); 
  if (!vi) return 0;
  Long64_t mask = 1; mask = mask<<(int)did;
  if (akt) { fActiveModu |=  mask; }
  else     { fActiveModu &= ~mask; }
  return vi;
}
//_____________________________________________________________________________
int StTGeoHelper::IsActive (StDetectorId did) const
{
  return ((fActiveModu & ((Long64_t)1)<<did)!=0);
}
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::IsActive (const TGeoVolume *volu) const
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
StVoluInfo *StTGeoHelper::IsFlag(const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg) const
{
  int volId = volu->GetNumber();
  StVoluInfo *inf = GetInfo(volId);
  if (!inf ) return 0;

  if(inf->TestBit(flg)==0) return 0;
  return inf;
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
    if (!IsHitPlane(vol) ) continue;		

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
  fAllHits  =        new StVoidArr();
  
  StTGeoIter it;
  StDetectorId detId=kUnknownId;
  const TGeoVolume *vol=0;
  for (;(vol=*it);++it) {
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
    if (hpi) hpi->SetDetId(detId);
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
    StHitPlaneInfo *hpi = IsHitPlane(vol);
    if (!hpi) 		continue;
    StHitPlane *hp = hpi->MakeHitPlane(it);
    if(!hp) 		continue;
    hp->SetDetId(detId);
    
  }

}
//_____________________________________________________________________________
int StTGeoHelper::SetHitErrCalc(StDetectorId modId,TNamed *hitErrCalc
                               ,const StTGeoSele *sel)

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
    assert(hp);
    if (hp->GetHitErrCalc()==hitErrCalc) 	continue;
    if (sel) {//check selector
      const TGeoNode *node = it.GetNode();
      if (!sel->Select(it.GetPath(),node->GetNumber(),hp->GetPnt()))	continue;
    }
    hp->SetHitErrCalc(hitErrCalc);
    kount++;
  }
  return kount;
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
StVoluInfo *StTGeoHelper::IsModule(const TGeoVolume *volu)  const
{
  StVoluInfo *ext = GetInfo(volu->GetNumber());
  if (!ext) return 0;
  if (!ext->IsModule()) return 0;
  return ext;
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::IsMODULE(const TGeoVolume *volu)  const
{
  StVoluInfo *ext = GetInfo(volu->GetNumber());
  if (!ext) return 0;
  if (!ext->IsMODULE()) return 0;
  return ext;
}    
//_____________________________________________________________________________
StVoluInfo *StTGeoHelper::IsModule(const TGeoNode   *node)  const
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
       const TGeoBBox *bb = (const TGeoBBox*)sh;
       par[4] = bb->GetDZ();
       par[0] = sqrt(par[0]);
       par[1] = sqrt(par[1]);
       if ((par[1]-par[0])*kHow <      (2*par[4])) return 4;
       if ((par[1]-par[0])      > kHow*(2*par[4])) return 3;
       return 0;
    }
  }
  return 0;
}
//_____________________________________________________________________________
StHitPlaneInfo *StTGeoHelper::MakeHitPlaneInfo(const StTGeoIter &it) 
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
static const float myDir[3][3][3]={{{1,0,0},{ 0,1,0},{ 0,0,1}} 
                                  ,{{0,1,0},{-1,0,0},{ 0,0,1}}
                                  ,{{0,0,1},{ 0,1,0},{-1,0,0}}};
           TCL::ucopy(myDir[ax-1][0],bhp->fDir[0],3*3);
	   TCL::ucopy(bb->GetOrigin(),bhp->fOrg,3);
           assert(bhp->fDir[0][0]+bhp->fDir[0][1]+bhp->fDir[0][2]>0.99);
	 }else if(ax==4) {
           for (int jk=0;jk<3;jk++){bhp->fDir[jk][jk]=1;}

         }else { assert(0 && "Wrong axis number"); }

	 SetInfo(bhp);
	 break;

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
void StHitPlane::SetLayer()
{
 const float *pnt = GetPnt();
 double myPnd[3]={pnt[0],pnt[1],pnt[2]};
 const float *dir = GetDir(pnt)[0];
 double myDir[3] = {dir[0],dir[1],dir[2]};
 fNex = StTGeoHelper::Look(50,myPnd,myDir);
 assert(fNex>1e-2);
}
//_____________________________________________________________________________
int StTGeoHelper::IsSensitive(const TGeoVolume *volu)
{
  if (!volu) volu = gGeoManager->GetCurrentVolume();
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
   gROOT->Macro("$STAR/StarDb/AgiGeometry/y2009.h");
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
//		Test of hardware id assignment quality
enum 		{kMaxTest=1000,kMaxFail=100};
static int 	nTest=0,nFail=0,detId=0;
//
//   Break(nCall);
  StHitPlaneHardMapIter it(fHitPlaneHardMap->find(hardw));
  StHitPlane *hp=0,*hpMap=0,*hpGeo=0;
  if (fOpt && it !=  fHitPlaneHardMap->end()) { //HitPlane found
     hpMap = (*it).second;fGoodHit=1;
  } 
  hp = hpMap;
  if (hpMap==0 || nTest < kMaxTest) { 
     nTest++;
     fGoodHit = 0;
     hpGeo = FindHitPlane(xyz,fGoodHit);
     if (!hpGeo) {
       double pnt[3]={xyz[0],xyz[1],xyz[2]};
       gGeoManager->SetCurrentPoint(pnt);
       Warning("AddHit","Hit(%g,%g,%g) in %s Not Found",pnt[0],pnt[1],pnt[2],GetPath());
       return 0;
     }
     hp = hpGeo;
     if (hpMap && hpMap != hpGeo) { //Different Hit planes found by map and geo
       double pnt[3]={xyz[0],xyz[1],xyz[2]};
       nFail++; 
       Warning("AddHit","Hit(%g,%g,%g) Different Hit Planes found %s %s"
              ,pnt[0],pnt[1],pnt[2],hpMap->GetName(),hpGeo->GetName());
       assert(nFail<=kMaxFail);
     }
     if (fGoodHit) (*fHitPlaneHardMap)[hardw]=hp;
  }
  if (hp->GetDetId() != detId) { // New detector started
    nTest=0; nFail=0; detId = hp->GetDetId();
  }
  assert(hp);

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
StHitPlane *StTGeoHelper::FindHitPlane(const float xyz[3],int &sure)
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
  assert(node);
  StHitPlane *hp = GetCurrentHitPlane();     
  sure = 1;
  if (hp  && hp->GetHitErrCalc() && IsHitted(pnt)) return hp;
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
    if (myStep > minDist) continue;
    hp = GetCurrentHitPlane();     
    if (!hp || !hp->GetHitErrCalc()) 		continue;
    if (!IsHitted(gGeoManager->GetLastPoint()))	continue;
    minDist = myStep; minHitPlane = hp; 	break;
  }
  return minHitPlane;  
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
void StTGeoHelper::Clear(const char *)
{
  ClearHits();
}
//_____________________________________________________________________________
int StTGeoHelper::InitHits()
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
StDetectorId StTGeoHelper::DetId(const char *detName) 
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
const char *StTGeoHelper::DetName(StDetectorId detId)
{
  const char *det =detectorNameById(detId);
  return det;
}
//_____________________________________________________________________________
const char *StTGeoHelper::ModName(StDetectorId detId)
{
   for (int i = 0;gMyMod[i].name; i++) 
   { if (gMyMod[i].id==detId)  return gMyMod[i].name;}
   return "";
}

//_____________________________________________________________________________
int StTGeoHelper::IsHitted(const double X[3]) const
{
  if (!IsSensitive()) return 0;
  const TGeoVolume *mo = GetModu();
  if (!IsActive(mo)) return 0;
  StVoluInfo *vi = GetInfo(mo->GetNumber());
  StActiveFunctor *af = vi->GetActiveFunctor();
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
}
//_____________________________________________________________________________
StHitPlane *StHitPlaneInfo::MakeHitPlane(const StTGeoIter &it)
{
static const double kCos45 = 1./sqrt(2.);
static int nCall=0; nCall++;
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
  assert(fabs(hp->fDir[0][0])+fabs(hp->fDir[0][1])+fabs(hp->fDir[0][2])>0.5);

  if (fabs(hp->fDir[2][2])<kCos45 && fabs(hp->fDir[1][2])>kCos45) {
//		Rotate around X to keep local Zaxis close to global one 
    double qq[3];
    TCL::ucopy(hp->fDir[2],qq,3);
    TCL::ucopy(hp->fDir[1],hp->fDir[2],3);
    TCL::ucopy(qq,hp->fDir[1],3);
    if (hp->fDir[2][2]<0) TCL::vcopyn(hp->fDir[2],hp->fDir[2],3);
    else                  TCL::vcopyn(hp->fDir[1],hp->fDir[1],3);
  }  
  
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
  hp->SetLayer();
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
   memset(fBeg,0,fEnd-fBeg+1);
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
const TGeoVolume *StTGeoHelper::FindModule(const char *patt) 
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
double StTGeoHelper::Look(double maxDist,const double pnt[3],const double dir[3])
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
    if (IsSensitive()) break;
  }
  return myStep;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
StVoluInfo::StVoluInfo(int voluNumber)     
{ SetUniqueID(voluNumber);
  fActiveFunctor=0;
  fNSens = 0;
}


//_____________________________________________________________________________
//_____________________________________________________________________________
StTGeoHitShape::StTGeoHitShape(double zMin,double zMax)
{
  fZMin = zMin; fZMax = zMax; fRMax = 0;
  memset(fRxy,0,sizeof(fRxy));
}
//_____________________________________________________________________________
void StTGeoHitShape::Update(double z1, double z2, double rxy)
{
   assert(z1>fZMin && z1<fZMax);
   assert(z2>fZMin && z2<fZMax);
   if (fRMax<rxy) fRMax=rxy;
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
void StTGeoHitShape::Get(double &zMin,double &zMax,double &rMax) const
{
  zMin = 3e33; zMax= -3e33; rMax=0;
  double step = (fZMax-fZMin)/kNZ;
  for (int i=0;i<kNZ;i++) {
    if (fRxy[i]<=0) continue;
    double z = fZMin+i*step;
    if (zMin>z) zMin=z;
    if (zMax<z+step) zMax=z+step;
    if (rMax<fRxy[i]) rMax=fRxy[i];
  }
  zMin-=step; zMax+=step; rMax += step;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StActiveFunctor)
//_____________________________________________________________________________
int StActiveFunctor::GetIPath(int nLev,int *copyNums,const char **voluNams) const
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
//_____________________________________________________________________________
#if 0
// generate dirs for FindHitPlane
{
  double PI = 3.14159265358;
  double toRad = PI/180;
  int lamStep = 30;
  int num=0;  
  for (int lam=0;lam<=180;lam+=lamStep) {
    double cosL = cos(toRad*lam);
    double sinL = sin(toRad*lam);
    double rad=sinL;
    int nPhi = (sinL*360+lamStep-1)/lamStep;
    if (!nPhi) nPhi = 1;
    int phiStep=360./nPhi;
    
    for (int phi=0;phi<360;phi+=phiStep) {
      double cosP = cos(toRad*phi);
      double sinP = sin(toRad*phi);
      double dir[3]={sinL*cosP,sinL*sinP,cosL};
      num++;
      printf("{%7.4f,\t%7.4f,\t%7.4f\t},\t//%2d\n",dir[0],dir[1],dir[2],num);
    }
  
  }
}
#endif



