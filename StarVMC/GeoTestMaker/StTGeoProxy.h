// $Id: StTGeoProxy.h,v 1.9 2017/05/02 19:35:06 perev Exp $
//
//
// Class StTGeoProxy
// ------------------
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __


#ifndef ST_TGEOHELPER_H
#define ST_TGEOHELPER_H
#include <map>
#include <vector>
#include "StEnumerations.h"
#include "TString.h"
#include "TNamed.h"
class TObjArray;
class TGeoNavigator;
class TGeoNode;
class TGeoVolume;
class TGeoMedium;
class TGeoMaterial;
class TGeoHMatrix;
class StVoluInfo;
class StTGeoHitShape;
class StTGeoIter;
class StHitPlaneInfo;
class StMultiKeyMap;
class StActorFunctor;

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
struct InvertLtStr
{
bool operator()(const TString& s1, const TString& s2) const
  {
    int n1 = s1.Length(),n2 = s2.Length();
    if (n1 != n2) return n1<n2;
    const char *c1 = s1.Data(),*c2 = s2.Data();
    for (int j=n1-1; j>=0; --j) {if (c1[j]!=c2[j]) return (c1[j]<c2[j]);}
    return 0;
  }
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitPlane;
typedef std::map< unsigned int, StHitPlane*>    StHitPlaneHardMap;
typedef std::pair<unsigned int, StHitPlane*>    StHitPlaneHardPair;
typedef StHitPlaneHardMap::const_iterator       StHitPlaneHardMapIter;

class StVoluInfo;
typedef std::map< unsigned int, StVoluInfo*>    StVoluInfoMap;
typedef StVoluInfoMap::const_iterator           StVoluInfoMapIter;


class StVoidArr:public std::vector<void*>{};


typedef std::map< const TString, StHitPlane*, InvertLtStr>  StHitPlanePathMap;
typedef std::pair<const TString, StHitPlane*>               StHitPlanePathPair;
typedef StHitPlanePathMap::const_iterator                   StHitPlanePathMapIter;
typedef float Mtx33F_t[3][3];

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StVoluInfo : public TObject
{
public:
enum E_VoluInfo {
      kModule      = BIT(15),   // The volume is a module
      kMODULE      = BIT(16),   // The volume is a module,with ID like TPCE,SVTT
      kActive      = BIT(17),   // The volume is active
      kHitted      = BIT(18),   // The volume has hits under it
      kHitPlane    = BIT(19)};  // The volume is a Hit plane
public:
enum E_Kind { kVoluInfo=1,kHitPlaneInfo=2};

         StVoluInfo(int voluNumber);
virtual ~StVoluInfo();
        int IsModule  ()        const   {return TestBit(kModule);}
        int IsMODULE()          const   {return TestBit(kMODULE);}
        int IsHitPlane()        const   {return TestBit(kHitPlane);}
        int IsActive()          const   {return TestBit(kActive);}
       void SetModule  (int s=1)        {SetBit(kModule,s)      ;}
       void SetMODULE  (int s=1)        {SetBit(kMODULE,s)      ;}
       void SetHitPlane(int s=1)        {SetBit(kHitPlane,s)    ;}
       void SetActive  (int s=1)        {SetBit(kActive,s)      ;}
       void SetActiveFunctor(StActorFunctor *af)	
       					{fActiveFunctor=af      ;}
        int GetVoluId()         const   {return GetUniqueID()   ;}
const  TGeoVolume* GetVolu()    const;
const char *GetName() const;
StActorFunctor *GetActiveFunctor() 	{return fActiveFunctor  ;}
virtual int Kind()              const   {return kVoluInfo	;}
       void AddSens() 			{fNSens++;		;}
        int GetSens() 			{return fNSens;		;}

protected:
StActorFunctor *fActiveFunctor;
int fNSens;
ClassDef(StVoluInfo,0) //
};



// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitPlaneInfo : public StVoluInfo
{
friend class StTGeoProxy;
public:
      StHitPlaneInfo(int volId);
virtual ~StHitPlaneInfo();
void  operator=(const StVoluInfo& ext){*((StVoluInfo*)this)=ext;}
      int Kind() const          	{return kHitPlaneInfo;}
      int Axis() const          	{return fAxi;}
const Mtx33F_t &GetDir() const   	{return fDir;}
const float   *GetOrg() const   	{return fOrg;}
      StHitPlane *MakeHitPlane(const StTGeoIter &it,StActorFunctor *act=0);
      StHitPlane *GetHitPlane (const TString &path) const;
      StHitPlane *RemHitPlane (const TString &path);
      void SetAxis(int axi)     	{fAxi=axi;}
      void SetDetId(StDetectorId id)	{ fDetId=id;}    
      StDetectorId GetDetId() const 	{ return fDetId;}    
void  Print(const char* opt="") const;
protected:
StDetectorId fDetId;
int   fAxi;
float fOrg[3];
float fDir[3][3];
StHitPlanePathMap fHitPlanePathMap;
ClassDef(StHitPlaneInfo,0) //
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitPlane : public TNamed
{
friend class StTGeoProxy;
friend class StHitPlaneInfo;
public:
      StHitPlane(const char *path, int volId);
virtual ~StHitPlane();
void  Clear(const char *opt="");
int   InitHits();
void  SetHitErrCalc(TNamed *hitErrCalc)	{fHitErrCalc = hitErrCalc;}
TNamed *GetHitErrCalc()	const 		{return (TNamed*)fHitErrCalc;}

virtual const Mtx33F_t &GetDir(const float *) const {return fDir;}
virtual const float    *GetOrg(const float *) const {return fOrg;}
virtual const float    *GetPnt()        const {return fOrg;}
virtual       void  AddHit(void *hit,const float xyz[3]);
virtual       int   Kind() const 	{return 0;}
              int   GetVoluId()  	{return GetUniqueID(); }
             void   ToLocal(const float xyz[3],float uv[3]) const;
     StDetectorId   GetDetId() const 	{ return fDetId;}    
             void   SetDetId(StDetectorId id){ fDetId=id;}    
int   GetNHits() const;
float GetLayer() const 		{ return fNex;}
void  SetLayer(); 	
const StMultiKeyMap *GetHitMap() const {return fHitMap;}
void        SetPath(const char *path) { fPath=path;  }
const char *GetPath() const           { return fPath;}
protected:
char  fBeg[1];
StDetectorId fDetId;
float fOrg[3];
float fDir[3][3];
float fNex;		//distance to next layer
TNamed *fHitErrCalc;
StMultiKeyMap *fHitMap;
const char *fPath;
char  fEnd[1];

ClassDef(StHitPlane,0) //
};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitTube : public StHitPlane
{
friend class StTGeoProxy;
friend class StHitPlaneInfo;
public:
      StHitTube(const char *path, int volId);
virtual ~StHitTube(){;}

virtual const Mtx33F_t &GetDir(const float *) const ;
virtual const float   *GetOrg(const float *) const ;
virtual const float   *GetPnt() const {return fPnt;}
virtual       int      Kind()   const {return 1;}
protected:
char  fBeg[1];
float fPnt[3];
float fRmed;
char  fEnd[1];

ClassDef(StHitTube,0) //
};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StActorFunctor : public TNamed
{
public:
    StActorFunctor(const char *name=""):TNamed(name,""){}
virtual ~StActorFunctor(){}

virtual int operator()(const double xyz[3]=0)=0;
        int Operator  (const float  xyz[3]);

       int  GetIPath(int nLev,int *copyNums,const char **voluNams=0) const;
       int  GetDetId() const 	{return mDetId;}
      void  SetDetId(int det) 	{mDetId = det ;}
      void  SetHit(void *hit) 	{mHit   = hit ;}
const char* GetPath()       const;
const TGeoVolume *GetVolu() const;
const TGeoNode   *GetNode() const;
      StVoluInfo *GetInfo() const;
      StHitPlaneInfo *GetHitPlaneInfo() const;
      StHitPlane     *GetHitPlane()     const;

protected:
int mDetId;
void *mHit;
ClassDef(StActorFunctor,0)
};
inline int StActorFunctor::Operator(const float xyz[3]) 
{
  if (!xyz) return (*this)();
  double d[3]={xyz[0],xyz[1],xyz[2]};
  return (*this)(d);
}
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StvSetLayer: public StActorFunctor
{
public:
    StvSetLayer();
   ~StvSetLayer(){}
int operator()(const double xyz[3]=0);
private:

ClassDef(StvSetLayer,0)
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StTGeoProxy : public TObject
{
         StTGeoProxy();
virtual ~StTGeoProxy();

public:
       int  Load(const char *geo);
       void SetOpt (int opt)	{fOpt = opt;}
       void Init(int mode=0);
       void InitLayers(StDetectorId did=kUnknownId);
       void Finish();
        int Edit(StDetectorId did,StActorFunctor *af);

 StVoluInfo *SetModule (const char *voluName,int akt=1);
       void InitInfo();
       void InitModuLev();

       void InitHitShape();

       void SetHitPlane(const char *moduName,const char *voluName,int axis=1);
       void InitHitPlane(StActorFunctor *act=0);
        int InitHits();
       void ClearHits();
       void Clear(const char *opt="");
       double Look(double maxDist,const double pnt[3],const double dir[3]);

        int SetHitErrCalc(StDetectorId modId,TNamed *hitErrCalc,StActorFunctor *sel=0);
static  StTGeoProxy *Instance();
static  StTGeoProxy *Inst(){return Instance();};

public:
        void  Print(const char *tit=0) const;
        void  ls(const char* opt="Mmps")                const;
        void  SetInfo   (StVoluInfo *ext);
         int  SetActive (const char *voluName,int act=1,StActorFunctor *af=0);
         int  SetActive (StDetectorId did,int act=1,StActorFunctor *af=0);
const TGeoVolume *FindModule(const char *patt);
  StVoluInfo *SetFlag   (const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg,int act=1);
         StVoluInfo *IsFlag    (const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg) const;
         StVoluInfo *IsModule  (const TGeoVolume *volu)        const;
         StVoluInfo *IsModule  (const TGeoNode   *node)        const;
         StVoluInfo *IsMODULE  (const TGeoVolume *volu)        const;
         StVoluInfo *IsActive  (const TGeoVolume *volu=0)      const;                  
         int  IsGoodHit ()  const 	{return fGoodHit;}
         int  IsActive  (StDetectorId did)              const;
         int  IsHitted  (const double X[3])             const;
static   int  IsSensitive(const TGeoVolume *volu=0);
static   StDetectorId  DetId(const char *detName);
static  const char *DetName(StDetectorId detId);
static  const char *ModName(StDetectorId detId);
static         int  MakeDir(int kode,float dir[3][3]);
static        void  Summary();



   StVoluInfo  *GetInfo  (int idx)       const;
   StVoluInfo *&GetINFO  (int idx);
const   char  *GetPath() const;
const TGeoVolume *GetModu() const;
const TGeoVolume *GetVolu() const;
StVoidArr     *GetSeedHits()         const {return fSeedHits       ;}
StVoidArr     *GetAllHits()          const {return fAllHits        ;}

StHitPlaneInfo* IsHitPlane(const TGeoVolume *volu) const;
StHitPlaneInfo* IsHitPlane(const TGeoNode   *node) const;
StHitPlane    * GetCurrentHitPlane ();

             int  MayHitPlane     (const TGeoVolume *volu) const;
  StHitPlaneInfo *MakeHitPlaneInfo(const StTGeoIter &iter);
        void  AddHitPlane(StHitPlane *pla);

const StHitPlane *AddHit(void *hit,StDetectorId detId, const float xyz[3],unsigned int hardw,int seed);
      StHitPlane *FindHitPlane(const float xyz[3],int &sure);


const StTGeoHitShape* GetHitShape() const {return fHitShape;}
        void SetHitLoadActor(StActorFunctor *fun) {fHitLoadActor = fun;}


static void Test();
static void Break(int kase);
private:


private:
char fBeg[1];
int fMode;     //0=fill infos + hitShape, 1= hit planes
int fOpt;      //0=Optimisation Off, !=0= Optimization ON
int fGoodHit;  //1=last loaded hit inside of sensitive volume
StDetectorId    fDetId;
Long64_t	fActiveModu;
StVoluInfoMap  *fVoluInfoArr;           // array of all StVoluIinfo
TObjArray      *fHitPlaneArr;           // array of StHitPlane's
StVoidArr      *fSeedHits;              // Vector for hits used in seed finder
StVoidArr      *fAllHits;               // Vector of all hits, mainly for debug
StTGeoHitShape *fHitShape;

//	Actor functorss
StActorFunctor *fHitLoadActor;		// functor used in AddHit

char fEnd[1];
public:
static int fgKount[3];

ClassDef(StTGeoProxy,0) //
};


// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StTGeoIter
{
public:
                  StTGeoIter();
                 ~StTGeoIter();
             void Reset();
StTGeoIter   &operator++();
const TGeoVolume *operator*() const {return fVolu;}
StTGeoIter   &Next();
StTGeoIter   &Down();
StTGeoIter   &Upp();
StTGeoIter   &Skip();

const TGeoNode   *GetNode(int idx=0) const;
const TGeoVolume *GetVolu(int idx=0) const;
const TGeoHMatrix*GetMatr(int idx=0) const;
             int  GetLev() const {return fLev;}
             int  GetIdx() const {return (fLev)? fStk[fLev-1]:0;}
const        int *GetStk() const {return fStk;}
             int  IsFirst()const {return fNow ==1;}
             int  IsLast ()const {return fNow ==2;}
             int  IsEnd()  const {return fVolu==0;}
             int  IsOmule()const; //candidate to module
             int  State()  const {return fNow;}
            void  LocalToMaster    (const double* local, double* master) const;
            void  LocalToMasterVect(const double* local, double* master) const;
            void  LocalToMaster    (const float*  local, float*  master) const;
            void  LocalToMasterVect(const float*  local, float*  master) const;
            void  Print(const char *tit="") const;
const       char *GetPath() const;
private:

private:
        int  fNow; // 1=first visit,2=last visit,3=first and last visit
        int  fKase;
        int  fLev;
        int  fStk[100];
TGeoVolume    *fVolu;
TGeoNavigator *fNavi;

};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StTGeoHitShape
{
public:
     StTGeoHitShape();
void Update(double z1, double z2, double rxy);
void Smooth(double zl=-100, double zr=100);
int  Outside(double z,double rxy) const;
void Get(double &zMin,double &zMax,double &rMax) const;
void Print(const char *opt=0) const;

private:
enum E_Safe {kSafe=110}; 	// Used Rmax = Rmax*kSafe/100, etc...
enum {kNZ=20};			// Number of Z bins

private:
double fZMin;
double fZMax;
double fRMin;
double fRMax;
double fZStp;
double fRMinMax;
double fRMaxMin;

double fRxy[kNZ][2];
};
#endif //ST_TGEOHELPER_H

