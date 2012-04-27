// $Id: StTGeoHelper.h,v 1.21 2012/04/27 00:14:25 perev Exp $
//
//
// Class StTGeoHelper
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
class StActiveFunctor;

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
virtual ~StVoluInfo(){;}
        int IsModule  ()        const   {return TestBit(kModule);}
        int IsMODULE()          const   {return TestBit(kMODULE);}
        int IsHitPlane()        const   {return TestBit(kHitPlane);}
        int IsActive()          const   {return TestBit(kActive);}
       void SetModule  (int s=1)        {SetBit(kModule,s)      ;}
       void SetMODULE  (int s=1)        {SetBit(kMODULE,s)      ;}
       void SetHitPlane(int s=1)        {SetBit(kHitPlane,s)    ;}
       void SetActive  (int s=1)        {SetBit(kActive,s)      ;}
       void SetActiveFunctor(StActiveFunctor *af)	
       					{fActiveFunctor=af      ;}
        int GetNumber()         const   {return GetUniqueID()   ;}
const  TGeoVolume* GetVolu()    const;
const char *GetName() const;
StActiveFunctor *GetActiveFunctor() 	{return fActiveFunctor  ;}
virtual int Kind()              const   {return kVoluInfo	;}
       void AddSens() 			{fNSens++;		;}
        int GetSens() 			{return fNSens;		;}

protected:
StActiveFunctor *fActiveFunctor;
int fNSens;
ClassDef(StVoluInfo,0) //
};



// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitPlaneInfo : public StVoluInfo
{
friend class StTGeoHelper;
public:
      StHitPlaneInfo(int volId);
virtual ~StHitPlaneInfo(){;}
void  operator=(const StVoluInfo& ext){*((StVoluInfo*)this)=ext;}
      int Kind() const          	{return kHitPlaneInfo;}
      int Axis() const          	{return fAxi;}
const Mtx33F_t &GetDir() const   	{return fDir;}
const float   *GetOrg() const   	{return fOrg;}
      StHitPlane *MakeHitPlane(const StTGeoIter &it);
      StHitPlane *GetHitPlane (const TString &path) const;
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
friend class StTGeoHelper;
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
              int   GetNumber()  	{return GetUniqueID(); }
             void   ToLocal(const float xyz[3],float uv[3]) const;
     StDetectorId   GetDetId() const 	{ return fDetId;}    
             void   SetDetId(StDetectorId id){ fDetId=id;}    
int   GetNHits() const;
const StMultiKeyMap *GetHitMap() const {return fHitMap;}

protected:
char  fBeg[1];
StDetectorId fDetId;
float fOrg[3];
float fDir[3][3];
TNamed *fHitErrCalc;
StMultiKeyMap *fHitMap;
char  fEnd[1];

ClassDef(StHitPlane,0) //
};
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StHitTube : public StHitPlane
{
friend class StTGeoHelper;
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
class StTGeoSele : public TNamed
{
public:
    StTGeoSele(const char *name=""):TNamed(name,""){}
virtual ~StTGeoSele(){}
virtual int Select(const char *path,int copyNumber, const float xyz[3]) const =0;
ClassDef(StTGeoSele,0)
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StActiveFunctor : public TNamed
{
public:
    StActiveFunctor(const char *name=""):TNamed(name,""){}
virtual ~StActiveFunctor(){}
virtual int operator()(const double xyz[3])=0;
int GetIPath(int nLev,int *copyNums,const char **voluNams=0) const;
ClassDef(StActiveFunctor,0)
};

// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __
class StTGeoHelper : public TObject
{
         StTGeoHelper();
virtual ~StTGeoHelper();

public:
       int  Load(const char *geo);
       void SetOpt (int opt)	{fOpt = (opt!=0);}
       void Init(int mode=0);
       void Finish();

 StVoluInfo *SetModule (const char *voluName,int akt=1);
       void InitInfo();
       void InitModuLev();

       void InitHitShape();

       void SetHitPlane(const char *moduName,const char *voluName,int axis=1);
       void InitHitPlane();
        int InitHits();
       void ClearHits();
       void Clear(const char *opt="");

        int SetHitErrCalc(StDetectorId modId,TNamed *hitErrCalc,const StTGeoSele *sel=0);
static  StTGeoHelper *Instance();
static  StTGeoHelper *Inst(){return Instance();};

public:
        void  Print(const char *tit=0) const;
        void  ls(const char* opt="Mmps")                const;
        void  SetInfo   (StVoluInfo *ext);
  StVoluInfo *SetActive (const char *voluName,int act=1,StActiveFunctor *af=0);
  StVoluInfo *SetActive (StDetectorId did,int act=1,StActiveFunctor *af=0);
const TGeoVolume *FindModule(const char *patt);
  StVoluInfo *SetFlag   (const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg,int act=1);
         StVoluInfo *IsFlag    (const TGeoVolume *volu,StVoluInfo::E_VoluInfo flg) const;
         StVoluInfo *IsModule  (const TGeoVolume *volu)        const;
         StVoluInfo *IsModule  (const TGeoNode   *node)        const;
         StVoluInfo *IsMODULE  (const TGeoVolume *volu)        const;
         StVoluInfo *IsActive  (const TGeoVolume *volu=0)      const;                  
         int  IsActive  (StDetectorId did)              const;
         int  IsHitted  (const double X[3])             const;
static   int  IsSensitive(const TGeoVolume *volu=0);
static   StDetectorId  DetId(const char *detName);
static  const char *DetName(StDetectorId detId);
static  const char *ModName(StDetectorId detId);



   StVoluInfo *GetInfo  (int idx)       const;
   StVoluInfo *GetINFO  (int idx);
const   char  *GetPath() const;
const TGeoVolume *GetModu() const;
const TGeoVolume *GetVolu() const;
StVoidArr     *GetSeedHits()         const {return fSeedHits       ;}
StVoidArr     *GetAllHits()          const {return fAllHits        ;}
StHitPlaneHardMap *GetPlaneHardMap() const {return fHitPlaneHardMap;}

StHitPlaneInfo* IsHitPlane(const TGeoVolume *volu) const;
StHitPlaneInfo* IsHitPlane(const TGeoNode   *node) const;
StHitPlane   *GetCurrentHitPlane ();

             int  MayHitPlane     (const TGeoVolume *volu) const;
  StHitPlaneInfo *MakeHitPlaneInfo(const StTGeoIter &iter);
        void  AddHitPlane(StHitPlane *pla);

const StHitPlane *AddHit(void *hit,const float xyz[3],unsigned int hardw,int seed);
      StHitPlane *FindHitPlane(const float xyz[3],int &sure);

        void  ShootZR(double z,double rxy);

const StTGeoHitShape* GetHitShape() const {return fHitShape;}

static void Test();
static void Break(int kase);
private:


private:
char fBeg[1];
int fMode;      //0=fill infos + hitShape, 1= hit planes
int fOpt;      //0=Optimisation Off, !=0= Optimization ON
Long64_t	fActiveModu;
TObjArray      *fVoluInfoArr;           // array of all StVoluIinfo
TObjArray      *fHitPlaneArr;           // array of StHitPlane's
StHitPlaneHardMap *fHitPlaneHardMap;    // StHitPlane[hardwarePosition]
StVoidArr      *fSeedHits;              // Vector for hits used in seed finder
StVoidArr      *fAllHits;               // Vector of all hits, mainly for debug
StTGeoHitShape *fHitShape;
char fEnd[1];
ClassDef(StTGeoHelper,0) //
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
     StTGeoHitShape(double zMin,double zMax);
void Update(double z1, double z2, double rxy);
int  Inside(double z,double rxy) const;
void Get(double &zMin,double &zMax,double &rMax) const;


private:
enum {kNZ=100};
double fZMin;
double fZMax;
double fRMax;
double fRxy[kNZ];
};
#endif //ST_TGEOHELPER_H

