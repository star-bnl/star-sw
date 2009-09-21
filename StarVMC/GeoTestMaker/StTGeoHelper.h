// $Id: StTGeoHelper.h,v 1.4 2009/09/21 23:42:20 perev Exp $
//
//
// Class StTGeoHelper
// ------------------


#ifndef ST_TGEOHELPER_H
#define ST_TGEOHELPER_H
#include <map>

#include "TString.h"
#include "TObject.h"
class TObjArray;
class TGeoNavigator;
class TGeoNode;
class TGeoVolume;
class TGeoMedium;
class TGeoMaterial;
class TGeoHMatrix;
class StVoluExt;
class StTGeoHitShape;
class StTGeoIter;
class StGenHitPlane;

class StTGeoHelper : public TObject
{
         StTGeoHelper();
virtual ~StTGeoHelper();

public:
static	StTGeoHelper *Instance(); 

public:
	void  Print(const char *tit=0) const;
        void  ls(const char* opt="Mmps")                const;
	void  SetModule   (const char *voluName);
	void  SetModule   (int   moduLevl=2) {fModuLev = moduLevl;}
	 int  IsModule    (const TGeoVolume *volu) 	const;
	 int  IsModule    (const TGeoNode   *node) 	const;
	 int  IsMODULE    (const TGeoVolume *volu) 	const;
StGenHitPlane* IsHitPlane (const TGeoVolume *volu) 	const;
StGenHitPlane* IsHitPlane (const TGeoNode   *node) 	const;
	 int  MayHitPlane (const TGeoVolume *volu)      const;
	 int  MakeGenHitPlane(const StTGeoIter &volu)        ;
const   char *GetPath() const;     
static	 int  IsSensitive(const TGeoVolume *volu) 	     ;

        void  ShootZR(double z,double rxy);

const StTGeoHitShape* GetHitShape() const {return fHitShape;}


	void  SetExt     (StVoluExt *ext);
   StVoluExt *GetExt     (int idx) 			const;
	void  SetPlane   (StGenHitPlane *pla);
const TGeoVolume *GetModule() const;
       void Init();
static void Test();
static void Break(int kase);
private:
char fBeg[1];
int fModuLev;
TObjArray      *fExtArr;
TObjArray      *fPlaArr;
StTGeoHitShape *fHitShape;
char fEnd[1];
ClassDef(StTGeoHelper,0) //
};

class StVoluExt : public TObject
{
enum E_StVoluExt {
      kModule      = BIT(15),   // The volume is a module, like TPCE,SVTT
      kHitPlane    = BIT(16),   // The volume is a Hit plane
      kMODULE      = BIT(17)    // The volume is a Module with Hits 
};
public:
enum E_Kind { kVoluExt,kBaseHitPlane };

   	 StVoluExt(int voluNumber)	{SetUniqueID(voluNumber);}
virtual ~StVoluExt(){;}
	int IsModule  ()   	const 	{return TestBit(kModule);}
	int IsMODULE() 		const 	{return TestBit(kMODULE);}
	int IsHitPlane() 	const 	{return TestBit(kHitPlane);}
       void SetModule  (int s=1)        {SetBit(kModule,s)      ;}
       void SetMODULE  (int s=1)     	{SetBit(kMODULE,s)      ;}
       void SetHitPlane(int s=1)     	{SetBit(kHitPlane,s)    ;}
        int GetNumber() 	const 	{return GetUniqueID()   ;}
const  TGeoVolume* GetVolu() 	const; 
virtual int Kind() 		const	{return kVoluExt;}
public:
ClassDef(StVoluExt,0) //
};


struct InvertLtStr
{
bool operator()(const TString& s1, const TString& s2) const
  {
    int n1 = s1.Length(),n2 = s2.Length();
    if (n1 != n2) return n1<n2;
    const char *c1 = s1.Data(),*c2 = s2.Data();
    for (int j=n1-1; j>=0; --j) {
      if (c1[j]==c2[j]) continue;
      return (c1[j]<c2[j]);
    }
    return 0;
  }
};

class StHitPlane; 
typedef std::map< const TString, StHitPlane*, InvertLtStr>  StHitPlaneMap;
typedef std::pair<const TString, StHitPlane*>               StHitPlanePair;
typedef StHitPlaneMap::const_iterator                       StHitPlaneMapIter;
typedef double Mtx33_t[3][3];

class StGenHitPlane : public StVoluExt
{
friend class StTGeoHelper;
public:
      StGenHitPlane(int volId);	
virtual ~StGenHitPlane(){;}
void  operator=(const StVoluExt& ext){*((StVoluExt*)this)=ext;}
      int Kind() const		{return kBaseHitPlane;}
const Mtx33_t &GetDir() const 	{return fDir;}
const double  *GetOrg() const	{return fOrg;}
      StHitPlane *MakeHitPlane(const StTGeoIter &it);
const StHitPlane *GetHitPlane (const TString &path) const;
protected:
double fOrg[3];
double fDir[3][3];
StHitPlaneMap fMap;
};

class StHitPlane 
{
friend class StTGeoHelper;
friend class StGenHitPlane;
public:
      StHitPlane();	
virtual ~StHitPlane(){;}
const Mtx33_t &GetDir() const {return fDir;}
const double  *GetOrg() const {return fOrg;}
protected:
double fOrg[3];
double fDir[3][3];
};


class StTGeoIter
{
public:
                  StTGeoIter();
                 ~StTGeoIter();
StTGeoIter   &operator++();
const TGeoVolume *operator*() const {return fVolu;}
StTGeoIter   &Next();
StTGeoIter   &Down();
StTGeoIter   &Upp();

const TGeoNode    *GetNode(int idx=0) const;
const TGeoVolume  *GetVolu(int idx=0) const;
const TGeoHMatrix *GetMatr(int idx=0) const;
             int  GetLev() const {return fLev;}
             int  GetIdx() const {return (fLev)? fStk[fLev-1]:0;}
const        int *GetStk() const {return fStk;}
             int  IsFirst()const {return fNow ==1;}
             int  IsLast ()const {return fNow ==2;}
             int  IsEnd()  const {return fVolu==0;}
             int  State()  const {return fNow;}
            void  LocalToMaster    (const double* local, double* master) const;
            void  LocalToMasterVect(const double* local, double* master) const;
            void  Print(const char *tit="") const;
const       char *GetPath() const;     
private:

private:
	int  fNow; // 1=first visit,2=last visit,3=first and last visit        	
	int  fKase;
TGeoVolume    *fVolu;
TGeoNavigator *fNavi;
	int  fLev;
	int  fStk[100];
    
};
class StTGeoHitShape
{
public:
     StTGeoHitShape(double zMin,double zMax);
void Update(double z1, double z2, double rxy);
int  Inside(double z,double rxy) const;
private:
enum {kNZ=100};
double fZMin;
double fZMax;
double fRxy[kNZ];
};
#endif //ST_TGEOHELPER_H   
   
