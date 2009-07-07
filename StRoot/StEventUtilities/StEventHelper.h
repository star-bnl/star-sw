// Author: Victor Perev   08/04/01
#ifndef ROOT_StEventHelper
#define ROOT_StEventHelper


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventHelper                                                        //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TNamed.h"
#include "TString.h"
#include "Gtypes.h"
#include "TPoints3DABC.h"
#include "StThreeVectorF.hh"
#include "StMCTruth.h"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "StHelixD.hh"
#include "StEnumerations.h"

class StEvent;
class StTrack;
class StHit;
class StVertex;
class StarClassLibrary;
class TExMap;
class TObjArray;
class StRefArray;
class StPtrVecHit;
class BetheBloch;
class THelixTrack;


//................................................................................
class StVertexHelper: public TObject
{
public:
  StVertexHelper(const StVertex *vtx=0);
  StVertexHelper(const StEvent  *evt);
 ~StVertexHelper(){};
       void    SetVertex(const StVertex *vtx);    
        int    IsValid(){ return fVtx!=0;}
        int    GetType();
        int    GetFlag();
const float   *GetErrMtx();
const StThreeVectorF &GetPoint();

        int    GetNTracks();            //excluding parent
const StTrack *GetTrack(int idx);       // -1=parent track  
const StTrack *GetParent() {return GetTrack(-1);}        
private:  
const StVertex *fVtx;
float fErrMtx[6];
ClassDef(StVertexHelper,0)
};

//................................................................................
class StTrackHelper: public TObject
{
public:
      StTrackHelper(const StTrack *trk=0);
     ~StTrackHelper();
       void     SetTrack(const StTrack *trk);    
        int     GetType() 	const;
        int     GetFlag() 	const;
        int     GetCharge()	const;
const StVertex *GetParent()	const;     
      float     GetImpact()     const;
      float     GetCurv()       const;
      float     GetLength()     const;
      int       numberOfFitPoints(int detid=0) const;
const StThreeVectorF &GetFirstPoint() 	const;
const StThreeVectorF &GetLastPoint()	const;
const StThreeVectorF &GetMom() 	const;
      float     GetPsi() const {return GetMom().phi();}
      float     GetDip() const {return atan2(GetMom().z(),GetMom().perp());}

        int     GetNHits()	const;
const StHit    *GetHit(int idx) const;
const StPtrVecHit *GetHits() 	const;
      StPhysicalHelixD *GetHelix(int idx=0) 	const;
      THelixTrack *GetTHelix(int idx=0) 	const;
      Float_t  *GetPoints(int &npoints) 	const;
StMCTruth GetTruth(int byCount=0,double rXYMin=0.,double rXYMax=1000.) const; 


private:
const StTrack *fTrk;
mutable const StPtrVecHit *fHits;
mutable StPhysicalHelixD  *fHelx[2];
mutable THelixTrack       *fTHlx[2];
ClassDef(StTrackHelper,0)
};

//................................................................................
class StHitHelper: public TObject
{
public:
      StHitHelper(const StHit *hit=0);
       void     SetHit(const StHit *hit);    
        int     GetDetId();
        int     GetFlag();
      float     GetCharge();
        int     IsUsed();
        int     IsFit();
const StThreeVectorF &GetPoint();

private:
const StHit *fHit;
ClassDef(StHitHelper,0)
};


//................................................................................
class TExMap;
class TArrayI;
class StErrorHelper: public TObject
{
public:
  StErrorHelper();
 ~StErrorHelper();
       void Add(int errn);    
       void Print(const char* option="") const;
       int  GetNTot() {return fNTot;}
       int  GetNErr() {return fNErr ;}
static TString Say(int errn,const char *klas="StTrack");

private:  
       void MakeArray();
private:  
		
int fNTot;
int fNErr;
int fKErr;
TExMap  *fMap;
TArrayI *fArr;
ClassDef(StErrorHelper,0)
};





//................................................................................
enum EHKind {kUDF = 0 
    ,kVTX = BIT( 1),kTRK = BIT( 2),kHIT = BIT( 3),kHRR = BIT( 4)//StEvent types
    ,kPRM = BIT( 5),kKNK = BIT( 6),kV0  = BIT( 7),kXI  = BIT( 8)//Vertex  types
    ,kTGB = BIT(10),kTPR = BIT(11),kTPT = BIT(12),kTSY = BIT(13)//Track   types
    ,kEGB = BIT(14),kEPR = BIT(15)                              //Track   types
    ,kUSE = BIT(20),kUNU = BIT(21),kFIT = BIT(22),kTHT = BIT(23)//Hit     types
    ,kTRR = BIT(30)};                                           //TObjArray

//#ifndef ..EVENTHELPER_ONLY..
extern const char *EHKindN[]; 
extern const int   EHKindS[];
//#endif
#ifdef __EVENTHELPER_ONLY__
const char *EHKindN[] = {"undefined" 
    ,"Vertex"   ,"Tracks"    ,"Hits"  ,"HitArr"
    ,"Primary"  ,"Kink"      ,"V0"    ,"Xi"
    ,"Global"   ,"PRIMARY"   ,"Tpt"   ,"SECONDARY"
    ,"estGlobal","estPrimary"                             
    ,"Used"     ,"Unused"    ,"Fitted","Track Hits"             
    ,"TObjArray",0};                                          
const int EHKindS[]= {kUDF
    ,kVTX,kTRK,kHIT,kHRR                                    
    ,kPRM,kKNK,kV0 ,kXI  
    ,kTGB,kTPR,kTPT,kTSY
    ,kEGB,kEPR                            
    ,kUSE,kUNU,kFIT,kTHT               
    ,kTRR,-1};                                       
#endif


//................................................................................
class StEventHelper : public TNamed{

protected:
TObject *fObject;
TExMap *fMap;

public:
    StEventHelper(const TObject *evt=0,const char *opt="");
    virtual ~StEventHelper();
    TObjArray *SelConts (const char *sel=".*");
    TObjArray *SelTracks(const char *sel=0, Int_t th=0);
    TObjArray *SelHits  (const char *RegEx, Int_t un=0,Int_t flag=0);
    TObjArray *SelVertex(const char *sel,   Int_t th=0);
    TObjArray *ExpandAndFilter(const TObject   *evobjs,int flag,TObjArray *out=0);
    TObjArray *MakePoints     (      TObjArray *evobjs,int flag);
    virtual void ls(Option_t* option="") const;
    virtual void Clear(Option_t *opt="");
    void Reset(const TObject *evt=0,const char *opt="");
static int Kind(const TObject *to);
static THelixTrack *MyHelix(THelixTrack *myHlx,const StHelixD *evHlx);
static void Break(int kase);
private:

ClassDef(StEventHelper,0)    
};
//................................................................................
class StPoints3DABC : public TPoints3DABC 
{
public:
   StPoints3DABC(const char *name="",const char *title="",const TObject *obj=0)
   { fSize=0; fXYZ=0;
     char buf[200];
     if (obj && (!name || !name[0])) {
       sprintf(buf,"%s(%p)",obj->GetName(),(void*)obj);name = buf;}
     fName=name;fTitle=title;fObj = obj;}

  ~StPoints3DABC(){delete [] fXYZ;};

         virtual const char *GetName () const {return  fName.Data();}
         virtual const char *GetTitle() const {return fTitle.Data();}
             virtual Float_t GetX(Int_t idx) const {return fXYZ[idx*3+0];}
             virtual Float_t GetY(Int_t idx) const {return fXYZ[idx*3+1];}
             virtual Float_t GetZ(Int_t idx) const {return fXYZ[idx*3+2];}
             virtual Int_t   Size() const          {return fSize;}
             virtual Int_t   Kind() const          {return 0;}
             virtual const TObject *GetObject() const    {return fObj;}
                     void    Add(StPoints3DABC *add);
                     Int_t   Add(float x, float y, float z){return TPoints3DABC::Add(x,y,z);} //WarnOff
// Dummies
               virtual Int_t DistancetoPrimitive(Int_t px, Int_t py){return -1;}
               virtual Int_t GetLastPosition() const {return 0;}
           virtual Option_t* GetOption() const{return 0;}
      virtual const Float_t* GetXYZ(Int_t idx)  {return fXYZ+3*idx;}
      virtual Float_t       *GetXYZ(Float_t *xyz,Int_t idx,Int_t num=1)  const
                             {return TPoints3DABC::GetXYZ(xyz,idx,num);}
               virtual  void PaintPoints(Int_t n, Float_t* p, Option_t* option){}
               virtual Int_t SetLastPosition(Int_t idx){return 0;}
               virtual Int_t SetNextPoint(Float_t x, Float_t y, Float_t z){return 0;}
                virtual void SetOption(Option_t* option){}
               virtual Int_t SetPoints(Int_t, Float_t * = 0, const Option_t * = ""){return 0;}
               virtual Int_t SetPoint(Int_t POINT, Float_t x, Float_t y, Float_t z){return 0;}
private:
                        void Init() const;

// data members                  
protected:  
 TString fName;
 TString fTitle;
 Int_t     fSize;
 Int_t     fN;
 Float_t  *fXYZ;
 const TObject  *fObj;
 ClassDef(StPoints3DABC,0)    
};


//................................................................................
class StTrackPoints : public StPoints3DABC 
{
public:
   StTrackPoints(const StTrack *st,const char *name="",const char *title="");
  ~StTrackPoints(){};
virtual Int_t   Kind() const          {return 2;}
virtual Int_t DistancetoPrimitive(Int_t px, Int_t py);

private:
  void Init() ;
  StTrackPoints (const StTrackPoints &);
  void operator=(const StTrackPoints &);

// data members                  
private:  
 ClassDef(StTrackPoints,0)    
};

//................................................................................
class StVertexPoints : public StPoints3DABC 
{
public:
   StVertexPoints(const StVertex *sv,const char *name="",const char *title="");
  ~StVertexPoints(){};
virtual Int_t   Kind() const          {return 1;}
private:
   StVertexPoints(const StVertexPoints&);
   void operator=(const StVertexPoints&);
// data members                  
private:  
 ClassDef(StVertexPoints,0)    
};
//................................................................................
class StInnOutPoints : public StPoints3DABC 
{
public:
   StInnOutPoints(const StTrack *sv,int inout,const char *name="",const char *title="");
  ~StInnOutPoints(){};
virtual Int_t   Kind() const          {return 4+fInnOut;}
private:
   StInnOutPoints(const StInnOutPoints&);
   void operator=(const StInnOutPoints&);
// data members                  
private:  
 int fInnOut;
 ClassDef(StInnOutPoints,0)    
};


//..............................................................................
class StHitPoints : public StPoints3DABC 
{
public:
   StHitPoints(const StHit *sh,const char *name="",const char *title="");
   StHitPoints(const StRefArray *arr,const char *name="",const char *title="");
  ~StHitPoints(){};
virtual Int_t   Kind() const          {return 3;}
private:
  void Init() ;

// data members                  
private:  
 ClassDef(StHitPoints,0)    
};

//..............................................................................
class StFilterABC : public TNamed {     //base class for StEvent filters
public:
   StFilterABC(const char *name,bool active=true);
   ~StFilterABC(){};

   virtual float        *GetPars() const        = 0;
   virtual const float  *GetDefs() const        = 0;
   virtual const char  **GetNams() const        = 0;
           bool         *GetActive() 		{ return &fActive;}
           void          SetDefs();
           bool          Active() const {return fActive;}
           void          SetActive(bool active){fActive = active;}
   virtual Int_t         Accept(StPoints3DABC *pnt, Color_t&, Size_t&, Style_t&)=0;
protected:
   bool   fActive;// the flag whether the filter is "on"

private:
   static int fgDial;
   ClassDef(StFilterABC,0)
};
//..............................................................................

class StFilterDef : public StFilterABC { // An example of default filter
public:
                StFilterDef(const char *name,bool active=true);
               ~StFilterDef(){};
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;
   Int_t   Accept(StPoints3DABC *pnt, Color_t&, Size_t&, Style_t&);

protected:

private:
  float fFirst;
  float fRandomSelect;
  float fRxyMin      ;
  float fRxyMax      ;
  float fZMin        ;
  float fZMax        ;
  float fPhiMin      ;
  float fPhiMax      ;
  float fLenMin      ;
  float fLenMax      ;
  float fPtMin       ;
  float fPtMax       ;
  float fPsMin       ;
  float fPsMax       ;
  float fQMin        ;
  float fQMax        ; 
  float fEncodedMethod;  // encoding method
  float fLast;

ClassDef(StFilterDef,0)
};
//..............................................................................

class StMuDstFilterHelper : public StFilterABC { // An example of default filter
public:
                StMuDstFilterHelper(const char *name,bool active=true);
               ~StMuDstFilterHelper();
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;
  Int_t         Accept(StPoints3DABC *pnt, Color_t&, Size_t&, Style_t&)
  { return Accept(pnt); } 

protected:
  Int_t Accept(const StTrack *track); // proxy for /StMuDSTMaker/COMMON/StMStMuL3Filter
  Int_t Accept(StPoints3DABC *pnt);

protected:
  BetheBloch* mBB;

private:
  float fFirst;          // do not touch this data member

  // --
  // This filter custom data members go here:
  // --
    float fpCutHigh;       // high momentum cut for RICH/Upsilon candidates 
    float fnHitsCutHighP;  // nHits cut for all tracks

    // following cuts apply only for tracks with pCutLow < p <pHigh
    float fpCutLow;             // low momentum cut
    float fnHitsCutLowP;    
    float fchargeForLowP;       // charge for tracks with pCutLow < p < pCutHigh, set to 0 for all tracks
    float fdEdxMassCutHigh;     // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
    float fdEdxFractionCutHigh; // cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
    float fdEdxMassCutLow;      // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
    float fdEdxFractionCutLow;
  // --  the last custom data-member  --

  float fLast;                // do not touch this data member

  ClassDef(StMuDstFilterHelper,0)
};

//..............................................................................
   class StTpcDedxPidAlgorithm;
   class StElectron;
   class StPionPlus;
   class StKaonPlus;
   class StProton;
  

class StColorFilterHelper : public StFilterABC { // An example of default coloring filter
public:
                StColorFilterHelper(const char *name,bool active=true);
               ~StColorFilterHelper();
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;

protected:
  virtual Int_t  Accept(StPoints3DABC *pnt, Color_t&, Size_t&, Style_t&);
          Int_t  Accept(const StTrack *track, Color_t&, Size_t&, Style_t&);
          Int_t  Accept(StPoints3DABC *pnt){return 1;}
          
protected:
   StTpcDedxPidAlgorithm *fPidAlgorithm;
   StElectron            *fElectron;
   StPionPlus            *fPion;
   StKaonPlus            *fKaon;
   StProton              *fProton;

private:
  float fFirst;          // do not touch this data member

  // --
  // This filter custom data members go here:
  // --
    float fNSigmaElectron ;   // nSigna cut for electron 
    float fNColorElectron ;   // the color index for electron
    float fNSigmaPion ;       // nSigna cut for electron
    float fNColorPion ;       // the color index for pion
    float fNSigmaKaon ;       // nSigna cut for pion
    float fNColorKaon ;       // the color index for kaon 
    float fNSigmaProton ;     // nSigna cut for kaon
    float fNColorProton ;     // the color index kaon
    
    float fNSigmaOther ;     // nSigna cut for other types
    float fNColorOther ;     // the color index for other types

  // --  the last custom data-member  --

  float fLast;                // do not touch this data member

  ClassDef(StColorFilterHelper,0)
};

//..............................................................................
class StHitIter 
{
private:
  class StkCell_t {
  public:
    const void *mV;	//pointer to collection of objects
    int mN;		//size of collection
    int mJ; 		//current index of object in collection
  };
enum {kINI=0,kDOW=1,kHOR=2,kUPP=3,kHIT=4,kEND};
protected:
    StHitIter(const void    *cont=0);
virtual const void *GetObj(const void *cont,int lev,int idx) const =0; 
virtual       int  GetSize(const void *cont,int lev        ) const =0; 
public:
virtual void Reset(const StEvent *evt=0){Reset(GetContainer(evt));}
virtual void Reset(const void *cont=0);
virtual StDetectorId DetectorId() const = 0;
virtual int          HitPlaneId() const{return 0;};
virtual int operator++();
virtual StHit *operator*();
virtual const void *GetContainer(const StEvent *ev) const=0;
virtual void Print(const char *opt="");
protected:
const void *fCont;
int fKase;
int fLev;
StkCell_t fStk[10];
};

//..............................................................................
class StTpcHitCollection;
class StTpcHitIter : public StHitIter {
public:
  StTpcHitIter(const StTpcHitCollection *hc):StHitIter(hc){}
  StTpcHitIter(const StEvent            *ev):StHitIter(GetContainer(ev)){}
  StDetectorId DetectorId() const {return kTpcId;}
  const void *GetContainer(const StEvent *ev) const;

protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};

//..............................................................................
class StSvtHitCollection;
class StSvtHitIter : public StHitIter {
public:
  StSvtHitIter(const StEvent            *ev):StHitIter(GetContainer(ev)){}
  StSvtHitIter(const StSvtHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kSvtId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StSsdHitCollection;
class StSsdHitIter : public StHitIter {
public:
  StSsdHitIter(const StEvent            *ev):StHitIter(GetContainer(ev)){}
  StSsdHitIter(const StSsdHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kSsdId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StFtpcHitCollection;
class StFtpcHitIter : public StHitIter {
public:
  StFtpcHitIter(const StEvent             *ev):StHitIter(GetContainer(ev)){}
  StFtpcHitIter(const StFtpcHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kFtpcWestId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StRnDHitCollection;
class StRnDHitIter : public StHitIter {
public:
  StRnDHitIter(const StEvent*ev,StDetectorId id):StHitIter(GetContainer(ev)){fId=id;}
  StRnDHitIter(const StRnDHitCollection *hc,StDetectorId id):StHitIter(hc){fId=id;}
  StDetectorId DetectorId() const {return fId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
private:
  StDetectorId fId;
};

//..............................................................................
class StTofCollection;
class StTofHitIter : public StHitIter {
public:
  StTofHitIter(const StEvent *ev):StHitIter(GetContainer(ev)){}
  StTofHitIter(const StTofCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kTofId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};

//..............................................................................
class StEvent;
class StAllHitIter : public StHitIter {
  enum {kMaxIters=10};
public:
  StAllHitIter(const StEvent *ev);
  virtual ~StAllHitIter();
  void  Reset(const StEvent *evt=0);
  int   AddDetector(StDetectorId detId);
  StDetectorId DetectorId() const;
  int operator++();
  StHit *operator*();

protected:
  const void *GetObj(const void *cont,int lev,int idx) const{return 0;} 
        int  GetSize(const void *cont,int lev        ) const{return 0;} 
private:
  const void *GetContainer(const StEvent *ev) const{return 0;}
  void  Reset(const void *cont){assert(0);}

private:
  const StEvent *fStEvent;
  int    fJter,fNter;  
  StHitIter 	*fIter[kMaxIters];
};





#endif //ROOT_StEventHelper
