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
#include "TPoints3DABC.h"

class StEvent;
class StTrack;
class StHit;
class StVertex;
class StarClassLibrary;
class TExMap;
class TObjArray;
class StRefArray;

class BetheBloch;

class StEventHelper : public TNamed{

protected:
TObject *fObject;
TExMap *fMap;

public:

    StEventHelper(const TObject *evt=0,const char *opt="");
    virtual ~StEventHelper();
    TObjArray *SelConts (const char *sel=".*");
    TObjArray *SelTracks(Int_t th);
    TObjArray *SelHits  (const char *RegEx, Int_t un);
    TObjArray *SelVertex(const char *sel,Int_t thFlag);
    virtual void ls(Option_t* option="") const;
    virtual void Clear(Option_t *opt="");
    void Reset(const TObject *evt=0,const char *opt="");
private:

ClassDef(StEventHelper,0)    
};
//______________________________________________________________________________
class StPoints3DABC : public TPoints3DABC 
{
public:
   StPoints3DABC(const char *name="",const char *title="",TObject *obj=0)
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
             virtual TObject *GetObject() const    {return fObj;}
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
 TObject  *fObj;
 ClassDef(StPoints3DABC,0)    
};


//______________________________________________________________________________
class StTrackPoints : public StPoints3DABC 
{
public:
   StTrackPoints(StTrack *st,const char *name="",const char *title="");
  ~StTrackPoints(){};
virtual Int_t DistancetoPrimitive(Int_t px, Int_t py);

private:
  void Init() ;
  StTrackPoints (const StTrackPoints &);
  void operator=(const StTrackPoints &);

// data members                  
private:  
 StTrack          *&fTrack;     
 ClassDef(StTrackPoints,0)    
};

//______________________________________________________________________________
class StVertexPoints : public StPoints3DABC 
{
public:
   StVertexPoints(StVertex *sv,const char *name="",const char *title="");
  ~StVertexPoints(){};
private:
   StVertexPoints(const StVertexPoints&);
   void operator=(const StVertexPoints&);
// data members                  
private:  
 StVertex *&fVertex;     
 ClassDef(StVertexPoints,0)    
};


//______________________________________________________________________________
class StHitPoints : public StPoints3DABC 
{
public:
   StHitPoints(StHit *sh,const char *name="",const char *title="");
   StHitPoints(StRefArray *arr,const char *name="",const char *title="");
  ~StHitPoints(){};
private:
  void Init() ;

// data members                  
private:  
 ClassDef(StHitPoints,0)    
};

//______________________________________________________________________________
class StFilterABC : public TNamed {  	//base class for StEvent filters
public:
   StFilterABC(const char *name,bool active=true);
   ~StFilterABC(){};

   virtual float        *GetPars() const	= 0;
   virtual const float  *GetDefs() const	= 0;
   virtual const char  **GetNams() const	= 0;
           bool         *GetActive() { return &fActive;}
           void          SetDefs();
           bool          Active() const {return fActive;}
           void          SetActive(bool active){fActive = active;}
           Int_t         AcceptCB(StPoints3DABC *pnt);
           void          Update();
protected:
   bool   fActive;// the flag whether the filter is "on"

protected:
   virtual Int_t         Accept(StPoints3DABC *pnt) =0;
private:
   static int fgDial;
   ClassDef(StFilterABC,0)
};
//______________________________________________________________________________
inline Int_t StFilterABC::AcceptCB(StPoints3DABC *pnt) 
{
   //  Apply the user-provided filter if the filter is activated
   return fActive ? Accept(pnt): 1;
}
//______________________________________________________________________________

class StFilterDef : public StFilterABC { // An example of default filter
public:
   		StFilterDef(const char *name,bool active=true);
               ~StFilterDef(){};
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;

protected:
  virtual Int_t   Accept(StPoints3DABC *pnt) ;



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
//______________________________________________________________________________

class StMuDstFilterHelper : public StFilterABC { // An example of default filter
public:
   		StMuDstFilterHelper(const char *name,bool active=true);
               ~StMuDstFilterHelper();
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;

protected:
  virtual Int_t         Accept(StPoints3DABC *pnt) ;
  Int_t Accept(const StTrack *track); // proxy for /StMuDSTMaker/COMMON/StMStMuL3Filter

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
















#endif //ROOT_StEventHelper
