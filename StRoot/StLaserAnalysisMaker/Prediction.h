// 
// $Log: Prediction.h,v $
// Revision 1.2  2000/01/31 15:14:52  fisyak
// Try to find out memory leak
//
// Revision 1.1.1.1  2000/01/29 16:23:06  fisyak
// First release of StLaserAnalysisMaker
//
#ifndef STAR_Prediction
#define STAR_Prediction
#include "TObject.h"
#include "TClonesArray.h"
class Prediction : public TObject {
  
 private:
  Int_t        fLT;
  Int_t        fId;           //A Id of prediction
  Int_t        fSector;       //Sector no. 
  Int_t        fPadrow;       //Padrow no.
  Int_t        fPadNo;
  Int_t        fTimeBin;    
  Float_t      ftY;           //Y component of the momentum
  Float_t      ftZ;           //Z component of the momentum
  Float_t      fY;            //Y coordinate of the  point
  Float_t      fZ;            //Z coordinate of the  point
  
  
 public:
  Prediction(Int_t LT = 0,Int_t Id=0, Int_t Sector=0, Int_t Padrow=0, 
	     Int_t PadNo=0, Int_t TimeBin=0,
	     Float_t tY=0, Float_t tZ=0, Float_t Y=0, Float_t Z=0);
  virtual ~Prediction() { }
  virtual Float_t GettY() const { return ftY; }
  virtual Float_t GettZ() const { return ftZ; }
  virtual Int_t   GetLT() const { return fLT; }
  virtual Float_t GetId() const { return fId; }
  virtual Float_t GetY() const { return fY; }
  virtual Float_t GetZ() const { return fZ; }
  virtual Int_t   GetSector() const { return fSector; }
  virtual Int_t   GetPadrow() const { return fPadrow; }
  virtual Int_t   GetPadNo() const { return fPadNo; }
  virtual Int_t   GetTimeBin() const { return fTimeBin; }
  virtual void    DisActivate() {fId = fId > 0 ? -fId : fId;}
  virtual void    Activate()    {fId = fId > 0 ? fId : -fId;}
  virtual void  Print();   
  ClassDef(Prediction,1)  //A track segment
};
class Average : public TObject {
  
 private:
  Int_t         fnY1;          // min/max Y and Z                   
  Int_t         fnY2;                                        
  Int_t         fnZ1;                                        
  Int_t         fnZ2;                                        
  Float_t       fADC;                                        
  Float_t       fADC3x3;                                     
  Float_t       fratio;                                      
  Float_t       fYav;           //Y component of the momentum
  Float_t       fZav;           //Z component of the momentum
  Float_t       fDYY;           //Y coordinate of the  point 
  Float_t       fCYZ;           //Z coordinate of the  point
  Float_t       fDZZ;           //Z coordinate of the  point
 public:
  virtual void    SetnY1   (Int_t   nY1)   {fnY1   = nY1;}   
  virtual void    SetnY2   (Int_t   nY2)   {fnY2   = nY2;}   		   
  virtual void    SetnZ1   (Int_t   nZ1)   {fnZ1   = nZ1;}   		   
  virtual void    SetnZ2   (Int_t   nZ2)   {fnZ2   = nZ2;}   		   
  virtual void    SetADC   (Float_t ADC)   {fADC   = ADC;}   		   
  virtual void    SetADC3x3(Float_t ADC3x3){fADC3x3= ADC3x3;}    	   
  virtual void    Setratio (Float_t ratio) {fratio = ratio;} 		   
  virtual void    SetYav   (Float_t Yav)   {fYav   = Yav;}   		   
  virtual void    SetZav   (Float_t Zav)   {fZav   = Zav;}   		   
  virtual void    SetDYY   (Float_t DYY)   {fDYY   = DYY;}   		   
  virtual void    SetCYZ   (Float_t CYZ)   {fCYZ   = CYZ;}   		   
  virtual void    SetDZZ   (Float_t DZZ)   {fDZZ   = DZZ;}   		    
  Average(Int_t   nY1 = 0,Int_t   nY2 = 0,Int_t   nZ1 = 0,Int_t   nZ2 = 0,
	  Float_t ADC = 0, Float_t ADC3x3 = 0, Float_t ratio = 0,
	  Float_t Yav = 0,Float_t Zav = 0,Float_t DYY = 0,Float_t CYZ = 0,Float_t DZZ =0):
    fnY1   ( nY1),   fnY2   ( nY2),   fnZ1   ( nZ1),   fnZ2   ( nZ2),   
    fADC   ( ADC),   fADC3x3(ADC3x3), fratio ( ratio), fYav   ( Yav),   
    fZav   ( Zav),   fDYY   ( DYY),   fCYZ   ( CYZ),   fDZZ   ( DZZ) {}
  virtual void    Print();   

  virtual ~Average() { }
  ClassDef(Average,1)  //A track segment
};
class YProf : public TObject {
  
 private:
  Float_t      fYY;
  Float_t      fAdcYY;
 public:
  YProf(Float_t YY=0,Float_t AdcYY=0){fYY = YY;fAdcYY = AdcYY;}
  virtual ~YProf() { }
  virtual Float_t GetYY(){return fYY;}
  virtual Float_t GetAdcYY(){return fAdcYY;}
  ClassDef(YProf,1)  //A track segment
};
class ZProf : public TObject {
  
 private:
  Float_t      fZZ;
  Float_t      fAdcZZ;
 public:
  ZProf(Float_t ZZ=0,Float_t AdcZZ=0){fZZ = ZZ;fAdcZZ = AdcZZ;}
  virtual Float_t GetZZ(){return fZZ;}
  virtual Float_t GetAdcZZ(){return fAdcZZ;}
  virtual ~ZProf() { }
  ClassDef(ZProf,1)  //A track segment
};
class Adc : public TObject {
  
 private:
  Float_t      fy;
  Float_t      fz;
  Float_t      fAdc;
 public:
  Adc(Float_t y=0, Float_t z=0,Float_t Adc=0){fy = y; fz = z ;fAdc = Adc;}
  virtual ~Adc() { }
  virtual Float_t Gety(){return fy;}
  virtual Float_t Getz(){return fz;}
  virtual Float_t GetAdc(){return fAdc;}
  ClassDef(Adc,1)  //A track segment
};
struct LaserTrack  {
  Int_t Sector;
  Int_t Mirror;
  Int_t ZBoundle;
  Int_t NoTracks; // no of laser tracks reconstracted by tpt
  Float_t psi;
  Float_t Dpsi;
  Float_t tanl;
  Float_t Dtanl;
  Float_t xl;
  Float_t Dxl;
  Float_t yl;
  Float_t Dyl;
  Float_t zl;
  Float_t Dzl;
};

class LTrack : public TObject {
 public: 
  LTrack(){};
  LTrack(const LaserTrack &L);
  ~LTrack() {};
 private:
  Int_t Sector;
  Int_t Mirror;
  Int_t ZBoundle;
  Int_t NoTracks; // no of laser tracks reconstracted by tpt
  Float_t psi;
  Float_t Dpsi;
  Float_t tanl;
  Float_t Dtanl;
  Float_t xl;
  Float_t Dxl;
  Float_t yl;
  Float_t Dyl;
  Float_t zl;
  Float_t Dzl;
  ClassDef(LTrack,1)
};
class LEvent : public TObject {
  
 private:
  LTrack        fLTrack;
  Prediction    fPred;
  Average       fAverage;
  Int_t         fNoY;
  Int_t         fNoZ;
  Int_t         fNoYZ;
  TClonesArray *fYProf;      //
  TClonesArray *fZProf;      //
  TClonesArray *fAdc;           //
 public:
  LEvent();
  virtual ~LEvent() { }
  virtual void Clear(Option_t *Option);
  virtual void SetPred(Prediction *pred=0) {if (pred) fPred = *pred;}
  virtual void SetLTrack(const LaserTrack &laser) {fLTrack = laser;}
  virtual void SetAverage(Float_t ADC = 0, Float_t ADC3x3 = 0, Float_t ratio = 0,
			  Int_t nY1 = 0,Int_t nY2 = 0,Int_t nZ1 = 0,Int_t nZ2 = 0,
			  Float_t Yav = 0,Float_t Zav = 0,
			  Float_t DYY = 0,Float_t CYZ = 0,Float_t DZZ = 0) {
    fAverage.SetADC(ADC);
    fAverage.SetADC3x3(ADC3x3);
    fAverage.Setratio(ratio);
    fAverage.SetnY1(nY1);
    fAverage.SetnY2(nY2);
    fAverage.SetnZ1(nZ1);
    fAverage.SetnZ2(nZ2);
    fAverage.SetYav(Yav);
    fAverage.SetZav(Zav);
    fAverage.SetDYY(DYY);
    fAverage.SetCYZ(CYZ);
    fAverage.SetDZZ(DZZ);
  }
  virtual Int_t             GetNoY()   {return fNoY;}
  virtual Int_t             GetNoZ()   {return fNoZ;}
  virtual Int_t             GetNoYZ()  {return fNoYZ;}
  virtual TClonesArray     *GetYProf() {return fYProf;}
  virtual TClonesArray     *GetZProf() {return fZProf;}
  virtual TClonesArray     *GetAdc()      {return fAdc;}
  virtual void              Print();
  virtual void              AddAdc(Float_t y, Float_t z, Float_t adc);
  virtual void              AddYProf(Float_t y, Float_t adc);
  virtual void              AddZProf(Float_t z, Float_t adc);
  ClassDef(LEvent,1)  //A track segment
};
#endif
    
