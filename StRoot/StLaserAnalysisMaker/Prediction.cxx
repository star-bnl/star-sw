// $Id: Prediction.cxx,v 1.1.1.1 2000/01/29 16:23:07 fisyak Exp $
// $Log: Prediction.cxx,v $
// Revision 1.1.1.1  2000/01/29 16:23:07  fisyak
// First release of StLaserAnalysisMaker
//
#include <iostream.h>
#include "Prediction.h"
ClassImp(Prediction)
Prediction::Prediction(Int_t LT, Int_t Id, Int_t Sector, Int_t Padrow, 
		       Int_t PadNo, Int_t TimeBin,
		       Float_t tY, Float_t tZ, Float_t Y, Float_t Z){
  fLT =     LT;
  fId =     Id;     
  fSector = Sector; 
  fPadrow = Padrow; 
  fPadNo  = PadNo; 
  fTimeBin =TimeBin;
  ftY =     tY;     
  ftZ =     tZ;     
  fY =      Y;      
  fZ =      Z;      
}
//________________________________________________________________________________
void Prediction::Print(){
  cout << "Prediction for Laser Track #" << fLT << " Id:\t"  << fId;
  cout << "\tSector:\t"       << fSector;   
  cout << "\tPadrow:\t"       << fPadrow;   
  cout << "\tPadNo:\t"        << fPadNo;    
  cout << "\tTimeBin:\t"      << fTimeBin << endl;
  cout << "\ttY:\t"           << ftY;       
  cout << "\ttZ:\t"           << ftZ;       
  cout << "\tY:\t"            << fY;        
  cout << "\tZ:\t"            << fZ << endl;  
}
//________________________________________________________________________________
ClassImp(LEvent)
LEvent::LEvent():fNoY(0),fNoZ(0),fNoYZ(0){
  fYProf = new TClonesArray("YProf",20);
  fZProf = new TClonesArray("ZProf",20);
  fAdc   = new TClonesArray("Adc",500);
}
//________________________________________________________________________________
void LEvent::AddYProf(Float_t y, Float_t adc){
  TClonesArray &kYProf  = *fYProf;
  new (kYProf[fNoY++]) YProf(y,adc);
}
//________________________________________________________________________________
void LEvent::AddZProf(Float_t z, Float_t adc){
  TClonesArray &kZProf  = *fZProf;
  new (kZProf[fNoZ++]) ZProf(z,adc);
}
//________________________________________________________________________________
void LEvent::AddAdc(Float_t y, Float_t z, Float_t adc){
  TClonesArray &kAdc    = *fAdc;
  new (kAdc[fNoYZ++]) Adc(y,z,adc);
}
//________________________________________________________________________________
void LEvent::Clear(Option_t *Option) {
    fNoY = 0;
    fNoZ = 0;
    fNoYZ = 0;
    fYProf->Clear(Option);
    fZProf->Clear(Option);
    fAdc->Clear(Option);
}
//________________________________________________________________________________
void LEvent::Print(){
  fPred.Print();
  fAverage.Print();
  cout << "Y Prof " << endl;
  Int_t i; 
  for (i=0;i<fNoY;i++) {
    YProf *p = (YProf*) fYProf->UncheckedAt(i);
    cout << "\tYY \t" << p->GetYY() << "\tAdcYY \t" << p->GetAdcYY() << endl; 
  }
  cout << "Z Prof " << endl;
  for (i=0;i<fNoZ;i++) {
    ZProf *p = (ZProf*) fZProf->UncheckedAt(i);
    cout << "\tZZ \t" << p->GetZZ() << "\tAdcZZ \t" << p->GetAdcZZ() << endl; 
  }
  cout << "ADC " << endl;
  for (i=0;i<fNoYZ;i++) {
    Adc *p = (Adc*) fAdc->UncheckedAt(i);
    cout << "\ty \t" << p->Gety() <<"\tz \t" << p->Getz() << "\tAdc \t" << p->GetAdc() << endl; 
  }
}
//________________________________________________________________________________
ClassImp(Average)
void Average::Print() {
  cout << "Average: ADC   \t"  << fADC;
  cout << "\tADC3x3       \t"  << fADC3x3;     
  cout << "\tratio  	  \t"  << fratio << endl;      
  cout << "\tnY1    	  \t"  << fnY1;        
  cout << "\tnY2    	  \t"  << fnY2;        
  cout << "\tnZ1    	  \t"  << fnZ1;        
  cout << "\tnZ2    	  \t"  << fnZ2 << endl;
  cout << "\tYav    	  \t"  << fYav;        
  cout << "\tZav    	  \t"  << fZav;
  cout << "\tDYY    	  \t"  << fDYY;
  cout << "\tCYZ    	  \t"  << fCYZ;
  cout << "\tDZZ    	  \t"  << fDZZ << endl;
}
//________________________________________________________________________________
ClassImp(YProf)
//________________________________________________________________________________
ClassImp(ZProf)
//________________________________________________________________________________
ClassImp(Adc)
//________________________________________________________________________________
ClassImp(LTrack)
LTrack::LTrack(LaserTrack *L){
  if (L) {
    Sector	= L->Sector;      
    Mirror	= L->Mirror;      
    ZBoundle	= L->ZBoundle;  
    NoTracks	= L->NoTracks;  
    psi	        = L->psi;            
    Dpsi	= L->Dpsi;          
    tanl	= L->tanl;          
    Dtanl	= L->Dtanl;        
    xl	        = L->xl;       
    Dxl	        = L->Dxl;      
    yl	        = L->yl;       
    Dyl	        = L->Dyl;      
    zl	        = L->zl;       
    Dzl	        = L->Dzl;       
    
  }
}
