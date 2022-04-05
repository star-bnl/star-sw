#ifndef ST_GLOBAL_ELECTRON_H
#define ST_GLOBAL_ELECTRON_H

#include "StThreeVectorF.hh"
#include "TObject.h"
//#include "Tracks.h"


class StGlobalElectron : public TObject 
{
public:
    StGlobalElectron() { /* no-op */ }
    StGlobalElectron(const StGlobalElectron &other);
    //StGlobalElectron(const Tracks &t);

    //get functions
    Char_t charge() const { return Charge; }
    Float_t getdEdx() const { return dEdx; }
    Float_t getdEdxSigmaElectron() const { return dEdxSigmaElectron; }

    Float_t p() const { return PGlobal.mag(); }
    Float_t eta() const { return PGlobal.pseudoRapidity(); }
    Float_t phi() const { return PGlobal.phi(); }
    Float_t pt() const { return PGlobal.perp(); }
    StThreeVectorF pGlobal() const { return PGlobal; }
    StThreeVectorF oGlobal() const { return OGlobal; }

//    Float_t pxGlobal() const { return PxGlobal; }
    Float_t dcaGlobal() const { return DCAGlobal; }
    Float_t chi2() const { return Chi2; }
    UChar_t nHitsFit() const { return NHitsFit; }
    UChar_t ndEdxPts() const { return NdEdxPts; }
    UChar_t nMaxPts() const { return NMaxPts; }

    //set functions
    void setAll(Float_t data[14]);
    void setCharge(Char_t c) { Charge = c; }
    void setdEdx(Float_t d) { dEdx = d; }
    void setdEdxSigmaElectron(Float_t n) { dEdxSigmaElectron = n; }
    void setPGlobal(StThreeVectorF p) { PGlobal = p; }
    void setPGlobal(Float_t px, Float_t py, Float_t pz);
    void setOGlobal(StThreeVectorF o) { OGlobal = o; }
    void setOGlobal(Float_t ox, Float_t oy, Float_t oz);
    void setDCAGlobal(Float_t d) { DCAGlobal = d; }
    void setChi2(Float_t c) { Chi2 = c; }
    void setNHitsFit(UChar_t n) { NHitsFit = n; }
    void setNdEdxPts(UChar_t n) { NdEdxPts = n; }
    void setNMaxPts(UChar_t n) { NMaxPts = n; }
   
private:
    
    Char_t          Charge;
    Float_t         dEdx;             
    Float_t         dEdxSigmaElectron;

    StThreeVectorF  PGlobal;
    StThreeVectorF  OGlobal;
/*
    Float_t         PxGlobal;         
    Float_t         PyGlobal;         
    Float_t         PzGlobal;         
    Float_t         OxGlobal;         
    Float_t         OyGlobal;         
    Float_t         OzGlobal;         
*/
    Float_t         DCAGlobal;        
    Float_t         Chi2;             
    UChar_t         NHitsFit;         
    UChar_t         NdEdxPts;         
    UChar_t         NMaxPts;          
    
    ClassDef(StGlobalElectron,2);
};

#endif
