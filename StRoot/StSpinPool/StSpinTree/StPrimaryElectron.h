#ifndef ST_PRIMARY_ELECTRON_H
#define ST_PRIMARY_ELECTRON_H

#include "TObject.h"
#include "TRef.h"
//#include "Tracks.h"

class StGlobalElectron;

class StPrimaryElectron : public TObject 
{
public:
    StPrimaryElectron() { /* no-op */ }
    StPrimaryElectron(const StPrimaryElectron &other);
    //StPrimaryElectron(const Tracks &t);
    StGlobalElectron* global() { return (StGlobalElectron*)mGlobalRef.GetObject(); }
    void setGlobal(StGlobalElectron* g) { mGlobalRef = (TObject*)g; }

    //get
    Char_t charge() const { return Charge; }
    Float_t getdEdx() const { return dEdx; }
    Float_t getdEdxSigmaElectron() const { return dEdxSigmaElectron; }
    Float_t p() const { return P; }
    Float_t dca() const { return DCA; }
    Float_t eta() const { return Eta; }
    Float_t phi() const { return Phi; }
    Float_t pt() const { return Pt; }
    Float_t dcaGlobal() const { return DCAGlobal; }
    Float_t chi2() const { return Chi2; }
    UChar_t nHitsFit() const { return NHitsFit; }
    UChar_t ndEdxPts() const { return NdEdxPts; }
    UChar_t nMaxPts() const { return NMaxPts; }
    Float_t projEta() const { return ProjEta; }
    Float_t projPhi() const { return ProjPhi; }
    Float_t projZ() const { return ProjZ; }
    Float_t pointEta() const { return PointEta; }
    Float_t pointPhi() const { return PointPhi; }
    Float_t pointZ() const { return PointZ; }
    Float_t e() const { return E; }
    Float_t pOverE() const { return POverE; }
    UChar_t nEtaStrips() const { return NEtaStrips; }
    UChar_t nPhiStrips() const { return NPhiStrips; }
    UChar_t nTowers() const { return NTowers; }
    Float_t zDist() const { return ZDist; }
    Float_t etaDist() const { return EtaDist; }
    Float_t phiDist() const { return PhiDist; }
    UChar_t nPoints() const { return NPoints; }
    UChar_t pointNTracks() const { return PointNTracks; }
    TRef globalRef() const { return mGlobalRef; }

    //set
    void setAll(Float_t data[29]);
    void setCharge(Char_t c) { Charge = c; }
    void setdEdx(Float_t d) { dEdx = d; }
    void setdEdxSigmaElectron(Float_t d) { dEdxSigmaElectron = d; }
    void setP(Float_t p) { P = p; }
    void setDCA(Float_t d) { DCA = d; }
    void setEta(Float_t e) { Eta = e; }
    void setPhi(Float_t p) { Phi = p; }
    void setPt(Float_t p) { Pt = p; }
    void setDCAGlobal(Float_t d) { DCAGlobal = d; }
    void setChi2(Float_t c) { Chi2 = c; }
    void setNHitsFit(UChar_t n) { NHitsFit = n; }
    void setNdEdxPts(UChar_t n) { NdEdxPts = n; }
    void setNMaxPts(UChar_t n) { NMaxPts = n; }
    void setProjEta(Float_t e) { ProjEta = e; }
    void setProjPhi(Float_t p) { ProjPhi = p; }
    void setProjZ(Float_t z) { ProjZ = z; }
    void setPointEta(Float_t e) { PointEta = e; }
    void setPointPhi(Float_t p) { PointPhi = p; }
    void setPointZ(Float_t z) { PointZ = z; }
    void setE(Float_t e) { E = e; }
    void setPOverE(Float_t poe) { POverE = poe; }
    void setNEtaStrips(UChar_t n) { NEtaStrips = n; }
    void setNPhiStrips(UChar_t n) { NPhiStrips = n; }
    void setNTowers(UChar_t n) { NTowers = n; }
    void setZDist(Float_t z) { ZDist = z; }
    void setEtaDist(Float_t e) { EtaDist = e; }
    void setPhiDist(Float_t p) { PhiDist = p; }
    void setNPoints(UChar_t n) { NPoints = n; }
    void setPointNTracks(UChar_t n) { PointNTracks = n; }

    Char_t          Charge;
    Float_t         dEdx;             
    Float_t         dEdxSigmaElectron;
    Float_t         P;                
    Float_t         DCA;              
    Float_t         Eta;              
    Float_t         Phi;              
    Float_t         Pt;               
    Float_t         DCAGlobal;        
    Float_t         Chi2;             
    UChar_t         NHitsFit;         
    UChar_t         NdEdxPts;         
    UChar_t         NMaxPts;          
    Float_t         ProjEta;          
    Float_t         ProjPhi;          
    Float_t         ProjZ;            
    Float_t         PointEta;         
    Float_t         PointPhi;         
    Float_t         PointZ;           
    Float_t         E;                
    Float_t         POverE;           
    UChar_t         NEtaStrips;       
    UChar_t         NPhiStrips;       
    UChar_t         NTowers;          
    Float_t         ZDist;            
    Float_t         EtaDist;          
    Float_t         PhiDist;          
    Float_t         R;                
    UChar_t         NPoints;          
    UChar_t         PointNTracks;     
    
private:    
    TRef mGlobalRef;

    ClassDef(StPrimaryElectron,1);
};

#endif
