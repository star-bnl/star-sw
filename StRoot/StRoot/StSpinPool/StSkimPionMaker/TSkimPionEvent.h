#ifndef STAR_Pi0Event
#define STAR_Pi0Event

// Header file for Skim Pion Event class and sub-classes with specific information

#include "TObject.h"
#include "TClonesArray.h"
#include "TRefArray.h"
#include "TRef.h"
#include "TH1.h"
#include "TMath.h"
#include <vector>

using namespace std;

class TSkimPionCandidate : public TObject {

private:
    Float_t m_Pt; 
    Float_t m_Mass; 
    Float_t m_Eta; 
    Float_t m_Phi; 
    Float_t m_Asymmetry;  
    Float_t m_CosAngle; 
    Float_t m_TowerId1; 
    Float_t m_TowerId2; 
    Int_t m_ChargedAssociation1; 
    Int_t m_ChargedAssociation2; 
    Int_t m_SMDFlagg1; 
    Int_t m_SMDFlagg2; 
    Float_t m_Energy1; 
    Float_t m_Energy2; 
    Float_t m_TowerEnergy1; 
    Float_t m_TowerEnergy2; 
    Float_t m_SMDe1; 
    Float_t m_SMDp1; 
    Float_t m_SMDe2; 
    Float_t m_SMDp2; 
    Float_t m_Phi1;
    Float_t m_Eta1;
    Float_t m_Phi2;
    Float_t m_Eta2;
	
public:
    TSkimPionCandidate();
    TSkimPionCandidate(const TSkimPionCandidate& orig);
    virtual ~TSkimPionCandidate() {};
    void Clear(const Option_t* option ="") {};
    // Getters:
    Float_t Pt() const {return m_Pt;}
    Float_t Mass() const {return m_Mass;}
    Float_t Eta() const {return m_Eta;}
    Float_t Phi() const {return m_Phi;}
    Float_t Asymmetry() const {return m_Asymmetry;}
    Float_t CosAngle() const {return m_CosAngle;}
    Float_t TowerId1() const {return m_TowerId1;}
    Float_t TowerId2() const {return m_TowerId2;}
    Int_t ChargedAssociation1() const {return m_ChargedAssociation1;}
    Int_t ChargedAssociation2() const {return m_ChargedAssociation2;}
    Int_t SMDFlagg1() const {return m_SMDFlagg1;}
    Int_t SMDFlagg2() const {return m_SMDFlagg2;}
    Float_t Energy1() const {return m_Energy1;}
    Float_t Energy2() const {return m_Energy2;}
    Float_t TowerEnergy1() const {return m_TowerEnergy1;}
    Float_t TowerEnergy2() const {return m_TowerEnergy2;}
    Float_t SMDe1() const {return m_SMDe1;}
    Float_t SMDp1() const {return m_SMDp1;}
    Float_t SMDe2() const {return m_SMDe2;}
    Float_t SMDp2() const {return m_SMDp2;}
    Float_t Phi1() const {return m_Phi1;}
    Float_t Eta1() const {return m_Eta1;}
    Float_t Phi2() const {return m_Phi2;}
    Float_t Eta2() const {return m_Eta2;}
    // Setters:
    void SetAll(Float_t data[24]);
	
    ClassDef(TSkimPionCandidate,1)
	};

class THit : public TObject {
private:
    Float_t m_X;
    Float_t m_Y;
    Float_t m_Z;
    Int_t m_Id;
    Float_t m_Energy;
    Float_t m_Pt;
    Int_t m_NTracks;
    Int_t m_SMDFlag;
    Float_t m_EnergySMDe;
    Float_t m_EnergySMDp;
    Float_t m_EnergyTower;
    Float_t m_SizeSMDe;
    Float_t m_SizeSMDp;
    Float_t m_SizeTower;
    Float_t m_Phi;
    Float_t m_Eta;
	
public:
    THit();	
    THit(const THit& orig);
    virtual ~THit() {};
    void Clear(const Option_t* option =""){};
    Float_t X() const {return m_X;}
    Float_t Y() const {return m_Y;}
    Float_t Z() const {return m_Z;}
    Int_t Id() const {return m_Id;}
    Float_t Energy() const {return m_Energy;}
    Float_t Pt() const {return m_Pt;}
    Int_t NTracks() const {return m_NTracks;}
    Int_t SMDFlag() const {return m_SMDFlag;}
    Float_t EnergySMDe() const {return m_EnergySMDe;}
    Float_t EnergySMDp() const {return m_EnergySMDp;}
    Float_t EnergyTower() const {return m_EnergyTower;}
    Float_t SizeSMDe() const {return m_SizeSMDe;}
    Float_t SizeSMDp() const {return m_SizeSMDp;}
    Float_t SizeTower() const {return m_SizeTower;}
    Float_t Phi() const {return m_Phi;}
    Float_t Eta() const {return m_Eta;}
    void SetAll(Float_t data[16]);
		
    ClassDef(THit,1)
	};


class TSkimPionEvent : public TObject {

private:
    Int_t m_EventNo; 
    Int_t m_RunNo; 
    Int_t m_FillNo;
    Float_t m_VertexX;
    Float_t m_VertexY;
    Float_t m_VertexZ;
    Int_t m_IsMB; 
    Int_t m_IsHTTPL2;
    Int_t m_IsHTTPL2_Test;
    Int_t m_IsSoftHTTPL2;
    Int_t m_IsSoftMB;
    Int_t m_HiTowerAdc6Bit; 
    Int_t m_BEMCPoints; 
    Float_t m_ChargedPtSum;
    Float_t m_NeutralEnergy;
    Int_t m_PrescaleMB; 
    Int_t m_PrescaleHTTPL2;
    Int_t m_PrescaleHTTPL2_Test;
    Int_t m_BBCTrig;
    Int_t m_NPi0Candidates;
    Int_t m_NHits;
    Int_t m_BunchX48;
    Int_t m_BunchX7;
    Int_t m_SpinBit4;
    Int_t m_MaskedXing;
    Int_t m_ValidSpin;
    Int_t m_PolLong;
    Int_t m_DbSpinBit;
    Float_t m_BBCVertexZ;
    Float_t m_BBCTimeBin;
    Int_t m_OnlyBBCVtx;
	
    TClonesArray *m_SkimPionCandidates; //->
    TClonesArray *m_Hits; //->
    static TClonesArray *aSkimPionCandidates;
    static TClonesArray *aHits;

public:
    TSkimPionEvent();
    virtual ~TSkimPionEvent();
    void Clear(const Option_t* option ="");
    static void   Reset(Option_t *option ="");
    //Getters
    Int_t EventNo() const {return m_EventNo;}
    Int_t RunNo() const {return m_RunNo;}
    Int_t FillNo() const {return m_FillNo;}
    Float_t VertexX() const {return m_VertexX;}
    Float_t VertexY() const {return m_VertexY;}
    Float_t VertexZ() const {return m_VertexZ;}
    Int_t IsMB() const {return m_IsMB;}
    Int_t IsHTTPL2() const {return m_IsHTTPL2;}
    Int_t IsHTTPL2_Test() const {return m_IsHTTPL2_Test;}
    Int_t IsSoftHTTPL2() const {return m_IsSoftHTTPL2;}
    Int_t IsSoftMB() const {return m_IsSoftMB;}
    Int_t HiTowerAdc6Bit() const {return m_HiTowerAdc6Bit;}
    Int_t BEMCPoints() const {return m_BEMCPoints;}
    Float_t ChargedPtSum() const {return m_ChargedPtSum;}
    Float_t NeutralEnergy() const {return m_NeutralEnergy;}
    Int_t PrescaleMB() const {return m_PrescaleMB;}
    Int_t PrescaleHTTPL2() const {return m_PrescaleHTTPL2;}
    Int_t PrescaleHTTPL2_Test() const {return m_PrescaleHTTPL2_Test;}
    Int_t BBCTrig() const {return m_BBCTrig;}
    Int_t NPi0Candidates() const {return m_NPi0Candidates;}
    Int_t NHits() const {return m_NHits;}
    Int_t BunchX48() const {return m_BunchX48;}
    Int_t BunchX7() const {return m_BunchX7;}
    Int_t SpinBit() const {return m_SpinBit4;}
    Int_t MaskedXing() const {return m_MaskedXing;}
    Int_t ValidSpin() const {return m_ValidSpin;}
    Int_t PolLong() const {return m_PolLong;}
    Int_t DbSpinBit() const {return m_DbSpinBit;}
    Float_t BBCVertexZ() const {return m_BBCVertexZ;}
    Int_t OnlyBBCVertex() const {return m_OnlyBBCVtx;}
    Float_t BBCTimeBin() const {return m_BBCTimeBin;}
    TClonesArray* SkimPionCandidates() const {return m_SkimPionCandidates;}
    TSkimPionCandidate* SkimPionCandidate(Int_t i) {return (TSkimPionCandidate*)m_SkimPionCandidates->At(i);}  
    TClonesArray* Hits() const {return m_Hits;}
    THit* Hit(Int_t i) {return (THit*)m_Hits->At(i);}
	
	
    //Setters
    void SetEventNo(Int_t evN) {m_EventNo = evN;}
    void SetRunNo(Int_t rN) {m_RunNo = rN;}
    void SetFillNo(Int_t fN) {m_FillNo = fN;}
    void SetVertex(Float_t vx, Float_t vy, Float_t vz) {m_VertexX = vx; m_VertexY = vy; m_VertexZ = vz;}
    void SetTriggers(Int_t tr[3]) {m_IsMB = tr[0]; m_IsHTTPL2 = tr[1]; m_IsHTTPL2_Test = tr[2];}
    void SetSoftTriggers(Int_t sftr[2]) {m_IsSoftMB = sftr[0]; m_IsSoftHTTPL2 = sftr[1];}
    void SetPrescales(Int_t pr[3]) {m_PrescaleMB = pr[0]; m_PrescaleHTTPL2 = pr[1]; m_PrescaleHTTPL2_Test = pr[2];}
    void SetHiTowerAdc6Bit(Int_t ht) {m_HiTowerAdc6Bit = ht;}
    void SetBEMCPoints(Int_t points) {m_BEMCPoints = points;}
    void SetChargedPtSum(Float_t sum) {m_ChargedPtSum = sum;}
    void SetNeutralEnergy(Float_t en) {m_NeutralEnergy = en;}
    void SetBBCTrig(Int_t bt) {m_BBCTrig = bt;}
    void SetBunchX48(Int_t b48) {m_BunchX48 = b48;}
    void SetBunchX7(Int_t b7) {m_BunchX7 = b7;}
    void SetSpinBit(Int_t sb) {m_SpinBit4 = sb;}
    void SetMaskedXing(Int_t mask) {m_MaskedXing = mask;}
    void SetValidSpin(Int_t val) {m_ValidSpin = val;}
    void SetPolLong(Int_t pol) {m_PolLong = pol;}
    void SetDbSpinBit(Int_t sb) {m_DbSpinBit = sb;}
    void SetBBCVertexZ(Float_t vtxz) {m_BBCVertexZ = vtxz;}
    void SetOnlyBBCVertex(Int_t flag = 1) {m_OnlyBBCVtx = flag;}
    void SetBBCTimeBin(Float_t bin) {m_BBCTimeBin = bin;}
    TSkimPionCandidate* AddSkimPionCandidate(TSkimPionCandidate& cand);
    THit* AddHit(THit& cand);
	
    ClassDef(TSkimPionEvent,1)
	};
#endif
