#ifndef STAR_Pi0Event
#define STAR_Pi0Event

// Header file for Pi0 Event class and sub-classes, including Monte Carlo events.

#include "TObject.h"
#include "TClonesArray.h"
#include "TRefArray.h"
#include "TRef.h"
#include "TH1.h"
#include "TMath.h"
#include <vector>

using namespace std;

class TPi0Candidate : public TObject {

private:
    Float_t m_Pt; 
    Float_t m_Mass; 
    Float_t m_Eta; 
    Float_t m_Phi; 
    Float_t m_Asymmetry;  
    Float_t m_CosAngle; 
    Float_t m_Distance; 
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
    Float_t m_SizeSMDe1;
    Float_t m_SizeSMDp1;
    Float_t m_SizeTower1;
    Float_t m_SizeSMDe2;
    Float_t m_SizeSMDp2;
    Float_t m_SizeTower2;
	
public:
    TPi0Candidate();
    TPi0Candidate(const TPi0Candidate& orig);
    virtual ~TPi0Candidate() {};
    void Clear(const Option_t* option ="") {};
    // Getters:
    Float_t Pt() const {return m_Pt;}
    Float_t Mass() const {return m_Mass;}
    Float_t Eta() const {return m_Eta;}
    Float_t Phi() const {return m_Phi;}
    Float_t Asymmetry() const {return m_Asymmetry;}
    Float_t CosAngle() const {return m_CosAngle;}
    Float_t Distance() const {return m_Distance;}
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
    Float_t SizeSMDe1() const {return m_SizeSMDe1;}
    Float_t SizeSMDp1() const {return m_SizeSMDp1;}
    Float_t SizeTower1() const {return m_SizeTower1;}
    Float_t SizeSMDe2() const {return m_SizeSMDe2;}
    Float_t SizeSMDp2() const {return m_SizeSMDp2;}
    Float_t SizeTower2() const {return m_SizeTower2;}
    // Setters:
    void SetAll(Float_t data[31]);
	
    ClassDef(TPi0Candidate,1)
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

class TMCPi0 : public TObject {
    //pt:vertexz:eta:phi:runNo:eventNo:hiTowerAdc6Bit:bbcTrig
private:
    Float_t m_Pt;
    Float_t m_Eta;
    Float_t m_Phi;
    Float_t m_Energy1;
    Float_t m_Energy2;
    Float_t m_TowerId1;
    Float_t m_TowerId2;	
    Float_t m_Phi1;
    Float_t m_Eta1;
    Float_t m_Phi2;
    Float_t m_Eta2;
	
public:
    TMCPi0();
    TMCPi0(const TMCPi0& orig);
    virtual ~TMCPi0() {};
    void Clear(const Option_t* option ="") {};
    //Getters
    Float_t Pt() const {return m_Pt;}
    Float_t Eta() const {return m_Eta;}
    Float_t Phi() const {return m_Phi;}
    Float_t Energy1() const {return m_Energy1;}
    Float_t Energy2() const {return m_Energy2;}
    Float_t TowerId1() const {return m_TowerId1;}
    Float_t TowerId2() const {return m_TowerId2;}
    Float_t Phi1() const {return m_Phi1;}
    Float_t Eta1() const {return m_Eta1;}
    Float_t Phi2() const {return m_Phi2;}
    Float_t Eta2() const {return m_Eta2;}
	
    // Setters
    void SetAll(Float_t data[11]);
	
    ClassDef(TMCPi0,1)
	};

class TMCEta : public TObject {
private:
    Float_t m_Pt;
    Float_t m_Eta;
    Float_t m_Phi;
    Int_t m_DecayMode; // how the eta decayed: 0 : gg; 1: 3 pi0; 2: pi+pi-pi0 (or other 3 particle modes containing one pi0); 3: Others
    // used for 2 gamma decay modes
    Float_t m_Energy1; 
    Float_t m_Energy2;
    Float_t m_TowerId1;
    Float_t m_TowerId2;	
    Float_t m_Phi1;
    Float_t m_Eta1;
    Float_t m_Phi2;
    Float_t m_Eta2;
    //used for decay modes containing Pi0s:
    Float_t m_Pi01Pt;
    Float_t m_Pi01Eta;
    Float_t m_Pi01Phi;
    Float_t m_Pi02Pt;
    Float_t m_Pi02Eta;
    Float_t m_Pi02Phi;
    Float_t m_Pi03Pt;
    Float_t m_Pi03Eta;
    Float_t m_Pi03Phi;
	
public:
    TMCEta();
    TMCEta(const TMCEta& orig);
    virtual ~TMCEta() {};
    void Clear(const Option_t* option ="") {};
    //Getters
    Float_t Pt() const {return m_Pt;}
    Float_t Eta() const {return m_Eta;}
    Float_t Phi() const {return m_Phi;}
    Int_t DecayMode() const {return m_DecayMode;}
    Float_t Energy1() const {return m_Energy1;}
    Float_t Energy2() const {return m_Energy2;}
    Float_t TowerId1() const {return m_TowerId1;}
    Float_t TowerId2() const {return m_TowerId2;}
    Float_t Phi1() const {return m_Phi1;}
    Float_t Eta1() const {return m_Eta1;}
    Float_t Phi2() const {return m_Phi2;}
    Float_t Eta2() const {return m_Eta2;}
    Float_t Pi01Pt() const {return m_Pi01Pt;}
    Float_t Pi01Eta() const {return m_Pi01Eta;}
    Float_t Pi01Phi() const {return m_Pi01Phi;}
    Float_t Pi02Pt() const {return m_Pi01Pt;}
    Float_t Pi02Eta() const {return m_Pi01Eta;}
    Float_t Pi02Phi() const {return m_Pi01Phi;}
    Float_t Pi03Pt() const {return m_Pi01Pt;}
    Float_t Pi03Eta() const {return m_Pi01Eta;}
    Float_t Pi03Phi() const {return m_Pi01Phi;}
	
    // Setters
    void SetAll(Float_t data[21]);
	
    ClassDef(TMCEta,1)
	};


class TPi0Event : public TObject {

private:
    Int_t m_EventNo; 
    Int_t m_RunNo; 
    Int_t m_FillNo;
    Float_t m_VertexX;
    Float_t m_VertexY;
    Float_t m_VertexZ; 
    Int_t m_IsMB; 
    Int_t m_IsHTTPF; 
    Int_t m_IsHTTPL2;
    Int_t m_IsHTTPL2_Test;
    Int_t m_IsHT2;
    Int_t m_IsJP1;
    Int_t m_HiTowerAdc6Bit; 
    Int_t m_BEMCPoints; 
    Float_t m_ChargedPtSum;
    Float_t m_NeutralEnergy;
    Int_t m_PrescaleMB; 
    Int_t m_PrescaleHTTPF; 
    Int_t m_PrescaleHTTPL2;
    Int_t m_PrescaleHTTPL2_Test;
    Int_t m_PrescaleHT2;
    Int_t m_PrescaleJP1;
    Int_t m_BBCTrig;
    Float_t m_MCVertexZ;  
    Int_t m_Accept; 
    Int_t m_IsSimu;
    Int_t m_NPi0Candidates; 
    Int_t m_NMCPi0s;
    Int_t m_NMCEtas;
    Int_t m_TriggerTower;
    Float_t m_HighestPi0MCPt;
    Int_t m_BunchX48;
    Int_t m_BunchX7;
    Int_t m_SpinBit4;
    Int_t m_MaskedXing;
    Int_t m_ValidSpin;
    Int_t m_PolLong;
    Int_t m_DbSpinBit;
    Float_t m_relLum[3];
    Float_t m_lumErr[3];
    Int_t m_NHits;
    Float_t m_BBCVertexZ;
    Int_t m_OnlyBBCVtx;
    // information from PYTHIA record for simulated events
    Float_t m_PythiaPartPt;
    Float_t m_PythiaX1;
    Float_t m_PythiaX2;
    Float_t m_BBCTimeBin;
    Int_t m_isBackground;
    vector <int> m_BackgroundTowers;
	
    //Int_t m_HighestTower; // not used for now
    //Float_t m_HighestTowerE;
    TClonesArray *m_Pi0Candidates; //->
    TClonesArray *m_MCPi0s; //->
    TClonesArray *m_MCEtas; //->
    TClonesArray *m_Hits; //->
    static TClonesArray *aPi0Candidates;
    static TClonesArray *aMCPi0s;
    static TClonesArray *aMCEtas;
    static TClonesArray *aHits;

public:
    TPi0Event();
    virtual ~TPi0Event();
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
    Int_t IsHTTPF() const {return m_IsHTTPF;}
    Int_t IsHTTPL2() const {return m_IsHTTPL2;}
    Int_t IsHTTPL2_Test() const {return m_IsHTTPL2_Test;}
    Int_t IsHT2() const {return m_IsHT2;}
    Int_t IsJP1() const {return m_IsJP1;}
    Int_t HiTowerAdc6Bit() const {return m_HiTowerAdc6Bit;}
    Int_t BEMCPoints() const {return m_BEMCPoints;}
    Float_t ChargedPtSum() const {return m_ChargedPtSum;}
    Float_t NeutralEnergy() const {return m_NeutralEnergy;}
    Int_t PrescaleMB() const {return m_PrescaleMB;}
    Int_t PrescaleHTTPF() const {return m_PrescaleHTTPF;}
    Int_t PrescaleHTTPL2() const {return m_PrescaleHTTPL2;}
    Int_t PrescaleHTTPL2_Test() const {return m_PrescaleHTTPL2_Test;}
    Int_t PrescaleHT2() const {return m_PrescaleHT2;}
    Int_t PrescaleJP1() const {return m_PrescaleJP1;}
    Int_t BBCTrig() const {return m_BBCTrig;}
    Int_t IsSimulation() const {return m_IsSimu;}
    Float_t MCVertexZ() const {return m_MCVertexZ;}
    Int_t IsAccepted() const {return m_Accept;}
    Int_t NPi0Candidates() const {return m_NPi0Candidates;}
    Int_t NMCPi0s() const {return m_NMCPi0s;}
    Int_t NMCEtas() const {return m_NMCEtas;}
    Int_t TriggerTower() const {return m_TriggerTower;}
    Float_t HighestPi0MCPt() const {return m_HighestPi0MCPt;}
    Int_t BunchX48() const {return m_BunchX48;}
    Int_t BunchX7() const {return m_BunchX7;}
    Int_t SpinBit() const {return m_SpinBit4;}
    Int_t MaskedXing() const {return m_MaskedXing;}
    Int_t ValidSpin() const {return m_ValidSpin;}
    Int_t PolLong() const {return m_PolLong;}
    Int_t DbSpinBit() const {return m_DbSpinBit;}
    Float_t RelLumUpUp() const {return m_relLum[0];}
    Float_t LumErrorUpUp() const {return m_lumErr[0];}
    Float_t RelLumUpDown() const {return m_relLum[1];}
    Float_t LumErrorUpDown() const {return m_lumErr[1];}
    Float_t RelLumDownUp() const {return m_relLum[2];}
    Float_t LumErrorDownUp() const {return m_lumErr[2];}
    Int_t NHits() const {return m_NHits;}
    Float_t BBCVertexZ() const {return m_BBCVertexZ;}
    Int_t OnlyBBCVertex() const {return m_OnlyBBCVtx;}
    Float_t PythiaPartPt() const {return m_PythiaPartPt;}
    Float_t PythiaX1() const {return m_PythiaX1;}
    Float_t PythiaX2() const {return m_PythiaX2;}
    Float_t BBCTimeBin() const {return m_BBCTimeBin;}
    Int_t IsBackground() const {return m_isBackground;}
    int NumberOfBackgroundTowers() const {return m_BackgroundTowers.size();}
    int BackgroundTower(int i) const {return m_BackgroundTowers.at(i);}
    TClonesArray* Pi0Candidates() const {return m_Pi0Candidates;}
    TPi0Candidate* Pi0Candidate(Int_t i) {return (TPi0Candidate*)m_Pi0Candidates->At(i);}  
    TClonesArray* MCPi0s() const {return m_MCPi0s;}
    TMCPi0* MCPi0(Int_t i) {return (TMCPi0*)m_MCPi0s->At(i);}
    TClonesArray* MCEtas() const {return m_MCEtas;}
    TMCEta* MCEta(Int_t i) {return (TMCEta*)m_MCEtas->At(i);}
    TClonesArray* Hits() const {return m_Hits;}
    THit* Hit(Int_t i) {return (THit*)m_Hits->At(i);}
	
	
    //Setters
    void SetEventNo(Int_t evN) {m_EventNo = evN;}
    void SetRunNo(Int_t rN) {m_RunNo = rN;}
    void SetFillNo(Int_t fN) {m_FillNo = fN;}
    void SetVertex(Float_t vx, Float_t vy, Float_t vz) {m_VertexX = vx; m_VertexY = vy; m_VertexZ = vz;}
    void SetTriggers(Int_t tr[6]) {m_IsMB = tr[0]; m_IsHTTPF = tr[1]; m_IsHTTPL2 = tr[2]; m_IsHT2 = tr[3]; m_IsJP1 = tr[4]; m_IsHTTPL2_Test = tr[5];}
    void SetPrescales(Int_t pr[6]) {m_PrescaleMB = pr[0]; m_PrescaleHTTPF = pr[1]; m_PrescaleHTTPL2 = pr[2]; m_PrescaleHT2 = pr[3]; m_PrescaleJP1 = pr[4]; m_PrescaleHTTPL2_Test = pr[5];}
    void SetHiTowerAdc6Bit(Int_t ht) {m_HiTowerAdc6Bit = ht;}
    void SetBEMCPoints(Int_t points) {m_BEMCPoints = points;}
    void SetChargedPtSum(Float_t sum) {m_ChargedPtSum = sum;}
    void SetNeutralEnergy(Float_t en) {m_NeutralEnergy = en;}
    void SetBBCTrig(Int_t bt) {m_BBCTrig = bt;}
    void SetSimulation(Int_t sim) {m_IsSimu = sim;}
    void SetMCVertexZ(Float_t mz) {m_MCVertexZ = mz;}
    void Accept(Int_t ac = 1) {m_Accept = ac;}
    void SetTriggerTower(Int_t tow) {m_TriggerTower = tow;}
    void SeHighestPi0MCPt(Float_t pt) {m_HighestPi0MCPt = pt;}
    void SetBunchX48(Int_t b48) {m_BunchX48 = b48;}
    void SetBunchX7(Int_t b7) {m_BunchX7 = b7;}
    void SetSpinBit(Int_t sb) {m_SpinBit4 = sb;}
    void SetMaskedXing(Int_t mask) {m_MaskedXing = mask;}
    void SetValidSpin(Int_t val) {m_ValidSpin = val;}
    void SetPolLong(Int_t pol) {m_PolLong = pol;}
    void SetDbSpinBit(Int_t sb) {m_DbSpinBit = sb;}
    void SetRelLum(Float_t l[3]) {m_relLum[0] = l[0];m_relLum[1] = l[1];m_relLum[2] = l[2];}
    void SetLumError(Float_t e[3]) {m_lumErr[0] = e[0];m_lumErr[1] = e[1];m_lumErr[2] = e[2];}
    void SetBBCVertexZ(Float_t vtxz) {m_BBCVertexZ = vtxz;}
    void SetOnlyBBCVertex(Int_t flag = 1) {m_OnlyBBCVtx = flag;}
    void SetPythiaPartPt(Float_t part) {m_PythiaPartPt = part;}
    void SetPythiaX1(Float_t x) {m_PythiaX1 = x;}
    void SetPythiaX2(Float_t x) {m_PythiaX2 = x;}
    void SetBBCTimeBin(Float_t bin) {m_BBCTimeBin = bin;}
    void SetBackground(int bgd) {m_isBackground = bgd;}
    void AddBackgroundTower(int tow) {m_BackgroundTowers.push_back(tow);}
    TPi0Candidate* AddPi0Candidate(TPi0Candidate& cand);
    TMCPi0* AddMCPi0(TMCPi0& cand);
    TMCEta* AddMCEta(TMCEta& cand);
    THit* AddHit(THit& cand);
	
    ClassDef(TPi0Event,3)
	};
#endif

