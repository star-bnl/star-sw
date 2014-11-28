
// Implementation file for Pi0Event and components

#include "TPi0Event.h"
//#include "Stiostream.h"

ClassImp(TPi0Event)
ClassImp(TPi0Candidate)
ClassImp(TMCPi0)
ClassImp(TMCEta)
ClassImp(THit)


TClonesArray* TPi0Event::aPi0Candidates = 0;
TClonesArray* TPi0Event::aMCPi0s = 0;
TClonesArray* TPi0Event::aMCEtas = 0;
TClonesArray* TPi0Event::aHits = 0;

// TPi0Event class

TPi0Event::TPi0Event() {
	if (!aPi0Candidates) aPi0Candidates = new TClonesArray("TPi0Candidate", 50);
	m_Pi0Candidates = aPi0Candidates;
	if (!aMCPi0s) aMCPi0s = new TClonesArray("TMCPi0", 50);
	m_MCPi0s = aMCPi0s;
	if (!aMCEtas) aMCEtas = new TClonesArray("TMCEta", 50);
	m_MCEtas = aMCEtas;
	if (!aHits) aHits = new TClonesArray("THit", 50);
	m_Hits = aHits;
	m_EventNo = 0;
	m_RunNo = 0;
	m_FillNo = 0;
	m_VertexZ = 0;
	m_IsMB = 0;
	m_IsHTTPF = 0;
	m_IsHTTPL2 = 0;
	m_IsHTTPL2_Test = 0;
	m_IsHT2 = 0;
	m_IsJP1 = 0;
	m_HiTowerAdc6Bit = 0;
	m_BEMCPoints = 0;
	m_PrescaleMB = 0;
	m_PrescaleHTTPF = 0;
	m_PrescaleHTTPL2 = 0;
	m_PrescaleHTTPL2_Test = 0;
	m_PrescaleHT2 = 0;
	m_PrescaleJP1 = 0;
	m_BBCTrig = 0;
	m_MCVertexZ = 0;
	m_Accept = 0;
	m_NPi0Candidates = 0;
	m_NMCPi0s = 0;
	m_NMCEtas = 0;
	m_NHits = 0;
	m_HighestPi0MCPt = 0;
	m_BunchX48 = 0;
	m_BunchX7 = 0;
	m_SpinBit4 = 0;
	m_MaskedXing = 0;
	m_ValidSpin = 0;
	m_PolLong = 0;
	m_DbSpinBit = 0;
	m_relLum[0] = 0;
	m_relLum[1] = 0;
	m_relLum[2] = 0;
	m_lumErr[0] = 0;
	m_lumErr[1] = 0;
	m_lumErr[2] = 0;
	m_BBCVertexZ = 0;
	m_OnlyBBCVtx = 0;
	m_PythiaPartPt = 0;
	m_PythiaX1 = 0;
	m_PythiaX2 = 0;
	m_BBCTimeBin = 0;
	m_isBackground = 0;	
}

TPi0Event::~TPi0Event() {
	Clear();	
}

void TPi0Event::Clear(const Option_t*) {
	//cout << "Clearing TPi0Event..." << endl;
	m_EventNo = 0;
	m_RunNo = 0;
	m_FillNo = 0;
	m_VertexZ = 0;
	m_IsMB = 0;
	m_IsHTTPF = 0;
	m_IsHTTPL2 = 0;
	m_IsHTTPL2_Test = 0;
	m_IsHT2 = 0;
	m_IsJP1 = 0;
	m_HiTowerAdc6Bit = 0;
	m_BEMCPoints = 0;
	m_PrescaleMB = 0;
	m_PrescaleHTTPF = 0;
	m_PrescaleHTTPL2 = 0;
	m_PrescaleHTTPL2_Test = 0;
	m_PrescaleHT2 = 0;
	m_PrescaleJP1 = 0;
	m_BBCTrig = 0;
	m_MCVertexZ = 0;
	m_Accept = 0;
	m_NPi0Candidates = 0;
	m_NMCPi0s = 0;
	m_NMCEtas = 0;
	m_NHits = 0;
	m_HighestPi0MCPt = 0;
	m_BunchX48 = 0;
	m_BunchX7 = 0;
	m_SpinBit4 = 0;
	m_MaskedXing = 0;
	m_ValidSpin = 0;
	m_PolLong = 0;
	m_DbSpinBit = 0;
	m_relLum[0] = 0;
	m_relLum[1] = 0;
	m_relLum[2] = 0;
	m_lumErr[0] = 0;
	m_lumErr[1] = 0;
	m_lumErr[2] = 0;
	m_BBCVertexZ = 0;
	m_OnlyBBCVtx = 0;
	m_PythiaPartPt = 0;
	m_PythiaX1 = 0;
	m_PythiaX2 = 0;
	m_BBCTimeBin = 0;
	m_isBackground = 0;
	m_BackgroundTowers.clear();	
	m_Pi0Candidates->Clear();
	m_MCPi0s->Clear();
	m_MCEtas->Clear();
	m_Hits->Clear();
	//cout << "Clear done!" << endl;
}	
void TPi0Event::Reset(Option_t * /*option*/)
{
	// Static function to reset all static objects for this event
	
	delete aPi0Candidates; aPi0Candidates = 0;
	delete aMCPi0s; aMCPi0s = 0;
	delete aMCEtas; aMCEtas = 0;
	delete aHits; aHits = 0;
}


TPi0Candidate* TPi0Event::AddPi0Candidate(TPi0Candidate& cand) {
	//cout << "Adding Pi0 candidate after " << m_NPi0Candidates << endl;
	TClonesArray &aCand = *m_Pi0Candidates;
	TPi0Candidate *piCand = new(aCand[m_NPi0Candidates++]) TPi0Candidate(cand);
	//cout << "Added Candidate " << m_NPi0Candidates << endl;
	return piCand;
}

TMCPi0* TPi0Event::AddMCPi0(TMCPi0& cand) {
	//cout << "Adding Pi0 candidate after " << m_NPi0Candidates << endl;
	TClonesArray &aCand = *m_MCPi0s;
	TMCPi0 *piCand = new(aCand[m_NMCPi0s++]) TMCPi0(cand);
	//cout << "Added Candidate " << m_NPi0Candidates << endl;
	return piCand;
}
TMCEta* TPi0Event::AddMCEta(TMCEta& cand) {
	//cout << "Adding Pi0 candidate after " << m_NPi0Candidates << endl;
	TClonesArray &aCand = *m_MCEtas;
	TMCEta *piCand = new(aCand[m_NMCEtas++]) TMCEta(cand);
	//cout << "Added Candidate " << m_NPi0Candidates << endl;
	return piCand;
}

THit* TPi0Event::AddHit(THit& cand) {
	//cout << "Adding Pi0 candidate after " << m_NPi0Candidates << endl;
	TClonesArray &aCand = *m_Hits;
	THit *piCand = new(aCand[m_NHits++]) THit(cand);
	//cout << "Added Candidate " << m_NPi0Candidates << endl;
	return piCand;
}


TPi0Candidate::TPi0Candidate() : TObject(){
	m_Pt = m_Mass = 0;
}

TPi0Candidate::TPi0Candidate(const TPi0Candidate& orig) : TObject(orig) {
	// copy a Pi0Candidate
	m_Pt = orig.m_Pt;
	m_Mass = orig.m_Mass;
	m_Eta = orig.m_Eta;
	m_Phi = orig.m_Phi;
	m_Asymmetry = orig.m_Asymmetry; 
	m_CosAngle = orig.m_CosAngle;
	m_Distance = orig.m_Distance;
	m_TowerId1 = orig.m_TowerId1;
	m_TowerId2 = orig.m_TowerId2;
	m_ChargedAssociation1 = orig.m_ChargedAssociation1;
	m_ChargedAssociation2 = orig.m_ChargedAssociation2;
	m_SMDFlagg1 = orig.m_SMDFlagg1;
	m_SMDFlagg2 = orig.m_SMDFlagg2;
	m_Energy1 = orig.m_Energy1;
	m_Energy2 = orig.m_Energy2;
	m_TowerEnergy1 = orig.m_TowerEnergy1;
	m_TowerEnergy2 = orig.m_TowerEnergy2;
	m_SMDe1 = orig.m_SMDe1;
	m_SMDp1 = orig.m_SMDp1;
	m_SMDe2 = orig.m_SMDe2;
	m_SMDp2 = orig.m_SMDp2;	
	m_Phi1 = orig.m_Phi1;
	m_Eta1 = orig.m_Eta1;
	m_Phi2 = orig.m_Phi2;
	m_Eta2 = orig.m_Eta2;
	m_SizeSMDe1 = orig.m_SizeSMDe1;
	m_SizeSMDp1 = orig.m_SizeSMDp1;
	m_SizeTower1 = orig.m_SizeTower1;
	m_SizeSMDe2 = orig.m_SizeSMDe2;
	m_SizeSMDp2 = orig.m_SizeSMDp2;
	m_SizeTower2 = orig.m_SizeTower2;
	
}

void TPi0Candidate::SetAll(Float_t data[25]) {
	m_Pt = data[0];
	m_Mass = data[1];
	m_Eta = data[2];
	m_Phi = data[3];
	m_Asymmetry = data[4]; 
	m_CosAngle = data[5];
	m_Distance = data[6];
	m_TowerId1 = data[7];
	m_TowerId2 = data[8];
	m_ChargedAssociation1 = (Int_t) data[9];
	m_ChargedAssociation2 = (Int_t) data[10];
	m_SMDFlagg1 = (Int_t)data[11];
	m_SMDFlagg2 = (Int_t)data[12];
	m_Energy1 = data[13];
	m_Energy2 = data[14];
	m_TowerEnergy1 = data[15];
	m_TowerEnergy2 = data[16];
	m_SMDe1 = data[17];
	m_SMDp1 = data[18];
	m_SMDe2 = data[19];
	m_SMDp2 = data[20];	
	m_Eta1 = data[21];
	m_Phi1 = data[22];
	m_Eta2 = data[23];
	m_Phi2 = data[24];
	m_SizeSMDe1 = data[25];
	m_SizeSMDp1 = data[26];
	m_SizeTower1 = data[27];
	m_SizeSMDe2 = data[28];
	m_SizeSMDp2 = data[29];
	m_SizeTower2 = data[30];
	
}

THit::THit() : TObject() {
	m_X = 0;
	m_Y = 0;
	m_Z = 0;
	m_Id = 0;
	m_Energy = 0;
	m_Pt = 0;
	m_NTracks = 0;
	m_SMDFlag = 0;
	m_EnergySMDe = 0;
	m_EnergySMDp = 0;
	m_EnergyTower = 0;
	m_SizeSMDe = 0;
	m_SizeSMDp = 0;
	m_SizeTower = 0;
	m_Phi = 0;
	m_Eta = 0;
}

THit::THit(const THit& orig) : TObject(orig) {
	m_X = orig.m_X;
	m_Y = orig.m_Y;
	m_Z = orig.m_Z;
	m_Id = orig.m_Id;
	m_Energy = orig.m_Energy;
	m_Pt = orig.m_Pt;
	m_NTracks = orig.m_NTracks;
	m_SMDFlag = orig.m_SMDFlag;
	m_EnergySMDe = orig.m_EnergySMDe;
	m_EnergySMDp = orig.m_EnergySMDp;
	m_EnergyTower = orig.m_EnergyTower;
	m_SizeSMDe = orig.m_SizeSMDe;
	m_SizeSMDp = orig.m_SizeSMDp;
	m_SizeTower = orig.m_SizeTower;
	m_Phi = orig.m_Phi;
	m_Eta = orig.m_Eta;
}	

void THit::SetAll(Float_t data[16]) {
	m_X = data[0];
	m_Y = data[1];
	m_Z = data[2];
	m_Id = (Int_t) data[3];
	m_Energy = data[4];
	m_Pt = data[5];
	m_NTracks = (Int_t) data[6];
	m_SMDFlag = (Int_t) data[7];
	m_EnergySMDe = data[8];
	m_EnergySMDp = data[9];
	m_EnergyTower = data[10];
	m_SizeSMDe = data[11];
	m_SizeSMDp = data[12];
	m_SizeTower = data[13];
	m_Phi = data[14];
	m_Eta = data[15];
}	

TMCPi0::TMCPi0() : TObject() {
	m_Pt = 0;
	m_Eta = 0;
	m_Phi = 0;
	m_Energy1 = 0;
	m_Energy2 = 0;
	m_TowerId1 = 0;
	m_TowerId2 = 0;
}

TMCPi0::TMCPi0(const TMCPi0& orig) : TObject(orig) {
	m_Pt = orig.m_Pt;
	m_Eta = orig.m_Eta;
	m_Phi = orig.m_Phi;
	m_Energy1 = orig.m_Energy1;
	m_Energy2 = orig.m_Energy2;
	m_TowerId1 = orig.m_TowerId1;
	m_TowerId2 = orig.m_TowerId2;
	m_Phi1 = orig.m_Phi1;
	m_Eta1 = orig.m_Eta1;
	m_Phi2 = orig.m_Phi2;
	m_Eta2 = orig.m_Eta2;	
}

void TMCPi0::SetAll(Float_t data[11]) {
	m_Pt = data[0];
	m_Eta = data[1];
	m_Phi = data[2];
	m_Energy1 = data[3];
	m_Energy2 = data[4];
	m_TowerId1 = data[5];
	m_TowerId2 = data[6];
	m_Eta1 = data[7];
	m_Phi1 = data[8];
	m_Eta2 = data[9];
	m_Phi2 = data[10];
}

TMCEta::TMCEta() : TObject() {
	m_Pt = 0;
	m_Eta = 0;
	m_Phi = 0;
	m_DecayMode = 0;
	m_Energy1 = 0;
	m_Energy2 = 0;
	m_TowerId1 = 0;
	m_TowerId2 = 0;
	m_Pi01Pt = 0;
	m_Pi01Eta = 0;
	m_Pi01Phi = 0;
	m_Pi02Pt = 0;
	m_Pi02Eta = 0;
	m_Pi02Phi = 0;
	m_Pi03Pt = 0;
	m_Pi03Eta = 0;
	m_Pi03Phi = 0;
	
}

TMCEta::TMCEta(const TMCEta& orig) : TObject(orig) {
	m_Pt = orig.m_Pt;
	m_Eta = orig.m_Eta;
	m_Phi = orig.m_Phi;
	m_DecayMode = orig.m_DecayMode;
	m_Energy1 = orig.m_Energy1;
	m_Energy2 = orig.m_Energy2;
	m_TowerId1 = orig.m_TowerId1;
	m_TowerId2 = orig.m_TowerId2;
	m_Phi1 = orig.m_Phi1;
	m_Eta1 = orig.m_Eta1;
	m_Phi2 = orig.m_Phi2;
	m_Eta2 = orig.m_Eta2;
	m_Pi01Pt = orig.m_Pi01Pt;
	m_Pi01Eta = orig.m_Pi01Eta;
	m_Pi01Phi = orig.m_Pi01Phi;
	m_Pi02Pt = orig.m_Pi02Pt;
	m_Pi02Eta = orig.m_Pi02Eta;
	m_Pi02Phi = orig.m_Pi02Phi;
	m_Pi03Pt = orig.m_Pi03Pt;
	m_Pi03Eta = orig.m_Pi03Eta;
	m_Pi03Phi = orig.m_Pi03Phi;
	
}

void TMCEta::SetAll(Float_t data[21]) {
	m_Pt = data[0];
	m_Eta = data[1];
	m_Phi = data[2];
	m_DecayMode = ((Int_t) data[3]);
	m_Energy1 = data[4];
	m_Energy2 = data[5];
	m_TowerId1 = data[6];
	m_TowerId2 = data[7];
	m_Eta1 = data[8];
	m_Phi1 = data[9];
	m_Eta2 = data[10];
	m_Phi2 = data[11];
	m_Pi01Pt = data[12];
	m_Pi01Eta = data[13];
	m_Pi01Phi = data[14];
	m_Pi02Pt = data[15];
	m_Pi02Eta = data[16];
	m_Pi02Phi = data[17];
	m_Pi03Pt = data[18];
	m_Pi03Eta = data[19];
	m_Pi03Phi = data[20];
	
}
