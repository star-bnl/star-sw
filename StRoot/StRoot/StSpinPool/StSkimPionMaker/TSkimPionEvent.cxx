// Implementation file for SkimPionEvent and components

#include "TSkimPionEvent.h"

ClassImp(TSkimPionEvent)
ClassImp(TSkimPionCandidate)
ClassImp(THit)


TClonesArray* TSkimPionEvent::aSkimPionCandidates = 0;
TClonesArray* TSkimPionEvent::aHits = 0;

// TSkimPionEvent class

TSkimPionEvent::TSkimPionEvent() {
	if (!aSkimPionCandidates) aSkimPionCandidates = new TClonesArray("TSkimPionCandidate", 50);
	m_SkimPionCandidates = aSkimPionCandidates;
	if (!aHits) aHits = new TClonesArray("THit", 50);
	m_Hits = aHits;
	m_EventNo = 0;
	m_RunNo = 0;
	m_FillNo = 0;
	m_VertexZ = 0;
	m_IsMB = 0;
	m_IsHTTPL2 = 0;
	m_IsHTTPL2_Test = 0;
	m_IsSoftHTTPL2 = 0;
	m_IsSoftMB = 0;
	m_HiTowerAdc6Bit = 0;
	m_BEMCPoints = 0;
	m_PrescaleMB = 0;
	m_PrescaleHTTPL2 = 0;
	m_PrescaleHTTPL2_Test = 0;
	m_BBCTrig = 0;
	m_NPi0Candidates = 0;
	m_NHits = 0;
	m_BunchX48 = 0;
	m_BunchX7 = 0;
	m_SpinBit4 = 0;
	m_MaskedXing = 0;
	m_ValidSpin = 0;
	m_PolLong = 0;
	m_DbSpinBit = 0;
	m_BBCVertexZ = 0;
	m_OnlyBBCVtx = 0;
	m_BBCTimeBin = 0;
}

TSkimPionEvent::~TSkimPionEvent() {
	Clear();	
}

void TSkimPionEvent::Clear(const Option_t*) {
	//cout << "Clearing TSkimPionEvent..." << endl;
	m_EventNo = 0;
	m_RunNo = 0;
	m_FillNo = 0;
	m_VertexZ = 0;
	m_IsMB = 0;
	m_IsHTTPL2 = 0;
	m_IsHTTPL2_Test = 0;
	m_IsSoftHTTPL2 = 0;
	m_IsSoftMB = 0;
	m_HiTowerAdc6Bit = 0;
	m_BEMCPoints = 0;
	m_PrescaleMB = 0;
	m_PrescaleHTTPL2 = 0;
	m_PrescaleHTTPL2_Test = 0;
	m_BBCTrig = 0;
	m_NPi0Candidates = 0;
	m_NHits = 0;
	m_BunchX48 = 0;
	m_BunchX7 = 0;
	m_SpinBit4 = 0;
	m_MaskedXing = 0;
	m_ValidSpin = 0;
	m_PolLong = 0;
	m_DbSpinBit = 0;
	m_BBCVertexZ = 0;
	m_OnlyBBCVtx = 0;
	m_BBCTimeBin = 0;
	m_SkimPionCandidates->Clear();
	m_Hits->Clear();
	//cout << "Clear done!" << endl;
}	
void TSkimPionEvent::Reset(Option_t * /*option*/)
{
	// Static function to reset all static objects for this event
	
	delete aSkimPionCandidates;
	aSkimPionCandidates = 0;
	delete aHits;
	aHits = 0;
}


TSkimPionCandidate* TSkimPionEvent::AddSkimPionCandidate(TSkimPionCandidate& cand) {
	//cout << "Adding SkimPion candidate after " << m_NSkimPionCandidates << endl;
	TClonesArray &aCand = *m_SkimPionCandidates;
	TSkimPionCandidate *piCand = new(aCand[m_NPi0Candidates++]) TSkimPionCandidate(cand);
	//cout << "Added Candidate " << m_NSkimPionCandidates << endl;
	return piCand;
}


THit* TSkimPionEvent::AddHit(THit& cand) {
	//cout << "Adding SkimPion candidate after " << m_NSkimPionCandidates << endl;
	TClonesArray &aCand = *m_Hits;
	THit *piCand = new(aCand[m_NHits++]) THit(cand);
	//cout << "Added Candidate " << m_NSkimPionCandidates << endl;
	return piCand;
}


TSkimPionCandidate::TSkimPionCandidate() : TObject(){
	m_Pt = m_Mass = 0;
}

TSkimPionCandidate::TSkimPionCandidate(const TSkimPionCandidate& orig) : TObject(orig) {
	// copy a SkimPionCandidate
	m_Pt = orig.m_Pt;
	m_Mass = orig.m_Mass;
	m_Eta = orig.m_Eta;
	m_Phi = orig.m_Phi;
	m_Asymmetry = orig.m_Asymmetry; 
	m_CosAngle = orig.m_CosAngle;
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
}

void TSkimPionCandidate::SetAll(Float_t data[24]) {
	m_Pt = data[0];
	m_Mass = data[1];
	m_Eta = data[2];
	m_Phi = data[3];
	m_Asymmetry = data[4]; 
	m_CosAngle = data[5];
	m_TowerId1 = data[6];
	m_TowerId2 = data[7];
	m_ChargedAssociation1 = (Int_t) data[8];
	m_ChargedAssociation2 = (Int_t) data[9];
	m_SMDFlagg1 = (Int_t)data[10];
	m_SMDFlagg2 = (Int_t)data[11];
	m_Energy1 = data[12];
	m_Energy2 = data[13];
	m_TowerEnergy1 = data[14];
	m_TowerEnergy2 = data[15];
	m_SMDe1 = data[16];
	m_SMDp1 = data[17];
	m_SMDe2 = data[18];
	m_SMDp2 = data[19];	
	m_Eta1 = data[20];
	m_Phi1 = data[21];
	m_Eta2 = data[22];
	m_Phi2 = data[23];
	
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
