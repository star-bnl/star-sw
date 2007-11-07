/*
 *  StGammaPizero.h
 *  EMCGammaCalib
 *
 *  Created by Alan Hoffman on 10/19/07.
 *
 */

#ifndef StGammaPizero_HH
#define StGammaPizero_HH

#include <iostream>
#include <vector>
#include "StGammaMaker/StGammaCandidate.h"
#include "StGammaMaker/StGammaEvent.h"
#include "StGammaMaker/StGammaStrip.h"
#include "StGammaStripCluster.h"
#include "TVector3.h"
using namespace std;

class StGammaPizero {
	
private:
    Float_t mPt; 
    Float_t mMass; 
    Float_t mEta; 
    Float_t mPhi; 
    Float_t mAsymmetry;  
    Float_t mCosAngle; 
    Float_t mDistance; 
    Float_t mTowerId1; 
    Float_t mTowerId2; 
    Int_t mChargedAssociation1; 
    Int_t mChargedAssociation2; 
    Int_t mSMDFlagg1; 
    Int_t mSMDFlagg2; 
    Float_t mEnergy1; 
    Float_t mEnergy2; 
    Float_t mPatchEnergy1; 
    Float_t mPatchEnergy2; 
    Float_t mSMDe1; 
    Float_t mSMDp1; 
    Float_t mSMDe2; 
    Float_t mSMDp2; 
    Float_t mPhi1;
    Float_t mEta1;
    Float_t mPhi2;
    Float_t mEta2;
    Float_t mSizeSMDe1;
    Float_t mSizeSMDp1;
    Float_t mSizeTower1;
    Float_t mSizeSMDe2;
    Float_t mSizeSMDp2;
    Float_t mSizeTower2;
	
public:
	StGammaPizero();
    StGammaPizero(const StGammaPizero& orig);
    virtual ~StGammaPizero() {};
    void Clear(const Option_t* option ="") {};
	void Reset();
	
    // Getters:
    Float_t Pt() const {return mPt;}
    Float_t Mass() const {return mMass;}
    Float_t Eta() const {return mEta;}
    Float_t Phi() const {return mPhi;}
    Float_t Asymmetry() const {return mAsymmetry;}
    Float_t CosAngle() const {return mCosAngle;}
    Float_t Distance() const {return mDistance;}
    Float_t TowerId1() const {return mTowerId1;}
    Float_t TowerId2() const {return mTowerId2;}
    Int_t ChargedAssociation1() const {return mChargedAssociation1;}
    Int_t ChargedAssociation2() const {return mChargedAssociation2;}
    Int_t SMDFlagg1() const {return mSMDFlagg1;}
    Int_t SMDFlagg2() const {return mSMDFlagg2;}
    Float_t Energy1() const {return mEnergy1;}
    Float_t Energy2() const {return mEnergy2;}
    Float_t PatchEnergy1() const {return mPatchEnergy1;}
    Float_t PatchEnergy2() const {return mPatchEnergy2;}
    Float_t SMDe1() const {return mSMDe1;}
    Float_t SMDp1() const {return mSMDp1;}
    Float_t SMDe2() const {return mSMDe2;}
    Float_t SMDp2() const {return mSMDp2;}
    Float_t Phi1() const {return mPhi1;}
    Float_t Eta1() const {return mEta1;}
    Float_t Phi2() const {return mPhi2;}
    Float_t Eta2() const {return mEta2;}
    Float_t SizeSMDe1() const {return mSizeSMDe1;}
    Float_t SizeSMDp1() const {return mSizeSMDp1;}
    Float_t SizeTower1() const {return mSizeTower1;}
    Float_t SizeSMDe2() const {return mSizeSMDe2;}
    Float_t SizeSMDp2() const {return mSizeSMDp2;}
    Float_t SizeTower2() const {return mSizeTower2;}
	
	//setters
	void SetPt(Float_t pt) {mPt = pt;}
	void SetMass(Float_t mass) {mMass = mass;}
	void SetEta(Float_t eta) {mEta = eta;}
    void SetPhi(Float_t phi) {mPhi = phi;}
    void SetAsymmetry(Float_t asym) {mAsymmetry = asym;}
    void SetCosAngle(Float_t cosangle) { mCosAngle = cosangle;}
    void SetDistance(Float_t dist) { mDistance = dist;}
    void SetTowerId1(Float_t id1) { mTowerId1 = id1;}
    void SetTowerId2(Float_t id2) { mTowerId2 = id2;}
    void SetChargedAssociation1(Int_t chargedassoc1) { mChargedAssociation1 = chargedassoc1;}
    void SetChargedAssociation2(Int_t chargedassoc2) { mChargedAssociation2 = chargedassoc2;}
    void SetSMDFlagg1(Int_t smdflag1) { mSMDFlagg1 = smdflag1;}
    void SetSMDFlagg2(Int_t smdflag2) { mSMDFlagg2 = smdflag2;}
    void SetEnergy1(Float_t energy1) { mEnergy1 = energy1;}
    void SetEnergy2(Float_t energy2) { mEnergy2 = energy2;}
    void SetPatchEnergy1(Float_t patchenergy1) { mPatchEnergy1 = patchenergy1;}
    void SetPatchEnergy2(Float_t patchenergy2) { mPatchEnergy2 = patchenergy2;}
    void SetSMDe1(Float_t smde1) { mSMDe1 = smde1;}
    void SetSMDp1(Float_t smdp1) { mSMDp1 = smdp1;}
    void SetSMDe2(Float_t smde2) { mSMDe2 = smde2;}
    void SetSMDp2(Float_t smdp2) { mSMDp2 = smdp2;}
    void SetPhi1(Float_t phi1) { mPhi1 = phi1;}
    void SetEta1(Float_t eta1) { mEta1 = eta1;}
    void SetPhi2(Float_t phi2) { mPhi2 = phi2;}
    void SetEta2(Float_t eta2) { mEta2 = eta2;}
    void SetSizeSMDe1(Float_t sizesmde1) { mSizeSMDe1 = sizesmde1;}
    void SetSizeSMDp1(Float_t sizesmdp1) { mSizeSMDp1 = sizesmdp1;}
    void SetSizeTower1(Float_t sizetower1) { mSizeTower1 = sizetower1;}
    void SetSizeSMDe2(Float_t sizesmde2) { mSizeSMDe2 = sizesmde2;}
    void SetSizeSMDp2(Float_t sizesmdp2) { mSizeSMDp2 = sizesmdp2;}
    void SetSizeTower2(Float_t sizetower2) { mSizeTower2 = sizetower2;}
	

	
};

#endif
