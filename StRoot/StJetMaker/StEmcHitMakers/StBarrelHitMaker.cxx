//StBarrelHitMaker.cxx
//R. Fatemi (IUCF)
//9/04

//std
using namespace std;
#include <string>
#include <iostream>
#include <cmath>

//root
#include <TVector3.h>
#include "TFile.h"

//star
#include "StChain.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//Barrel
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

//local
#include "StJetMaker/StEmcHitMakers/StBarrelHitMaker.h"

ClassImp(StBarrelHitMaker)
  

StBarrelHitMaker::StBarrelHitMaker(const char* name) : StMaker(name)
{
    mHitEnergy=0.;
    mPrint=false;
}

void StBarrelHitMaker::Clear(Option_t* opt)
{
    mSumEEMC=0.;
    mNumHitTow=0;

    mHits.clear();
    return StMaker::Clear();
}

Int_t StBarrelHitMaker::Init()
{
    cout <<" StBarrelHitMaker :: "<< GetName() <<endl;

    return kStOk;
}

void StBarrelHitMaker::findHits()
{
    //Get pointers to retreive the emc info
    StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
    StEmcADCtoEMaker* adc2e =dynamic_cast<StEmcADCtoEMaker*>( GetMaker("Eread") );
    assert(adc2e);
    StBemcData* data = adc2e->getBemcData();
    assert(data);

    const int maxHits = 4800;
    for(int hitId = 0; hitId < maxHits; ++hitId) {
	/*
	if (data->TowerStatus[hitId] != 1) {
	    //cout <<"skip tower:\t"<<hitId<<endl;
	    continue;
	}
	double energy = data->TowerEnergy[hitId];
	int adc = data->TowerADC[hitId];

	if (energy > mHitEnergy) {
	    
	    
	    float x,y,z;
	    geom->getXYZ(hitId+1, x, y, z);
	    StThreeVectorD position(x,y,z);
	    
	    //Here you have all your "good" towers above a specific energy threshold -- so fill up your container!
	    BemcHit h;
	    h.setAdc(adc);
	    h.setEnergy(energy);
	    h.setPosition(position);
	    mHits.push_back(h);
    
	}
	*/
    }
}

Int_t StBarrelHitMaker::Make()
{
    cout <<" Start StBarrelHitMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
    findHits();

    if (mPrint) {
	cout <<"------------------ Barrel Towers: ----------------------------"<<endl;
	for (BemcHitVec::iterator it=mHits.begin(); it!=mHits.end(); ++it) {
	    cout <<"\t"<<(*it)<<endl;
	}
    }
    return kStOk;
}

Int_t StBarrelHitMaker::Finish()
{
    return kStOk;
}
