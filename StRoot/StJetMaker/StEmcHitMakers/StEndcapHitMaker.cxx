//StEndcapHitMaker.cxx
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

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//local
#include "StJetMaker/StEmcHitMakers/StEndcapHitMaker.h"

ClassImp(StEndcapHitMaker)
  

    StEndcapHitMaker::StEndcapHitMaker(const char* name, StMuDstMaker* uDstMaker) : StMaker(name), mMuDst(uDstMaker)
{
    mHitEnergy=0.;
    mPrint=false;
}

void StEndcapHitMaker::Clear(Option_t* opt)
{
    mSumEEMC=0.;
    mNumHitTow=0;

    mHits.clear();
    return StMaker::Clear();
}

Int_t StEndcapHitMaker::Init()
{
    cout <<" StEndcapHitMaker :: "<< GetName() <<endl;

    mMuEmcCol=new StMuEmcCollection();
    mEeGeom=new EEmcGeomSimple();
    mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
    if(mEeDb==0) {
	cout <<"eemcDB must be in the chain, fix it or drop:\t"<<GetName()<<endl;
	assert(mEeDb); // eemcDB must be in the chain, fix it
    }
    mEeDb->setThreshold(0);

    return kStOk;
}

double StEndcapHitMaker::getZVertex()
{

    StMuDst* uDst = mMuDst->muDst();
    StMuEvent* uEvent = uDst->event();
    StThreeVectorF vertex = uEvent->primaryVertexPosition();
    double zv = vertex.z();
    if (mPrint) cout <<"Z vertex = "<<zv <<endl;
    return zv;
}


void StEndcapHitMaker::findHits()
{
    /*
    mMuEmcCol = mMuDst->muDst()->emcCollection();
    if(mMuEmcCol) {

	mSumEEMC = 0.;

	for (int m=0; m<mMuEmcCol->getNEndcapTowerADC(); ++m) {

	    int rawadc;//no pedestal subtraction
	    double adc; //ped subtracted
	    int sec;//sector 1-12
	    int sub;//subsector 1-5
	    int etabin;//etabin 1-12
	    double energy;//energy from calibration
	    double eta;//eta as calculated from vertex z=0//rhf

	    mMuEmcCol->getEndcapTowerADC(m,rawadc,sec,sub,etabin);


	    //find eta and phi values from sector, subsector and etabin assuming z=0,0,0
	    TVector3 towerCenter = mEeGeom->getTowerCenter(sec-1,sub-1,etabin-1); //careful, this is indexed from 0
	    
	    if (!mSimu){
		const EEmcDbItem *dbItem=mEeDb->getT(sec,sub-1+'A',etabin);
		assert(dbItem); 

		//check that the channel was ok
		if (dbItem->fail) cout <<"This one channel is set to fail"<<endl;
		if (dbItem->fail) dbItem->print();
		if (dbItem->fail) continue;
		
		adc = rawadc - (dbItem->ped);
		if(dbItem->gain<=0) continue;
		energy=adc/(dbItem->gain);
		
		if (mPrint){
		    //dbItem->print();     
		    //printf("Raw ADC =%d,ped =%f, ADC=%f,gain=%f,Energy= %f\n",rawadc,dbItem->ped,adc,dbItem->gain,energy);
		}
	    }
	    if (mSimu){
		adc=rawadc;
		eta=towerCenter.PseudoRapidity();//rhf
		energy=(adc*0.015)/(sin(getTheta(eta)));//rhf
	    }

	    mSumEEMC += energy;
	    if(energy < mHitEnergy) continue;//Use SetHitEnergyThreshold

	    mNumHitTow++;

	    //Here you have all your "good" towers above a specific energy threshold -- so fill up your container!
	    EemcHit h;
	    h.setRawAdc(rawadc);
	    h.setAdc(adc);
	    h.setEnergy(energy);
	    h.setSector(sec);
	    h.setSubSector(sub);
	    h.setEtaBin(etabin);
	    StThreeVectorD position( towerCenter.X(), towerCenter.Y(), towerCenter.Z() );
	    h.setPosition(position);
	    mHits.push_back(h);
	    
	    
	    //implement software trigger capabilites here as a user set function? Be sure to use dbItem->stat to check for hot towers masked out

	}
    }
    */
}

Int_t StEndcapHitMaker::Make()
{
    cout <<" Start StEndcapHitMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
    findHits();

    if (mPrint){
	cout <<"------------------ Endcap Towers: ----------------------------"<<endl;
	for (EemcHitVec::iterator it=mHits.begin(); it!=mHits.end(); ++it) {
	    cout <<"\t"<<(*it)<<endl;
	}
    }

    return kStOk;
}

Int_t StEndcapHitMaker::Finish()
{
    return kStOk;
}
