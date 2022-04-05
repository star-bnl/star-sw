#include "StGammaMaker.h"

using namespace std;

ClassImp(StGammaMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaMaker::StGammaMaker(const char *name, detectorSwitch detector, analysisType analysis): 
StMaker(name)
{

    LOG_DEBUG << "StGammaMaker()" << endm;

    // Instantiate switches
    mUseBemc = true;
    mUseEemc = true;
    
    if(detector == kBemc) mUseEemc = false;
    if(detector == kEemc) mUseBemc = false;
    
    if(analysis == kData) 
    {
        mSimu = false;
    }
    else
    {
        mSimu = true;
    }
    
    // Fetch necessary makers already in the chain
    if(mUseEemc)
    {
    
        mMuDstMaker = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
        assert(mMuDstMaker);
        
        mEemcAnalysis = dynamic_cast<StEEmcA2EMaker*>(GetMakerInheritsFrom("StEEmcA2EMaker"));
        assert(mEemcAnalysis);
    
    }
    else
    {
        mMuDstMaker = NULL;
        mEemcAnalysis = NULL;
    }
    
    // Instantiate the entire gamma family
    if(mSimu) 
    {
        mAsymMaker     = new StMCAsymMaker("mAsymMaker");
        mPythiaMaker   = new StGammaPythiaEventMaker("mGammaPythiaMaker");
        mScheduleMaker = new StGammaScheduleMaker("mGammaScheduleMaker");
        mScheduleMaker->rearrange();
    }
    
    mEventMaker = new StGammaEventMaker("mGammaEventMaker");
    mRawMaker   = new StGammaRawMaker("mGammaRawMaker");
    if(mUseBemc) mRawMaker->useBemc();
    if(mUseEemc) mRawMaker->useEemc();
    
    if(mUseBemc) mBemcClusterMaker = new StBarrelEmcClusterMaker();
    if(mUseEemc) 
    {
        mEemcClusterMaker = new StMyClusterMaker("mEemcClusterMaker", mEemcAnalysis, mMuDstMaker);
        mEemcClusterMaker->setEtaCut(1);
        mEemcClusterMaker->setPhiCut(1);
    }

    mCandidateMaker = new StGammaCandidateMaker("mGammaCandidateMaker");
    if(mUseBemc) mCandidateMaker->useBemc();
    if(mUseEemc) mCandidateMaker->useEemc();
    mTreeMaker = new StGammaTreeMaker("mGammaTreeMaker");

}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaMaker::Init()
{

    if(mSimu) LOG_INFO << "Preparing to run over simulation..." << endm;
    if(!mSimu) LOG_INFO << "Preparing to run over data..." << endm;
    
    if(mUseBemc) LOG_INFO << "Including the BEMC" << endm;
    if(mUseEemc) LOG_INFO << "Including the EEMC" << endm;

    return StMaker::Init();

}

void StGammaMaker::setSeedEnergyThreshold(double threshold)
{
    if(mUseBemc) mBemcClusterMaker->setSeedThreshold(threshold);
    if(mUseEemc) mEemcClusterMaker->setSeedEnergy(threshold);
}

void StGammaMaker::setClusterEnergyThreshold(double threshold)
{
    if(mUseBemc) mBemcClusterMaker->setClusterThreshold(threshold);
}

void StGammaMaker::addTimestamp(int date, int time, double weight)
{

    if(mSimu)
    {
        mScheduleMaker->addTimestamp(date, time, weight);
    }
    else
    {
        LOG_WARN << "addTimestamp() - StGammaMaker not prepared for simulation, ignoring timestamp request!" << endm;
    }
    
}
