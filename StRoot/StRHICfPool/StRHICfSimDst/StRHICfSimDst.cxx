#include "StRHICfSimDst.h"

ClassImp(StRHICfSimDst)

StRHICfSimDst::StRHICfSimDst()
{
    Init();
}

StRHICfSimDst::~StRHICfSimDst()
{

}

void StRHICfSimDst::Clear(Option_t* option)
{
    if(mSimEvent){mSimEvent -> Clear();}
    if(mSimTrackArray){mSimTrackArray -> Clear("C");}
    if(mSimBTofArray){mSimBTofArray -> Clear("C");}
    if(mSimBBC){mSimBBC -> Clear();}
    if(mSimRHICfHit){mSimRHICfHit -> Clear();}
    if(mSimRHICfPointArray){mSimRHICfPointArray -> Clear("C");}
    if(mSimZDC){mSimZDC -> Clear();}
}

Int_t StRHICfSimDst::Init()
{
    if(!mSimEvent){mSimEvent = new StRHICfSimEvent();}
    if(!mSimTrackArray){mSimTrackArray = new TClonesArray("StRHICfSimTrack");}
    if(!mSimBTofArray){mSimBTofArray = new TClonesArray("StRHICfSimBTof");}
    if(!mSimBBC){mSimBBC = new StRHICfSimBBC();}
    if(!mSimRHICfHit){mSimRHICfHit = new StRHICfSimRHICfHit();}
    if(!mSimRHICfPointArray){mSimRHICfPointArray = new TClonesArray("StRHICfSimRHICfPoint");}
    if(!mSimZDC){mSimZDC = new StRHICfSimZDC();}

    return 1;
}

Int_t StRHICfSimDst::CreateDstArray(TTree* tree)
{
    if(mSimEvent){tree -> Branch("StRHICfSimEvent", &mSimEvent);}
    if(mSimTrackArray){tree -> Branch("StRHICfSimTrack", &mSimTrackArray);}
    if(mSimBTofArray){tree -> Branch("StRHICfSimBTof", &mSimBTofArray);}
    if(mSimBBC){tree -> Branch("StRHICfSimBBC", &mSimBBC);}
    if(mSimRHICfHit){tree -> Branch("StRHICfSimRHICfHit", &mSimRHICfHit);}
    if(mSimRHICfPointArray){tree -> Branch("StRHICfSimRHICfPoint", &mSimRHICfPointArray);}
    if(mSimZDC){tree -> Branch("StRHICfSimZDC", &mSimZDC);}

    return 1;
}

Int_t StRHICfSimDst::ReadDstArray(TTree* tree)
{
    if(mSimEvent){tree -> SetBranchAddress("StRHICfSimEvent", &mSimEvent);}
    if(mSimTrackArray){tree -> SetBranchAddress("StRHICfSimTrack", &mSimTrackArray);}
    if(mSimBTofArray){tree -> SetBranchAddress("StRHICfSimBTof", &mSimBTofArray);}
    if(mSimBBC){tree -> SetBranchAddress("StRHICfSimBBC", &mSimBBC);}
    if(mSimRHICfHit){tree -> SetBranchAddress("StRHICfSimRHICfHit", &mSimRHICfHit);}
    if(mSimRHICfPointArray){tree -> SetBranchAddress("StRHICfSimRHICfPoint", &mSimRHICfPointArray);}
    if(mSimZDC){tree -> SetBranchAddress("StRHICfSimZDC", &mSimZDC);}

    return 1;
}

Int_t StRHICfSimDst::ReadDstArray(TChain* chain)
{
    if(mSimEvent){chain -> SetBranchAddress("StRHICfSimEvent", &mSimEvent);}
    if(mSimTrackArray){chain -> SetBranchAddress("StRHICfSimTrack", &mSimTrackArray);}
    if(mSimBTofArray){chain -> SetBranchAddress("StRHICfSimBTof", &mSimBTofArray);}
    if(mSimBBC){chain -> SetBranchAddress("StRHICfSimBBC", &mSimBBC);}
    if(mSimRHICfHit){chain -> SetBranchAddress("StRHICfSimRHICfHit", &mSimRHICfHit);}
    if(mSimRHICfPointArray){chain -> SetBranchAddress("StRHICfSimRHICfPoint", &mSimRHICfPointArray);}
    if(mSimZDC){chain -> SetBranchAddress("StRHICfSimZDC", &mSimZDC);}

    return 1;
}

StRHICfSimEvent* StRHICfSimDst::GetSimEvent(){return mSimEvent;}
StRHICfSimTrack* StRHICfSimDst::GetSimTrack(int idx){return (StRHICfSimTrack*)mSimTrackArray->ConstructedAt(idx);}
StRHICfSimBTof* StRHICfSimDst::GetSimBTof(int idx){return (StRHICfSimBTof*)mSimBTofArray->ConstructedAt(idx);}
StRHICfSimBBC* StRHICfSimDst::GetSimBBC(){return mSimBBC;}
StRHICfSimRHICfHit* StRHICfSimDst::GetSimRHICfHit(){return mSimRHICfHit;}
StRHICfSimRHICfPoint* StRHICfSimDst::GetSimRHICfPoint(int idx){return (StRHICfSimRHICfPoint*)mSimRHICfPointArray->ConstructedAt(idx);}
StRHICfSimZDC* StRHICfSimDst::GetSimZDC(){return mSimZDC;}

Int_t StRHICfSimDst::GetSimTrackNum(){return mSimTrackArray->GetEntries();}
Int_t StRHICfSimDst::GetSimBTofNum(){return mSimBTofArray->GetEntries();}
Int_t StRHICfSimDst::GetSimRHICfPointNum(){return mSimRHICfPointArray->GetEntries();}
