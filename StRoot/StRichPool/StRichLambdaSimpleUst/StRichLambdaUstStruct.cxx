#include "StRichLambdaUstStruct.h"

ClassImp(StRichLambdaUstStruct)

StRichLambdaUstStruct::StRichLambdaUstStruct(StRichLambdaUstStruct& in) {
    nPrimaries = in.GetNPrimaries();
    flowMult = in.GetFlowMult();
    nNegPrimaries = in.GetNNegPrimaries();
    nCTB = in.GetNCTB();
    nCTBpre = in.GetNCTBpre();
    triggerWord = in.GetTriggerWord();
    triggerActionWord = in.GetTriggerActionWord();
    triggerMask = in.GetTriggerMask();
    l3Triggered = in.GetL3Triggered();
    l3Available = in.GetL3Available();
    
    zdcSum = in.GetZdcSum();
    vertX = in.GetVertX();
    vertY = in.GetVertY();
    vertZ = in.GetVertZ();
    evID = in.GetEvID();
    runID = in.GetRunID();
    nPixels = in.GetNPixels();
    fPixels = in.GetFPixels();
    nHits = in.GetNHits();
    fHits = in.GetFHits();
    nRichPrimaries = in.GetNRichPrimaries();
    fRichPrimaries = in.GetFRichPrimaries();
    nRichGlobals = in.GetNRichGlobals();
    fRichGlobals = in.GetFRichGlobals();
    nPhotons = in.GetNPhotons();
    fPhotons = in.GetFPhotons();
}

void StRichLambdaUstStruct::AddPixel (StRichLambdaUstPixel & in) 
{
    TClonesArray& pixels = *fPixels;
    new(pixels[nPixels++]) StRichLambdaUstPixel(in);
}
void StRichLambdaUstStruct::AddHit (StRichLambdaUstHit & in) 
{
    TClonesArray& hits = *fHits;
    new(hits[nHits++]) StRichLambdaUstHit(in);
}
void StRichLambdaUstStruct::AddRichPrimary (StRichLambdaUstTrack & in) 
{
    TClonesArray& primaries = *fRichPrimaries;
    new(primaries[nRichPrimaries++]) StRichLambdaUstTrack(in);
}

void StRichLambdaUstStruct::AddRichGlobal (StRichLambdaUstTrack & in) 
{
    TClonesArray& globals = *fRichGlobals;
    new(globals[nRichGlobals++]) StRichLambdaUstTrack(in);
}
void StRichLambdaUstStruct::AddPhoton (StRichLambdaUstPhoton & in) 
{
    TClonesArray& globals = *fPhotons;
    new(globals[nPhotons++]) StRichLambdaUstPhoton(in);
}

void StRichLambdaUstStruct::ClearTracksAndPixels () 
{
    fPixels->Clear();
    fHits->Clear();
    fRichGlobals->Clear();
    fRichPrimaries->Clear();
    fPhotons->Clear();
    nPixels=0;
    nHits=0;
    nRichGlobals = 0;
    nRichPrimaries = 0;
    nPhotons = 0;
    
}




