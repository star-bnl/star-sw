#include "StRichUstStruct.h"

ClassImp(StRichUstStruct)

StRichUstStruct::StRichUstStruct(StRichUstStruct& in) {
    nPrimaries = in.GetNPrimaries();
    nNegPrimaries = in.GetNNegPrimaries();

    backgroundRate = in.GetBackgroundRate();
    zdcWestRate = in.GetZDCWestRate();
    zdcEastRate = in.GetZDCEastRate();
    zdcCoincidenceRate = in.GetZDCCoincidenceRate();
    
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

}

void StRichUstStruct::AddPixel (StRichUstPixel & in) 
{
    TClonesArray& pixels = *fPixels;
    new(pixels[nPixels++]) StRichUstPixel(in);
}
void StRichUstStruct::AddHit (StRichUstHit & in) 
{
    TClonesArray& hits = *fHits;
    new(hits[nHits++]) StRichUstHit(in);
}
void StRichUstStruct::AddRichPrimary (StRichUstTrack & in) 
{
    TClonesArray& primaries = *fRichPrimaries;
    new(primaries[nRichPrimaries++]) StRichUstTrack(in);
}

void StRichUstStruct::AddRichGlobal (StRichUstTrack & in) 
{
    TClonesArray& globals = *fRichGlobals;
    new(globals[nRichGlobals++]) StRichUstTrack(in);
}


void StRichUstStruct::ClearTracksAndPixels () 
{
    fPixels->Clear();
    fHits->Clear();
    fRichGlobals->Clear();
    fRichPrimaries->Clear();

    nPixels=0;
    nHits=0;
    nRichGlobals = 0;
    nRichPrimaries = 0;

    
}




