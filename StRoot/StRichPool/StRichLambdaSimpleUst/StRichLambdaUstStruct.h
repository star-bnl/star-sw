#ifndef ST_RICH_UST_STRUCT_HH
#define ST_RICH_UST_STRUCT_HH
#include "TObject.h"
#include "TClonesArray.h"
#include "StRichLambdaUstTrack.h"
#include "StRichLambdaUstHit.h"
#include "StRichLambdaUstPixel.h"
#include "StRichLambdaUstPhoton.h"

class StRichLambdaUstStruct: public TObject {
private:
    Int_t nPrimaries ;
    Int_t nNegPrimaries ;
    Int_t flowMult;
    
    Int_t nCTB ;
    Int_t nCTBpre;
    
    Float_t zdcSum;
    Int_t triggerWord;
    Int_t triggerActionWord;
    Int_t triggerMask;

    Int_t l3Triggered;
    Int_t l3Available;
    
    
    Float_t vertX ;
    Float_t vertY ;
    Float_t vertZ ;
    Int_t evID ;
    Int_t runID ;
    Int_t nPixels ;
    TClonesArray* fPixels;
    Int_t nHits ;
    TClonesArray* fHits;
    Int_t nRichPrimaries;
    TClonesArray* fRichPrimaries;
    Int_t nRichGlobals;
    TClonesArray* fRichGlobals;
    Int_t nPhotons;
    TClonesArray* fPhotons;
    
public:
     StRichLambdaUstStruct(){
	 fPixels = new TClonesArray("StRichLambdaUstPixel",16000);
	 fHits = new TClonesArray("StRichLambdaUstHit",16000);
	 fRichGlobals = new TClonesArray("StRichLambdaUstTrack",10000);
	 fRichPrimaries = new TClonesArray("StRichLambdaUstTrack",10000);
	 fPhotons = new TClonesArray("StRichLambdaUstPhoton",10000);
	 nPixels = 0;
	 nHits = 0;
	 nRichGlobals = 0;
	 nRichPrimaries = 0;
	 nPhotons = 0;
	 
     }
     StRichLambdaUstStruct(StRichLambdaUstStruct&);
    ~StRichLambdaUstStruct(){
	fPixels->Delete();
	fHits->Delete();
	fRichPrimaries->Delete();
	fRichGlobals->Delete();
	fPhotons->Delete();
	
	delete fPixels;
	delete fHits;
	delete fRichPrimaries;
	delete fRichGlobals;
	delete fPhotons;
	
	fPixels=0;
	fHits=0;
	fRichPrimaries=0;
	fRichGlobals=0;
	fPhotons = 0;
	
    }
    Int_t GetNPrimaries() const { return nPrimaries;}
    void SetNPrimaries(Int_t q) {nPrimaries = q;}
    Int_t GetFlowMult() const { return flowMult;}
    void SetFlowMult(Int_t q) {flowMult = q;}
    Int_t GetL3Triggered() const { return l3Triggered;}
    void SetL3Triggered(Int_t q) {l3Triggered = q;}
    Int_t GetL3Available() const { return l3Available;}
    void SetL3Available(Int_t q) {l3Available = q;}

    Int_t GetNNegPrimaries() const { return nNegPrimaries;}
    void SetNNegPrimaries(Int_t q) {nNegPrimaries = q;}

    Int_t GetNCTB() const { return nCTB;}
    void SetNCTB(Int_t q) {nCTB = q;}

    Int_t GetNCTBpre() const { return nCTBpre;}
    void SetNCTBpre(Int_t q) {nCTBpre = q;}

    Int_t GetTriggerActionWord() const { return triggerActionWord;}
    void SetTriggerActionWord(Int_t q) {triggerActionWord = q;}

    Int_t GetTriggerWord() const { return triggerWord;}
    void SetTriggerWord(Int_t q) {triggerWord = q;}

    Int_t GetTriggerMask() const { return triggerMask;}
    void SetTriggerMask(Int_t q) {triggerMask = q;}

    Float_t GetZdcSum() const { return zdcSum;}
    void SetZdcSum(Float_t q) {zdcSum = q;}

    Float_t GetVertX() const { return vertX;}
    void SetVertX(Float_t q) {vertX = q;}

    Float_t GetVertY() const { return vertY;}
    void SetVertY(Float_t q) {vertY = q;}

    Float_t GetVertZ() const { return vertZ;}
    void SetVertZ(Float_t q) {vertZ = q;}

    Int_t GetEvID() const { return evID;}
    void SetEvID(Int_t q) {evID = q;}

    Int_t GetRunID() const { return runID;}
    void SetRunID(Int_t q) {runID = q;}

    Int_t GetNPixels() const { return nPixels;}
    void SetNPixels(Int_t q) {nPixels = q;}

    TClonesArray* GetFPixels() { return fPixels;}
    void SetFPixels(TClonesArray* q) {fPixels = q;}

    Int_t GetNHits() const { return nHits;}
    void SetNHits(Int_t q) {nHits = q;}

    TClonesArray* GetFHits() { return fHits;}
    void SetFHits(TClonesArray* q) {fHits = q;}

    Int_t GetNRichPrimaries() const { return nRichPrimaries;}
    void SetNRichPrimaries(Int_t q) {nRichPrimaries = q;}

    TClonesArray* GetFRichPrimaries() { return fRichPrimaries;}
    void SetFRichPrimaries(TClonesArray* q) {fRichPrimaries = q;}

    Int_t GetNRichGlobals() const { return nRichGlobals;}
    void SetNRichGlobals(Int_t q) {nRichGlobals = q;}

    TClonesArray* GetFRichGlobals() { return fRichGlobals;}
    void SetFRichGlobals(TClonesArray* q) {fRichGlobals = q;}

    Int_t GetNPhotons() const { return nPhotons;}
    void SetNPhotons(Int_t q) {nPhotons = q;}

    TClonesArray* GetFPhotons() { return fPhotons;}
    void SetFPhotons(TClonesArray* q) {fPhotons = q;}

    void AddPixel(StRichLambdaUstPixel &);
    void AddRichPrimary(StRichLambdaUstTrack &);
    void AddRichGlobal(StRichLambdaUstTrack &);
    void AddHit(StRichLambdaUstHit &);
    void AddPhoton(StRichLambdaUstPhoton &);

    void ClearTracksAndPixels();
    
    ClassDef(StRichLambdaUstStruct,1)
};
#endif


