//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// header file for class StTpcEvalMaker
//-------------------------------------------------

#ifndef StTpcEvalMaker_H
#define StTpcEvalMaker_H

#include "StMaker.h"

#include "TTree.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StTpcEvalHistograms.h"

class TFile;

class StTpcDb;
class StEvent;
class StGlobalTrack;
class StMcEvent;
class StMcTrack;

class mcTrackInfo;
class rcTrackInfo;
class MatchedTrackPair;
class StTpcEvalEvent;

class StTpcEvalMaker : public StMaker {
    
public:
    
    StTpcEvalMaker(const char* name = "TpcEval", const char* title = "event/TpcEval");
    virtual ~StTpcEvalMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    void fillHeader(); //!
    void mcHitIteration(); //!
    void mcTrackIteration(); //!
    void rcHitIteration(); //!
    void rcTrackIteration(); //!
    
    void addMcTrack(StMcTrack*, mcTrackInfo*); //!
    void addRcTrack(StGlobalTrack*, rcTrackInfo*); //!
    void scanTrackPair(MatchedTrackPair*, StMcTrack*, StGlobalTrack*); //!
    
    void FillTpcEvalEvent(); // Filling of persistent event
    StTpcEvalHistograms  histograms; //!
    
    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTpcEvalMaker.h,v 1.1.1.1 2000/05/23 00:25:03 snelling Exp $ built "__DATE__" "__TIME__; return cvs;}	
    
    
 private:

    StTpcDb*         mStTpcDb; //!
    StEvent*         mStEvent; //!
    StMcEvent*       mStMcEvent; //!
    mcTpcHitMapType* mmcTpcHitMap; //!
    mcTrackMapType*  mmcTrackMap; //!
    rcTpcHitMapType* mrcTpcHitMap; //!
    rcTrackMapType*  mrcTrackMap; //!
    StTpcEvalEvent*  mTpcEvalEvent; //! Pointer to our event structure
    TTree*           mTrackPairTree; // Pointer to Tree
    TFile*           mOutputFile; //! Pointer to output file

  ClassDef(StTpcEvalMaker,1)
};

#endif
