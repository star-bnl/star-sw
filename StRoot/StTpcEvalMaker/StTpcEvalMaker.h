//  $Id: StTpcEvalMaker.h,v 1.6 2003/09/10 19:47:40 perev Exp $
//  $Log: StTpcEvalMaker.h,v $
//  Revision 1.6  2003/09/10 19:47:40  perev
//  ansi corrs
//
//  Revision 1.5  2001/07/10 09:22:07  flierl
//  add posibility to cut on vertex z-positions
//
//  Revision 1.4  2001/06/19 12:49:37  flierl
//  add l3 option
//
//  Revision 1.3  2001/04/06 22:27:21  flierl
//  add zillion of comments
//
//  Revision 1.2  2000/05/25 20:38:09  snelling
//  Added TPC evaluation histograms
//
//-----------------------------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-----------------------------------------------------------------------
// header file for class StTpcEvalMaker
//-----------------------------------------------------------------------

#ifndef StTpcEvalMaker_H
#define StTpcEvalMaker_H

#include "StMaker.h"
#include "TTree.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StTpcEvalHistograms.h"

class TFile;

class StTpcDb;
class StEvent;
class StL3Trigger;
class StGlobalTrack;
class StMcEvent;
class StMcTrack;

class mcTrackInfo;
class rcTrackInfo;
class MatchedTrackPair;
class StTpcEvalEvent;
class StTpcEvalHistograms;

class StTpcEvalMaker : public StMaker {
    
public:
    // constructors
    StTpcEvalMaker(const char* name = "TpcEval", const char* title = "event/TpcEval") ;
    virtual ~StTpcEvalMaker() ;

    // mantadory MAKER member functions
    virtual void  Clear(const char* opt="") ;
    virtual Int_t Init() ;
    virtual Int_t Make() ;
    virtual Int_t Finish() ;

    /////
    // there are 2 ways of fillin/accesing histos :
    // A via StTpcEvalEvent ( idea : matching results per event go into a tree-like object )
    // B via StTpcEvalHistograms ( idea : just loop over matching results and fill some histos )
    // at april 2001 only B is used
    ////
    // loop over matched hits and fill distances (...) into histos
    void HitIteration() ; 
    // switch above on/off
    void DoHitIteration(Bool_t flag=kFALSE) ; 
    // loop over hits and fill distance to remaining hits into histo 
    void HitSeparation() ;  
    // switch above on/off
    void DoHitSeparation(Bool_t flag=kFALSE) ;
    // loop over monte carlo tracks and fill matching info into histos
    void mcTrackIteration() ; 
    // loop over reconstructed tracks and fill matching info into histos
    void rcTrackIteration() ; 
    // fill StTpcEvalEvent header
    void fillHeader() ; 
    // fill matched track pair object with some info
    void addMcTrack(StMcTrack*, mcTrackInfo*)  ; 
    void addRcTrack(StGlobalTrack*, rcTrackInfo*); 
    // examine a track pair
    void scanTrackPair(MatchedTrackPair*, StMcTrack*, StGlobalTrack*) ; 

    // getters
    StTpcEvalHistograms* GetHistos() ;
    TTree*  GetTrackTree() ;

    // l3 switch
    void useL3Trigger() {mL3TriggerOn = true;}
 
    // vertex switch
    void useVertexConstraint(Double_t constraint) { mVertexConstraint = constraint; }

    // Filling of persistent event
    // not implemented yet 
    // void FillTpcEvalEvent() ; 
   
    // return cvs version
    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTpcEvalMaker.h,v 1.6 2003/09/10 19:47:40 perev Exp $ built "__DATE__" "__TIME__; return cvs;}	
        
 private:

    Bool_t           mHitIteration;    //! switch for hit iteration
    Bool_t           mHitSeparation;   //! switch for hit separation
    Bool_t           mL3TriggerOn;     //!
    Double_t         mVertexConstraint ; //! switch on/off and set vertex constraint
    StTpcDb*         mStTpcDb;         //! database
    StEvent*         mStEvent;         //! stevent object
    StL3Trigger*     ml3TriggerEvent;  //! stl3trigger
    StMcEvent*       mStMcEvent;       //! stmcevent object
    mcTpcHitMapType* mmcTpcHitMap;     //! matched hits from associationmaker using mc as key
    mcTrackMapType*  mmcTrackMap;      //! matched tracks from associationmaker using mc as key
    rcTpcHitMapType* mrcTpcHitMap;     //! matched hits from associatiomaker using rc as key
    rcTrackMapType*  mrcTrackMap;      //! matched tracks from associationmaker using rc as key
    StTpcEvalHistograms  histograms ;  // poitner to object which holds all histos
    StTpcEvalEvent*  mTpcEvalEvent;    //! Pointer to our event structure
    TTree*           mTrackPairTree;   // Pointer to Tree holding StTpcEvalEvent objects
    TFile*           mOutputFile;      //! Pointer to output file


  ClassDef(StTpcEvalMaker,0)
};

// inline function definitions
inline void StTpcEvalMaker::DoHitIteration(Bool_t flag)  { mHitIteration=flag ; }
inline void StTpcEvalMaker::DoHitSeparation(Bool_t flag) { mHitSeparation=flag ; }
inline StTpcEvalHistograms* StTpcEvalMaker::GetHistos() { return &histograms ; }
inline TTree* StTpcEvalMaker::GetTrackTree() {return mTrackPairTree ; }
#endif



