//  StdEdxMakerSt.h
//  M.L. Miller
// 5/00

#ifndef StdEdxMaker_HH
#define StdEdxMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

//Forward Declarations
class TFile;
class TNtuple;
class TCanvas;
class TH2F;
class TrackEntry;
class StEvent;
class StPrimaryVertex;
class StTrack;
class AnaTrackId;
class TrMean;
class VertexEntry;
class EventEntry;
class DeDxPreparation;

class StdEdxMaker : public StMaker {
 public:

    //Maker-Chain Stuff
    StMaker* currentChain;

    //Constructor-Destructor
    StdEdxMaker(const char* name = "PionSpec");
    virtual ~StdEdxMaker();
    
    //Standard Maker Methods
    virtual Int_t Init();
    virtual Int_t Make();
    
    //Encapsulated Analysis Result (Implemented as a singleton)
    //    AnaResult* m_AnaResult; //!
    static void kill();             //Delete the singleton instance
    TrackEntry* m_Track;            //!Data collection for root tree
    TTree* m_EventTree;             //!The data tree
    
    //    EvtAna* m_EvtAna; //!
    // Methods------------------------------------------------------
    void clear();
    void clearAll();
    void run(StEvent*);
    void primaryProcess(StEvent*);     //Loop over primary vertices
    void globalProcess(StEvent* rEvent);     //Loop over global tracks
    void vertexAna(StPrimaryVertex*);     //Loop over primary tracks
    void trackAna( StTrack* );  //analyze a track
    void fillHitVector(const DeDxPreparation*); //store hits from tracks
    void printHitVector();
private:
    int m_PrimTrackNumber;       //!A Simple Counter
    int m_GlobTrackNumber;       //!
    VertexEntry *m_Vertex;       //!This really shouldn't be a member
    EventEntry *m_Event;         //!

    DeDxPreparation* dedxprep;   //!
    TrMean* trmean;              //!
    
    StEvent* m_StEvent;          //!
 public:
    //More Maker Stuff
    virtual const char* GetCVS() const
	{static const char cvs[]="Tag $Name:  $ $Id: StdEdxMaker.h,v 1.1.1.1 2000/11/08 02:18:21 fisyak Exp $ built "__DATE__" "__TIME__; return cvs;}	
    ClassDef(StdEdxMaker, 1)
	
	};
#endif
