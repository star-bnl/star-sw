//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

#include <string>
using std::string;

//forward declarations (must #include these in the source file)
class StiTrackContainer;
class TFile;
class TNtuple;
class TTree;
class StMcTrack;
class StTrack;
class StiTrack;
class StTrackPairInfo;


//Temp class to be stored in TTree, eventually move to it's own .h, .cxx files
#include "TObject.h"

class TClonesArray;


class TrackEntry
{
public:
    TrackEntry();
    virtual ~TrackEntry() {};

    void setMcTrack(StMcTrack*);
    void setGlobalTrack(StTrack*);
    void setStiTrack(StiTrack*);

    double getMcTrackId();
    double getMcTrackPt();

    void clear();

private:
    //temp kinematic info : MC
    double mcTrackId;
    double mcTrackPsi;
    double mcTrackRapidity;
    double mcTrackE;
    double mcTrackPx;
    double mcTrackPy;
    double mcTrackPz;
    double mcTrackPt;
    double mcTrackEta;

    //temp kinematic info : global
    short  globalTrackQ;
    double globalTrackM;
    double globalTrackPsi;
    double globalTrackChi2;
    double globalTrackNHit;
    double globalTrackPx;
    double globalTrackPy;
    double globalTrackPz;
    double globalTrackPt;
    double globalTrackEta;
    double globalTrackFitPoints;

    //temp kinematic info : Sti
    double stiTrackM;
    double stiTrackQ;
    double stiTrackPsi;
    double stiTrackChi2;
    double stiTrackNHit;
    double stiTrackY;
    double stiTrackTanL;
    double stiTrackPx;
    double stiTrackPy;
    double stiTrackPz;
    double stiTrackPt;
    double stiTrackEta;


    ClassDef(TrackEntry,1) 
};

class StiEvaluator
{
 public:
    static StiEvaluator* instance(const string val="empty");
    static void kill();

    friend class nobody;

    void evaluateForEvent(const StiTrackContainer*);
    
 private:
    //singleton stuff
    StiEvaluator(); //Not implemented
    StiEvaluator(const string&); //Must pass file-name
    virtual ~StiEvaluator(); 
    
    static StiEvaluator* sinstance;

 private:
    void build();

    void fillTree(StiTrack*, StTrackPairInfo*);

    string mFileName;
    TFile* mFile;
    TTree* mTree;
    TrackEntry* mEntry;
};

#endif
