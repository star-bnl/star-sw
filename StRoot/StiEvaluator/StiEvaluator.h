//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

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
/*
class ArrayEntry : public TObject
{
public:
    ArrayEntry() {};
    virtual ~ArrayEntry() {};

    void setMcTrack(StMcTrack*);
    void setTptTrack(StTrack*);
    void setStiTrack(StiTrack*);
    


private:
    double mval;
    //add info for Monte Carlo
    double McTrackP[4];
    double McTrackV[3];
    double McTrackID;
    double McTrackPt;
    double McTrackPhi;
    //add info for StiTracks
    double StiTrackP[4];
    double StiTrackV[3];
    double StiTrackM;
    double StiTrackQ;
    double StiTrackPt;
    double StiTrackPsi;
    double StiTrackNHit;
    //add info for Tpt tracks
    double TptTrackP[4];
    double TptTrackV[3];
    double TptTrackM;
    double TptTrackQ;
    double TptTrackPt;
    double TptTrackPsi;
    double TptTrackNHit;

    ClassDef(ArrayEntry,1)
};
*/
class TrackEntry
{
public:
    TrackEntry();
    virtual ~TrackEntry() {};

    void setA(double val);
    void setB(double val);

    void setMcTrack(StMcTrack*);
    void setTptTrack(StTrack*);
    void setStiTrack(StiTrack*);

    double getMcTrackID();
    double getMcTrackPt();

    double a() const;
    double b() const;

    void clear();
    //void addArrayEntry(const ArrayEntry&);

private:
    double ma;
    double mb;

    //temp kinematic info : MC
    double McTrackID;
    double McTrackPsi;
    double McTrackPt;
    double McTrackChi2;
    //temp kinematic info : Tpt
    short  TptTrackQ;
    double TptTrackM;
    double TptTrackPt;
    double TptTrackPsi;
    double TptTrackChi2;
    double TptTrackNHit;
    //temp kinematic info : Sti
    double StiTrackM;
    double StiTrackPt;
    double StiTrackPsi;
    double StiTrackChi2;
    double StiTrackNHit;

    TClonesArray* mArray; //! Temporary fix to compile
    int mCounter;
    
    ClassDef(TrackEntry,1) 
};

class StiEvaluator
{
 public:
    static StiEvaluator* instance();
    static void kill();

    friend class nobody;

    void evaluateForEvent(const StiTrackContainer*);
    
 private:
    //singleton stuff
    StiEvaluator();
    virtual ~StiEvaluator();
    static StiEvaluator* sinstance;

 private:
    void build();
    void fillTuple(StiTrack*, StTrackPairInfo*);
    
    TFile* mFile;
    TNtuple* mNtuple;
    TTree* mTree;
    TrackEntry* mEntry;
    
};

#endif
