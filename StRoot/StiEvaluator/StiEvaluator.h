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


//Temp class to be stored in TTree, eventually move to it's own .h, .cxx files
#include "TObject.h"

class TClonesArray;

class ArrayEntry : public TObject
{
public:
    ArrayEntry() {};
    virtual ~ArrayEntry() {};

    void setVal(double);
    
private:
    double mval;
    
    ClassDef(ArrayEntry,1)
};

class TreeEntry
{
public:
    TreeEntry();
    virtual ~TreeEntry() {};

    void setA(double val);
    void setB(double val);

    double a() const;
    double b() const;

    void clear();
    void addArrayEntry(const ArrayEntry&);

private:
    double ma;
    double mb;
    TClonesArray* mArray; //! Temporary fix to compile
    int mCounter;
    
    ClassDef(TreeEntry,1) 
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
    void fillTuple();
    
    TFile* mFile;
    TNtuple* mNtuple;
    TTree* mTree;
    TreeEntry* mEntry;
    
};

#endif
