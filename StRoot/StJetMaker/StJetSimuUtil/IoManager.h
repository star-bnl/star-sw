//IoManager.h

#ifndef IoManager_HH
#define IoManager_HH

//std:
#include <cmath>
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

#include "StJetMaker/StJet.h"

class TFile;
class TTree;
class HasArray;
class HasPairArray;
class TChain;
#include "TClonesArray.h"

class IoManager : public TObject
{
public:
    IoManager();
    virtual ~IoManager();
    
    void write();
    void openWrite(const char*);
    void openRead(const char*);
    
    void getEvent(int i);
    
    TTree* assocTree() {return mAssocTree;}
    TTree* recoTree() {return mRecoTree;}
    TTree* pythiaTree() {return mPythiaTree;}

    HasPairArray* assocArray() {return mAssocArray;}
    HasArray* pythiaArray() {return mPythiaArray;}
    HasArray* recoArray() {return mRecoArray;}

    
private:
    TFile* mFile;
    
    TTree* mAssocTree;
    TTree* mRecoTree;
    TTree* mPythiaTree;
    
    HasPairArray* mAssocArray;
    HasArray* mPythiaArray;
    HasArray* mRecoArray;
    
    ClassDef(IoManager,1)
};


//now define an associated pair:
class HasArray 
{
public:
    HasArray() : mArray(new TClonesArray("StJet",100)) {};
    virtual ~HasArray() {mArray->Delete(); delete mArray; mArray=0;}
    TClonesArray* mArray;
    ClassDef(HasArray,1)
};

class HasPairArray 
{
public:
    HasPairArray() : mArray(new TClonesArray("JetPair",100)) {};
    virtual ~HasPairArray() {mArray->Delete(); delete mArray; mArray=0;}
    TClonesArray* mArray;
    ClassDef(HasPairArray,1)
};

    
//to go in private of StPythiaAssociator

class JetPair : public TObject
{
public:
    JetPair() {
    }
    virtual ~JetPair() {};
    
    StJet pythiaJet;
    StJet recoJet;
    
    double deltaPhi;
    double deltaEta;
    double deltaR;

    int pythiaRank;
    int pairRank;
private:
    ClassDef(JetPair,1)
};

inline double gJetDeltaPhi(StJet* a, StJet* b)
{
    double p1 = a->Phi();
    double p2 = b->Phi();
    float dp = p1 - p2;
    while(dp >  M_PI) {dp -= 2.0 * M_PI;}
    while(dp < -1.*M_PI) {dp += 2.0 * M_PI;}
    return dp;
};

struct PtSorter {
    bool operator()(const StJet* lhs, const StJet* rhs) const {
	return lhs->Pt() > rhs->Pt();
    }
};

struct JetPairSorter {
    bool operator()(const JetPair& lhs, const JetPair& rhs) {
	if (lhs.pythiaRank < rhs.pythiaRank) {
	    return true;
	}
	else if (lhs.pythiaRank > rhs.pythiaRank) {
	    return false;
	}
	else {
	    return lhs.deltaR < rhs.deltaR;
	}
    }
};


inline ostream& operator<<(ostream& os, const JetPair& jp)
{
    os <<"py-jet:\tPt:\t"<<jp.pythiaJet.Pt()<<"\tEta:\t"<<jp.pythiaJet.Eta()<<"\tPhi:\t"<<jp.pythiaJet.Phi()<<endl;
    os <<"rc-jet:\tPt:\t"<<jp.recoJet.Pt()<<"\tEta:\t"<<jp.recoJet.Eta()<<"\tPhi:\t"<<jp.recoJet.Phi()<<endl;
    os << "dR\t"<<jp.deltaR<<"\tpyRank:\t"<<jp.pythiaRank<<"\tpairRank:\t"<<jp.pairRank;
    return os;

}

#endif





