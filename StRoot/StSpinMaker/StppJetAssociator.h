//StppJetAssociator.h
//M.L. Miller (Yale Software)
//11/02

#ifndef StppJetAssociator_HH
#define StppJetAssociator_HH

#include "Stiostream.h"
#include <cmath>
#include <vector>
#include <map>
using std::vector;
using std::multimap;

#include "StJet.h"

class StJet;

class AssocJetPair
{
public:
    AssocJetPair(StJet* j1, StJet* j2);
    ~AssocJetPair() {};

    double deltaPhi() const;
    double deltaEta() const;
    double distance() const;
    StJet* jet1();
    StJet* jet2();
    
private:
    AssocJetPair(); //not implemented
    
    StJet* mJet1;
    StJet* mJet2;    
    double mDeltaPhi;
    double mDeltaEta;
    double mDistance;
};

inline ostream& operator<<(ostream& os, AssocJetPair& ap)
{
    return os <<"jet1-et: "<<ap.jet1()->et()<<"\tjet2-et: "<< ap.jet2()->et()
	      <<"\tdistance: "<<ap.distance();
}

class StppJetAssociator
{
public:
    //useful typdefs
    typedef vector<StJet*> StJetVec;
    typedef multimap<StJet*, AssocJetPair> JetPairMap; //note: order by pointer value, not et
    
    StppJetAssociator();
    virtual ~StppJetAssociator() {};

    ///action
    void setAssociationDistance(double v) {mAssocDistance=v;}
    double associationDistance() const {return mAssocDistance;}
    
    void associate(StJetVec::iterator ktBegin, StJetVec::iterator ktEnd,
		   StJetVec::iterator coneBegin, StJetVec::iterator coneEnd);
    
    ///map of associated cone jets keyed by cluster jet
    JetPairMap& clusterKeyedMap();
    
    ///map of associated cluster jets keyed by cone jet
    JetPairMap& coneKeyedMap();

private:
    void clear();
    
private:
    double mAssocDistance;
    JetPairMap mClusterKeyedMap;
    JetPairMap mConeKeyedMap;
};

//non-members

inline StppJetAssociator::JetPairMap& StppJetAssociator::clusterKeyedMap()
{
    return mClusterKeyedMap;
}

inline StppJetAssociator::JetPairMap& StppJetAssociator::coneKeyedMap()
{
    return mConeKeyedMap;
}

inline double AssocJetPair::deltaPhi() const
{
    return mDeltaPhi;
}

inline double AssocJetPair::deltaEta() const
{
    return mDeltaEta;
}

inline double AssocJetPair::distance() const
{
    return mDistance;
}

inline StJet* AssocJetPair::jet1()
{
    return mJet1;
}

inline StJet* AssocJetPair::jet2()
{
    return mJet2;
}

#endif
