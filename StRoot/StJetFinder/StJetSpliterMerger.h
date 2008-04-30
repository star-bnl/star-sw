// -*- mode: c++;-*-
// $Id: StJetSpliterMerger.h,v 1.3 2008/04/30 01:43:09 tai Exp $
//StJetSpliterMerger.h
//M.L. Miller (Yale Software)
//10/02

#ifndef StJetSpliterMerger_HH
#define StJetSpliterMerger_HH

//std
#include <vector>
#include <map>
using std::vector;
using std::multimap;

//local
#include "StConeJetFinder.h"
#include "StJetEtCell.h"

//useful struct for recording overlaping jets
struct EtNeighbor {

    typedef StConeJetFinder::ValueCellList ValueCellList;
    typedef vector<StJetEtCell*> CellVec;
	
    EtNeighbor();
    EtNeighbor(ValueCellList::iterator it, int n, double et);
	
    //are these the same?  If so, book-keep
    bool check(StJetEtCell* lhs, StJetEtCell* rhs);
	
    //careful, this gets invalidated after insert/delete/sorts in list
    ValueCellList::iterator location; 
	
    //book-keep
    int nCommonCells;
    double sharedEt;
    CellVec cells; //shared cells
};

//order the EtNeighbor objects
struct EtNeighborLessThan
{
    bool operator()(const EtNeighbor& lhs, const EtNeighbor& rhs) const {
	return lhs.sharedEt<rhs.sharedEt;
    };
};

/*!
  \class StJetSpliterMerger
  \author M.L. Miller (Yale Software)
  A work class to disentangle a container of jets that overlap (i.e., share cells).
  Implemented as per the specification of Fermilab RunII Jet Physics working group.
  Two jets are "split" if the shared_energy/highest_jet_energy is greater than the
  splitFraction (0<splitFraction<1).
*/
class StJetSpliterMerger
{
public:
    typedef StConeJetFinder::ValueCellList ValueCellList;
    typedef StJetEtCell::CellList CellList;

    StJetSpliterMerger() {};
    virtual ~StJetSpliterMerger() {};

    ///split jets if E_shared/E_neighbor>splitFraction
    void setSplitFraction(double v) {mSplitFraction=v;}
    double splitFraction() const {return mSplitFraction;}

    ///action
    void splitMerge(ValueCellList& jets);

private:
    typedef vector<StJetEtCell*> CellVec;
    void copyPreJets(ValueCellList& preJets);
    //void copyPostJets(ValueCellList& preJets);
    void split(StJetEtCell& root, StJetEtCell& neighbor, CellVec& commonCells);
    void merge(StJetEtCell& root, StJetEtCell& neighbor, CellVec& commonCells);

private:
    double mSplitFraction;
    ValueCellList mPreJets;

    typedef multimap<EtNeighbor, EtNeighbor, EtNeighborLessThan> EtNeighborMap;
    EtNeighborMap mOverlapMap;
	
};

//non-members

//are these the same?  If so, book-keep
inline bool EtNeighbor::check(StJetEtCell* lhs, StJetEtCell* rhs) 
{
    if (*lhs==*rhs) {
	sharedEt += lhs->eT();
	++nCommonCells;
	cells.push_back(lhs);
	return true;
    }
    else {
	return false;
    }
}

inline ostream& operator<<(ostream& os, const EtNeighbor& n) 
{
    return os <<"sumEt:\t"<<n.sharedEt<<"\tnCells:\t"<<n.nCommonCells;
}

//inlines

#endif
