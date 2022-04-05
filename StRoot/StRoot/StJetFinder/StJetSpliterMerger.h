// -*- mode: c++;-*-
// $Id: StJetSpliterMerger.h,v 1.8 2008/05/08 05:02:14 tai Exp $
//StJetSpliterMerger.h
//M.L. Miller (Yale Software)
//10/02

#ifndef StJetSpliterMerger_HH
#define StJetSpliterMerger_HH

//std
#include <vector>
#include <map>
#include <list>
using std::vector;
using std::multimap;

//local
#include "StConeJetFinder.h"
#include "StEtaPhiCell.h"

//useful struct for recording overlaping jets
struct EtNeighbor {

  //  typedef StConeJetFinder::ValueCellList ValueCellList;
  typedef vector<StEtaPhiCell*> CellVec;
	
  EtNeighbor() : nCommonCells(0), sharedEt(0) { }
	
    //are these the same?  If so, book-keep
  void check(StEtaPhiCell* lhs, StEtaPhiCell* rhs);
	
    //careful, this gets invalidated after insert/delete/sorts in list
    StEtaPhiCell::CellList::iterator _otherCell; 
	
    //book-keep
    int nCommonCells;
    double sharedEt;
    CellVec cells; //shared cells

  friend bool operator<(const EtNeighbor& lhs, const EtNeighbor& rhs){
	return lhs.sharedEt < rhs.sharedEt;
  }

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
    typedef StEtaPhiCell::CellList CellList;

    StJetSpliterMerger() {};
    virtual ~StJetSpliterMerger() {};

    ///split jets if E_shared/E_neighbor>splitFraction
    void setSplitFraction(double v) {mSplitFraction=v;}
    double splitFraction() const {return mSplitFraction;}

    ///action
    void splitMerge(CellList& jets);

private:

  typedef vector<StEtaPhiCell*> CellVec;
  void split(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells);
  void merge(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells);

  double mSplitFraction;

  CellList _preJets;
  std::list<EtNeighbor> _OverlapList;
	
};

//non-members

//are these the same?  If so, book-keep
inline void EtNeighbor::check(StEtaPhiCell* lhs, StEtaPhiCell* rhs) 
{
  //  if (!(*lhs==*rhs)) return;
  if (!lhs->isSamePosition(*rhs)) return;

  sharedEt += lhs->eT();
  ++nCommonCells;
  cells.push_back(lhs);
}

inline ostream& operator<<(ostream& os, const EtNeighbor& n) 
{
    return os <<"sumEt:\t"<<n.sharedEt<<"\tnCells:\t"<<n.nCommonCells;
}

struct PreJetLazyUpdater //assume proto-jet updated
{
    PreJetLazyUpdater() : sumEt(0.) {};
    double sumEt;
	
    void operator()(StEtaPhiCell& cell);
    void operator()(StEtaPhiCell* cell);
};

struct PostMergeUpdater
{
    void operator()(StEtaPhiCell& cell);
};


//inlines

#endif
