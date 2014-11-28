// -*- mode: c++;-*-
// $Id: StEtaPhiCell.h,v 1.5 2008/05/06 03:06:11 tai Exp $
#ifndef STETAPHICELL_H
#define STETAPHICELL_H

#include "StProtoJet.h"

#include <vector>
#include <list>
#include <iostream>

double deltaphi(double p1, double p2);

class StEtaPhiCell {

public:

  typedef std::list<StProtoJet> JetList;
  typedef StProtoJet::FourVecList FourList;
  typedef std::list<StEtaPhiCell*> CellList;

  StEtaPhiCell(); 
  StEtaPhiCell(double etaMin, double etaMax, double phiMin, double phiMax);
  virtual ~StEtaPhiCell();

  virtual StEtaPhiCell* clone() const = 0;

  bool isSamePosition(const StEtaPhiCell& other) const {
    return (mEtaMin == other.mEtaMin 
	    && mEtaMax == other.mEtaMax 
	    && mPhiMin == other.mPhiMin 
	    && mPhiMax == other.mPhiMax);
  }

  virtual double eT() const { return mEt; }

  StProtoJet& protoJet() {return mProtoJet;}

  bool empty() const {return mProtoJet.size() == 0;}

  void setNtimesUsed(int v) { mNtimesUsed = v; }
  int nTimesUsed() const { return mNtimesUsed; }

  const StProtoJet& centroid();

  double eta() const { return (mEtaMax+mEtaMin)/2.; }
  double phi() const { return (mPhiMax+mPhiMin)/2.; }

  virtual void clear();

  void setEt(double v) { mEt = v; }
  double Et() const { return mEt; }

  void update() {
    protoJet().update();
    mEt = protoJet().eT();
  }

  CellList& cellList() { return mCells; }
  const CellList& cellList() const { return mCells; }

  virtual void addCell(StEtaPhiCell* cell);

  virtual void addProtoJet(const StProtoJet&);

  double distance(const StEtaPhiCell& rhs) const;

protected:

  int mNtimesUsed;
  double mEt;
  bool mUpToDate;
  CellList mCells;
  StProtoJet mProtoJet;

private:

  double deltaPhi(const StEtaPhiCell& rhs) const { return deltaphi( phi(), rhs.phi() ); }
  double deltaEta(const StEtaPhiCell& rhs) const { return eta()-rhs.eta(); }

  friend std::ostream& operator<<(std::ostream& os, const StEtaPhiCell& cell);
  friend struct PostMergeUpdater;

  double mEtaMin;
  double mEtaMax;
  double mPhiMin;
  double mPhiMax;

  StProtoJet mCentroid;

};

struct StJetEtCellEtGreaterThan {

  bool operator()(StEtaPhiCell* lhs, StEtaPhiCell* rhs)
  {
    return lhs->eT() > rhs->eT();
  }

};

inline std::ostream& operator<<(std::ostream& os, const StEtaPhiCell& cell)
{
    os <<"eta: "<<cell.eta()<<"\tphi: "<<cell.phi()<<"\tet: "<<cell.eT()
      //<<"\tsize: "<<cell.size()<<"\tnUsed: "<<cell.nTimesUsed();
       <<"\tcells:"<< std::endl;
    const StEtaPhiCell::CellList& l = cell.cellList();
    for (StEtaPhiCell::CellList::const_iterator it=l.begin(); it!=l.end(); ++it) {
      std::cout <<**it;
    }

    return os;
}

#endif // STETAPHICELL_H
