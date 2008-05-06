// -*- mode: c++;-*-
// $Id: StEtaPhiCell.h,v 1.4 2008/05/06 02:13:16 tai Exp $
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

  //simple access
  double eta() const { return (mEtaMax+mEtaMin)/2.; }
  double phi() const { return (mPhiMax+mPhiMin)/2.; }

  //  virtual double eT() const = 0;
  virtual double eT() const { return mEt; }

  int nTimesUsed() const { return mNtimesUsed; }

  ///Allow jet-finder power to over-ride eT
  void setEt(double v) { mEt = v; }

  double Et() const { return mEt; }


  void update() {
    protoJet().update();
    mEt = protoJet().eT();
  }


  ///operators (sort by Et)
  virtual bool operator>(const StEtaPhiCell& rhs) const {
    return eT() > rhs.eT();
  }
    
  ///operators (sort by Et)
  virtual bool operator<(const StEtaPhiCell& rhs) const {
    return eT() < rhs.eT();
  }

  ///equality via cell geometry/location
  bool operator==(const StEtaPhiCell* rhs) const {
    return ( (*this)==(*rhs) );
  };

  ///equality via cell geometry/location
  bool operator==(const StEtaPhiCell& rhs) const {
    return (mEtaMin==rhs.mEtaMin && mEtaMax==rhs.mEtaMax
	    && mPhiMin==rhs.mPhiMin && mPhiMax==rhs.mPhiMax);
  };

  ///access to the centroid.  If mUpToDate==true, just return.  Else calculate, store, return
  const StProtoJet& centroid();

  ///Add a protojet to this cell
  //  virtual void add(const StProtoJet&) = 0;
  virtual void add(const StProtoJet&);
  ///Add another cell to this cell
  //  virtual void add(StEtaPhiCell* cell) = 0;
  virtual void add(StEtaPhiCell* cell);

  ///count how many jets ref this cell
  void setNtimesUsed(int v) { mNtimesUsed = v; }
  ///internal reset for next pass at jet-finding
  virtual void clear();
  //  virtual void clear() = 0;

  ///distance measures (to be moved to polymorphic behavior)
  double deltaPhi(const StEtaPhiCell& rhs) const { return deltaphi( phi(), rhs.phi() ); }
  ///distance measures (to be moved to polymorphic behavior)
  double deltaEta(const StEtaPhiCell& rhs) const { return eta()-rhs.eta(); }
  ///distance measures (to be moved to polymorphic behavior)
  double distance(const StEtaPhiCell& rhs) const;

  ///write access to the contents of the cell
  StProtoJet& protoJet() {return mProtoJet;}

  ///daugter cells of this one
  CellList& cellList() { return mCells; }
  const CellList& cellList() const{ return mCells; }

  ///does this cell itself have any energy
  bool empty() const {return mProtoJet.size()==0;}

  ///number of particles in protojet
  unsigned int size() const { return mProtoJet.size(); }

protected:

  friend std::ostream& operator<<(std::ostream& os, const StEtaPhiCell& cell);
  friend struct PreJetUpdater;
  friend struct PreJetInitializer;
  friend struct PostMergeUpdater;

  double mEtaMin;
  double mEtaMax;
  double mPhiMin;
  double mPhiMax;
  double mEt;
  int mNtimesUsed;

  ///lazy cache of centroid
  bool mUpToDate;
  StProtoJet mCentroid;

  ///remember the cells cluster w/ this one
  CellList mCells;
  StProtoJet mProtoJet;


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
