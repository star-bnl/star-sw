// -*- mode: c++;-*-
// $Id: StJetEtCell.h,v 1.17 2008/05/06 03:06:12 tai Exp $
//StJetEtCell.h
//M.L. Miller (Yale Software) (adapted from Akio Ogawa's work)
//07/02
#ifndef STJETETCELL_H
#define STJETETCELL_H

#include "StEtaPhiCell.h"

#include "StProtoJet.h"

#include <vector>
#include <list>
#include <iostream>


/*!
  \class StJetEtCell
  \author M.L. Miller (Yale Software)
  The work object of StConeJetFinder and derived classes.  A collection of cells constitutes a
  grid.  Protojets are filled into the grid by calling StJetEtCell::add(StProtoJet&).  The protojets
  are stored in a container, so there is no loss of information.  Thus, StJetEtCell is used for
  computation efficiency.
 */

class StJetEtCell : public StEtaPhiCell {

public:

  StJetEtCell(); 
  StJetEtCell(const StJetEtCell& c);
  StJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax);
  virtual ~StJetEtCell();

  StEtaPhiCell* clone() const;

  double eT() const { return mEt; }

  void addProtoJet(const StProtoJet&);
  void addCell(StEtaPhiCell* cell);
  void clear(); 

};

#endif // STJETETCELL_H
