// -*- mode: c++;-*-
// $Id: StJetEtCell.h,v 1.15 2008/05/05 01:46:07 tai Exp $
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
  StJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax);
  virtual ~StJetEtCell();


  double eT() const { return mEt; }

  void add(const StProtoJet&);
  void add(StEtaPhiCell* cell);
  void clear(); 

};

#endif // STJETETCELL_H
