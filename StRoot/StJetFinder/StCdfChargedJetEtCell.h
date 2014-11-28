// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCell.h,v 1.10 2008/05/07 22:43:59 tai Exp $
//StCdfChargedJetEtCell.h
//M.L. Miller (Yale Software)
//12/02
#ifndef STCDFCHARGEDJETETCELL_HH
#define STCDFCHARGEDJETETCELL_HH

#include "StEtaPhiCell.h"


/*!
  \class StCdfChargedJetEtCell
  \author M.L. Miller (Yale Software)
  A cell class derived from StJetEtCell, differing only in the implementation of the eT() method.
  Here, eT() returns not the total eT of the cell but the eT of the most energetic four-vector
  in the cell
 */

class StCdfChargedJetEtCell : public StEtaPhiCell
{

public:

  StCdfChargedJetEtCell();
  StCdfChargedJetEtCell(const StCdfChargedJetEtCell& c); 
  StCdfChargedJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax);
  virtual ~StCdfChargedJetEtCell();

  StEtaPhiCell* clone() const;

  ///order cells by lcp-pt instead of overall et
  double eT() const {return _leadingPt;}

  ///Add a protojet to the cell
  void addProtoJet(const StProtoJet&);
    
  ///Add another cell to this one
  void addCell(StEtaPhiCell* cell);
    
  ///internal reset for next pass at jet-finding
  void clear(); 

private:
  ///pt of Leading charged particle in cell
    double _leadingPt;
};


//inlines
inline void StCdfChargedJetEtCell::clear()
{
  _leadingPt = 0.0;
  StEtaPhiCell::clear();
}

#endif // STCDFCHARGEDJETETCELL_HH
