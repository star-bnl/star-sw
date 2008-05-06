// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCell.h,v 1.8 2008/05/06 03:06:10 tai Exp $
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
    double eT() const {return mLcpPt;}

    ///Add a protojet to the cell
    void addProtoJet(const StProtoJet&);
    
    ///Add another cell to this one
    void addCell(StEtaPhiCell* cell);
    
    ///internal reset for next pass at jet-finding
    void clear(); 

protected:
    ///pt of Leading charged particle in cell
    double mLcpPt;
};


//inlines
inline void StCdfChargedJetEtCell::clear()
{
    mLcpPt=0.0;
    StEtaPhiCell::clear();
}

#endif // STCDFCHARGEDJETETCELL_HH
