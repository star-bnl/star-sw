// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCell.h,v 1.5 2008/05/05 00:32:47 tai Exp $
//StCdfChargedJetEtCell.h
//M.L. Miller (Yale Software)
//12/02

#ifndef StCdfChargedJetEtCell_HH
#define StCdfChargedJetEtCell_HH

#include "StJetEtCell.h"


/*!
  \class StCdfChargedJetEtCell
  \author M.L. Miller (Yale Software)
  A cell class derived from StJetEtCell, differing only in the implementation of the eT() method.
  Here, eT() returns not the total eT of the cell but the eT of the most energetic four-vector
  in the cell
 */

class StCdfChargedJetEtCell : public StJetEtCell
{
public:

    StCdfChargedJetEtCell(); 
    StCdfChargedJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax);
    virtual ~StCdfChargedJetEtCell();

    ///order cells by lcp-pt instead of overall et
    virtual double eT() const {return mLcpPt;}

    ///Add a protojet to the cell
    virtual void add(const StProtoJet&);
    
    ///Add another cell to this one
    virtual void add(StEtaPhiCell* cell);
    
    ///internal reset for next pass at jet-finding
    virtual void clear(); 

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

#endif // StCdfChargedJetEtCell_HH
