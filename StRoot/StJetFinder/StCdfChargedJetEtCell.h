//StCdfChargedJetEtCell.h
//M.L. Miller (Yale Software)
//12/02

#ifndef StCdfChargedJetEtCell_HH
#define StCdfChargedJetEtCell_HH

#include "StJetEtCell.h"

class StCdfChargedJetEtCell : public StJetEtCell
{
public:

    StCdfChargedJetEtCell(); 
    StCdfChargedJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax);
    virtual ~StCdfChargedJetEtCell();

    //order cells by lcp-pt instead of overall et
    virtual double eT() const {return mLcpPt;}

    //action
    virtual void add(const StProtoJet&);
    virtual void add(StJetEtCell* cell);
    virtual void clear(); ///internal reset for next pass at jet-finding

protected:
    double mLcpPt; ///pt of Leading charged particle in cell
};

#endif

//inlines
inline void StCdfChargedJetEtCell::clear()
{
    mLcpPt=0.0;
    StJetEtCell::clear();
}
