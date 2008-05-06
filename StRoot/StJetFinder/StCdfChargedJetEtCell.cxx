//StCdfChargedJetEtCell.cxx
//M.L. Miller (Yale Software)
//12/02

#include "StCdfChargedJetEtCell.h"

StCdfChargedJetEtCell::StCdfChargedJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax)
    : StEtaPhiCell(etaMin, etaMax, phiMin, phiMax), mLcpPt(0.0)
{
}

StCdfChargedJetEtCell::StCdfChargedJetEtCell(const StCdfChargedJetEtCell& c)
  : StEtaPhiCell(c)
{

}

StCdfChargedJetEtCell::StCdfChargedJetEtCell() : StEtaPhiCell(), mLcpPt(0.0)
{

}

StCdfChargedJetEtCell::~StCdfChargedJetEtCell()
{
}

StEtaPhiCell* StCdfChargedJetEtCell::clone() const
{
  return new StCdfChargedJetEtCell(*this);
}

void StCdfChargedJetEtCell::addProtoJet(const StProtoJet& pj)
{
    StEtaPhiCell::addProtoJet(pj);
    if (pj.pt()>mLcpPt) {
	mLcpPt = pj.pt();
    }
}

void StCdfChargedJetEtCell::addCell(StEtaPhiCell* cell)
{
    StEtaPhiCell::addCell(cell);
}
