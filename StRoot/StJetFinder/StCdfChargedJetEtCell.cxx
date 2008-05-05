//StCdfChargedJetEtCell.cxx
//M.L. Miller (Yale Software)
//12/02

#include "StCdfChargedJetEtCell.h"

StCdfChargedJetEtCell::StCdfChargedJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax)
    : StJetEtCell(etaMin, etaMax, phiMin, phiMax), mLcpPt(0.0)
{
}

StCdfChargedJetEtCell::StCdfChargedJetEtCell() : StJetEtCell(), mLcpPt(0.0)
{
}

StCdfChargedJetEtCell::~StCdfChargedJetEtCell()
{
}

void StCdfChargedJetEtCell::add(const StProtoJet& pj)
{
    StEtaPhiCell::add(pj);
    if (pj.pt()>mLcpPt) {
	mLcpPt = pj.pt();
    }
}

void StCdfChargedJetEtCell::add(StEtaPhiCell* cell)
{
    StEtaPhiCell::add(cell);
}
