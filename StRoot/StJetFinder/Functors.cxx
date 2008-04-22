//#if defined(WIN32)
//#include "stdafx.h"

//Functors.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include <cmath>

//StJetFinder
#include "Functors.h"
#include "StJetEtCell.h"
#include "StProtoJet.h"

bool StJetEtCellEquals::operator()(const StJetEtCell* lhs, const StJetEtCell* rhs) const
{
	return (*lhs)==(*rhs);
}

bool StEtGridKeyLessThan::operator()(const StEtGridKey& lhs, const StEtGridKey& rhs) const
{
    if (lhs.iEta < rhs.iEta) {return true;}
    else if (lhs.iEta > rhs.iEta) {return false;}
    else {return lhs.iPhi<rhs.iPhi;}
}

//bool StJetEtCellEtLessThan::operator()(const StJetEtCell* lhs, const StJetEtCell* rhs) const
bool StJetEtCellEtLessThan::operator()(StJetEtCell* lhs, StJetEtCell* rhs) 
{
    return lhs->eT()<rhs->eT();
}

// bool StJetEtCellEtGreaterThan::operator()(StJetEtCell* lhs, StJetEtCell* rhs)
// {
//     return lhs->eT()>rhs->eT();
// }

// void StJetEtCellClearer::operator()(StJetEtCell* lhs)
// {
//     lhs->clear();
// }

// bool StJetEtCellIsNotEmpty::operator ()(const StJetEtCell* cell)
// {
//     return !cell->empty();
// }

void StProtoJetUpdater::operator()(StJetEtCell* cell)
{
	cell->protoJet().update();
}
