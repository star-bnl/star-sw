// -*- mode: c++;-*-
// $Id: StKtCluJetFinder.h,v 1.5 2008/05/08 02:22:26 tai Exp $
#ifndef StKtCluJetFinder_HH
#define StKtCluJetFinder_HH

#include "StJetFinder.h"
#include "StKtCluPars.h"

/*!
  \class StKtCluJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the Ellis/Soper kt-cluster algorithm
 */
class StKtCluJetFinder : public StJetFinder
{
public:
    StKtCluJetFinder(const StKtCluPars& pars);
    virtual ~StKtCluJetFinder();

  virtual void Init();

  void findJets(JetList& protoJetList, const FourVecList& particleList);

    ///No operation
    virtual void clear() {};
    
    ///No operation
    virtual void print() {};

private:
    ///not implemented
    StKtCluJetFinder();
    StKtCluPars mPars;
};

#endif

