// -*- mode: c++;-*-
// $Id: StKtCluJetFinder.h,v 1.4 2008/04/22 00:15:00 tai Exp $
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

    /*! Pass a list of protojets.  This list will be packed with jets+beam jets after..
      The user is responsible for filtering the jets. */
    virtual void findJets(JetList& protojets);

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

