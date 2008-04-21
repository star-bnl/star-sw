//StJetFinder.h
//M.L. Miller (Yale Software)
//4/02

#ifndef StJetFinder_HH
#define StJetFinder_HH

#include <iostream>

#include <list>
using std::list;

#include "TFile.h"
#include "TNtuple.h"

#include "StProtoJet.h"

#include "StJetPars.h"


/*!
  \class StJetFinder
  \author M.L. Miller (Yale Software)
  An abstract base class to define the interface for all jet finding algorithms.  A list
  of StProtoJet objects is passed in the call to findJets().  The user is responsible for
  filtering the jets from the beam jets after this call.
 */

class StJetFinder
{
public:
    typedef list<StProtoJet> JetList;
    typedef StProtoJet::FourVecList FourVecList;
    StJetFinder();
    virtual ~StJetFinder();

    //action

    /*! Pass a list of protojets.  This list will be packed with jets+beam jets after.
      The user is responsible for filtering the jets.
    */
    virtual void findJets(JetList& protojets) = 0;
    virtual void clear() = 0;
    virtual void print() = 0;


protected:
    JetList mJets;
};

#endif

