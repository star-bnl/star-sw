//!#include "stdafx.h"

//StKonstJetFinder.cxx
//adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//std
#include <iostream>
#include <algorithm>
#include <time.h>
using std::for_each;
using std::sort;

//StJetFinder
#include "StJetEtCell.h"
#include "StKonstJetFinder.h"
#include "StProtoJet.h"

ClassImp(StKonstJetFinder)

StKonstJetFinder::StKonstJetFinder(StKonstJetFinderPars& pars) 
    : mPars(pars)
{
    cout <<"StKonstJetFinder::StKonstJetFinder()"<<endl;
    pt = new Float_t[MAXTRACKS_KJF];
    eta = new Float_t[MAXTRACKS_KJF];
    phi = new Float_t[MAXTRACKS_KJF];
    mass = new Float_t[MAXTRACKS_KJF];
    typedef StProtoJet* StProtoJetP;
    tracks = new StProtoJetP[MAXTRACKS_KJF];
    jets =   new StProtoJet [MAXJETS_KJF];
}

StKonstJetFinder::~StKonstJetFinder()
{
    clearAndDestroy();
    delete pt;
    delete eta;
    delete phi;
    delete mass;
    delete tracks;
    delete jets;
}

void StKonstJetFinder::clearAndDestroy(void)
{
}

bool StKonstJetFinder::acceptSeed(const StJetEtCell* cell)
{
    return (cell->nTimesUsed()==0 && cell->empty()==false);
}

void StKonstJetFinder::findJets(JetList& protojets)
{
  jetFinder.setParameters(mPars.coneRadius, mPars.etSeed, mPars.minJetEt, mPars.minCellEt);
  jetFinder.setmModeInfoCluster(mPars.modeInfoCluster);

  // Here is a bit of a cludge because Konstantin's code uses indexes
  // whereas C++ Templates does not use them explicitly.  Therefore,
  // in the following it is necessary to extract the ProtoJet information
  // piece by piece and feed it into the array.  Worse, to combine the
  // protojets based upon index within Konstantin's code, we need to 
  // place them in an array so we can access them by the same set of
  // indices (which C++ Templates does not promise will work.)
  Int_t numTracks = 0;
  for(JetList::iterator it=protojets.begin(); it!=protojets.end(); ++it)
  {
    StProtoJet &pj = *it;
    pt[numTracks] = pj.pt();
    eta[numTracks] = pj.eta();
    phi[numTracks] = pj.phi();
    mass[numTracks] = pj.mass();
    tracks[numTracks] = &pj;
    numTracks++;
  }
  for(int i = 0; i < MAXJETS_KJF; i++)
    jets[i].clear();

  Int_t numJets = kfindJets(numTracks, 
      pt, eta, phi, mass, jets, tracks);
  
  // Definitely, do not clear the protojets until we are done with 
  // tracks, because this protojets.clear might easily have the side effect
  // of invalidating the pointers within tracks -> Segmentation Violation!
  protojets.clear();
  for(int i = 0; i < numJets; i++)
  {
    protojets.push_back(jets[i]);
  }  
}

void StKonstJetFinder::clear()
{
}

void StKonstJetFinder::print()
{
    cout <<"StKonstConeJetFinder::print()"<<endl;
}
