// -*- mode: c++;-*-
#include "StKtCluJetFinder.h"

//std
#include <cmath>
#include <float.h>

//root
#include "TObject.h"

//JetFinder
#include "FourVec.h"
#include "StProtoJet.h"
#include "StProtoJetPair.h"
#include "StJetFinder.h"


using namespace std;

    
StKtCluJetFinder::StKtCluJetFinder(const StKtCluPars& pars) : mPars(pars)
{
    cout <<"StKtCluJetFinder::StKtCluJetFinder()"<<endl;
}

StKtCluJetFinder::~StKtCluJetFinder()
{
    cout <<"StKtCluJetFinder::~StKtCluJetFinder()"<<endl;
}

void StKtCluJetFinder::Init()
{

}

void StKtCluJetFinder::findJets(JetList& pj, const FourVecList& particleList)
{
  pj.clear();
  
  for(FourVecList::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
    pj.push_back(StProtoJet(*particle));
  }

    if (pj.size()<2) return; //nothing to be done!
    
    mJets.clear();
    int nSteps = 0;
	
    while (pj.empty()==false) {
		
	if ( mPars.debug() ) {
	    cout <<"\n --- protojets after clustering "<<nSteps<<" steps"<<endl;
	    for (JetList::const_iterator it3=pj.begin(); it3!=pj.end(); ++it3) {
		cout <<*it3<<endl;
	    }
	    cout <<"   --- jets after clustering "<<nSteps<<" steps"<<endl;
	    for (JetList::const_iterator it4=mJets.begin(); it4!=mJets.end(); ++it4) {
		cout <<*it4<<endl;
	    }
			
	}

	//Find the smallest d in protojets:
	double d_min_pj = DBL_MAX;
	JetList::iterator bestProto=pj.end();
		
	//Loop on protojets first:
	for (JetList::iterator protoIt=pj.begin(); protoIt!=pj.end(); ++protoIt) {
	    double d = (*protoIt).d();
	    if (d<d_min_pj) {
		bestProto = protoIt;
		d_min_pj = d;
	    }
	}
		
	//Find the smallest d in protojet pairs:
	double d_min_jets = DBL_MAX;
	JetList::iterator bestJet1=pj.end();
	JetList::iterator bestJet2=pj.end();
		
	for (JetList::iterator jetIt1=pj.begin(); jetIt1!=pj.end(); ++jetIt1) {
	    for (JetList::iterator jetIt2=jetIt1; jetIt2!=pj.end(); ++jetIt2) {
		if (jetIt1!=jetIt2) {
		    StProtoJetPair myPair(*jetIt1, *jetIt2, mPars.r());
		    double d = myPair.d();
		    if (d < d_min_jets) {
			d_min_jets=d;
			bestJet1 = jetIt1;
			bestJet2 = jetIt2;
		    }
		}
	    }
	}
		
	//Now we choose what to do:
		
	if (d_min_pj < d_min_jets) {
			
	    //this proto-jet is unmergable, move it into jet list
			
	    if (bestProto==pj.end()) {
		cout <<"StJetFinder::findJets(). ERROR:\t"
		     <<"Trying to use protojet iterator that doesn't exist"<<endl;
	    }
	    (*bestProto).update();
	    mJets.push_back(*bestProto);
	    pj.erase(bestProto);
	}
		
	//else if (d_min_pj > d_min_jets) {
	else {
	    //these two protojets should be merged
			
	    if (bestJet1==pj.end() || bestJet2==pj.end()) {
		cout <<"StJetFinder::findJets(). ERROR:\t"
		     <<"Trying to use 2 protojet iterators, one doesn't exist"<<endl;
	    }
	    (*bestJet1).merge(*bestJet2);
	    pj.erase(bestJet2);
	}
	/*
	  else {
	  cout <<"JetFinder::findJets().  ERROR:\t"
	  <<"No choice made"<<endl;
	  }
	*/
	++nSteps;
    }
	
    //Now we're all done, copy the jets into the original container, return
    for (JetList::iterator it=mJets.begin(); it!=mJets.end(); ++it) {
	pj.push_back(*it);
    }
	
}
