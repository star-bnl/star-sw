/***************************************************************************
 *
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: 
 *    fill pair information into one huge ntuple 
 *
 ***************************************************************************/
 

#include "StHbtMaker/CorrFctn/ntuplePair.h"

#ifdef __ROOT__
ClassImp(ntuplePair)
//______________________________
ntuplePair::ntuplePair()
{
    // create tree
    mTree = new TTree("PairTree","A Tree with all pairs");
    // create Branch
    mTree->Branch("pair",&mPair,"qinv/F:kt/F:rap/F:entSep/F:qual/F");            
}
//_____________________________
ntuplePair::~ntuplePair()
{
    // clean up
    delete mTree;
}
//________________________________
void ntuplePair::AddRealPair(const StHbtPair* Pair)
{
    // extract Pair info and fill tree    
    mPair.qinv    = Pair->qInv() ;
    mPair.kt      = Pair->kT() ;
    mPair.rap     = Pair->rap() ;
    mPair.entSep  = Pair->NominalTpcEntranceSeparation() ;
    mPair.qual    = Pair->quality() ;
    // fill tree
    mTree->Fill();
}
//________________________________
void ntuplePair::AddMixedPair(const StHbtPair* Pair)
{
    // nothing to do here
}
//________________________________
void ntuplePair::Finish()
{
    // nothing to do here
}
//________________________________
StHbtString ntuplePair::Report()
{
    StHbtString t;
    char Ctemp[100];
    sprintf(Ctemp,"Ntuple pair filled.\n");
    t+=Ctemp;
    return t;
};

#endif // ifdef ROOT

