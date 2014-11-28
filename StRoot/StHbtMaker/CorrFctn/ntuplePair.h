/***************************************************************************
 *
 *
 * Author: Dominik Flierl flierl@bnl.gov
 ***************************************************************************
 *
 * Description:
 *   pairs information into one huge ntuple 
 *
 **************************************************************************/

#ifndef ntuplePair_hh
#define ntuplePair_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#ifdef __ROOT__

#include "StHbtMaker/Infrastructure/StHbtTFile.hh"

class ntuplePair : public StHbtCorrFctn 
{

    // define ntuple content in a nested struct
    struct mPair_t 
    {
	float qinv ;
	float kt ;
	float rap ;
	float entSep ;
	float qual ;
    } ;
    
 private:   
    // my tree
    StHbtTree* mTree ;
    // my tree consists of mtracks
    mPair_t mPair ;
    
 public:
    ntuplePair() ;
    virtual ~ntuplePair() ;
    
    // mandatory members
    virtual StHbtString Report() ;
    virtual void AddRealPair(const StHbtPair*) ;
    virtual void AddMixedPair(const StHbtPair*) ;
    virtual void Finish() ;
    
    // access the tree from 
    StHbtTree* GetNtuplePair() { return mTree; } ;
    
    // root macro
    ClassDef(ntuplePair, 0)
};

#endif // ifdeff ROOT
#endif // ifdeff ntupleTrack_hh

