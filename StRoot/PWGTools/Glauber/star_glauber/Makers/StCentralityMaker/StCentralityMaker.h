/******************************************************************************
 * $Id: StCentralityMaker.h,v 1.4 2012/04/25 05:17:24 hmasui Exp $
 * $Log: StCentralityMaker.h,v $
 * Revision 1.4  2012/04/25 05:17:24  hmasui
 * Remove SetModeNBD() and corresponding data member, merged into GetNegativeBinomial() function
 *
 * Revision 1.3  2010/11/20 19:01:32  hmasui
 * Move mode flag for NBD to contrl npp and x from StFastGlauberMcMaker
 *
******************************************************************************/

#ifndef __StCentralityMaker_h__
#define __StCentralityMaker_h__

#include "Rtypes.h"
#include <vector>
class StCentrality ;
class StNegativeBinomial ;

//____________________________________________________________________________________________________
// Class StCentralityMaker: Centrality maker
//   - Have 3 different NBD parameters: default, low (npp, x), high (npp, x)
//
//   - Access centrality class (StCentrality) by
//    StCentrality* GetCentrality(const UInt_t id) const
//    where id = 0, 1, 2
//    id = 0: default centrality
//    id = 1: low (npp, x)
//    id = 2: high (npp, x)
//
//   - Each centrality class (StCentrality) can provide 3 different centrality bins by
//    StCentrality::GetCentrality(const UInt_t multiplicity, const UInt_t mode) const
//    where mode = 0, 1 or 2
//    mode = 0: default centrality
//    mode = 1: -5% total cross section
//    mode = 2: +5% total cross section
//
class StCentralityMaker {
  public:
    StCentralityMaker(); /// Default constructor
    StCentralityMaker(const Char_t* system); /// Call init(const Char_t* system)
    virtual ~StCentralityMaker(); /// Default destructor

    // Get NBD
    //    Make sure the proper "mode" has been set by StCentralityMaker::SetModeNBD(const UInt_t mode) ;
    //    Default mode is 0
    //
    //    mode     description
    //    0        default Npp
    //    1        Small Npp, large x
    //    2        Large Npp, small x
    const StNegativeBinomial* GetNegativeBinomial(const UInt_t id = 0) const ;

    // Initialization for specific systems (see details in source)
    void Init(const Char_t* system = "AuAu_200GeV") ;
    
    // Get centrality, use StCentrality::GetCentrality(const UInt_t multiplicity) 
    // to access the centrality values from multiplicity
    StCentrality* GetCentrality(const UInt_t id = 0) const ;

  private:
    // Functions

    /// Initialization of NBD (default is TPC refmult)
    void Init(const Double_t npp, const Double_t k, const Double_t x,
        const Double_t efficiency=1.0, const Double_t triggerbias=1.0, const Bool_t useTpc=kTRUE) ;

    // Data members
    std::vector<StNegativeBinomial*> mNBinomial ; /// Negative binomial utility
    std::vector<StCentrality*> mCentrality      ; /// Centrality utility

    ClassDef(StCentralityMaker, 0)
};
#endif

