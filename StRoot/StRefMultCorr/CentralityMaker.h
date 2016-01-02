//----------------------------------------------------------------------------------------------------
// $Id: CentralityMaker.h,v 1.4 2013/05/10 18:33:33 hmasui Exp $
// $Log: CentralityMaker.h,v $
// Revision 1.4  2013/05/10 18:33:33  hmasui
// Add TOF tray mult, preliminary update for Run12 U+U
//
// Revision 1.3  2012/05/19 00:49:14  hmasui
// Update refmult3
//
// Revision 1.2  2012/05/08 03:19:39  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.1  2012/04/23 21:32:16  hmasui
// Interface for future extention of centrality correction maker to other centrality measures, like refmult2
//
//----------------------------------------------------------------------------------------------------
//  * Interface of StRefMultCorr for possible extention of StRefMultCorr class to the other 
//    centrality measure, such as refmult2.
//  * This interface is also useful when StRefMultCorr needs to be called from two or more different 
//    makers in order to have exactly the same corrected refmult and centrality bins among different makers.
//
//  There is only one change you have to make
//    Replace
//      StRefMultCorr* refmultCorr = new StRefMultCorr();
//    to
//      StRefMultCorr* refmultCorr = CentralityMaker::instance()->getRefMultCorr();
//
//  authors: Hiroshi Masui
//----------------------------------------------------------------------------------------------------

#ifndef __CentralityMaker_h__
#define __CentralityMaker_h__

class StRefMultCorr ;
#include "Rtypes.h"

//____________________________________________________________________________________________________
class CentralityMaker {
  public:
    static CentralityMaker* instance(); // Use this function to access StRefMultCorr
    virtual ~CentralityMaker(); /// Default destructor

    // Interface
    StRefMultCorr* getRefMultCorr()  ; // For refmult
    StRefMultCorr* getRefMult2Corr() ; // For refmult2
    StRefMultCorr* getRefMult3Corr() ; // For refmult3
    StRefMultCorr* getTofTrayMultCorr() ; // For TOF tray multiplicity
    StRefMultCorr* getgRefMultCorr()  ; // For grefmult //Run14 AuAu200GeV

    // Print help messages
    void help() const ;

  private:
    CentralityMaker() ; // Constructor is private
    static CentralityMaker* fInstance ; // Static pointer of CentralityMaker

    // Centrality correction classes
    StRefMultCorr* fRefMultCorr  ; // refmult based centrality
    StRefMultCorr* fRefMult2Corr ; // refmult2 based centrality
    StRefMultCorr* fRefMult3Corr ; // refmult3 based centrality
    StRefMultCorr* fTofTrayMultCorr ; // tofTrayMult based centrality
    StRefMultCorr* fgRefMultCorr  ; // grefmult based centrality

    ClassDef(CentralityMaker, 0)
};
#endif

