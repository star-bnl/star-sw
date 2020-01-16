//----------------------------------------------------------------------------------------------------
// $Id: CentralityMaker.cxx,v 1.3 2020/01/16 23:51:55 tnonaka Exp $
// $Log: CentralityMaker.cxx,v $
// Revision 1.3  2020/01/16 23:51:55  tnonaka
// gRefmult for Run14 and Run16 added
//
// Revision 1.2  2019/07/11 03:28:49  tnonaka
// Toftray commented out
//
// Revision 1.5  2015/05/22 06:51:56  hmasui
// Add grefmult for Run14 Au+Au 200 GeV
//
// Revision 1.4  2013/05/10 18:33:33  hmasui
// Add TOF tray mult, preliminary update for Run12 U+U
//
// Revision 1.3  2012/05/19 00:49:16  hmasui
// Update refmult3
//
// Revision 1.2  2012/05/08 03:19:36  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.1  2012/04/23 21:32:12  hmasui
// Interface for future extention of centrality correction maker to other centrality measures, like refmult2
//
//
//----------------------------------------------------------------------------------------------------

#include <iostream>
#include "StRefMultCorr.h"
#include "CentralityMaker.h"

using std::cout ;
using std::endl ;

ClassImp(CentralityMaker)

  CentralityMaker* CentralityMaker::fInstance = 0 ;

//____________________________________________________________________________________________________
CentralityMaker::CentralityMaker()
{
  // Create instance for centrality classes
  fRefMultCorr  = new StRefMultCorr("refmult") ;
  fRefMult2Corr = new StRefMultCorr("refmult2") ;
  fRefMult3Corr = new StRefMultCorr("refmult3") ;
 // fTofTrayMultCorr = new StRefMultCorr("toftray") ;
  fgRefMultCorr  = new StRefMultCorr("grefmult") ;
  fgRefMultCorr_Run14_AuAu200_VpdMB5_P16id = new StRefMultCorr("grefmult","Run14_AuAu200_VpdMB5","P16id") ;
  fgRefMultCorr_Run14_AuAu200_VpdMB30_P16id = new StRefMultCorr("grefmult","Run14_AuAu200_VpdMB30","P16id") ;
  fgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_LowMid_P16id = new StRefMultCorr("grefmult","Run14_AuAu200_VpdMBnoVtx_LowMid","P16id") ;
  fgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_High_P15ic = new StRefMultCorr("grefmult","Run14_AuAu200_VpdMBnoVtx_High","P15ic") ;
  fgRefMultCorr_Run16_AuAu200_VpdMB5_P16ij = new StRefMultCorr("grefmult","Run16_AuAu200_VpdMB5","P16ij") ;
  fgRefMultCorr_Run16_AuAu200_VpdMBnoVtx_P16ij = new StRefMultCorr("grefmult","Run16_AuAu200_VpdMBnoVtx","P16ij") ;
}

//____________________________________________________________________________________________________
CentralityMaker::~CentralityMaker()
{ }

//____________________________________________________________________________________________________
CentralityMaker* CentralityMaker::instance()
{
  if ( !fInstance ) {
    // Initialize StRefMultCorr only once
    fInstance = new CentralityMaker() ;
  }

  return fInstance ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getRefMultCorr()
{
  return fRefMultCorr ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getRefMult2Corr()
{
  return fRefMult2Corr ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getRefMult3Corr()
{
  return fRefMult3Corr ;
}

/*
//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getTofTrayMultCorr()
{
  return fTofTrayMultCorr ;
}
*/

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr()
{
  return fgRefMultCorr ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run14_AuAu200_VpdMB5_P16id()
{
  return fgRefMultCorr_Run14_AuAu200_VpdMB5_P16id ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run14_AuAu200_VpdMB30_P16id()
{
  return fgRefMultCorr_Run14_AuAu200_VpdMB30_P16id ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_LowMid_P16id()
{
  return fgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_LowMid_P16id ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_High_P15ic()
{
  return fgRefMultCorr_Run14_AuAu200_VpdMBnoVtx_High_P15ic ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run16_AuAu200_VpdMB5_P16ij()
{
  return fgRefMultCorr_Run16_AuAu200_VpdMB5_P16ij ;
}

//____________________________________________________________________________________________________
StRefMultCorr* CentralityMaker::getgRefMultCorr_Run16_AuAu200_VpdMBnoVtx_P16ij()
{
  return fgRefMultCorr_Run16_AuAu200_VpdMBnoVtx_P16ij ;
}

//____________________________________________________________________________________________________
void CentralityMaker::help() const
{
  cout << endl;
  cout << "//------------------------------------------------------------------------------" << endl;
  cout << "How to get centrality bins by CentralityMaker ?" << endl;
  cout << "  (Please also take a look at StRoot/StRefMultCorr/macros/getCentralityBins.C" << endl;
  cout << endl;
  cout << "1. Initialize run index to read proper data base" << endl;
  cout << "  Suppose we want to get centrality from refmult at run index 11078000" << endl;
  cout << endl;
  cout << "  // NOTE:" << endl;
  cout << "  //  Use BBC coincidence rate (NOT ZDC coincidence rate) for refmult2)" << endl;
  cout << "  StRefMultCorr* refmultCorr = CentralityMaker::instance()->getRefMultCorr();" << endl;
  cout << "  refmultCorr->init(11078000);" << endl;
  cout << endl;
  cout << "2. Initialize relevant variables event-by-event" << endl;
  cout << endl;
  cout << "  // NOTE:" << endl;
  cout << "  //  1st argument is original multiplicity" << endl;
  cout << "  //  If one wants to have centrality from refmult2, you have to put refmult2" << endl;
  cout << "  refmultCorr->initEvent(refmult, vz, zdcCoincidenceRate);" << endl;
  cout << endl;
  cout << "3. Get centrality bins" << endl;
  cout << endl;
  cout << "  const Int_t cent16 = refmultCorr->getCentralityBin16() ;" << endl;
  cout << "  const Int_t cent9  = refmultCorr->getCentralityBin9() ;" << endl;
  cout << "//------------------------------------------------------------------------------" << endl;
  cout << endl;
}


