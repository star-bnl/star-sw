//////////////////////////////////////////////////////////////////////////
//
// StEmcPmtSimulator.h 
//
// This class simulates EMC response with accounting primary 
// and secondary photostatistics. It supports the modes for 
// simple simulator also.
//
// mode = 2; Taking into account only primary photostatistics.
//
// mode = 3; Taking into account primary and secondary 
//           photostatistics(full simulation);
//
// mode = 4; Taking into account primary and secondary 
//           photostatistics(fast simulation);
//                                                                    
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEmcPmtSimulator
#define STAR_StEmcPmtSimulator

#ifndef ROOT_Rtypes
#include <Rtypes.h>
#endif
#include <TMath.h>
#include <TRandom.h>

#include "StEmcSimpleSimulator.h"
#include "tables/St_controlEmcPmtSimulator_Table.h"
#include "StPmtSignal.h"

class StEmcPmtSimulator : public StEmcSimpleSimulator{
protected:

  StPmtSignal mPmtSignal;
  TRandom     mRandom;
  
  // Working variable
  Double_t mC2;  // Number of PHE on one Gev
  Double_t mC3;
  Int_t    mVer; 

public: 
  StEmcPmtSimulator(UInt_t det);
  virtual ~StEmcPmtSimulator() {/* nothing */};
  Bool_t   setControlDefault(UInt_t det);

  virtual void    init();
  virtual Int_t   getAdc(const Double_t de, const Double_t eta);
  virtual void    print();

  ClassDef(StEmcPmtSimulator, 1) // Emc simulator with accounting primary and secondary photostatistics
};
#endif

//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcPmtSimulator.h,v 1.1 2000/10/23 22:53:14 pavlinov Exp $
//  $Log: StEmcPmtSimulator.h,v $
//  Revision 1.1  2000/10/23 22:53:14  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
