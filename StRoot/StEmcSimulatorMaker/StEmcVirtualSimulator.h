//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcVirtualSimulator                                                //
//                                                                      //
// This class provides the interface for EMC simulator.                 //
// It is an abstract class which is inherited by any simulator class    //
// (for example StEmcSimpleSimulator or StEmcPmtSimulator).             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEmcVirtualSimulator
#define STAR_StEmcVirtualSimulator

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

class StEmcVirtualSimulator {

public: 
  StEmcVirtualSimulator();
  virtual ~StEmcVirtualSimulator();

  virtual void     init() = 0;
  virtual Int_t    getAdc(const Double_t, const Double_t) = 0;
  virtual Float_t  getEnergy() = 0;
  virtual void     print() = 0;

  ClassDef(StEmcVirtualSimulator, 1)   // Abstract class for Emc simulator
};
#endif
//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcVirtualSimulator.h,v 1.1 2000/10/23 22:53:15 pavlinov Exp $
//  $Log: StEmcVirtualSimulator.h,v $
//  Revision 1.1  2000/10/23 22:53:15  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
