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
  virtual void     setPedestal(const UInt_t type, const Float_t pedMean, const Float_t pedRMS) = 0; 
  virtual void     setParameters(const Float_t calibCoeff,const UInt_t type, const Float_t pedMean, const Float_t pedRMS, const Float_t gainUnc)=0;  
  virtual Double_t getPedestal(const Int_t type, const Double_t pedMean, const Double_t pedRMS) = 0;
  virtual Double_t deductPedestal(const Int_t type, const Int_t adc, const Double_t pedMean) = 0;
  virtual Int_t    getAdc(const Double_t de, const Double_t eta) = 0;
  virtual Float_t  getEnergy() = 0;
  virtual void     print() = 0;

  ClassDef(StEmcVirtualSimulator, 1)   // Abstract class for Emc simulator
};
#endif
//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcVirtualSimulator.h,v 1.3 2004/08/06 13:24:48 suaide Exp $
//  $Log: StEmcVirtualSimulator.h,v $
//  Revision 1.3  2004/08/06 13:24:48  suaide
//  New features added and fixed some bugs in the database
//
//  Revision 1.2  2002/06/04 16:09:37  pavlinov
//  added option with DB(pedestal ans calibration  coefficients
//
//  Revision 1.1  2000/10/23 22:53:15  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
