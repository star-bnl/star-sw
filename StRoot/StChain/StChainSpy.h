// $Id: StChainSpy.h,v 1.1 1998/10/07 18:44:01 perev Exp $
// $Log: StChainSpy.h,v $
// Revision 1.1  1998/10/07 18:44:01  perev
// Add Spy classes for Farm Monitor
//
// Revision 1.11  1998/09/18 14:35:29  fisyak
// Fix makers
//
// Revision 1.10  1998/09/08 22:43:09  fisyak
// Modify St_dst_Maker to account new calling sequence
//
// Revision 1.9  1998/09/08 13:42:00  love
// new St_tpctest_Maker module
//
// Revision 1.8  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.7  1998/08/07 19:34:53  fisyak
// Add St_run_Maker
//
// Revision 1.6  1998/07/20 15:08:08  fisyak
// Add tcl and tpt
//

#ifndef STAR_StChainSpy
#define STAR_StChainSpy

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChain                                                              //
//                                                                      //
// Main base class to control chains for the different STAR "chains"    //
//                                                                      //
// This class :                                                         //
//   - Initialises the run default parameters                           //
//   - Provides API to Set/Get run parameters                           //
//   - Creates the support lists (TClonesArrays) for the Event structure//
//   - Creates the physics objects makers                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TTree
#include <TTree.h>
#define ROOT_TTree
#endif

#include "St_DataSet.h"

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StChain.h"
#include "StFarmSpy.h"

//static Char_t      *m_VersionCVS=" 

class StChainSpy : public StChain {
public:
  StChainSpy():StChain(){}
  StChainSpy(const char *name, const char *title="STAR Big Full Chain")
  :StChain(name, title){}
  virtual           ~StChainSpy(){};
  
  virtual Int_t      Finish();
  virtual Int_t      Init();
  virtual void       StartMaker(StMaker *mk);
  virtual void       EndMaker  (StMaker *mk,Int_t iret);
  virtual Int_t      Make() {return StChain::Make();}
  virtual Int_t      Make(Int_t i);
  virtual void	     Fatal(int Ierr, const char *com);  
  ClassDef(StChainSpy,1)   //StChain control class
};

#endif
