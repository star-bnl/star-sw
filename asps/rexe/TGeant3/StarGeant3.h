#ifndef STARGEANT3_H
#define STARGEANT3_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: StarGeant3.h,v 1.1 2000/04/23 19:18:09 fisyak Exp $ */

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//                                                                           //
//    Generic interface to MC for StarRoot                                    //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include "TGeant3.h"

class StarGeant3 : public TGeant3
{

private:

public:
  StarGeant3(const char *title);
  StarGeant3() {}
  virtual ~StarGeant3() {}

  void   SetColors();

  //
  //
  // Control Methods

  virtual void Init();
  virtual void FinishGeometry();
  virtual void ProcessEvent();
  virtual void ProcessRun(Int_t nevent);

  ClassDef(StarGeant3,1) //Generic MonteCarlo Class

};

#endif

