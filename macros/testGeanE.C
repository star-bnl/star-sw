// $Id: testGeanE.C,v 1.5 2009/11/27 19:31:22 fisyak Exp $
// $Log: testGeanE.C,v $
// Revision 1.5  2009/11/27 19:31:22  fisyak
// Freeze
//
// Revision 1.4  2009/03/30 18:53:55  fisyak
// Switch from upper to lower error matrix packing
//
// Revision 1.3  2009/03/30 16:31:45  fisyak
// Fix transport matrix (fortran <=> c convention
//
// Revision 1.2  2009/03/29 22:56:21  fisyak
// freeze full GeanE example
//
// Revision 1.1  2009/03/24 21:59:04  fisyak
// Freeze
//
//  GeanE test 
#include "Riostream.h"
#if !defined(__CINT__)
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TDataSet.h"
#include "TGeoManager.h"
#include "TCernLib.h"
#include "StarRoot/TRSymMatrix.h"
#include "StarRoot/TRMatrix.h"
#include "StarRoot/TRVector.h"
#include "StarVMC/StarVMCApplication/StGeanePropagator.h"
#else
class StGeanePropagator;
#endif
//  root.exe  LoadGeanE.C testGeanE.C
/*                 CHOPT                                                *
 *                     'B'   'Backward tracking' - i.e. energy loss     *
 *                                        added to the current energy   *
 *                     'E'   'Exact' calculation of errors assuming     *
 *                                        helix (i.e. pathlength not    *
 *                                        assumed as infinitesimal)     *
 *                     'L'   Tracking upto prescribed Lengths reached   *
 *                     'M'   'Mixed' prediction (not yet coded)         *
 *                     'O'   Tracking 'Only' without calculating errors *
 *                     'P'   Tracking upto prescribed Planes reached    *
 *                     'V'   Tracking upto prescribed Volumes reached   *
 *                     'X'   Tracking upto prescribed Point approached  */
//________________________________________________________________________________
void testGeanE() {
  StGeanePropagator *fgPropagator = StGeanePropagator::instance();
  if (! fgPropagator) return;
  fgPropagator->SetDebug(1);
  //     Main routine for a call to geane tracking alone.
  TString direction(""); // B
  Int_t ikine = 5;
  fgPropagator->SetParticle(ikine);
  fgPropagator->SetSensVolume("tpad");
  fgPropagator->SetSensVolume("svtd");
  fgPropagator->SetSensVolume("sfsd");
  Float_t xxi[3] = {  0, 0, 0};
  fgPropagator->SetXYZ(xxi);
  Float_t plab[3] = { 1, 0.1, 0.1};
  fgPropagator->SetpXYZ(plab);
  Float_t rc[15];
  TCL::vzero(rc,15);
  fgPropagator->SetRC(rc);
  fgPropagator->SetRD(rc);
  while(1) {
    if (fgPropagator->Propagate(direction)) break;
#if 0
    TRVector    PD(3);
    TRVector    PD(5);
    TRSymMatrix RC(5);
    TRSymMatrix RD(5);
    TRVector    PC(3); 
    fgPropagator->ConvRd2Rc(PD,fgPropagator->GetRD(),PC,RC);
    fgPropagator->SetRC(RC);
#endif
    //    break;
  } 
}
