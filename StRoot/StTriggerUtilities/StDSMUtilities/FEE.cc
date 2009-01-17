//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 1 Jan 2009
//

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>

#ifdef __ROOT__
#include "/star/u/pibero/tcl8.5.3/generic/tcl.h"
#else
#include <tcl8.4/tcl.h>
#endif
#include "FEE.hh"

bool  FEE::debug = false;

int   FEE::pedestalShift = 24;
int   FEE::adc[NTOWERS+1];
float FEE::ped[NTOWERS+1];
int   FEE::status[NTOWERS+1];
int   FEE::ped4[NTOWERS+1];

Tcl_Interp* FEE::tcl = Tcl_CreateInterp();

static char script[1024];	 // Script buffer for Tcl commands
static int nbytes;		 // Length of script in buffer

void FEE::initTcl()
{
  assert(Tcl_EvalFile(tcl, "getTowerPedestal.tcl") == TCL_OK);
  assert(Tcl_EvalFile(tcl, "getLUTscale.tcl"     ) == TCL_OK);
  assert(Tcl_EvalFile(tcl, "getLUTped.tcl"       ) == TCL_OK);
  assert(Tcl_EvalFile(tcl, "getLUTrange.tcl"     ) == TCL_OK);
  assert(Tcl_EvalFile(tcl, "getLUTvalue.tcl"     ) == TCL_OK);
}

void FEE::setPedestalShift(int pedestalShift)
{
  FEE::pedestalShift = pedestalShift;
  nbytes = sprintf(script, "set PedestalShift %d", pedestalShift);
  if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
  assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);
}

void FEE::setTriggerPatch(int triggerPatch)
{
  this->triggerPatch = triggerPatch;
  patch = triggerPatch % 10;
  board = patch / 2 + 1;
  mask  = patch % 2 + 1;
}

void FEE::setTowerPedestal(int id, float ped)
{
  FEE::ped[id] = ped;
  nbytes = sprintf(script, "expr [getTowerPedestal %f]", ped);
  if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
  assert(Tcl_EvalEx(tcl, script, nbytes, 0) == TCL_OK);
  const char* result = Tcl_GetStringResult(tcl);
  assert(*result);
  if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, result);
  FEE::ped4[id] = atoi(result);
}

void FEE::writeLUT()
{
  for (int sum = 0; sum < 4096; ++sum) {
    nbytes = sprintf(script, "set lutUseMask %d", formula);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "set lutScale %d", parameters[0]);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "set lutPed(%d,%d) %d", mask, board, parameters[1]);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "set lutSigma(%d,%d) %d", mask, board, parameters[2]);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "set lutUsePowerup %d", parameters[3]);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "set triggermask%d(%d) %d", mask, board, triggermask());
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, TCL_EVAL_GLOBAL) == TCL_OK);

    nbytes = sprintf(script, "expr [getLUTvalue %d %d %d]", board, mask-1, sum);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, script);
    assert(Tcl_EvalEx(tcl, script, nbytes, 0) == TCL_OK);

    const char* result = Tcl_GetStringResult(tcl);
    assert(*result);
    if (debug) printf("%s:%d: %s\n", __FILE__, __LINE__, result);
    lut[sum] = atoi(result);
  }
}

void FEE::action()
{
  int highTower = 0;
  int patchSum  = 0;

  for (int i = 0; i < 16; ++i) {
    int id = towers[i];
    if (!FEE::status[id]) continue;

    // Simulate FEE action

    int operationBit = FEE::ped4[id] & 0x10;
    int ped = FEE::ped4[id] & 0x0f;
    int adc = FEE::adc[id] >> 2; // 10-bit

    if (operationBit)
      adc -= ped;
    else
      adc += ped;

    patchSum += adc >> 2;	// 8-bit

    if (bitConvMode > 0) {
      int upperBits = (adc >> bitConvMode) & ~0x1f;
      adc >>= bitConvMode;
      adc  &= 0x1f;
      if (upperBits) adc |= 0x20;
    }

    if (adc > highTower) highTower = adc;
  }

  if (!highTowerStatus) highTower = 0;

  if (!patchSumStatus)
    patchSum = 0;
  else
    patchSum = lut[patchSum];

  output = highTower | patchSum << 6;
}
