/*:>-------------------------------------------------------------------
**:
**:     program: tpc2l3_inc.h (StAF version)
**:
**:     Includefile for tpc2l3 routine,
**:     includes data structures and defines
**:
**:     author: cs - Christof Struck, struck@star.physics.yale.edu
**:     last change: 09/16/98 cs
**:
**:>-----------------------------------------------------------------*/

// includes
#include "PAM.h"
#include "tpc2l3.h"
#include <stdlib.h>

// definitions
#define NPADROWS_IN   13         //  0..12
#define NPADROWS_OUT  32         // 13..44


#if defined(__cplusplus)
extern "C" {
#endif

// prototypes
void i960map ( int i960no, int *StartRow, int *EndRow );


#if defined(__cplusplus)

}
#endif
