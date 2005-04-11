#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "StiDebug.h"
#include "TMath.h"

void StiDebug::Break(int kase)
{
TMath::BesselI0(0.0);
static int myBreak=-2005;
if (kase!=myBreak) return;
  printf("*** Break(%d) ***\n",kase);
}		
