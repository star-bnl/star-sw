#include "StarRandom.h"
ClassImp(StarRandom);
#include "assert.h"

#include "Math/GSLRndmEngines.h"

#include <iostream>
#include <string>
#include "StMessMgr.h"
#include "TRandom.h"

#include "TSystem.h"

using namespace std;
StarRandom *StarRandom::sInstance = 0;
// ----------------------------------------------------------------------------
StarRandom::StarRandom() {sInstance = this;}
// ----------------------------------------------------------------------------
StarRandom &StarRandom::Instance()
{
  if (! sInstance) new StarRandom();
  return (*sInstance);
}
