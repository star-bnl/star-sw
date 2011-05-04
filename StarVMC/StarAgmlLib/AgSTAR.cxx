#include "AgSTAR.h"
#include "AgBlock.h"

Int_t agexist( TString name )
{
  if ( AgBlock::Find(name) ) { return 1; }
  return 0;
}
