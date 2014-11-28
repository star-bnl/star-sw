#include "AgSTAR.h"
#include "AgBlock.h"

Int_t agexist( TString name, Bool_t cs )
{
  // Does the block exist
  if ( AgBlock::Find(name) ) { return 1; }

  // Return if a case-sensitive search
  if ( cs ) return 0; 
  
  // Otherwise try upper and lower cases
  name.ToUpper(); if ( AgBlock::Find(name) ) { return 1; }
  name.ToLower(); if ( AgBlock::Find(name) ) { return 1; }

  return 0;
}
