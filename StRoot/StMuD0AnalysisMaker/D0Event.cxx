#include "D0Event.h"
ClassImp(D0Event)

TClonesArray *D0Event::fgPairs = 0;

//______________________________________________________________________________
D0Event::D0Event()
{
   // Create an D0Event object.
   // When the constructor is invoked for the first time, the class static
   // variable fgPairs is 0 and the TClonesArray fgPairs is created.

   if (!fgPairs) fgPairs = new TClonesArray("KpiPair", 1000);
   fPairs = fgPairs;
}

//______________________________________________________________________________
KpiPair *D0Event::AddPair() {
  KpiPair *pai = (KpiPair*) fPairs->ConstructedAt(fNPairs++);
  return pai;
}

