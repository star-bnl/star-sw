// $Id: StarMCPrimaryGenerator.cxx,v 1.2 2011/02/11 16:12:52 fisyak Exp $

#include "TVirtualMC.h"
#include "TVirtualMCStack.h"
#include "TVirtualMCApplication.h"
#include "TRandom.h"
#include "TPDGCode.h"
#include "TDatabasePDG.h"
#include "TVector3.h"
#include "TMath.h"
#include "TRandom.h"
#include "Stiostream.h"
#include "StarMCPrimaryGenerator.h"
StarMCPrimaryGenerator *StarMCPrimaryGenerator::fgInstance = 0;
ClassImp(StarMCPrimaryGenerator);
//________________________________________________________________________________
void StarMCPrimaryGenerator::Print(Option_t *option) const {
  if (fStarMcStack) fStarMcStack->Print();
}
