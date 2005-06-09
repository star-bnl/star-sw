// $Id: StarMCPrimaryGenerator.cxx,v 1.3 2005/06/09 20:13:47 fisyak Exp $

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
ClassImp(StarMCPrimaryGenerator)
