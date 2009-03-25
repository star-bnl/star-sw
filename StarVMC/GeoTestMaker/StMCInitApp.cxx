// $Id: StMCInitApp.cxx,v 1.1 2009/03/25 23:15:10 perev Exp $
//
//
// Class StMCInitApp
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include "Stiostream.h"
#include "StMCInitApp.h"
#include "TVirtualMC.h"
#include "TVirtualMCApplication.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TGeoManager.h"
#include "StVMCApplication.h"
#include "TGeant3TGeo.h"
#include "StMCStack.h"
#include "StMCConstructGeometry.h"
#include "StMCSimplePrimaryGenerator.h"
#include "StMCSteppingHist.h"

ClassImp(StMCInitApp)

//_____________________________________________________________________________
StMCInitApp::StMCInitApp()  
{
}   
//_____________________________________________________________________________
int  StMCInitApp::Fun()
{
  StVMCApplication  *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  gMC  = new TGeant3TGeo("C++ Interface to Geant3"); 
  Info("Init","TGeant3TGeo has been created.");
  StMCConstructGeometry *geo = new StMCConstructGeometry(app->GetName());
  app->SetConstructGeometry(geo);

  StMCSimplePrimaryGenerator *gen = new StMCSimplePrimaryGenerator( 10, 48, 1.,1., 0.,6, -15,15,  0.,0., "G");
  app->SetPrimaryGenerator(gen);


  gMC->SetStack(new StMCStack(100));
  gMC->Init();
  gMC->BuildPhysics(); 

  Info("Init","switch off physics");
  gMC->SetProcess("DCAY", 0);
  gMC->SetProcess("ANNI", 0);
  gMC->SetProcess("BREM", 0);
  gMC->SetProcess("COMP", 0);
  gMC->SetProcess("HADR", 0);
  gMC->SetProcess("MUNU", 0);
  gMC->SetProcess("PAIR", 0);
  gMC->SetProcess("PFIS", 0);
  gMC->SetProcess("PHOT", 0);
  gMC->SetProcess("RAYL", 0);
  gMC->SetProcess("LOSS", 4); // no fluctuations 
  //  gMC->SetProcess("LOSS 1"); // with delta electron above dcute
  gMC->SetProcess("DRAY", 0);
  gMC->SetProcess("MULS", 0);
  gMC->SetProcess("STRA", 0);
  gMC->SetCut("CUTGAM",	1e-3  );
  gMC->SetCut("CUTELE", 	1e-3  );
  gMC->SetCut("CUTHAD", 	.001  );
  gMC->SetCut("CUTNEU", 	.001  );
  gMC->SetCut("CUTMUO", 	.001  );
  gMC->SetCut("BCUTE", 	.001  );
  gMC->SetCut("BCUTM", 	.001  );
  gMC->SetCut("DCUTE", 	1e-3  );
  gMC->SetCut("DCUTM", 	.001  );
  gMC->SetCut("PPCUTM", 	.001  );
  gMC->SetCut("TOFMAX", 	50.e-6);

  return 0;
}





		
		
		
