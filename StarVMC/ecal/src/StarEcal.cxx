/* $Id: StarEcal.cxx,v 1.1 2004/07/16 22:56:49 potekhin Exp $ */

#include <TVirtualMC.h>

#include "StarEcal.h"
#include <iostream.h>
#include "StarMaterial.h"
#include "StarMedium.h"
#include "StarVolume.h"
#include "StarRotation.h"

#include "CALG.h"


ClassImp(StarEcal)


//_______________________________________________________________________
  StarEcal::StarEcal()
{
  cout<<"Constructing Star ECAL"<<endl;
}
//_______________________________________________________________________
//_______________________________________________________________________
StarEcal::StarEcal(const char* name_, const char *title_):
  StarDetector(name_,title_)
{
}

//_______________________________________________________________________
StarEcal::~StarEcal()
{
}
//_______________________________________________________________________
void StarEcal::CreateGeometry(void) {

  // the ECAL envelope:------------------------------
  Float_t ecal[3];
  ecal[0] = CALG::rmin;       // ECAL inner radius
  ecal[1] = CALG::rmax;       // ECAL outer radius
  ecal[2] = CALG::length/2.0; // ECAL half length
  StarVolume::Volume("CALB","TUBE","Pb",ecal,3);

  Double_t posX =  0.0;
  Double_t posY =  0.0;
  Double_t posZ =  0.0;
  gMC->Gspos("CALB", 1 ,"WRLD", posX, posY, posZ, 0, "ONLY");


  //  StarVolume::Division("TESS","TPEA", 12,2);



}
