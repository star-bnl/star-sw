// $Id: StXTrak.cxx,v 1.2 2016/06/01 01:06:33 perev Exp $
/// \File StXTrak.cxx
/// \author V.Perev 2016
//
/*!

\class StXTrak

A class StXTrak is a auxiliary for Sti/StiCA/Stv packages.
<br>
Main tasks:
<ul>
<li> Xtend/prolong StTrack to far detector;
<li> Save produced data into StEvent.
</ul>
*/
#include <math.h>
#include <string.h>
#include <assert.h>
#include "TCernLib.h"
#include "StXTrak.h"
#include "TGeoManager.h"
#include "TGeoSwim.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoMaterial.h"
#include "StThreeVectorD.hh"
#include "THelixTrack.h"

//#include "Sti/StiElossCalculator.h"
#include "StvUtil/StvELossTrak.h"
#include "StiUtilities/StiDebug.h"

//_____________________________________________________________________________
StXTrak::StXTrak(MyMag *myMag,StvELossTrak* eLoss) 
{
mELoss=eLoss;		//ELoss calculator
mMyMag=myMag;		//Mag field calculator
}



//_____________________________________________________________________________
double StXTrak::Path(double posp[3],double momp[3], int charge)
{
return 0;
}
#include "StarMagField/StarMagField.h"
//______________________________________________________________________________
/*! Calculates mag field 
  Field is calcualated via StarMagField class 
*/
void MyMag::operator()(const double x[3],double b[3]) 
{
  
static const double EC = 2.99792458e-4;
static StarMagField *magf = StarMagField::Instance();
     magf->BField(x,b);
     b[0]*=EC;
     b[1]*=EC;
     b[2]*=EC;
}

MyLoss::MyLoss() 
{
 mELoss = new StvELossTrak;
 mELoss->Reset(1);
}


double MyLoss::operator()(const TGeoMaterial* mate,double P,double len
                       ,double *theta2)
{
  mELoss->Reset(1);
  mELoss->Set(mate,P);
  mELoss->Add(len);
  double dP = mELoss->PLoss(P);
  if (theta2){};
  return dP;
}
