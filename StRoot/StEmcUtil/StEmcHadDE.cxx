//******************************************************************************
//                                                                            
// StEmcHadDe.cxx
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/02/16
//
//******************************************************************************
//
// 2001/12/12 - When using StEmcGeom::getBin(phi.eta,m,e,s) check if s > 0.
//              
// 2001/12/20 - StEmcGeom.h removed.
// 
//------------------------------------------------------------------------------

#include "StEmcHadDE.h"
#include <math.h>

#include "StMcEvent.hh"
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StParticleDefinition.hh"

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"

#include "StEmcPosition.h"

ClassImp(StEmcHadDE)

//------------------------------------------------------------------------------
StEmcHadDE::StEmcHadDE(UInt_t particleId):TObject()
{  
  bemcModuleLength = 257.43;

  for ( UInt_t i = 0; i < 30; i++ )
  {
    for ( UInt_t j = 0; j < 10; j++ )
    {
      for ( UInt_t k = 0; k < 40; k++ )
      {
        depEnergy[i][j][k]=0;
        sigmaDepEnergy[i][j][k]=0;
      }
    }
  }

  if (particleId==8) // Default option
#include "pionPlusGeant.dat"

  if (particleId==9)
#include "pionMinusGeant.dat"

  if (particleId==14)
#include "protonGeant.dat"
}
//------------------------------------------------------------------------------
StEmcHadDE::~StEmcHadDE()
{
}
//------------------------------------------------------------------------------
Float_t StEmcHadDE::getDepEnergy( StTrack* track, Double_t magField, 
                                  Int_t nTowersdEta, Int_t nTowersdPhi )
{
  StEmcPosition* emcPosition = new StEmcPosition();

  StThreeVectorD finalPosition;
  StThreeVectorD finalMomentum;
  Bool_t trackOnEmcOk = emcPosition->trackOnEmc( &finalPosition, &finalMomentum, track, magField );

  if ( trackOnEmcOk )
  {
    Double_t finalEta = finalPosition.pseudoRapidity();
    Double_t finalPhi = finalPosition.phi();
    Double_t momentumMag = finalMomentum.magnitude();
    Float_t distTowerToTrack = emcPosition->getDistTowerToTrack( finalEta, finalPhi, 
                                                     nTowersdEta, nTowersdPhi );
    if ( distTowerToTrack != -1 )
    {
      Float_t mdepEnergy = interp3D( momentumMag, finalEta, distTowerToTrack );  
      delete emcPosition;
      return mdepEnergy;
    }
  }

  delete emcPosition;
  return -1;

}
//------------------------------------------------------------------------------
Float_t StEmcHadDE::getDepEnergy( StMcTrack* track, Double_t magField, 
                                  Int_t nTowersdEta, Int_t nTowersdPhi )
{
  StEmcPosition* emcPosition = new StEmcPosition();

  StThreeVectorD finalPosition;
  StThreeVectorD finalMomentum;
  Bool_t trackOnEmcOk = emcPosition->trackOnEmc( &finalPosition, &finalMomentum, track, magField );

  if ( trackOnEmcOk )
  {
    Double_t finalEta = finalPosition.pseudoRapidity();
    Double_t finalPhi = finalPosition.phi();
    Double_t momentumMag = finalMomentum.magnitude();
    Float_t distTowerToTrack = emcPosition->getDistTowerToTrack( finalEta, finalPhi, 
                                                     nTowersdEta, nTowersdPhi );
    if ( distTowerToTrack != -1 )
    {
      Float_t mdepEnergy = interp3D( momentumMag, finalEta, distTowerToTrack );  
      delete emcPosition;
      return mdepEnergy;
    }
  }

  delete emcPosition;
  return -1;

}
//------------------------------------------------------------------------------
Float_t StEmcHadDE::getDepEnergy( StTrack* track, Double_t magField,
                                  Float_t dist )
{
  StEmcPosition* emcPosition = new StEmcPosition();

  StThreeVectorD finalPosition;
  StThreeVectorD finalMomentum;
  Bool_t trackOnEmcOk = emcPosition->trackOnEmc( &finalPosition, &finalMomentum, track, magField ); 

  if ( trackOnEmcOk )
  {
    Double_t finalEta = finalPosition.pseudoRapidity();
    Double_t momentumMag = finalMomentum.magnitude();
    Float_t distTowerToTrack = dist;
 
    if ( distTowerToTrack != -1 )
    {
      Float_t mdepEnergy = interp3D( momentumMag, finalEta, distTowerToTrack );  
      delete emcPosition;
      return mdepEnergy;  
    }
  }

  delete emcPosition;
  return -1;
}
//------------------------------------------------------------------------------
Float_t StEmcHadDE::getDepEnergy( Double_t momentum, Double_t eta, Double_t phi, 
                                  Int_t dEta, Int_t dPhi )
{  
  StEmcPosition* emcPosition = new StEmcPosition();

  Double_t finalEta = eta;
  Double_t finalPhi = phi;
  Double_t finalMomentum = momentum;
  Float_t distTowerToTrack = emcPosition->getDistTowerToTrack( finalEta, finalPhi, dEta, dPhi );

  if (distTowerToTrack != -1)
  {
    Float_t mdepEnergy = interp3D( finalMomentum, finalEta, distTowerToTrack );  
    delete emcPosition;
    return mdepEnergy;  
  }

  delete emcPosition;
  return -1;
}
//------------------------------------------------------------------------------
Float_t StEmcHadDE::interp3D( Double_t momentum, Double_t eta, Float_t dist )
{
  if ( momentum >= 10.0 ) momentum = 9.99;
  pNdx = UInt_t( momentum/pBin );
  etaNdx = UInt_t( fabs(eta)/etaBin );
  distNdx = UInt_t( dist/distBin );
  Float_t minorp = pNdx*pBin;
  Float_t minorEta = etaNdx*etaBin;
  Float_t minorDist = distNdx*distBin;

  Float_t t = ( momentum-minorp )/pBin;
  Float_t u = ( fabs(eta)-minorEta )/etaBin;
  Float_t v = ( dist-minorDist )/distBin;
  
  Float_t interpDepEnergy = 
     (1.0 - t) * (1.0 - u) * (1.0 - v) * depEnergy[pNdx][etaNdx][distNdx]
           + t * (1.0 - u) * (1.0 - v) * depEnergy[pNdx+1][etaNdx][distNdx]
                   + t * u * (1.0 - v) * depEnergy[pNdx+1][etaNdx+1][distNdx]
                   + t * (1.0 - u) * v * depEnergy[pNdx+1][etaNdx][distNdx+1]
           + (1.0 - t) * u * (1.0 - v) * depEnergy[pNdx][etaNdx+1][distNdx]
                   + (1.0 - t) * u * v * depEnergy[pNdx][etaNdx+1][distNdx+1]
           + (1.0 - t) * (1.0 - u) * v * depEnergy[pNdx][etaNdx][distNdx+1]
                           + u * u * v * depEnergy[pNdx+1][etaNdx+1][distNdx+1];
  return interpDepEnergy;

}
//------------------------------------------------------------------------------



