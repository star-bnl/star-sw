// $Id: StMCChamberParameterisation.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Id: StMCChamberParameterisation.cc,v 1.7 2002/01/09 17:24:09 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
// by Ivana Hrivnacova, 21.4.2002

#include <TError.h>

#include "StMCChamberParameterisation.h"

ClassImp(StMCChamberParameterisation)

//_____________________________________________________________________________
StMCChamberParameterisation::StMCChamberParameterisation(
                                 Int_t    NoChambers, 
                                 Double_t startZ,      
                                 Double_t spacingZ,   
                                 Double_t widthChamber, 
                                 Double_t lengthInitial, 
                                 Double_t lengthFinal )
  : TObject()				 
{
   fNoChambers =  NoChambers; 
   fStartZ     =  startZ; 
   fHalfWidth  =  widthChamber*0.5;
   fSpacing    =  spacingZ;
   fHalfLengthFirst = 0.5 * lengthInitial; 
   // fHalfLengthLast = lengthFinal;
   if( NoChambers > 0 ){
      fHalfLengthIncr =  0.5 * (lengthFinal-lengthInitial)/NoChambers;
      if (spacingZ < widthChamber) {
        Fatal("StMCChamberParameterisation",
              "StMCChamberParameterisation construction: Width>Spacing");
      }
   }
   
}

//_____________________________________________________________________________
StMCChamberParameterisation::~StMCChamberParameterisation()
{}


//_____________________________________________________________________________
void StMCChamberParameterisation::ComputeTransformation (
                                     Int_t copyNo, Double_t* position) const
{
  position[0] = 0.;
  position[1] = 0.;
  position[2] = fStartZ + (copyNo+1) * fSpacing;
}

//_____________________________________________________________________________
void StMCChamberParameterisation::ComputeDimensions(
                                      Int_t copyNo, Double_t* dimension) const
{
  Double_t  halfLength= fHalfLengthFirst + copyNo * fHalfLengthIncr;
  dimension[0] = halfLength;
  dimension[1] = halfLength;
  dimension[2] = fHalfWidth;
}
