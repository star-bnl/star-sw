// $Id: StarChamberParametrization.cxx,v 1.1 2004/07/12 20:36:38 potekhin Exp $
//

#include <TError.h>

#include "StarChamberParametrization.h"

ClassImp(StarChamberParametrization)

//_____________________________________________________________________________
StarChamberParametrization::StarChamberParametrization(
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
        Fatal("StarChamberParametrization",
              "StarChamberParametrization construction: Width>Spacing");
      }
   }
   
}

//_____________________________________________________________________________
StarChamberParametrization::~StarChamberParametrization()
{}


//_____________________________________________________________________________
void StarChamberParametrization::ComputeTransformation (
                                     Int_t copyNo, Double_t* position) const
{
  position[0] = 0.;
  position[1] = 0.;
  position[2] = fStartZ + (copyNo+1) * fSpacing;
}

//_____________________________________________________________________________
void StarChamberParametrization::ComputeDimensions(
                                      Int_t copyNo, Double_t* dimension) const
{
  Double_t  halfLength= fHalfLengthFirst + copyNo * fHalfLengthIncr;
  dimension[0] = halfLength;
  dimension[1] = halfLength;
  dimension[2] = fHalfWidth;
}
