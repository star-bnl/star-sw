// $Id: StMCChamberParameterisation.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Id: StMCChamberParameterisation.hh,v 1.6 2002/01/09 17:24:08 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
//  A parameterisation that describes a series of boxes along Z
//    The boxes have equal width, & their lengths are a linear equation.
//    They are spaced an equal distance apart, starting from given location.
//
// by Ivana Hrivnacova, 21.4.2002


#ifndef StMCChamberParameterisation_H
#define StMCChamberParameterisation_H

#include <TObject.h>

class StMCChamberParameterisation : public TObject
{ 
  public:  
    StMCChamberParameterisation(Int_t    NoChambers, 
                                Double_t startZ, 
                                Double_t spacing,
                                Double_t widthChamber, 
                                Double_t lengthInitial,
                                Double_t lengthFinal );

    virtual ~StMCChamberParameterisation();
   
    // methods
    void ComputeTransformation (Int_t copyNo, Double_t* position) const;    
    void ComputeDimensions (Int_t copyNo, Double_t* dimension) const;


  private:
    Int_t     fNoChambers;   
    Double_t  fStartZ;       //  Z of center of first 
    Double_t  fHalfWidth;    //  The half-width of each tracker chamber
    Double_t  fSpacing;      //  The distance between the chambers' center
    Double_t  fHalfLengthFirst;  //  The first half-length 
    Double_t  fHalfLengthIncr;   //  The Increment for the half-length 
    
  ClassDef(StMCChamberParameterisation,1) //StMCChamberParameterisation  
};

#endif


