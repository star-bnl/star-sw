// $Id: StarChamberParametrization.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $
//
//  A parametrization that describes a series of boxes along Z
//   The boxes have equal width, & their lengths are a linear equation.
//   They are spaced an equal distance apart, starting from given location.


#ifndef StarChamberParametrization_H
#define StarChamberParametrization_H

#include <TObject.h>

class StarChamberParametrization : public TObject
{ 
  public:  
    StarChamberParametrization(Int_t    NoChambers, 
                                Double_t startZ, 
                                Double_t spacing,
                                Double_t widthChamber, 
                                Double_t lengthInitial,
                                Double_t lengthFinal );

    virtual ~StarChamberParametrization();
   
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
    
  ClassDef(StarChamberParametrization,1) //StarChamberParametrization  
};

#endif // StarChamberParametrization_H


