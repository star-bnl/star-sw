//******************************************************************************
//                                                                            
// StEmcHadDE.h
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/02/16
//
//******************************************************************************
//
// 2001/12/21 - Member functions to calculate track position on an Emc tower
//              were moved to a new class StEmcPosition.
//
// 2001/12/20 - StEmcGeom.h removed.
//
//******************************************************************************

/*! \class StEmcHadDE
\author Marcia M. de Moura

This class applies the hadronic profile on EMC data to estimate the amount of energy 
deposited on EMC due do hadronic tracks

*/

#ifndef StEmcHadDE_H
#define StEmcHadDE_H

#include "TObject.h"

#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

class StTrack;
class StMcTrack;

class StEmcHadDE : public TObject
{
  public:            
             StEmcHadDE(UInt_t=8);                                      ///< Default profile is pion plus
    virtual  ~StEmcHadDE();
    
    Float_t  getDepEnergy(StMcTrack*, Double_t, Int_t, Int_t);          ///< Return deposited energy in one tower for StMcTrack
    Float_t  getDepEnergy(StTrack*, Double_t, Int_t, Int_t);            ///< Return deposited energy in one tower for StTrack
    Float_t  getDepEnergy(Double_t, Double_t, Double_t, Int_t, Int_t);  ///< Return deposited energy in one tower
    Float_t  getDepEnergy(StTrack*, Double_t, Float_t);                 ///< Return deposited energy in one tower
    Float_t  interp3D(Double_t, Double_t, Float_t);                     ///< Interpolation method
    Float_t  getNormDepEnergy(StTrack*, Double_t, Int_t, Int_t);            ///< Return deposited energy in one tower for StTrack

  protected:     
    UInt_t  pNdx, etaNdx, distNdx;
    Float_t pBin, etaBin, distBin;
    Float_t bemcModuleLength;

    Float_t depEnergy[30][10][40];
    Float_t sigmaDepEnergy[30][10][40];
    Float_t normDEfactor(Float_t);

  ClassDef(StEmcHadDE,1)

};
#endif
