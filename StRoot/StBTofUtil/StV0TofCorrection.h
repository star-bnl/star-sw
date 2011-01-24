// $Id: StV0TofCorrection.h,v 1.1 2011/01/24 21:01:49 geurts Exp $
/*! \file StV0TofCorrection.h
    \brief header file of StV0TofCorrection class created by Patrick Huck (phuck@lbl.gov) [12/13/2010]

    A detailed class description and example analysis results can be found at 
    http://drupal.star.bnl.gov/STAR/system/files/stv0tofcorrection_v3.pdf
*/
// ---
// $Log: StV0TofCorrection.h,v $
// Revision 1.1  2011/01/24 21:01:49  geurts
// *** empty log message ***
//
// ---
#ifndef STV0TOFCORRECTION_H
#define STV0TOFCORRECTION_H

#include <stdio.h>
#include <stdarg.h>

#include <TROOT.h>
#include "TMath.h"
#include "StPhysicalHelixD.hh"
#include "StarClassLibrary/StLorentzVectorD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include <iostream> 
#include "TVirtualStreamerInfo.h"

#include <vector>

//! template class defining an inline container 
/*! used to store 3d-vectors and mother tracks */ 
template < class T, class container = std::vector<T> > class StInlineContainer : public container {
 private:
  Int_t NrArgs; //!< number of arguments
  StInlineContainer<T>* pContainer; //!< pointer to inline container
  
 public:
  StInlineContainer() { NrArgs = 0; pContainer = NULL; }
  virtual ~StInlineContainer() {}
  explicit StInlineContainer(const T vec) : container(1,vec) { } //!< explicit constructor
  //! operater () overloaded for filling container
  StInlineContainer<T> operator()(const T vec) {
    NrArgs++;
    pContainer->setNrArgs(NrArgs);
    pContainer->push_back(vec);
    return *pContainer;
  }
  //! setter for number of arguments
  void  setNrArgs(Int_t c) { NrArgs = c; }
  //! getter for number of arguments
  Int_t getNrArgs()        { return NrArgs; }
  //! set pointer to container
  void  setPointer2Container(StInlineContainer<T>* p) { pContainer = p; }
};

//! A class providing tools to correct the time of flight of V0 particles
/*! detailed class description see http://drupal.star.bnl.gov/STAR/system/files/stv0tofcorrection_v3.pdf */
class StV0TofCorrection {
 private:
  //! inline function for time-of-flight calculation 
  inline Float_t calcTof(StLorentzVectorD trackAB, Float_t PathAB); 
  //! function for calculation of corrected path length
  Float_t calcPathCorr(StPhysicalHelixD helixA, StThreeVectorD vectorAB, StThreeVectorD vectorTof) ;
  //! function for calculation of path length on helix to a space point
  Float_t calcPathOnHelixToPoint(StPhysicalHelixD helixA, StThreeVectorD space_vec) ; 
  //! inline function for input check
  inline Bool_t inputOk();
  //! inline function for m2 cuts
  inline Bool_t cutOnMass2(Float_t Mass2, Int_t pidnr);
  //! set number of decays
  void setNrDecays(Int_t n) { NrDecays = n; } 

  Float_t clight; //!< speed of light in cm/ns
  Bool_t cleared, cleared2; //!< status of containers (cleared?) 

  Int_t NrDecays; //!< number of decays
  StInlineContainer<StThreeVectorD>* Vectors3D; //!< container for 3d-vectors
  StInlineContainer<StLorentzVectorD>* tracks; //!< container for mother tracks

 public:
  StV0TofCorrection() ; //!< constructor
  virtual ~StV0TofCorrection() ; //!< destructor

  //! function for initialization of 3d-vectors container
  StInlineContainer<StThreeVectorD> setVectors3D(const StThreeVectorD vec) {
    if ( cleared ) {
      Vectors3D = new StInlineContainer<StThreeVectorD>;
    }
    else {
      Error("StV0TofCorrection::setVectors3D","Make sure to call clearContainers() function every time a correction of a particle is done! Otherwise you're using the same container over and over again!");
    }
    Vectors3D->setNrArgs(0);
    Vectors3D->setPointer2Container(Vectors3D);
    cleared = kFALSE;
    return (*Vectors3D)(vec);
  }

  //! function for initialization of mother tracks container
  StInlineContainer<StLorentzVectorD> setMotherTracks(const StLorentzVectorD tr) {
    if ( cleared2 ) {
      tracks  = new StInlineContainer<StLorentzVectorD>;
      tracks->setNrArgs(0);
      tracks->setPointer2Container(tracks);
      cleared2 = kFALSE;
      return (*tracks)(tr);
    }
    else {
      Error("StV0TofCorrection::setMotherTracks","Again: Don't forget clearContainers()!");
      return (*tracks);
    }
  }

  //! main function for beta correction
  Bool_t correctBeta(StPhysicalHelixD helixA, Float_t TofA, Float_t& BetaCorr, Float_t MomentumA = -999., Int_t pidnr = -1) ;
 
  //! function for finalization by destroying containers (free allocated memory and set pointers to NULL)
  void clearContainers() {
    delete Vectors3D; delete tracks;
    Vectors3D = NULL; tracks = NULL;
    cleared = kTRUE; cleared2 = kTRUE;
  }

  ClassDef(StV0TofCorrection,1)

};


#endif

