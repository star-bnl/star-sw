/***************************************************************************
 *
 * $Id: StMuProbPidTraits.h,v 1.2 2003/02/21 14:32:47 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

/** @class StMuProbPidTraits
 * Class to hold the new Yuri Fisyak pid probabilities.
 * Possible particles are:
 * 0 Electron
 * 1 Proton
 * 2 KaonMinus
 * 3 PionMinus
 * 4 MuonMinus
 * 5 Deuteron
 * 6 Triton
 * 7 He3
 * 8 Alpha
*/

#ifndef StMuProbPidTraits_h
#define StMuProbPidTraits_h

#include "TObject.h"

#define __NPARTICLES__ 9

class StMuProbPidTraits : public TObject {
public:
  StMuProbPidTraits(){/* no-op*/};
  double dEdxFit() const;                      ///< returns the fitted dEdx value
  double dEdxTruncated() const;                ///< returns the truncated 70% dEdx value
  double dEdxTrackLength() const;              ///< returns the track length in TPC used for dE/dx calculations
  double ndf() const;                          ///< returns number of degrees of freedon
  double probability(unsigned int i) const;    ///< returns the probability of the track being of particle type i
  double sum() const;                          ///< returns the sum of all probabilities
  int    numberOfParticles() const;            ///< returns the number of particles avaiable

  void   setdEdxFit(double dedx );             ///< sets the fitted dEdx value;
  void   setdEdxTruncated(double dedx);        ///< sets the truncated dEdx value;
  void   setdEdxTrackLength(double dedx );     ///< sets the track length in TPC used for dE/dx calculations
  void   setNdf(unsigned int);                 ///< set number of degrees of freedom
  void   setProbability(unsigned int, double); ///< set the probability for particle i

private:
  unsigned char mNDF;
  float mdEdxFit;
  float mdEdxTruncated;
  float mdEdxTrackLength;
  float mProbabilities[__NPARTICLES__];      ///< array holding the pid probabilities

  ClassDef(StMuProbPidTraits,2)
};

inline double StMuProbPidTraits::dEdxFit() const {return mdEdxFit;}
inline double StMuProbPidTraits::dEdxTruncated() const { return mdEdxTruncated;}
inline double StMuProbPidTraits::dEdxTrackLength() const {return mdEdxTrackLength;}
inline double StMuProbPidTraits::ndf() const  { return mNDF;}
inline double StMuProbPidTraits::probability(unsigned int i) const { return (i<__NPARTICLES__) ? mProbabilities[i] : 0;}  
inline double StMuProbPidTraits::sum() const { double s=0; for (int i=0;i<__NPARTICLES__;i++) s += mProbabilities[i]; return s;}
inline int    StMuProbPidTraits::numberOfParticles() const { return __NPARTICLES__; }

inline void   StMuProbPidTraits::setdEdxFit(double dEdx ) { mdEdxFit = dEdx; }
inline void   StMuProbPidTraits::setdEdxTruncated(double dEdx) { mdEdxTruncated = dEdx; }
inline void   StMuProbPidTraits::setdEdxTrackLength(double dEdxTrackLength) { mdEdxTrackLength = dEdxTrackLength; }
inline void   StMuProbPidTraits::setProbability(unsigned int i, double prob) { if (i<__NPARTICLES__) mProbabilities[i]=prob;}
inline void   StMuProbPidTraits::setNdf(unsigned int i) { mNDF = i; }



#endif

/***************************************************************************
 *
 * $Log: StMuProbPidTraits.h,v $
 * Revision 1.2  2003/02/21 14:32:47  laue
 * Yuri's updates to the PID probabilities. dE/dx track length in TPC added
 *
 * Revision 1.1  2002/11/18 20:29:01  laue
 * New class. Wrapper for Yuri's new StProbPidTraits.
 *
 *
 **************************************************************************/
