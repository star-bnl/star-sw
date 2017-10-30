//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifndef KFMCTRACK_H
#define KFMCTRACK_H

#include <iostream>
#include <vector>
#include <cmath>
using std::ostream;
using std::istream;


/**
 * @class KFMCTrack
 * store MC track information for Performance
 */
class KFMCTrack
{
 public:
  KFMCTrack():fMotherId(-1),fPDG(0),fNMCPoints(0),fIsReconstructed(0),fIsOutOfDetector(0) {};

  int MotherId()          const { return fMotherId; }
  int    PDG()            const { return fPDG;}
  float Par( int i )      const { return fPar[i]; }
  float X()               const { return fPar[0]; }
  float Y()               const { return fPar[1]; }
  float Z()               const { return fPar[2]; }
  float L()               const { return sqrt(X()*X() + Y()*Y() + Z()*Z()); }
  float Px()              const { return fPar[3]; }
  float Py()              const { return fPar[4]; }
  float Pz()              const { return fPar[5]; }
  float P()               const { return sqrt(fPar[3]*fPar[3] + fPar[4]*fPar[4] + fPar[5]*fPar[5]); }
  float Pt()              const { return sqrt(fPar[3]*fPar[3] + fPar[4]*fPar[4]); }
  const float *Par()      const { return fPar; }
  int   NMCPoints()       const { return fNMCPoints; }
  int   NMCPixelPoints()  const { return fNMCPixelPoints; }
  bool  IsReconstructed() const { return fIsReconstructed; }
  bool  IsOutOfDetector() const { return fIsOutOfDetector; }
  
  void SetPar( int i, float v )  { fPar[i] = v; }
  void SetX( float v )           { fPar[0] = v; }
  void SetY( float v )           { fPar[1] = v; }
  void SetZ( float v )           { fPar[2] = v; }
  void SetPx( float v )          { fPar[3] = v; }
  void SetPy( float v )          { fPar[4] = v; }
  void SetPz( float v )          { fPar[5] = v; }
  void SetQP( float v )          { fPar[6] = v; }
  void SetMotherId( int v )      { fMotherId = v; }
  void SetPDG( int v )           { fPDG = v; }
  void SetNMCPoints( int v )     { fNMCPoints = v; }
  void SetNMCPixelPoints( int v ){ fNMCPixelPoints = v; }
  void SetReconstructed()        { fIsReconstructed = 1; }
  void SetNotReconstructed()        { fIsReconstructed = 0; }
  void SetOutOfDetector() { fIsOutOfDetector = 1; }
  
 protected:

  int   fMotherId;      //* index of mother track in tracks array. -1 for primary tracks. -2 if a mother track is not in the acceptance
  int   fPDG;           //* particle pdg code
  float fPar[7];        //* x,y,z,ex,ey,ez,q/p
  int   fNMCPoints;     //* N MC points
  int   fNMCPixelPoints;  
  
  bool fIsReconstructed;
  bool fIsOutOfDetector;
};

#endif
