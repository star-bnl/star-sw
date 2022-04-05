// any problems, send an email to chajecki@mps.ohio-state.edu
#ifndef ALICESTHBTTRACK_H
#define ALICESTHBTTRACK_H
#include "TObject.h"
#include "TClonesArray.h"

class AliStHbtTrack : public TObject {

public:

  AliStHbtTrack() {}
  virtual ~AliStHbtTrack() {}

  int  GetCharge() {return mCharge;}
  void SetCharge(int charge) {mCharge = charge;}

  int  GetNTpcHits() {return mNTpcHits;}
  void SetNTpcHits(int nHits) {mNTpcHits = nHits;}

  void SetP(double Px, double Py, double Pz) { fPx = Px; fPy = Py; fPz = Pz; }
  double GetPx() {return fPx;}
  double GetPy() {return fPy;}
  double GetPz() {return fPz;}

  void SetTrackId (int id) { mTrackId = id;}
  int  GetTrackId () { return mTrackId;}
  
  void SetPidProbElectron (float pid) { mPidProbElectron = pid;}
  float GetPidProbElectron () { return mPidProbElectron; }

  void SetPidProbPion (float pid) { mPidProbPion = pid;}
  float GetPidProbPion () { return mPidProbPion; }

  void SetPidProbKaon (float pid) { mPidProbKaon = pid;}
  float GetPidProbKaon () { return mPidProbKaon; }

  void SetPidProbProton (float pid) { mPidProbProton = pid;}
  float GetPidProbProton () { return mPidProbProton; }

  void SetVertex(float vx, float vy, float vz) {mVertexX = vx; mVertexY = vy; mVertexZ = vz;}
  float GetVertexX () {return mVertexX;}
  float GetVertexY () {return mVertexY;}
  float GetVertexZ () {return mVertexZ;}

  void SetdEdx (float dedx) { mdEdx = dedx; }
  float GetdEdx () { return mdEdx;}

  void SetImpactParameters(float xy, float z) { mImpactParameterXY = xy; mImpactParameterZ = z;}
  float GetImpactParameterXY() { return mImpactParameterXY;}
  float GetImpactParameterZ() { return mImpactParameterZ;}

  unsigned int GetTopologyMap(int word) { return mMap[word];}
  void SetTopologyMap(int word, unsigned int map) { mMap[word]=map;}

private:

  double fPx;                  // x component of track momentum
  double fPy;                  // y component of track momentum
  double fPz;                  // z component of track momentum
  char mCharge;                // charge of the track
  unsigned int  mMap[6];       // topology map
  int mTrackId;                // track id

  float mPidProbElectron;      // pid probabilities
  float mPidProbPion;          //
  float mPidProbKaon;          //
  float mPidProbProton;        //

  float mVertexX;              // x component of track vertex
  float mVertexY;              // y component of track vertex
  float mVertexZ;              // z component of track vertex

  float mdEdx;                 // dEdx of track
  int mNTpcHits;               // # of hits in tpc
  float mImpactParameterXY;     // x comp. of impact parameter 
  float mImpactParameterZ;     // y comp. of impact parameter

  ClassDef(AliStHbtTrack,1)
};


#endif
