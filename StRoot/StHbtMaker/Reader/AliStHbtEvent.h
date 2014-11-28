// any problems, send an email to chajecki@mps.ohio-state.edu
#ifndef ALICESTHBTEVENT_H
#define ALICESTHBTEVENT_H
#include "TObject.h"
#include "TClonesArray.h"

class AliStHbtTrack;

class AliStHbtEvent : public TObject {
 public:

  AliStHbtEvent(); 
  ~AliStHbtEvent() {}
  
  void SetVertex(float vx, float vy, float vz) {mVertexX = vx; mVertexY = vy; mVertexZ = vz;}
  float GetVertexX () {return mVertexX;}
  float GetVertexY () {return mVertexY;}
  float GetVertexZ () {return mVertexZ;}
  
  int   GetRunNumber();
  void  SetRunNumber(int number);

  int   GetEventNumber();
  void  SetEventNumber(int number);
  
  int   GetMultiplicity() {return mRefMult;}
  void  SetMultiplicity(int mult) {mRefMult = mult;}
  
  float GetMagField() {return mMagField;}
  void  SetMagField(float field) {mMagField = field;}

  int  GetTrigger() { return mTrigger;}
  void SetTrigger(int trg) { mTrigger = trg;}

  float GetZdcNEnergy () { return mZdcNEnergy;}
  void  SetZdcNEnergy (float en) {mZdcNEnergy = en;}

  float GetZdcPEnergy () { return mZdcPEnergy;}
  void  SetZdcPEnergy (float en) {mZdcPEnergy = en;}

  float GetZdcGammaEnergy () { return mZdcGammaEnergy;}
  void  SetZdcGammaEnergy (float en) {mZdcGammaEnergy = en;}

  int   GetZdcParticipants () { return mZdcParticipants;}
  void  SetZdcParticipants (int part) {mZdcParticipants = part;}


  void Clear(const Option_t* option);  

  AliStHbtTrack* AddTrack();
  
  TClonesArray* Tracks() {return fTracks;}
  
 private:
  
  int mRunNumber;            // run number
  int mEventNumber;          // event number
  float mVertexX;            // x comp. of vertex position
  float mVertexY;            // y comp. of vertex position
  float mVertexZ;            // z comp. of vertex position
  float   mMagField;         // magnetic field
  unsigned short mRefMult;   // Tpc multiplicity
  TClonesArray *fTracks;     // Array of Track objects
  unsigned short mNTracks;   // Number of Track's in event
  
  int   mTrigger;            // triger word
  float mZdcNEnergy;         // energy of neutrons deposited in ZDC
  float mZdcPEnergy;         // energy of protons deposited in ZDC
  float mZdcGammaEnergy;     // energy of gammas deposited in ZDC
  int   mZdcParticipants;    // zdc participants

  ClassDef(AliStHbtEvent,1)
};

#endif
