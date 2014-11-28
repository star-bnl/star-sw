#ifndef __StarParticleStack_h__
#define __StarParticleStack_h__

#include "TObjArray.h"
#include "StarCallf77.h"
#include "TVirtualMCStack.h"
#include "TDataSet.h"
#include "TMCProcess.h"
#include "TParticle.h"
#include "TClonesArray.h"
#include <vector>
#include <list>


/**
   \class StarParticleStack
   \author Jason C. Webb
   \brief Implementation of the VMC particle stack for use in STAR

*/

class StarGenParticle;

class StarParticleStack : public TVirtualMCStack
{
 public:

  StarParticleStack( const Char_t *name = "stack" );
  ~StarParticleStack();

  ///
  /// ROOT/VMC method to create a new particle and push into stack
  ///
  /// @param toBeDone   - 1 if particles should go to tracking, 0 otherwise
  /// @param parent     - number of the parent track, -1 if track is primary
  /// @param pdg        - PDG encoding
  /// @param px, py, pz - particle momentum [GeV/c]
  /// @param e          - total energy [GeV]
  /// @param vx, vy, vz - position [cm]
  /// @param tof        - time of flight [s]
  /// @param polx, poly, polz - polarization
  /// @param mech       - creator process VMC code
  /// @param ntr        - track number (is filled by the stack
  /// @param weight     - particle weight
  /// @param is         - generation status code  
  virtual void  PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
			  Double_t px, Double_t py, Double_t pz, Double_t e,
			  Double_t vx, Double_t vy, Double_t vz, Double_t tof,
			  Double_t polx, Double_t poly, Double_t polz,
			  TMCProcess mech, Int_t& ntr, Double_t weight,
			  Int_t is);



  /// The stack has to provide two pop mechanisms:
  /// The first pop mechanism required.
  /// Pop all particles with toBeDone = 1, both primaries and seconadies.
  /// @param itrack is the index of the track in the array of all particles
  virtual TParticle* PopNextTrack(Int_t& itrack);
  
  /// The second pop mechanism required.
  /// Pop only primary particles with toBeDone = 1, stacking of secondaries
  /// is done by MC
  /// @param i is the index of the track to be "popped".  Note that "pop" here is a misnomer... no stack is being popped... we are only accessing the track with index i.  Nor is there really any distinction between "primary" and "secondary" here...  So I'd like to see Alice's actual implementation of this rather than their poorly documented examples.  Looks like they use this just to index a track.
  virtual TParticle* PopPrimaryForTracking(Int_t i);

  /// Set the current track number
  virtual void       SetCurrentTrack(Int_t trackNumber);
  
  /// Total number of tracks
  virtual Int_t      GetNtrack()    const;
  
  /// Total number of primary tracks
  virtual Int_t      GetNprimary()  const { return mNumPrimary; }
  
  /// Current track particle
  virtual TParticle* GetCurrentTrack() const;

  /// Current track number
  virtual Int_t      GetCurrentTrackNumber() const;
  
  /// Number of the parent of the current track
  virtual Int_t      GetCurrentParentTrackNumber() const;

  /// Retrieve the ith particle in the array
  virtual TParticle *GetParticle( const Int_t i ) const;

  /// Clear the stack
  virtual void Clear( const Option_t *opts="" );

 private:
 protected:

  Int_t               mNumPrimary;
  Int_t               mCurrent;

  Int_t               mArraySize;
  TClonesArray       *mArray; 

  Int_t               mStackSize;
  std::list  <TParticle *> mStack;
  std::list  <Int_t>       mStackIdx;


  ClassDef(StarParticleStack,1);
    
};

#endif
