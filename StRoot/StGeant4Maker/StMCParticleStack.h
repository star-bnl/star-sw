#ifndef __StMCParticleStack_h__
#define __StMCParticleStack_h__

#include "TObjArray.h"
#include "StarCallf77.h"
#include "TVirtualMCStack.h"
#include "TDataSet.h"
#include "TMCProcess.h"
#include "TParticle.h"
#include "TClonesArray.h"
#include <vector>
#include <list>
#include <map>
#include <iostream>

class DetectorHit;


/**
   \class StMCParticleStack
   \author Jason C. Webb
   \brief Implementation of the VMC particle stack for use in STAR

*/

class StarMCParticle;
class StarMCVertex;

class StarMCParticle {

  // TODO: add primary, secondary flags

public:
  StarMCParticle( TParticle* particle=0, StarMCVertex* start=0 );
//  StarMCParticle( const StarMCParticle& );
//  StarMCParticle( const StarMCParticle&& ) = default; 
 ~StarMCParticle(){ /* nada */ };

  const TParticle* particle() const { return mStacked; }

  double px() const { assert(mStacked); return mStacked->Px(); }
  double py() const { assert(mStacked); return mStacked->Py(); }
  double pz() const { assert(mStacked); return mStacked->Pz(); }
  double pt() const { return sqrt( px()*px()+py()*py() ); }
  double E()  const { assert(mStacked); return mStacked->Energy(); }

  double vx() const;
  double vy() const;
  double vz() const;

  const char* GetName() const { assert(mStacked); return mStacked->GetName(); }
  const int   GetPdg()  const { assert(mStacked); return mStacked->GetPdgCode(); }
  const int   GetStatus() const { assert(mStacked); return mStacked->GetStatusCode(); }

  const StarMCVertex* start() const { return mStartVertex; }
  const StarMCVertex* stop()  const { return mStopVertex; }

  void  setStartVertex( StarMCVertex* v ){ mStartVertex = v; }
  void  addIntermediateVertex( StarMCVertex* v ){ mIntermediateVertices.push_back(v); }
  void  setStopVertex( StarMCVertex* v ){ mStopVertex = v; }

  std::vector<StarMCVertex*> intermediate() const { return mIntermediateVertices; }

  void setIdStack( int id ) { mIdStack = id; } 
  int     idStack() const { return mIdStack; } 

  //const long long numberOfHits(){ return mNumHits; } 
  //void  addHit(){ mNumHits++; } 

  const long long numberOfHits(){ return mHits.size(); } 
  void  addHit( DetectorHit* hit );//{ mHits.push_back(hit); } 

  std::vector<DetectorHit*>& hits(){ return mHits; }
  

private:
protected:

  TParticle*                 mStacked;              /// Pointer to the stacked particle
  StarMCVertex*              mStartVertex;          /// Pointer to the start vertex
  std::vector<StarMCVertex*> mIntermediateVertices; /// Pointer to the intermediate vertex
  StarMCVertex*              mStopVertex;           /// Pointer to the stop vertex  

  int                        mIdStack;              /// Track identity on stack 
  long long                  mNumHits;              /// Number of hits registered on the track 

  std::vector<DetectorHit*>  mHits;                 /// List of hits on track

};

class StarMCVertex {
public:
  StarMCVertex();
  StarMCVertex( double vx, double vy, double vz, double vt, StarMCParticle* parent=0 );
//  StarMCVertex( const StarMCVertex& );
//  StarMCVertex( const StarMCVertex&& ) = default;
 ~StarMCVertex(){ /* nada */ };

  double distance( double vx_, double vy_, double vz_ ){ 
    double vx2 = (vx_-mVertex[0])*(vx_-mVertex[0]);
    double vy2 = (vy_-mVertex[1])*(vy_-mVertex[1]);
    double vz2 = (vz_-mVertex[2])*(vz_-mVertex[2]);
    return sqrt(vx2+vy2+vz2);
  };

  double vx() const { return mVertex[0]; }
  double vy() const { return mVertex[1]; }
  double vz() const { return mVertex[2]; }
  double tof() const { return mVertex[3]; }

  void setParent  ( StarMCParticle* _parent   ){ mParent = _parent; } 
  void addDaughter( StarMCParticle* daughter ){ mDaughters.push_back( daughter ); } 

  const             StarMCParticle*   parent()   { return mParent; } 
  const std::vector<StarMCParticle*>& daughters(){ return mDaughters; } 

  void setMedium( const int medium ) { mMedium = medium; }
  int  medium() const { return mMedium; }

  void setProcess( const TMCProcess p ) { mMechanism=p; }
  TMCProcess process() const { return mMechanism; }

  void setVolume( const char* name ){ mVolume = name; }
  std::string volume(){ return mVolume; }

  void setIntermediate( bool stat=true ){ mIntermediate=true; }
  bool intermediate(){ return mIntermediate; }

private:
protected:

  double                       mVertex[4];  /// Position of the vertex
  StarMCParticle*              mParent;     /// Parent particle 
  std::vector<StarMCParticle*> mDaughters;  /// Decay daughters / interaction products
  TMCProcess                   mMechanism;  /// Creation mechanism
  int                          mMedium;     /// Medium ID
  std::string                  mVolume;     /// Name of the volume
  bool                         mIntermediate; /// Vertex is an intermediate vtx

};

std::ostream&  operator<<(std::ostream& os,  const StarMCParticle& p);
std::ostream&  operator<<(std::ostream& os,  const      TParticle& p);
std::ostream&  operator<<(std::ostream& os,  const StarMCVertex&   v);

class StMCParticleStack : public TVirtualMCStack
{
 public:

  StMCParticleStack( const Char_t *name = "stack" );
  ~StMCParticleStack();


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
  virtual void  PushTrack(int toBeDone, int parent, int pdg,
			  double px, double py, double pz, double e,
			  double vx, double vy, double vz, double tof,
			  double polx, double poly, double polz,
			  TMCProcess mech, int& ntr, double weight,
			  int is);



  /// The stack has to provide two pop mechanisms:
  /// The first pop mechanism required.
  /// Pop all particles with toBeDone = 1, both primaries and seconadies.
  /// @param itrack is the index of the track in the array of all particles
  virtual TParticle* PopNextTrack(int& itrack);
  
  /// The second pop mechanism required.
  /// Pop only primary particles with toBeDone = 1, stacking of secondaries
  /// is done by MC
  /// @param i is the index of the track to be "popped".  Note that "pop" here is a misnomer... no stack is being popped... we are only accessing the track with index i.  Nor is there really any distinction between "primary" and "secondary" here...  So I'd like to see Alice's actual implementation of this rather than their poorly documented examples.  Looks like they use this just to index a track.
  virtual TParticle* PopPrimaryForTracking(int i);

  /// Set the current track number
  virtual void       SetCurrentTrack(int trackNumber);
  
  /// Total number of tracks
  virtual int      GetNtrack()    const;
  
  /// Total number of primary tracks
  virtual int      GetNprimary()  const { return mNumPrimary; }
  
  /// Current track particle
  virtual TParticle* GetCurrentTrack() const;

  /// Current track number
  virtual int      GetCurrentTrackNumber() const;
  
  /// Number of the parent of the current track
  virtual int      GetCurrentParentTrackNumber() const;

  /// Retrieve the ith particle in the array
  virtual TParticle *GetParticle( const int i ) const;

  /// Clear the stack
  virtual void Clear( const Option_t *opts="" );

  /// Get the current stack size
  virtual int GetStackSize(){ return mStack.size(); }


  /// Obtain the current particle truth
//  const StarMCParticle* GetCurrentTruth() const { return mParticleTable.back(); }

  std::vector<StarMCParticle*>&    GetTruthTable()   { return mTruthTable; }
  std::vector<StarMCParticle*>& GetParticleTable(){ return mParticleTable; }
  std::vector<StarMCVertex*>&   GetVertexTable()  { return mVertexTable; }

  StarMCVertex* GetVertex( double vx, double vy, double vz, double vt, int proc=-1 );

  StarMCParticle* GetPersistentTrack( int stackIndex );

  int GetIdTruth( StarMCParticle* part ){ return mIdTruthFromParticle[part]; }

  void SetScoring( const float rmax, const float zmax, const float emin ){
    mScoringRmax=rmax;
    mScoringZmax=zmax;
    mScoringEmin=emin;
  };

 private:
 protected:

  int                 mNumPrimary;
  int                 mCurrent;

  int                 mArraySize;
  TClonesArray       *mArray; 

  int                      mStackSize;
  std::list  <TParticle *> mStack;
  std::list  <int>         mStackIdx;


  std::vector<StarMCParticle *> mTruthTable;
  std::vector<StarMCParticle *> mParticleTable;
  std::vector<StarMCVertex   *> mVertexTable;

  std::map<int, StarMCParticle*> mStackToTable;

  std::map<StarMCParticle*, int> mIdTruthFromParticle;

  float mScoringRmax;
  float mScoringZmax;
  float mScoringEmin;
  
  //  ClassDef(StMCParticleStack,0);
    
};

#endif
