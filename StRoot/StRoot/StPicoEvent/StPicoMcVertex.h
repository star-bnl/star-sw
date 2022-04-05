/**
 * \class StPicoMcVertex
 * \brief Holds information about Monte Carlo vertex
 *
 * The class stores information about the reconstructed vertex
 */

#ifndef StPicoMcVertex_h
#define StPicoMcVertex_h

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class StPicoMcVertex : public TObject {

 public:

  /// Default constructor
  StPicoMcVertex();
  /// Copy constructor
  StPicoMcVertex(const StPicoMcVertex &vertex);
  /// Destructor
  virtual ~StPicoMcVertex();
  /// Print MC vertex parameters
  virtual void Print(const Char_t *option = "") const;


  //
  // Getters
  //

  /// Return ID of the vertex
  Int_t id() const                    { return mId; }
  /// Return number of daughters
  Int_t numberOfDaughters() const     { return mNoDaughters; }
  /// Return if intermediate vertex
  Int_t isIntermediate() const        { return mIsInterm; }
  /// ID of the parent track
  Int_t idOfParentTrack() const       { return mIdParTrk; }
  /// Production time
  Float_t time() const                { return mTime; }
  /// Vertex position
  TVector3 position() const           { return TVector3(mVx, mVy, mVz); }
  
  //
  // Setters
  //

  /// Set vertex ID
  void setId(Int_t id)                  { mId = id; }
  /// Set number of daughter tracks
  void setNumberOfDaughters(Int_t ntrk) { mNoDaughters = ntrk; }
  /// Set ID of the parent track
  void setIdOfParentTrack(Int_t id)     { mIdParTrk = id; }
  /// Set flag of the intermediate vertex
  void setIsIntermediate(Int_t interim) { mIsInterm = interim; }
  /// Set GEANT production time (ns)
  void setTime(Float_t time)            { mTime = time; }
  /// Set vertex position
  void setPosition(Float_t x, Float_t y, Float_t z) { mVx = x; mVy = y; mVz = z; }
  
 private:
  /// Unique vertex index
  Int_t   mId;
  /// Number of daughters
  UShort_t mNoDaughters;
  /// ID of the parent track
  Int_t   mIdParTrk;
  /// Is intermediate vertex
  Int_t   mIsInterm;
  /// GEANT vertex production time (ns)
  Float_t mTime;
  /// Vertex X position
  Float_t mVx;
  /// Vertex Y position
  Float_t mVy;
  /// Vertex Z position
  Float_t mVz;

  ClassDef(StPicoMcVertex, 1)
};


#endif // #define StPicoMcVertex_h
