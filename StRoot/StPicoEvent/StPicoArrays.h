/**
 * \class StPicoArray
 * \brief Stores pico arrays
 *
 * StPicoArrays is a pure C++ class that holds names of the pico arrays,
 * names of TBrances and TClones arrays
 */

#ifndef StPicoArrays_h
#define StPicoArrays_h

//_________________
class StPicoArrays {

 public:
  /// Default constructor
  StPicoArrays();

  /// Should be changed to constexpr once ROOT 6 is available at STAR
  enum { NAllPicoArrays = 18};

  /// Names of the TBranches in the TTree/File
  static const char* picoArrayNames[NAllPicoArrays];

  /// Names of the classes, the TClonesArrays are arrays of this type
  static const char* picoArrayTypes[NAllPicoArrays];

  /// Maximum sizes of the TClonesArrays
  static int picoArraySizes[NAllPicoArrays];

  /// Array names
  enum TypeIndex { Event=0, Track, EmcTrigger, MtdTrigger,
		   BTowHit, BTofHit, MtdHit, BbcHit, EpdHit, FmsHit,
		   BEmcPidTraits, BTofPidTraits, MtdPidTraits, TrackCovMatrix,
                   BEmcSmdEHit, BEmcSmdPHit, ETofHit, ETofPidTraits };
};

#endif
