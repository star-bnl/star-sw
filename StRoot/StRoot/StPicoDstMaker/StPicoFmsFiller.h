/**
 * \class StPicoFmsFiller
 * \brief Fills FMS information
 *
 * A helper class that stores FMS information to the PicoDst
 */

#ifndef StPicoFmsFiller_h
#define StPicoFmsFiller_h

// Forward declarations
class StMuDst;
class StPicoDst;
class StFmsDbMaker;

//_________________
class StPicoFmsFiller {

 public:

  /// Constructor
  StPicoFmsFiller(StPicoDst& picoDst);

  /// Fill FMS info
  void fill(const StMuDst& muDst, const StFmsDbMaker* fmsDbMaker = nullptr);

 private:
  
  /// PicoDst address
  StPicoDst&  mPicoDst;

};

#endif

