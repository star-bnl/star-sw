#ifndef StPicoBbcFiller_h
#define StPicoBbcFiller_h

class StMuDst;
class StPicoDst;


/**
 * This class fills a TClonesArray of StPicoBbcTile objects that make up the BBC
 * detector. The tile objects for the BBC and EPD detectors are the same, hence
 * the name.
 */
class StPicoBbcFiller
{
public:

  StPicoBbcFiller(StPicoDst& picoDst, int year = 2017);

  void fill(const StMuDst& muDst);

private:

  StPicoDst&  mPicoDst;

};

#endif

