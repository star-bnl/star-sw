#ifndef StPicoBbcFiller_h
#define StPicoBbcFiller_h

/*
   this makes a TClonesArray of StPicoBbcTile objects
   that make up the BBC detector.  The tile
   objects for these two detectors are the same, hence
   the name.

   One can either make one collection of all of these tiles,
   or one can make two collections where one is the BBC and
   the other is the EPD.  Here, I make the former choice.
   Dmitri, you can choose whichever you like.  It doesn't
   matter, since each tile "knows" what detector it belongs
   to, and can be asked to identify through isEpd() and
   isBbc().
*/


class StMuDst;
class StPicoDst;


class StPicoBbcFiller
{
public:
  StPicoBbcFiller(StPicoDst& picoDst, int year = 2017);

  void Fill(const StMuDst& muDst);

private:

  StPicoDst&  mPicoDst;

};

#endif

