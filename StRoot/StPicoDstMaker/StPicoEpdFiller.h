#ifndef StPicoEpdFiller_h
#define StPicoEpdFiller_h

/*
   this makes a TClonesArray of StPicoEpdTile objects
   that make up the BBC and EPD detectors.  The tile
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


class StPicoEpdFiller
{
public:
  StPicoEpdFiller(StPicoDst& picoDst, int year = 2017);

  void Fill(const StMuDst& muDst);

private:

  StPicoDst&  mPicoDst;

  struct EPDAnalysisMap {
    Short_t qt_board_address; // channel number used in QT board or other physical numbering scheme 0x10...
    Short_t qt_channel_ADC; // QT board channel used 0....31
    Short_t qt_channel_TAC; // QT board channel used 0....31
  };

  EPDAnalysisMap mEPDMap[2][12][31];

  void SetDefaultMapping_30may2017();

};

#endif

