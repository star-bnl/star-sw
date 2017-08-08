#ifndef StPicoEpdFiller_h
#define StPicoEpdFiller_h

class StMuDst;
class StPicoDst;


/**
 * This class fills a TClonesArray of StPicoEpdTile objects that make up the BBC
 * detector. The tile objects for the BBC and EPD detectors are the same, hence
 * the name.
 */
class StPicoEpdFiller
{
public:

  StPicoEpdFiller(StPicoDst& picoDst, int year = 2017);

  void fill(const StMuDst& muDst);

private:

  StPicoDst&  mPicoDst;

  struct EPDAnalysisMap
  {
    Short_t qt_board_address; // channel number used in QT board or other physical numbering scheme 0x10...
    Short_t qt_channel_ADC; // QT board channel used 0....31
    Short_t qt_channel_TAC; // QT board channel used 0....31
  };

  EPDAnalysisMap mEPDMap[2][12][31];

  void setDefaultMapping_30may2017();

};

#endif

