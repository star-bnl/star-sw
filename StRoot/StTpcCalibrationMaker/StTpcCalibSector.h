#ifndef STAR_StTpcCalibSector
#define STAR_StTpcCalibSector
// Local
class StTpcCalibSetup;
// STAR
class StTpcDb;
class StTPCReader;
// root
class TH1F;
class TH2F;
class TH2S;

class StTpcCalibSector {
private :
  int mSectorId; 
  const StTpcCalibSetup* mSetup; //!
  int* mNumberOfPadAtRow; //!

  TH1F* mHCorrupted;//!
  TH2F* mHCorruptedMap;//!
  TH1F* mHMeanRMS;//!
  TH2S* mHBadMap;//!
  TH1S* mHBadFEE;//!
  TH1S* mHBadRDO;//!

  TH1F* mHNSequence;//!
  TH2F* mHNSequenceMap;//!
  TH2F* mHFoundMap;//!
  TH2F* mHDeadMap;//!

  TH1F* mHAmp;//!
  TH2F* mHAmpMap;//!
  TH1F* mHT0;//!
  TH2F* mHT0Map; //! 

  TH2F* mHInnerCalibMap;//!
  TH2F* mHOuterCalibMap;//!

public :
  StTpcCalibSector(const StTpcCalibSetup* aSetup, 
		const int aSectorId,  int* aNumberOfPadAtRow);

  ~StTpcCalibSector();
  // Call at make
  void updateBad   (StTPCReader *aRMSReader);
  void updateDead  (StTPCReader* aZSupReader);
  void updateGain  (StTPCReader* aZSupReader);
  // Call at finish
  void findBad();
  void findDead();
  void calcGainCoeficient();


  void writeBadTable(ofstream* aOutFile);
  void findBadElectronics(int** aPadToFeeConvertor,
			  int** aPadtoRDOConvertor);
  void readBadTable(ifstream* aInFile);

  void writeDeadTable(ofstream* aOutFile);
  void readDeadTable(ifstream* aInFile);

  void writeCalibCoefTable(ofstream* aOutFile);

  void writeBadHisto();
  void writeDeadHisto();
  void writeGainHisto();
  void writeAllHisto();

  ClassDef(StTpcCalibSector, 1) //
};

#endif
