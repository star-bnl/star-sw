// $Id: StTpcCalibSector.h,v 1.10 2003/09/02 17:59:11 perev Exp $
// $Log: StTpcCalibSector.h,v $
// Revision 1.10  2003/09/02 17:59:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.9  2003/04/30 20:38:51  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.8  2001/02/17 20:16:09  perev
// add defence against Solaris typedef in iostream
//
// Revision 1.7  1999/11/09 20:36:13  fisyak
// Add protection for rootcint
//
// Revision 1.6  1999/10/11 08:04:12  fretiere
// Fix bugg + add README, LOG and ID
//
///////////////////////////////////////////////////////////////////////////////
#ifndef STAR_StTpcCalibSector
#define STAR_StTpcCalibSector
// Local
class StTpcCalibSetup;
// STAR
//class StTpcDb;
class StTPCReader;
// root
class TH1F;
class TH2F;
class TH2S;


class StTpcCalibSector {
private :
  int mSectorId; //!
  const StTpcCalibSetup* mSetup; //!
  const int* mNumberOfPadAtRow; //! To be replaced by db

  int mNEvt;

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
  TH1S* mHDeadFEE;//!
  TH1S* mHDeadRDO;//!

  TH1F* mHAmp;//!
  TH2F* mHAmpMap;//!
  TH1F* mHT0;//!
  TH2F* mHT0Map; //! 

  TH2F* mHInnerCalibMap;//!
  TH2F* mHOuterCalibMap;//!

public :
  // Called at Init
  StTpcCalibSector(const StTpcCalibSetup* aSetup, 
		   const int aSectorId,  
		   const int* aNumberOfPadAtRow); // To be replaced by db
  void readBadTable(ifstream* aInFile);
  void readDeadTable(ifstream* aInFile);

  // Called at make
  void updateBad   (StTPCReader *aRMSReader);
  void updateDead  (StTPCReader* aZSupReader);
  void updateGain  (StTPCReader* aZSupReader);

  // Called at finish
  void findBad();
  void findDead();
  void calcGainCoeficient();

  void writeBadTable(ofstream* aOutFile);
  void findBadElectronics(int** aPadToFeeConvertor,
			  int** aPadtoRDOConvertor);

  void writeDeadTable(ofstream* aOutFile);
  void findDeadElectronics(int** aPadToFeeConvertor,
			  int** aPadtoRDOConvertor);

  void writeCalibCoefTable(ofstream* aOutFile);

  void writeBadHisto();
  void writeDeadHisto();
  void writeGainHisto();
  void writeAllHisto();

  // Additional
  virtual ~StTpcCalibSector();

  ClassDef(StTpcCalibSector, 1) //
};

#endif
