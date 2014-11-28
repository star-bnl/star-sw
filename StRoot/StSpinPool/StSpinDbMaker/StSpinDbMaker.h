// $Id: StSpinDbMaker.h,v 1.8 2014/08/06 11:43:40 jeromel Exp $

/*! \class StSpinDbMaker 
\author Jan Balewski
*/  

#ifndef STAR_StspinDbMaker
#define STAR_StspinDbMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StSpinPool/StSpinDbMaker/cstructs/spinConstDB.hh"

// needed DB c-structs  
class spinDbV124_st;
class spinDbStar_st;
class spinDbBXmask_st;

class StSpinDbMaker : public StMaker {
  private:
  spinDbV124_st   *mTabSpinV124;
  spinDbStar_st   *mTabSpinStar;
  spinDbBXmask_st *mTabSpinBXmask;

  // static Char_t  m_VersionCVS = "$Id: StSpinDbMaker.h,v 1.8 2014/08/06 11:43:40 jeromel Exp $";

  void clearTables(); /// clear local lookup tables
  void requestDataBase(); /// reads tables from STAR-DB
  void optimizeTables(); /// produces local lookup tables
  void auxilairyVariables();

  int mNFound; /// # of good tables found in DB
  TString mDbName; ///name of the DB used
  int mDbDate; /// yyyymmdd
  int mNfilledBunches[SPINDbMaxRing]; /// calculated based on DB tables
  TString mCADpolPattern; /// calculated based on DB tables (if defined by CAD)

  template <class St_T, class T_st> void getTable(TDataSet *mydb,  TString tabName,  T_st **outTab); /// generic method for uploading any spinDb table from STAr DB

  void setDBname(TString name){ mDbName=name;}
  int  numberOfFilledBunches(enum spinDbEnum);

 public:  
  bool isPolDir(enum spinDbEnum);  /// defined at cstructs/spinConstDB.hh
  int  spin8bits[SPINDbMaxBXings]; /// vs. STAR==yellow bXing
  int  spin4bits[SPINDbMaxBXings]; /// vs. STAR==yellow bXing

  ///Note, <b> -1 or false </b> is returned if input is invalid.
  void print(int level=0);/// dump spinDb content for current time stamp

  bool isValid(){ return mNFound==3;} /// true if all needed DB tables were found
  bool isPolDirTrans(){return isPolDir(polDirTrans);} /// Returns true if beams are transversely polarized, false otherwise  
  bool isPolDirLong(){return isPolDir(polDirLong);} /// Returns true if beams are longitudinally polarized, false otherwise 

  int   spin8usingBX48(int bx48); /// 8bit spin information
  int   spin4usingBX48(int bx48); /// 4bit spin information
  int   BXyellowUsingBX48(int bx48); /// bXing at STAR IP, [0,119]

  int   BX48offset(); /// STAR==yellow bXing=(bx48+48off)%120
  bool  isBXfilledUsingBX48(int bx48);
  bool  isMaskedUsingBX48(int bx48); // returns true _also_ if DB is empty

  int   spin8usingBX7(int bx7); /// 8bit spin information
  int   spin4usingBX7(int bx7); /// 4bit spin information
  int   BXyellowUsingBX7(int bx7); /// bXing at STAR IP, [0,119]
  int   BX7offset(); /// STAR==yellow bXing=(bx7+7off)%120
  bool  isBXfilledUsingBX7(int bx7);

  int   offsetBX48minusBX7(int bx48, int bx7); ///should be zero for every run
  bool  isBXfilledUsingBXyellow(int bxStar);
  bool  isBXmaskedUsingBXyellow(int bxStar);
  //BXyellowUsingBX7() is deprecaited, use BXstar.. instead,JB
  int   numberOfFilledBunchesBlue() { return numberOfFilledBunches(blueRing); }

  int   numberOfFilledBunchesYellow(){ return numberOfFilledBunches(yellRing);}
  TString cadPolPattern() { return  mCADpolPattern; } /// defined only for 2005 run by CAD , based on first 4 filled bunches in both rings. Note, those pairs do NOT collide at STAR with each other.

  // added in 2009, Jan B.
  bool isBXfilledUsingInternalBX(int bx); // w/o any bXing offsets added, only cogging included
  const char *getV124comment();
  int   BXstarUsingBX48(int bx48); /// bXing at STAR IP, [0,119]
  int   BXstarUsingBX7(int bx7); /// bXing at STAR IP, [0,119]

 
  const unsigned char *getRawV124bits();  /// experts only
  const int  *getBucketOffsets();  /// experts only
  const int  *getSpin8bits();  /// experts only

  StSpinDbMaker(const char *name="SpinDbMaker");

  virtual       ~StSpinDbMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun  (int runumber); ///< to access STAR-DB
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSpinDbMaker.h,v 1.8 2014/08/06 11:43:40 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StSpinDbMaker,0)   

 };

#endif

// $Log: StSpinDbMaker.h,v $
// Revision 1.8  2014/08/06 11:43:40  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.7  2009/09/26 20:34:50  balewski
// additional methods added for 2009 data processing,
// since allignment of STAR bXing changed from yellow beam (2005) to blue (2009) the names of some methods were adjusted
//
// Revision 1.6  2006/10/24 20:19:37  balewski
// cleanup: - spin4 for abort gaps, drop STARbXing
//
// Revision 1.5  2006/06/08 00:37:02  balewski
// wrong  ifdef was used
//
// Revision 1.4  2006/01/05 18:21:24  balewski
// added get: cadPollPatt, nFillBunch
// changed BXstar --> BXyellow
//
// Revision 1.3  2005/10/05 13:41:47  balewski
// more get-methods
//
// Revision 1.2  2005/10/03 20:40:17  balewski
// clenup
//
// Revision 1.1  2005/09/30 23:47:46  balewski
// start
//
