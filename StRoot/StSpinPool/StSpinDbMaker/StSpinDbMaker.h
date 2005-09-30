// $Id: StSpinDbMaker.h,v 1.1 2005/09/30 23:47:46 balewski Exp $

/*! \class StSpinDbMaker 
\author Jan Balewski
*/  

#ifndef STAR_SteemcDbMaker
#define STAR_SteemcDbMaker

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
 public:

  // static Char_t  m_VersionCVS = "$Id: StSpinDbMaker.h,v 1.1 2005/09/30 23:47:46 balewski Exp $";
  void clearTables();///< clear lookup tables
  void requestDataBase(); ///< reads tables from STAR-DB
  void optimizeTables(); ///< produces lookup tables
  
  // pointers to Db tables for each sector
  int nFound;
  TString dbName; //name of the DB used 

  template <class St_T, class T_st> void getTable(TDataSet *mydb,  TString tabName,  T_st **outTab);

  bool isPolDir(enum spinDbEnum);  
  int  spin8bits[SPINDbMaxBXings]; // vs. STAR bXing
  int  spin4bits[SPINDbMaxBXings]; // vs. STAR bXing

 public:  
  //Note, -1 or false is returned if input is invalid.
  void print(int level=0);// dump spinDb content for current time stamp
  void setDBname(TString name){ dbName=name;}

  bool valid(){ return nFound==3;} // true if all DB tables found
  bool isPolDirTrans(){return isPolDir(polDirTrans);}
  bool isPolDirLong(){return isPolDir(polDirLong);}

  int   spin8usingBX48(int bx48); // 8bit spin information
  int   spin4usingBX48(int bx48); // 4bit spin information
  int   BXstarUsingBX48(int bx48); // bXing at STAR IP, [0,119]
  int   BX48offset(); // STAR bXing=(bx48+48off)%120
  bool  isBXfilledUsingBX48(int bx48);

  int   spin8usingBX7(int bx7); // 8bit spin information
  int   spin4usingBX7(int bx7); // 4bit spin information
  int   BXstarUsingBX7(int bx7); // bXing at STAR IP, [0,119]
  int   BX7offset(); // STAR bXing=(bx7+7off)%120
  bool  isBXfilledUsingBX7(int bx7);

  int  offsetBX48minusBX7(int bx48, int bx7); //should be zero for every run
   
  // expert only ....
  const unsigned char *getRawV124bits();
  const int *getBucketOffsets();
  const int  *getSpin8bits();

  StSpinDbMaker(const char *name="SpinDbMaker");

  virtual       ~StSpinDbMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun  (int runumber); ///< to access STAR-DB
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSpinDbMaker.h,v 1.1 2005/09/30 23:47:46 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StSpinDbMaker,0)   

 };

#endif

// $Log: StSpinDbMaker.h,v $
// Revision 1.1  2005/09/30 23:47:46  balewski
// start
//
