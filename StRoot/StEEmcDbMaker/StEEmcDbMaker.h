// $Id: StEEmcDbMaker.h,v 1.20 2004/04/09 18:38:11 balewski Exp $

/*! \class StEEmcDbMaker 
\author Jan Balewski

Interface to STAR-DB. Info from various DB tables is 
correlated for every ADC channel and stored in memory in
 mDbItem[].
mDbItem1[] has single index based on the channel name, 
calculated by EEname2index() utility.

<pre>
----------------------------------------
Usage:


// My Makers  1
  StEEmcDbMaker  *myMk1=new StEEmcDbMaker("EEmcDB");
  myMk1->setTimeStampDay(20031215);  // format: yyyymmdd

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  // My Makers  2 
  StEEfast2slowMaker *myMk= new StEEfast2slowMaker("EE-fast2slow");
  myMk->setDb(myMk1);
 
Example how to use this maker:
www.star.bnl.gov/STAR/eemc -->How To

</pre>
*/ 

#ifndef STAR_SteemcDbMaker
#define STAR_SteemcDbMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

// needed DB c-structs  
class eemcDbADCconf_st;
class eemcDbPMTcal_st;
class eemcDbPMTname_st;
class eemcDbPIXcal_st;
class eemcDbPMTped_st;
class eemcDbPMTstat_st;
class kretDbBlobS_st;


class  EEmcDbItem;
class  EEmcDbCrate;
class  StEEmcDbIndexItem1;//old, drop it

class DbFlavor {
 public:
  static const int mx=100;
  char flavor[mx];
  char nameMask[mx];
  DbFlavor(){flavor[0]=0; nameMask[0]=0;}
};


class StEEmcDbMaker : public StMaker {
 private:
  enum {mxAdcCrate=119, mxAdcChan=192,
	minTwCrateID=1,maxTwCrateID=6, 
	minMapmtCrateID=64, maxMapmtCrateID=119,
        maxTwCrateCh=128, maxMapmtCrateCh=192};
  
  // static Char_t  m_VersionCVS = "$Id: StEEmcDbMaker.h,v 1.20 2004/04/09 18:38:11 balewski Exp $";

  int mfirstSecID, mlastSecID;
  int mNSector;
  int myTimeStampDay;
  unsigned int myTimeStampUnix;
  void  clearItemArray();
  void mRequestDataBase(); ///< reads tables from STAR-DB
  void mOptimizeMapping(int isec);
  void mOptimizeOthers(int isec);
  void mOptimizeFibers(); ///< decodes crates -->fiber map

  //........... old
  StEEmcDbIndexItem1   *mDbItem1; //!  assess via logical name (sec/sub/eta)
  StEEmcDbIndexItem1   ***mLookup; //! access via crate/chan
  // ........... end
  
  // pointers to Db tables for each sector
  int *mDbsectorID; //!
  eemcDbADCconf_st  **mDbADCconf; //!
  eemcDbPMTcal_st   **mDbPMTcal ; //!
  eemcDbPMTname_st   **mDbPMTname ; //!
  eemcDbPIXcal_st   **mDbPIXcal ; //!
  eemcDbPMTped_st   **mDbPMTped ; //!
  eemcDbPMTstat_st  **mDbPMTstat ; //!
  kretDbBlobS_st  *mDbFiberConfBlob; //!
  
  // local fast look-up tables
  EEmcDbItem   *byIndex; //!  assess via plain index
  EEmcDbItem   ***byCrate; //! access via crate/chan
  EEmcDbCrate *mDbFiber; // maps tw & mapmt crates to DAQ fibers
  int nFiber; // # of existing crates(Tw+Mapmt)
  
  float KsigOverPed; // defines threshold
  int nFound;
  TString dbName; //name of the DB used 
  DbFlavor dbFlavor; // used if flavor is requested
  
  template <class St_T, class T_st> void getTable(TDataSet *eedb, int secID, TString tabName, TString mask, T_st **outTab);

 protected:
 public:  
  
  void setSectors(int ,int); ///< limit the range of sectors for speed
  void setThreshold(float x);// defines threshold for ADCs

  const EEmcDbCrate * getFiber(int icr);
  const  int getNFiber(){return nFiber;}
  const  EEmcDbItem* getByIndex1(int ikey); ///< returns full DB info for one pixel
  void exportAscii(char *fname="eemcDbDump.dat") const; 
  void print() {exportAscii();}

  const  EEmcDbItem*  getByCrate(int crateID, int channel); // full DB info, crateID counts from 1, channel from 0  

  const EEmcDbItem* getTail(int sec, char sub, int eta, char type);  
  const EEmcDbItem* getStrip(int sec, int strip, char type);  

#if 0 // tmp out, remove the old methods firts
  const EEmcDbItem* getT(int sec, char sub, int eta){getTail(sec,sub,eta,'T');}
  const EEmcDbItem* getP(int sec, char sub, int eta){getTail(sec,sub,eta,'P');}
  const EEmcDbItem* getQ(int sec, char sub, int eta){getTail(sec,sub,eta,'Q');}
  const EEmcDbItem* getR(int sec, char sub, int eta){getTail(sec,sub,eta,'R');}
  const EEmcDbItem* getU(int sec, int strip){getStrip(sec,strip,'U');}
  const EEmcDbItem* getV(int sec, int strip){getStrip(sec,strip,'V');}
#endif  

  //
  // Methods to acces DB info for T=tower, P=preshower-1, Q=preshower-2,
  // R=postshower, U=SMD-U strip, V=SMD-V strip
  //


  //.......................  OLD ............... DB access, drop it
  const  StEEmcDbIndexItem1* getT(int sec, char sub, int eta); ///< returns full DB info for one Tower channel
  const  StEEmcDbIndexItem1* getP(int sec, char sub, int eta); ///< returns full DB info for one preshower-1 channel
  const  StEEmcDbIndexItem1* getQ(int sec, char sub, int eta); ///< returns full DB info for one preshower-2 channel
  const  StEEmcDbIndexItem1* getR(int sec, char sub, int eta); ///< returns full DB info for one postshower  channel
  const  StEEmcDbIndexItem1* getU(int sec, int strip ); ///< returns full DB info for one SMD strip/channel, U plane
  const  StEEmcDbIndexItem1* getV(int sec, int strip ); ///< returns full DB info ...                      , V plane


  const  StEEmcDbIndexItem1* getByIndex(int i); ///< returns full DB info for one any channel
  const  StEEmcDbIndexItem1* get(int crate, int channel); ///< returns full DB info for any ADC channel

  //.......................  OLD ............... end

  void setTimeStampDay( int ); ///< to fix  time stamp for all events, default =not fixed 
  void setPreferedFlavor(const char *flavor, const char *tableNameMask);

  void setDBname(TString name){ dbName=name;}

  int valid(){ return nFound;} // return # of valid BD records
  unsigned int getTimeStampUnix(){return myTimeStampUnix;} ///< if zero then not fixed 

  StEEmcDbMaker(const char *name="EEmcDbMaker");

  virtual       ~StEEmcDbMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun  (int runumber); ///< to access STAR-DB
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcDbMaker.h,v 1.20 2004/04/09 18:38:11 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcDbMaker,0)   

 };

#endif

// $Log: StEEmcDbMaker.h,v $
// Revision 1.20  2004/04/09 18:38:11  balewski
// more access methods, not important for 63GeV production
//
// Revision 1.19  2004/04/08 16:28:06  balewski
// *** empty log message ***
//
// Revision 1.18  2004/04/04 06:10:37  balewski
// *** empty log message ***
//
// Revision 1.17  2004/03/30 04:44:57  balewski
// *** empty log message ***
//
// Revision 1.16  2004/03/19 21:31:53  balewski
// new EEMC data decoder
//
// Revision 1.15  2004/01/06 21:19:34  jwebb
// Added methods for accessing preshower, postshower and SMD info.
//
// Revision 1.14  2003/11/20 16:01:25  balewski
// towards run4
//
// Revision 1.13  2003/10/03 22:44:27  balewski
// fix '$' problem in db-entries name
//
// Revision 1.12  2003/09/10 19:47:08  perev
// ansi corrs
//
// Revision 1.11  2003/09/02 19:02:49  balewski
// fix for TMemeStat
//
// Revision 1.10  2003/08/27 03:26:46  balewski
// flavor option added:  myMk1->setPreferedFlavor("set-b","eemcPMTcal");
//
// Revision 1.9  2003/08/26 03:02:30  balewski
// fix of pix-stat and other
//
// Revision 1.8  2003/08/25 17:57:12  balewski
// use teplate to access DB-tables
//
