// $Id: StEEmcDbMaker.h,v 1.26 2004/06/25 22:55:53 balewski Exp $

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
#include "StEEmcUtil/EEfeeRaw/EEdims.h"



class DbFlavor {
 public:
  static const int mx=100;
  char flavor[mx];
  char nameMask[mx];
  DbFlavor(){flavor[0]=0; nameMask[0]=0;}
};


class StEEmcDbMaker : public StMaker {
  // private:
 public:

  // static Char_t  m_VersionCVS = "$Id: StEEmcDbMaker.h,v 1.26 2004/06/25 22:55:53 balewski Exp $";

  int mfirstSecID, mlastSecID;
  int mNSector;
  int myTimeStampDay;
  unsigned int myTimeStampUnix;
  void  clearItemArray();
  void mRequestDataBase(); ///< reads tables from STAR-DB
  int mOptimizeMapping(int isec);
  void mOptimizeOthers(int isec);
  void mOptimizeFibers(); ///< decodes crates -->fiber map

  
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
  const EEmcDbItem   *byStrip[MaxSectors][MaxSmdPlains][MaxSmdStrips]; //! access via sec/UV/strip

  EEmcDbCrate *mDbFiber; // maps tw & mapmt crates to DAQ fibers
  int nFiber; // # of existing crates(Tw+Mapmt)
  
  float KsigOverPed; // defines threshold
  int nFound;
  TString dbName; //name of the DB used 
  DbFlavor dbFlavor; // used if flavor is requested
  
  template <class St_T, class T_st> void getTable(TDataSet *eedb, int secID, TString tabName, TString mask, T_st **outTab);

  const EEmcDbItem* getStrip(int sec, char uv, int strip);  //ranges: sec=1-12, uv=U,V ,strip=1-288; slow method

  TString mAsciiDbase; // Ascii database filename (default NONE)

 protected:
 public:  
  
  void setSectors(int ,int); ///< limit the range of sectors for speed
  void setThreshold(float x);// defines threshold for ADCs


  void setAsciiDatabase ( const Char_t *dbfile );


  const EEmcDbCrate * getFiber(int icr);
  const  int getNFiber(){return nFiber;}
  const  EEmcDbItem* getByIndex(int ikey); ///< returns full DB info for one pixel
  void exportAscii(char *fname="eemcDbDump.dat") const; 
  void print() {exportAscii();}

  const  EEmcDbItem*  getByCrate(int crateID, int channel); // full DB info, crateID counts from 1, channel from 0  

  const  EEmcDbItem*  getByStrip0(int isec, int iuv, int istrip);  //ranges: isec=0-11, iuv=0,1 ,istrip=0-287; fast method

  const  EEmcDbItem*  getByStrip(int sec, char uv, int strip) //ranges: sec=1-12, uv=U,V ,strip=1-288; fast method
    {return getByStrip0(sec-1,uv-'U',strip-1);}

  const EEmcDbItem* getTile(int sec,char sub, int eta, char type); //ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method

  const EEmcDbItem* getT(int sec, char sub, int eta){return getTile(sec,sub,eta,'T');}



  const EEmcDbItem* getP(int sec, char sub, int eta){return getTile(sec,sub,eta,'P');}
  const EEmcDbItem* getQ(int sec, char sub, int eta){return getTile(sec,sub,eta,'Q');}
  const EEmcDbItem* getR(int sec, char sub, int eta){return getTile(sec,sub,eta,'R');}
  const EEmcDbItem* getU(int sec, int strip){return getStrip(sec,strip,'U');}
  const EEmcDbItem* getV(int sec, int strip){return getStrip(sec,strip,'V');}


  //
  // Methods to acces DB info for T=tower, P=preshower-1, Q=preshower-2,
  // R=postshower, U=SMD-U strip, V=SMD-V strip
  //

  void setTimeStampDay( int ); ///< to fix  time stamp for all events, default =not fixed 

  void setPreferredFlavor(const char *flavor, const char *tableNameMask); 
  void setPreferedFlavor(const char *flavor, const char *tableNameMask){    
     setPreferredFlavor( flavor, tableNameMask); // typo...
  }


  void setDBname(TString name){ dbName=name;}

  int valid(){ return nFound;} // return # of valid BD records
  unsigned int getTimeStampUnix(){return myTimeStampUnix;} ///< if zero then not fixed 

  StEEmcDbMaker(const char *name="EEmcDbMaker");

  virtual       ~StEEmcDbMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun  (int runumber); ///< to access STAR-DB
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcDbMaker.h,v 1.26 2004/06/25 22:55:53 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcDbMaker,0)   

 };

#endif

// $Log: StEEmcDbMaker.h,v $
// Revision 1.26  2004/06/25 22:55:53  balewski
// now it survives missing fiberMap in DB , also gMessMgr is used
//
// Revision 1.25  2004/06/04 13:30:24  balewski
// use gMessMgr for most of output
//
// Revision 1.24  2004/05/26 21:30:36  jwebb
// Fixed typo, added setPreferredFlavor method.  Kept setPreferedFlavor for
// backwards compatibility.
//
// Revision 1.23  2004/05/14 20:55:36  balewski
// fix to process many runs, by Piotr
//
// Revision 1.22  2004/04/28 20:38:11  jwebb
// Added StEEmcDbMaker::setAsciiDatabase().  Currently not working, since
// tube name missing for some towers, triggereing a "clear" of all EEmcDbItems.
//
// Revision 1.21  2004/04/12 16:19:52  balewski
// DB cleanup & update
//
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
