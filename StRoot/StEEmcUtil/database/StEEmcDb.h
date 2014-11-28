/*! \class StEEmcDb
\author Jan Balewski

Interface to STAR-DB. Info from various DB tables is 
correlated for every ADC channel and stored in memory in
 mDbItem[].
mDbItem1[] has single index based on the channel name, 
calculated by EEname2index() utility.
*/  

#ifndef STAR_StEEmcDb_H
#define STAR_StEEmcDb_H

#include <TObject.h>
#include <TString.h>
#include <TDataSet.h>

class StMaker;

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

class DbFlavor : public TObject {
 public:
  static const int mx=100;
  char flavor[mx];
  char nameMask[mx];
  DbFlavor(){flavor[0]=0; nameMask[0]=0;}
};

class StEEmcDb : public TDataSet {
private:

  int mfirstSecID, mlastSecID;
  int mNSector;
  void  clearItemArray();
  void requestDataBase(StMaker *anyMaker); ///< reads tables from STAR-DB
  int optimizeMapping(int isec);
  void optimizeOthers(int isec);
  void optimizeFibers(); ///< decodes crates -->fiber map
 
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
  EEmcDbItem   *byIndex; //!  assess via  index
  EEmcDbItem   ***byCrate; //! access via crate/chan
  const EEmcDbItem   *byStrip[MaxSectors][MaxSmdPlains][MaxSmdStrips]; //! access via sec/UV/strip

  EEmcDbCrate *mDbFiber; // maps tw & mapmt crates to DAQ fibers
  int nFiber; // # of existing crates(Tw+Mapmt)
  
  float KsigOverPed; // defines threshold
  int nFound;
  TString dbName; //name of the DB used 
  DbFlavor dbFlavor; // used if flavor is requested
  
  template <class St_T, class T_st> void getTable(TDataSet *eedb, int secID, TString tabName, TString mask, T_st **outTab);

  const EEmcDbItem* getStrip(int sec, char uv, int strip) const;  //ranges: sec=1-12, uv=U,V ,strip=1-288; slow method

  TString mAsciiDbase; // Ascii database filename (default NONE)

  // tmp solution to hold list of strings
  TString *chGainL;
  int nChGain, mxChGain;
  void changeGainsAction(const char *fname);

  TString *chMaskL;
  int nChMask, mxChMask;
  void changeMaskAction(const char *fname);

// protected:
public:  
  
  void setSectors(int ,int); ///< limit the range of sectors for speed
  int getFirstSector() const {return mfirstSecID;}
  int getLastSector() const {return mlastSecID;}

  void setThreshold(float x);// defines threshold for ADCs

  void setAsciiDatabase ( const Char_t *dbfile );
  void changeGains(char *fname);// Replace gains for  initialized channels
  void changeMask(char *fname);// Replace stat/fail mask for initialized channels 
  const EEmcDbCrate * getFiber(int icr) const;
  void setFiberOff(int icr);
  const int getNFiber() const {return nFiber;}
  const EEmcDbItem* getByIndex(int ikey) const; ///< returns full DB info for one pixel
  void exportAscii(const char *fname="eemcDbDump.dat") const; 
  void print() const {exportAscii();}

  const  EEmcDbItem*  getByCrate(int crateID, int channel) const; // full DB info, crateID counts from 1, channel from 0  

  const  EEmcDbItem*  getByStrip0(int isec, int iuv, int istrip) const;  //ranges: isec=0-11, iuv=0,1 ,istrip=0-287; fast method

  const  EEmcDbItem*  getByStrip(int sec, char uv, int strip) const //ranges: sec=1-12, uv=U,V ,strip=1-288; fast method
    {return getByStrip0(sec-1,uv-'U',strip-1);}

  const EEmcDbItem* getTile(int sec,char sub, int eta, char type) const; //ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method

  const EEmcDbItem* getT(int sec, char sub, int eta) const {return getTile(sec,sub,eta,'T');}

  const EEmcDbItem* getP(int sec, char sub, int eta) const {return getTile(sec,sub,eta,'P');}
  const EEmcDbItem* getQ(int sec, char sub, int eta) const {return getTile(sec,sub,eta,'Q');}
  const EEmcDbItem* getR(int sec, char sub, int eta) const {return getTile(sec,sub,eta,'R');}
  const EEmcDbItem* getU(int sec, int strip) const {return getStrip(sec,'U',strip);}
  const EEmcDbItem* getV(int sec, int strip) const {return getStrip(sec,'V',strip);}
  const EEmcDbItem* StBarrelIndex2Item(int StDetId , int Bmod, int Beta, int  Bsub) const;

  //
  // Methods to acces DB info for T=tower, P=preshower-1, Q=preshower-2,
  // R=postshower, U=SMD-U strip, V=SMD-V strip
  //

  void setPreferredFlavor(const char *flavor, const char *tableNameMask); 
  void setPreferedFlavor(const char *flavor, const char *tableNameMask){    
     setPreferredFlavor( flavor, tableNameMask); // typo...
  }

  int getFirstSecID() const { return mfirstSecID;}
  int getLastSecID() const { return mlastSecID;}
  void setDBname(TString name){ dbName=name;}

  int valid() const { return nFound;} // return # of valid BD records
  float getKsigOverPed() const {return KsigOverPed;} // defines threshold

  StEEmcDb(const Char_t *name = "StEEmcDb"); // default name for this object
  virtual ~StEEmcDb();

  void loadTables(StMaker *anyMaker); ///< to access STAR-DB
  
  ClassDef(StEEmcDb, 1)

 };

#endif
