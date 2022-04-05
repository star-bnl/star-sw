#ifndef L2EMCDB_H
#define L2EMCDB_H
#include <stdio.h>

/*********************************************************
 * $Id: L2EmcDb.h,v 1.6 2008/01/30 00:47:15 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************
 * Descripion:
 * StRoot-free DB container , common for BTOW + ETOW + ESMD
 *********************************************************
 */

class L2EmcDb {
 public:
  /* use it to decalare any ENDCAP local array 12+1 secors */
  enum { EindexMax=13000};
  /* use it to decalare any local BTOW array 12+1+extra */
  enum { BindexMax=5400};
#define EmcDbIndexMax  (L2EmcDb::EindexMax+L2EmcDb::BindexMax) /* use it to decalare any local array */
  
  enum {ETOW_DATSIZE=160, ETOW_DATUSED=128, BTOW_DATSIZE=160, ETOW_MAXFEE=6, BTOW_MAXFEE=30,MxEmcNameLen=16};

  struct EmcCDbItem {
  int  key; /* unique EmcDbC ID in form 1 to EmcIndex-1, not all used */
  char name[MxEmcNameLen]; /* geographical name of the pixel */
  char tube[MxEmcNameLen]; /* ETOW: name of PMT; ESMD:  MAPMT pixel; BTOW: softId-mod-sub-eta */

  /*  for towers/pre/post use (sec,sub,eta)
      for SMD use sec,plane,strip) */
  int  sec,eta; /*ETOW: 1-12, 1-12; BTOW: 1-12, 1-40 */
  char sub;/*ETWO: A-E;  BTOW: a-j*/
  char plane; /* ESMD:  U-V */
  int  strip; /* ESMD:   1-288  */

  int   crate, chan; /* hardware channel */
  int   rdo; /* index of channel in the raw data block, counts from 0 */
  float gain; 
  float ped,thr,sigPed; /* in ADC channals */
  unsigned  stat; /* bits, see eemcConstDB.hh for definitions */
  unsigned  fail; /* bits, see eemcConstDB.hh for definitions */
} ;

  L2EmcDb(char *inpP, char *logP);
  ~L2EmcDb(); // saves DB for last run
  const  EmcCDbItem *getByIndex(int i);
  const  EmcCDbItem *getByName(char *name); // slow
  int    name2index(char *name);// slow

  static void  printItem(const EmcCDbItem *x);
  bool  isEmpty(const EmcCDbItem *x);
  bool  isBTOW(const EmcCDbItem *x);
  bool  isETOW(const EmcCDbItem *x);

  int   initRun(int runNo);
  int   getRun() { return run_number;}
  void  finishRun();

  enum {mxTxt=1000};
  char logPath[mxTxt];
  char inpPath[mxTxt];
  char pedFile[mxTxt];
  char maskFile[mxTxt];


  void setPedFile( const char *c ); /**< sets the name of the pedestal file, defaults to pedestal.current */
  void setMaskFile( const char *c ); /**< sets the name of the mask file, defaults to ... */

 private:
  EmcCDbItem dbByIndex[EmcDbIndexMax]; // the data container
 
  void  clearItem(EmcCDbItem *x); 
  void  clearTables();
  int   importItem(EmcCDbItem *x, FILE *fd);
  void  exportItem(EmcCDbItem *x, FILE *fd);
  int   BtowName2Index(int sect, char *xee); //slow
  int   readAsciiDb(char *fname, char *lbl);
  void  writeAsciiDb(FILE *fd);

  int   changeMaskFullCrate(const char *fname, char BEflag, char *lbl); 
  int   changePedsByName(const char *fname, char *lbl);
  int   changeMaskByName(const char *fname, char *lbl); 

  int   run_number;
 
  enum {txMxLbl=6, txMxSize=100};
  char db_labels[txMxLbl][txMxSize];

};


/***********************************************************

  Descripion:

  1) Lets define a unique 'name' for any active element in EEMC
         name="ssTann" for towers, ss=sector 01-12, a=subsector='A' -'E'
                        nn=tower in eta =01-12
              "ssxkkk"  for SMD  x='U' or 'V' plain orientation,
                                 kkk=001-288 strip ID
              "ssPann"  for Pres1
              "ssQann"  for Pres2
              "ssRann"  for Post
              "ssLann"  for LED pixel
  1.b) Extension for BTOW towers
         name="sstann" for towers, ss=sector 01-12, t='t'
                      a=subsector='a'-'j' interlaced
                      nn=tower  etaBin =1-40
         Geograohical location of towers:
           BTOW subs 'a' infront of ETOW subs 'A'   
           BTOW subs 'b'  spans  ETOW subs 'A'
           BTOW subs 'c'  spans  ETOW subs 'B' , etc ...
           BTOW eta bin 1 is at physical eta of +0.95   
           BTOW eta bin 20 is at physical eta of +0.0?   
           BTOW eta bin 21 is at physical eta of -0.0?   
           BTOW eta bin 40 is at physical eta of -0.95   

  1.c) Extension for spare channels --> use sector 13
       Add 1000 to the ETOW index range.
       Add  400 to the BTOW index range.

       spare ETOW chan 120-127 per crate 1-6 mapp into
         consecutive 13TA01, ..02, ..12, 13TB01,... 13TD12 
       
       spare BTOW chan papping ???
      

  2) Lets define sector oriented index for any   PMT/MAPMT pixel
       Index within sector:

         Tower   1- 60  =(sub-1)*5+eta, where sub=1-5, eta=1-12	  
          Pre1 101-160  =  the same
          Pre2 201-260  =  the same
          Post 301-360  =  the same
         smd-U 401-688   couting from shortest at eta=2
         smd-V 701-988   couting from shortest at eta=2

       Index jumps by 1000 from sector to sector
       --> add (sectorID-1)*1000

       Index total range [1 - 12,999], spare ~15% or ~180/sector

   2.b) Extension for BTOW towers
        Tower   =12,000+ (sec-1)*400 + (sub-1)*40 +eta
              where sec=1-12, sub=1-10, eta=1-40	  
      Index total range [13,001- 17,200], 200 spares 
	      
****************************************************/


#endif 


/*
*********************************************************************
  $Log: L2EmcDb.h,v $
  Revision 1.6  2008/01/30 00:47:15  balewski
  Added L2-Etow-calib

  Revision 1.5  2007/12/19 02:30:16  balewski
  new L2-btow-calib-2008

  Revision 1.4  2007/11/14 03:58:07  balewski
  cleanup of common timing measurement

  Revision 1.3  2007/11/06 22:07:24  balewski
  added timeStamp controlled L2 setup from Jason

  Revision 1.2  2007/10/22 23:10:03  balewski
  split L2 to generic and year specific, not finished

  Revision 1.1  2007/10/11 00:33:14  balewski
  L2algo added

  Revision 1.4  2006/03/28 19:33:23  balewski
  ver16b , in L2new

  Revision 1.3  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/

