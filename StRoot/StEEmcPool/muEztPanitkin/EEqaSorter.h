// \class  EEqaSorter
// \author Jan Balewski, Hal Spinka
// $Id: EEqaSorter.h,v 1.5 2009/02/24 04:07:45 ogrebeny Exp $

#ifndef EEqaSorter_h
#define EEqaSorter_h

#include <TObject.h>
#include <TString.h>

class TObjArray;
class TH1F;
class TFile;

class EEqaSorterA;
class EEqaSorterC;
class EEdsmAna;
class StEEmcDb;

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
class EztEmcRawData;
class EztEventHeader;
class SpyGeneric;
class SpyCopyCat;

class EEqaSorter :public TObject{ 
 
 private:
  enum {mxH=8};
  TH1F *hCorT[mxH]; // corruption histos ETOW
  TH1F *H4jpCor; // added in 2005 to help shift crew assess ETOW
  TH1F *hCorS[mxH]; // corruption histos ESMD

  EEqaSorterA *sortA;
  EEqaSorterC *sortC;
  EEdsmAna *dsm;
  StEEmcDb *eeDb; //!

  TH1F * H1tot;
  const Char_t *pathInp;
  const Char_t *pathOut; 
  int timeStamp; // for event
  void crateHealth(EztEmcRawData *eRaw, TH1F **, int es,  int ver) const;
  void xRayETOW(EztEmcRawData *t, int token) const;
  void xRayESMD(EztEmcRawData *s, int token, int ver, int runNo) const;

  // spy tool
  SpyGeneric **mySpy;
  SpyCopyCat **mySpyCC;
  int nSpy, nSpyCC;
  int minSecSpy; // time delay
  int nEveSpy; //event sample this time
  int spyMode; // internal switch
  int lastSpyRun;

 public:
  EEqaSorter(StEEmcDb *dbx = 0);
  virtual ~EEqaSorter();
  void setPath(const char *path_in, const char *path_out) {pathInp = path_in; pathOut = path_out;}
  void initHisto(TObjArray *HList = 0, int nb=150, int mx=600);
  void initRun();
  void sort(EztEmcRawData *t, EztEmcRawData *s, 
	    int runNo,  int token, int ver, 
	    const unsigned char * dsm0inp, 
	    const unsigned short int  * dsm1inp ,
	    const unsigned short int  * dsm2inp,   
	    const unsigned short int  * dsm3inp);
  void initSpy(const TObjArray *HList, int minSec, int mode); // must be called after histos initialized
  //mode: 1=balewski@rcf, 2=eemc@evp,3=operator@evp 
  void spy(EztEmcRawData *t, EztEmcRawData *s, int runNo=888999, int eventId=777);
  void clear(); 
  void Finish();
  void resetHisto();
  void saveHisto(const Char_t *filename = "out/eemcQA.hist.root") const;
  void saveHisto(TFile *f) const;

   ClassDef(EEqaSorter,1) 
};
     
#endif

// $Log: EEqaSorter.h,v $
// Revision 1.5  2009/02/24 04:07:45  ogrebeny
// Fixed part of the trigger histograms
//
// Revision 1.4  2009/02/04 20:33:26  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.3  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.2  2009/01/18 01:01:28  ogrebeny
// Better separate EMC histogramming from OnlinePlots infrastructure
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.2  2004/02/26 04:22:23  balewski
// more Hal's plots
//
// Revision 1.1  2004/02/17 03:09:18  balewski
// *** empty log message ***
//
// Revision 1.4  2004/01/29 17:23:14  balewski
// fix for BTOW
//
// Revision 1.3  2004/01/27 16:29:39  balewski
// reset added
//
// Revision 1.7  2004/01/21 18:53:33  spinka
// Various problems fixed for towers.  More remain.
//

/* how to merge with Panitkin code
1) disable  ClassDef(EEqaSorter) in .cxx & .h

2) histo I/O

histoHandler.cxx:17:#include "EEqaSorter.h"
histoHandler.cxx:18:EEqaSorter *eeqaA;

increae memeory 3x
  mfile = TMapFile::Create("hsimple.map","RECREATE", 90000000, ....

histoHandler.cxx:293:  eeqaA=new EEqaSorter; // creates EEMC related histos

line ~350
 eeqaA->saveHistoAdd();

3) sorter
eventLoop.cxx:18:#include "EEqaSorter.h"
eventLoop.cxx:19:extern EEqaSorter *eeqaA;
eventLoop.cxx:1655:             eeqaA->sort(evp->token);

4) presenter
test_3.h test_3.h: static const int MAX_SUBTABS = 11;


test_3.cxx:3:#include "EEqaPresenter.h"

line ~200
 TabNames[10][0]="EEMC";

line ~290

  //ETOW and ESMD
   nSubTabs[10]=4;
   TabNames[10][1]="Errors";
   TabNames[10][2]="Towers";
   TabNames[10][3]="SMD sec 5,6";
   TabNames[10][4]="SMD sec 7,8";
  //          [9] is last

5) add canvas divisions to
CanvasDescriptions.txt

10,1,2,2,1,cr1
10,2,3,2,1,cr1
10,3,4,2,1,cr1
10,4,8,1,1,cr1

*/


