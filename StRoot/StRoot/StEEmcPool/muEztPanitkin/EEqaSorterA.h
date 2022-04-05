// \class  EEqaSorterA
// \author Jan Balewski, Hal Spinka
// $Id: EEqaSorterA.h,v 1.3 2009/02/24 04:07:45 ogrebeny Exp $

#ifndef EEqaSorterA_h
#define EEqaSorterA_h

#include "TObject.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class TObjArray;

class TH1F;
class TH2F;
class EztEmcRawData;

class EEqaSorterA :public TObject{ 

private:
  TH2F **hCrate;     // chan vs. ADC per crate
  TH1F **hCrateHot;   // counts/chan for  ADC>4*ped4+thr1
  int hotTwThres; // used to count hot towers
  int feePed[MaxTwCrateID*MaxTwCrateCh]; // pedestals loaded to FEE, note DAQ ped=4*ped4  

  void sortDaqTower1(const EztEmcRawData *t);
  void sortDaqTowerHot(const EztEmcRawData *t);
  void sortDaqMapmt0(const EztEmcRawData *s, int ver);

public:
  EEqaSorterA();
  virtual ~EEqaSorterA();
  void initCrateHisto(TObjArray *HList, int nb = 150, int mx = 600);
  int  usePed4(const Char_t *filename = "eemcPed4.dat");
  void sort(const EztEmcRawData *t, const EztEmcRawData *s, int ver );
  void Finish();
  void resetHisto();

  void saveHisto(TFile *f) const;

   ClassDef(EEqaSorterA,1) 
};
     
#endif

// $Log: EEqaSorterA.h,v $
// Revision 1.3  2009/02/24 04:07:45  ogrebeny
// Fixed part of the trigger histograms
//
// Revision 1.2  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.5  2004/02/17 03:09:18  balewski
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
1) disable  ClassDef(EEqaSorterA) in .cxx & .h

2) histo I/O

histoHandler.cxx:17:#include "EEqaSorterA.h"
histoHandler.cxx:18:EEqaSorterA *eeqaA;

increae memeory 3x
  mfile = TMapFile::Create("hsimple.map","RECREATE", 90000000, ....

histoHandler.cxx:293:  eeqaA=new EEqaSorterA; // creates EEMC related histos

line ~350
 eeqaA->saveHistoAdd();

3) sorter
eventLoop.cxx:18:#include "EEqaSorterA.h"
eventLoop.cxx:19:extern EEqaSorterA *eeqaA;
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


