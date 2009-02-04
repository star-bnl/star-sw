// \class  RawPixels
// \author Jan Balewski
// $Id: RawPixels.h,v 1.2 2009/02/04 20:33:27 ogrebeny Exp $

#ifndef RawPixels_h
#define RawPixels_h

#include "TObject.h"

class TObjArray;
class TH1F;
class TH2F;
class TFile;
class EztEmcRawData;
class StEEmcDb;


class RawPixels :public TObject{ 
 public:  // type of data conversion
  enum convMode{kRawAdc=0, kPedSub, kPedAndGain};
 private:
  TH1F *hInfo;
  TH1F **hPix;// ped subt spectra , for each tower/pre/post/smd
  TH2F **hSmd; //   strip vs. ADC 
  StEEmcDb *eeDb;
  int c_x1,c_x2; // histo range
  convMode c_convMode;

 public:
  RawPixels(TObjArray*L,StEEmcDb*dbx);
  void setLimits(int x1, int x2){c_x1=x1;c_x2=x2;}
  void doRawAdc(){ c_convMode=kRawAdc;}
  void doPedSub(){ c_convMode=kPedSub;}
  void doPedAndGain(){ c_convMode=kPedAndGain;}
  void initHisto();

  void sort(EztEmcRawData  *eRaw);
  TObjArray *HList;

  ClassDef(RawPixels,1) 
};
     
#endif

// $Log: RawPixels.h,v $
// Revision 1.2  2009/02/04 20:33:27  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.5  2004/04/16 17:25:22  balewski
// smarter peds
//
// Revision 1.4  2004/01/12 03:06:31  balewski
// *** empty log message ***
//
// Revision 1.3  2003/12/01 05:01:31  balewski
// *** empty log message ***
//
// Revision 1.2  2003/11/27 06:30:53  balewski
// *** empty log message ***
//
// Revision 1.1  2003/11/24 16:02:34  balewski
// start
//


