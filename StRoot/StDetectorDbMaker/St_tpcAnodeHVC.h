#ifndef St_tpcAnodeHVC_h
#define St_tpcAnodeHVC_h

#include "TChair.h"
#include "tables/St_tpcAnodeHV_Table.h"

class St_tpcAnodeHVC : public TChair {
 public:
  static St_tpcAnodeHVC* 	instance();
  tpcAnodeHV_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcAnodeHV*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  unsigned short 	sector(Int_t i = 0) 	const {return Struct(i)->sector;}
  unsigned short 	socket(Int_t i = 0) 	const {return Struct(i)->socket;}
  Float_t 	voltage(Int_t i = 0) 	const {return Struct(i)->voltage;}
  bool		livePadrow(int sector = 1, int padrow = 1) { return voltagePadrow(sector,padrow) > 500; }
  float		voltagePadrow(int sector = 1, int padrow = 1) {
    // sector=1..24 , padrow=1..45
    int e1=0; int e2=0;
    elemsFromSecPad(sector,padrow,e1,e2);
    if (e1==0) return -99;
    float v1=voltage(e1-1);
    if (e1==e2 || v1==voltage(e2-1)) return v1;
    return 0; // different voltages on influencing HVs
  }
  bool tripped() { return (voltage() < -100); }
 protected:
  St_tpcAnodeHVC(St_tpcAnodeHV *table=0) : TChair(table) {}
  virtual ~St_tpcAnodeHVC() {fgInstance = 0;}
  virtual void elemsFromSecPad(int sector, int padrow, int& e1, int& e2) {
    int elemOffset = (sector-1)*19;
    e1 = elemOffset; e2 = elemOffset;
    switch (padrow) {
      case  1: e1+= 1; e2+= 1; break;
      case  2: e1+= 2; e2+= 2; break;
      case  3:
      case  4: e1+= 3; e2+= 3; break;
      case  5:
      case  6: e1+= 4; e2+= 4; break;
      case  7: e1+= 5; e2+= 5; break;
      case  8:
      case  9: e1+= 6; e2+= 6; break;
      case 10: e1+= 7; e2+= 7; break;
      case 11: e1+= 7; e2+= 8; break;
      case 12: e1+= 8; e2+= 8; break;
      case 13: e1+=17; e2+=17; break;
      case 14:
      case 15:
      case 16: e1+= 9; e2+= 9; break;
      case 17: e1+= 9; e2+=10; break;
      case 18:
      case 19:
      case 20: e1+=10; e2+=10; break;
      case 21: e1+=10; e2+=11; break;
      case 22:
      case 23:
      case 24: e1+=11; e2+=11; break;
      case 25: e1+=11; e2+=12; break;
      case 26:
      case 27:
      case 28: e1+=12; e2+=12; break;
      case 29: e1+=12; e2+=13; break;
      case 30:
      case 31:
      case 32: e1+=13; e2+=13; break;
      case 33: e1+=13; e2+=14; break;
      case 34:
      case 35:
      case 36: e1+=14; e2+=14; break;
      case 37: e1+=14; e2+=15; break;
      case 38:
      case 39:
      case 40: e1+=15; e2+=15; break;
      case 41: e1+=15; e2+=16; break;
      case 42:
      case 43:
      case 44: e1+=16; e2+=16; break;
      case 45: e1+=16; e2+=19; break;
      default: e1=0; e2=0;
    }
  }
 private:
  static St_tpcAnodeHVC* fgInstance;
  ClassDefChair(St_tpcAnodeHV, tpcAnodeHV_st )
  ClassDef(St_tpcAnodeHVC,1) //C++ TChair for tpcAnodeHV table class
};
#endif
