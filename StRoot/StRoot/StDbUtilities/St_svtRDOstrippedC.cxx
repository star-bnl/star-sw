#include <assert.h>
#include "TMath.h"
#include "TDatime.h"
#include "TString.h"
#include "Stiostream.h"
#include "St_svtRDOstrippedC.h"
ClassImp(St_svtRDOstrippedC);
St_svtRDOstrippedC *St_svtRDOstrippedC::fgsvtRDOstrippedC = 0;
struct RDO_t {
  const Char_t *name;
  Int_t ladNum, barNum;
  const Char_t *rdo;
  Int_t ndet1, ndet2; // wafers range
};
static const Int_t NRDOS = 72;
static const RDO_t RDOS[NRDOS] = {
  {"L01B1E", 1,1, "E1",1,2},{"L02B1E", 2,1, "E2",1,2},{"L03B1E", 3,1, "E4",1,2},{"L04B1E", 4,1, "E5",1,2},{"L05B1E", 5,1, "E7",1,2},
  {"L06B1E", 6,1, "E8",1,2},{"L07B1E", 7,1,"E10",1,2},{"L08B1E", 8,1,"E11",1,2},			        		            

  {"L01B2E", 1,2, "E3",1,3},{"L02B2E", 2,2, "E3",1,3},		            			        		            
  {"L03B2E", 3,2, "E3",1,3},{"L04B2E", 4,2, "E6",1,3},{"L05B2E", 5,2, "E6",1,3},{"L06B2E", 6,2, "E6",1,3},{"L07B2E", 7,2, "E9",1,3},
  {"L08B2E", 8,2, "E9",1,3},{"L09B2E", 9,2, "E9",1,3},{"L10B2E",10,2,"E12",1,3},{"L11B2E",11,2,"E12",1,3},{"L12B2E",12,2,"E12",1,3},
  		            			        		            			        		            
  {"L01B3E", 1,3, "E1",1,3},{"L02B3E", 2,3, "E1",1,4},{"L03B3E", 3,3, "E2",1,3},{"L04B3E", 4,3, "E2",1,4},{"L05B3E", 5,3, "E4",1,3}, 
  {"L06B3E", 6,3, "E4",1,4},{"L07B3E", 7,3, "E5",1,3},{"L08B3E", 8,3, "E5",1,4},{"L09B3E", 9,3, "E7",1,3},{"L10B3E",10,3, "E7",1,4}, 
  {"L11B3E",11,3, "E8",1,3},{"L12B3E",12,3, "E8",1,4},{"L13B3E",13,3,"E10",1,3},{"L14B3E",14,3,"E10",1,4},{"L15B3E",15,3,"E11",1,3},
  {"L16B3E",16,3,"E11",1,4}, 	   	    	        		            			        		            
  		            			        		            			        		            
  {"L01B1W", 1,1, "W1",3,4},{"L02B1W", 2,1, "W2",3,4},{"L03B1W", 3,1, "W4",3,4},{"L04B1W", 4,1, "W5",3,4},{"L05B1W", 5,1, "W7",3,4},
  {"L06B1W", 6,1, "W8",3,4},{"L07B1W", 7,1,"W10",3,4},{"L08B1W", 8,1,"W11",3,4},			        		            

  {"L01B2W", 1,2, "W3",4,6},{"L02B2W", 2,2, "W3",4,6},		            			        		            
  {"L03B2W", 3,2, "W3",4,6},{"L04B2W", 4,2, "W6",4,6},{"L05B2W", 5,2, "W6",4,6},{"L06B2W", 6,2, "W6",4,6},{"L07B2W", 7,2, "W9",4,6},
  {"L08B2W", 8,2, "W9",4,6},{"L09B2W", 9,2, "W9",4,6},{"L10B2W",10,2,"W12",4,6},{"L11B2W",11,2,"W12",4,6},{"L12B2W",12,2,"W12",4,6},
  		            			        		            			        		            
  {"L01B3W", 1,3, "W1",4,7},{"L02B3W", 2,3, "W1",5,7},{"L03B3W", 3,3, "W2",4,7},{"L04B3W", 4,3, "W2",5,7},{"L05B3W", 5,3, "W4",4,7}, 
  {"L06B3W", 6,3, "W4",5,7},{"L07B3W", 7,3, "W5",4,7},{"L08B3W", 8,3, "W5",5,7},{"L09B3W", 9,3, "W7",4,7},{"L10B3W",10,3, "W7",5,7}, 
  {"L11B3W",11,3, "W8",4,7},{"L12B3W",12,3, "W8",5,7},{"L13B3W",13,3,"W10",4,7},{"L14B3W",14,3,"W10",5,7},{"L15B3W",15,3,"W11",4,7},
  {"L16B3W",16,3,"W11",5,7} 	   	    
};
static const Int_t NB    =  3;
static const Int_t NL    = 16;
static const Int_t NWmax =  7;
static svtRDOstripped_st *pointers[3][16][7];
static svtRDOstripped_st *DataOld = 0;
static Int_t _debug = 0;
//________________________________________________________________________________
St_svtRDOstrippedC::St_svtRDOstrippedC (St_svtRDOstripped *table) : TChair(table), fDate(0), fDelay(0), fSwitchedOff(0) {
  if (fgsvtRDOstrippedC) delete fgsvtRDOstrippedC; fgsvtRDOstrippedC = this;
  Init();
}
//________________________________________________________________________________
void St_svtRDOstrippedC::Init() {
  memset (&pointers[0][0][0], 0, NB*NL*NWmax*sizeof(svtRDOstripped_st *));
  St_svtRDOstripped *Table =  (St_svtRDOstripped *) GetThisTable();
  if (Table) {
    svtRDOstripped_st *Data =  Table->GetTable();
    DataOld = Data;
    Int_t N = Table->GetNRows();
    TString rdo, RDO;
    Int_t i, j;
    for (j = 0; j < N; j++) {
      Int_t barrel = Data[j].barNum;
      Int_t ladder = Data[j].ladNum;
      Int_t NW     = NWmax;
      if (barrel == 1) NW = 4;
      if (barrel == 2) NW = 6;
      rdo = Data[j].rdo;
      for (Int_t wafer = 1; wafer <= NW; wafer++) {
	Int_t k = -1;
	for (i = 0; i < NRDOS; i++) { // West or East
	  RDO = RDOS[i].rdo;
	  if (RDOS[i].barNum == barrel && 
	      RDOS[i].ladNum == ladder && 
	      rdo == RDO &&
	      RDOS[i].ndet1  <= wafer  && wafer <= RDOS[i].ndet2) {k = i; break;}
	}
	if (k >= 0) 
	  pointers[barrel-1][ladder-1][wafer-1] = Data + j;
      }
    }
  }
}
//________________________________________________________________________________
svtRDOstripped_st *St_svtRDOstrippedC::pRDO(Int_t barrel, Int_t ladder, Int_t wafer) {
  svtRDOstripped_st *p = 0;
  if (!  GetThisTable()) return p;
  if (((St_svtRDOstripped *)GetThisTable())->GetTable() != DataOld) Init();
  if (barrel >= 1 && barrel <=  NB &&
      ladder >= 1 && ladder <=  NL &&
      wafer  >= 1 && wafer  <=  NWmax)  p = pointers[barrel-1][ladder-1][wafer-1];
  return p;
}
//________________________________________________________________________________
Int_t St_svtRDOstrippedC::svtRDOstrippedStatus(Int_t barrel, Int_t ladder, Int_t wafer) {
  svtRDOstripped_st *p = pRDO(barrel, ladder, wafer);
  Int_t iOK = 8; // barrel, ladder, wafer is missing
  if (p) {
    iOK = 0;
    if (TMath::Abs(-1500 - p->hvVolt) > 2) iOK += 1; // bad voltage
    if (p->lvFault)                        iOK += 2; // RDO fail
    if (! iOK && fDate && fDelay) {// Check time
      if (_debug) {
	TDatime t; t.Set(fDate);
	cout << " fSwitchedOff " << fSwitchedOff
	     << " fDelay " << fDelay << "\t"; t.Print();
      }
      Int_t utOff = 0;
      if (p->dateOff > 0) {
	 TDatime tOff(p->dateOff,p->timeOff);
	 if (_debug) {cout << " tOff "; tOff.Print();}
	 utOff = tOff.Convert();
      }
      Int_t utOn = 0;
      if (p->date > 0) {
	TDatime tOn(p->date,p->time);
	utOn = tOn.Convert();
	if (_debug) {cout << " tOn "; tOn.Print();}
      }
      Int_t DifOffOn = utOn - utOff;
      if (DifOffOn > fSwitchedOff) {
	Int_t DifOn = fDate - utOn;
	if (_debug) {cout << "DiffOffOn " << DifOffOn << "\tDifOn " << DifOn  << endl;}
	if (DifOn < fDelay) {
	  iOK += 4; // out of date
	}
      }
    }
  }
  return iOK;
}
//________________________________________________________________________________
void St_svtRDOstrippedC::PrintRDOmap() {
  Int_t status = -1;
  Int_t N =  NRDOS/2;
  cout << "Half Ladder  L/B RDO ndet1 ndet2  sta: 0 - o.k.; 1 - bad voltage; 2 - RDO fail; 4 - out of date; 8 - barrel, ladder, wafer is missing" << endl;
  St_svtRDOstrippedC *svtRDOS = St_svtRDOstrippedC::instance();
  for (Int_t i = 0; i < N; i++) {
    if (svtRDOS) status = svtRDOS->svtRDOstrippedStatus(RDOS[i].barNum,RDOS[i].ladNum,RDOS[i].ndet1);
    cout << Form("%11s %2i/%1i %3s  %3i %3i %3i", RDOS[i].name, RDOS[i].ladNum, RDOS[i].barNum, RDOS[i].rdo, RDOS[i].ndet1,RDOS[i].ndet2,status);
    cout << Form("%11s %2i/%1i %3s  %3i %3i %3i", RDOS[N+i].name, RDOS[N+i].ladNum, RDOS[N+i].barNum, RDOS[N+i].rdo, RDOS[N+i].ndet1, RDOS[i].ndet2,status) 
	 << endl;
  }
}

