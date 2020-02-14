/*
  root.exe 'Load.C("RTS")' rdoFromRowPad.C+
 */
#include "Rtypes.h"
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"
typedef UChar_t u_char;
#include "RTS/include/TPX/tpx_altro_to_pad.h"
//#include "RTS/src/DAQ_ITPC/itpcCore.h"
extern int *tpx_altro_to_row_override ;
extern int tpx_fy16_map;
extern void tpx_to_altro(int row, int pad, int &rdo, int &a, int &ch);
extern void tpx_from_altro(int rdo, int a, int ch, int &row, int &pad);
static Int_t idx = 0;
#if 0
extern void itpc_rowpad_to_rdo(int row, int pad, int &rdo);
//________________________________________________________________________________
Int_t itpcRdoFromRowPad(Int_t row, Int_t pad) {
  static Int_t RowPa2RdoMap[40][182] = {0};
  if (RowPa2RdoMap[0][0] == 0) {
    for (Int_t rowC = 1; rowC <= 40; rowC++) {
      for (Int_t padC = 1; padC <= 182; padC++) {
	Int_t rdo = 0;
	itpc_rowpad_to_rd(rowC,padC,rdo);
	RowPa2RdoMap[rowC-1][padC-1] = do;
      }
    }
  }
  return RowPa2RdoMap[row-1][pad-1];
}
#endif
//________________________________________________________________________________
Int_t RdoFromRowPad(Int_t row, Int_t pad) {
  static Int_t RowPa2RdoMap[72][182] = {0};
  if (RowPa2RdoMap[0][0] == 0) {
    for (Int_t rdo = 1; rdo <= 6; rdo++) {
      for (Int_t altro = 0; altro < 256; altro++) {
	for (Int_t ch = 0; ch < 16; ch++) {
	  Int_t rowC = tpx_altro_to_pad[rdo-1][altro][ch].row;
	  Int_t padC = tpx_altro_to_pad[rdo-1][altro][ch].pad;
	  if (rowC == 255 || padC == 255) continue;
	  if (rowC ==   0 || padC ==   0) continue;
	  if (RowPa2RdoMap[rowC-1][padC-1]) {
	    cout << "OverWrite RowPa2RdoMap[" << rowC - 1 << "][" << padC - 1 << "] = " << RowPa2RdoMap[rowC-1][padC-1] << " with rdo " << rdo << endl;
	  }
	  RowPa2RdoMap[rowC-1][padC-1] = rdo;
	}
      }
    }
  }
  return RowPa2RdoMap[row-1][pad-1];
}
//________________________________________________________________________________
void Print(Int_t row, Int_t pmin, Int_t pmax, Int_t rdo) {
  if (! idx) {
    cout << "  tpcRDOMap_st data[???] = {" << endl << "  //idx, nrows, row, padMin, padMax, rdo" << endl; 
  }
  idx++;
  //  cout << "row\t" << row << "\tpad[min,max]\t" << pmin << "\t" << pmax << "\trdo\t" << rdo_old  << endl;
  cout << Form("  {%5i,  ???, %3i, %6i, %6i, %3i},",idx, row, pmin, pmax, rdo) << endl;
}
//________________________________________________________________________________
void rdoFromRowPad() {
  tpx_fy16_map = 1;
  idx = 0;
  for (int row = 1; row <= 72; row++) {
    Int_t pmin = 999, pmax = -1;
    Int_t rdo_old = -1;
    for (int pad = 1; pad <= 182; pad++) {
      int rdo, a, ch;
      tpx_to_altro(row, pad, rdo, a, ch);
#if 0
      if (row >= 44) {
	cout << "row\t" << row << "\tpad\t" << pad << "\trdo\t" << rdo << "\taltro\t" << a << "\tchannel\t" << ch << endl;
      }
#endif
      //      if (row != 7 && row != 8) continue;
      //      if (! a ) continue;
      if (rdo != rdo_old && rdo_old > 0) {
	//	cout << "row\t" << row << "\tpad[min,max]\t" << pmin << "\t" << pmax << "\trdo\t" << rdo_old << endl; //"\taltro\t" << a << "\tchannel\t" << ch << endl;
	Print(row,pmin,pmax,rdo_old);
	pmin = 999; pmax = -1;
      }
      rdo_old = rdo;
      pmin = TMath::Min(pad, pmin);
      pmax = TMath::Max(pad, pmax);
    }
    if (rdo_old > 0) {
      //      cout << "row\t" << row << "\tpad[min,max]\t" << pmin << "\t" << pmax << "\trdo\t" << rdo_old  << endl;
      Print(row,pmin,pmax,rdo_old);
      pmin = 999; pmax = -1;
    }
  }
}
