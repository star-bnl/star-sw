#include "Rtypes.h"
#include "Riostream.h"
typedef UChar_t u_char;
#include "RTS/include/TPX/tpx_altro_to_pad.h"
//________________________________________________________________________________
Int_t RdoFromRowPad(Int_t row, Int_t pad) {
  static Int_t RowPa2RdoMap[45][182] = {0};
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
void rdoFromRowPad() {
  for (Int_t row = 1; row <= 45; row++) {
    for (Int_t pad = 1; pad <= 182; pad++) {
      Int_t rdo = RdoFromRowPad(row,pad);
      if (! rdo) break;
      cout << "row/pad = " << row << "/" << pad << " rdo = " << rdo << endl;
    }
  }
}
