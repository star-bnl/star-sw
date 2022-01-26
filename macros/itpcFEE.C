/* from online/RTS/src/ITPC_SUPPORT/itpc_gains_txt.C
  root.exe -q -b itpcFEE.C+
*/
//#include <iostream>     // std::cout
#include <algorithm>    // std::sort
#include <vector>       // std::vector
#include "Riostream.h"
#include "TString.h"
#include "RTS/src/DAQ_ITPC/itpc_padplane.h"

void itpcFEE() {
  vector<int> rows[41];
  for (Int_t fee = 0; fee< 56; fee++) { 
    for (Int_t row = 0; row < 41; row++) rows[row].clear();
    Int_t p1 = 0, p2 = 0;
    Int_t row = 0;
    Int_t pad = 0;
    for (Int_t pin = 0; pin < 79; pin++) {
      row = itpc_padplane[fee][pin].row;
      pad = itpc_padplane[fee][pin].pad;
      if (! row || ! pad) continue;
      rows[row].push_back(pad);
      //      if (row == 4) cout << "fee = " << fee << "\tpin = " << pin << "\trow = " << row << "\tpad = " << pad << "\tsize = " << rows[row].size() << endl;
    }
    //    cout << "fee = " << fee << endl;
    for (Int_t r = 0; r < 41; r++) {
      if (rows[r].empty()) continue;
      sort(rows[r].begin(),rows[r].end());
      //      cout << "\trow " << r << "\tsize = " << rows[r].size() << "\tpads = ";
      p1 = p2 = 0;
      for (auto p : rows[r]) {
	//	if (r == 4)	cout << "r = " << r << "\tp = " << p << endl;
        if (p1 == 0) {
	  p1 = p2 = p;
	} else {
	  if (p == p2 + 1) p2 = p;
	  else {
	    //	    if (r == 4)	    cout << "{" << r << "," << p1 << "," << p2 << "," << fee << "}," << endl;
	    cout << Form("{%02i,%3i,%3i,%3i},", r, p1, p2, fee) << endl;
	    p1 = p2 = p;
	  }
	}
      }
      if (p1 <= p2) {
	cout << Form("{%02i,%3i,%3i,%3i},", r, p1, p2, fee) << endl;
      }
    }
    
  }
}
