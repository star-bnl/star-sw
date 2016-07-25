#include <map>
#include "Riostream.h"
void mapT() {
  map<Int_t,Int_t> R;
  cout << "R[0] = " << R[0] << " size = " << R.size() << endl;
  R[0] = 1;
  cout << "R[0] = " << R[0] << " size = " << R.size() << endl;
}
