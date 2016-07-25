#include "Rtypes.h"
#include "Riostream.h"
using namespace std;
void Concrete() {
  //"Stand.Concrete"         9     2.35
  struct comp_t {
    const Char_t *name;
    Double_t      w;
    Double_t      A;
    Double_t      Z;
    Double_t      v;
  };
  comp_t comp[9] = {
    {"Hydrogen",          0.6,  1.00794, 1, 0},            
    {"Carbon",            3.0, 12.011,   6, 0},
    {"Oxygen",           50.0, 15.999,   8, 0},
    {"Sodium",            1.0, 22.990,  11, 0},
    {"Aluminium",         3.0, 26.980,  13, 0},
    {"Silicon",          20.0, 28.090,  14, 0},
    {"Potassium",         1.0, 39.098,  19, 0},
    {"Calcium",          20.0, 40.078,  20, 0},
    {"Iron",              1.4, 55.850,  26, 0}
  };
  Double_t vol = 0;
  for (Int_t i = 0; i < 9; i++) {
    vol += comp[i].w/comp[i].A;
  }
  cout << "<Mixture name=\"Concrete\" dens=\"2.35\"  >" << endl;

  for (Int_t i = 0; i < 9; i++) {
    //    comp[i].v = 100*comp[i].w/comp[i].A/vol;
    comp[i].w /= 100;
    cout << "<Component name=\"" << comp[i].name << " \" a=\"" << comp[i].A << "\" z=\"" << comp[i].Z << "\" w=\"" << comp[i].w << "\"  />" <<  endl;
  }
  cout << "</Mixture>" << endl;
}
