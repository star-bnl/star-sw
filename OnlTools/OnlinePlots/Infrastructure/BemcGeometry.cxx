#include "BemcGeometry.h"



#include <iostream>
#include <fstream>
#include <stdlib.h>

#include "TEnv.h"

using namespace std;

BemcGeometry* BemcGeometry::_instance=0;

BemcGeometry* BemcGeometry::instance() {
  if ( !_instance ) {
    _instance = new BemcGeometry();
  }
  return _instance;
}

BemcGeometry::BemcGeometry() {
  char filename[1024];
  char line[1024];
  unsigned int id;
  float eta;
  float phi;
  sprintf(filename,"%s%s",gEnv->GetValue("Online.plotsDir","."),"/local/bemcGeom.dat");

  ifstream in(filename);
  while ( in.good() ) {
    in.getline(line,1024);
    if ( strstr(line,"#") ) continue;
    sscanf(line,"%d %f %f",&id,&eta,&phi);
    //cout << id << " " << eta <<" " << phi << endl;
    if ( id<=4800) {
      mEta[id] = eta;
      mPhi[id] = phi;
    }
  }
}

