//  storeEvent.cc
//  
///////////////////////////////////////////////////////////////////////////////
//
// storeEvent.cc
//
// Description: store StEvent data in a file
// Author List: Curtis Lansdell 7/99
// Notes: -> how to get filename, event number, number of events requested,
//           and number of events actually processed?
//
///////////////////////////////////////////////////////////////////////////////
#include "StEvent.h"
#include <fstream.h>

void storeEvent(StEvent &event, ofstream &fout, Int_t &nProc) {

  nProc++;
  fout << "Event: " << nProc << endl;
  fout << "  # tracks:         " << event.trackCollection()->size() << endl;
  fout << "  # vertices:       " << event.vertexCollection()->size() << endl;
  fout << "  # TPC hits:       " << event.tpcHitCollection()->size() << endl;
  fout << "  # SVT hits:       " << event.svtHitCollection()->size() << endl;
  fout << "  # FTPC hits:      " << event.ftpcHitCollection()->size() << endl;
  fout << "  primary vertex:   " << event.primaryVertex()->position() << endl;
  fout << endl;

  cout << "StEvent data from StAnalysisMaker" << endl;
  cout << "  # tracks:         " << event.trackCollection()->size() << endl;
  cout << "  # vertices:       " << event.vertexCollection()->size() << endl;
  cout << "  # TPC hits:       " << event.tpcHitCollection()->size() << endl;
  cout << "  # SVT hits:       " << event.svtHitCollection()->size() << endl;
  cout << "  # FTPC hits:      " << event.ftpcHitCollection()->size() << endl;
  cout << "  primary vertex:   " << event.primaryVertex()->position() << endl;
  cout << "Number of events processed so far: " << nProc << endl << endl;
}
