/***************************************************************************
 *
 * $Id: StTRGReader.cxx,v 1.1 2000/01/24 20:35:37 ward Exp $
 *
 * Author: Herbert Ward, Dec 28 1999, 13:10 EST.
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.cxx,v $
 * Revision 1.1  2000/01/24 20:35:37  ward
 * Access trigger data.
 *
 *
 **************************************************************************/
#include "StTRGReader.h"
#include "Stypes.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQReader.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define PP printf(

StTRGReader::StTRGReader(StDAQReader *daqr) {
  fDAQReader = daqr;
  fTRGImpReader = 0;
  Update();
}
void StTRGReader::Update() {
  delete fTRGImpReader;
  fTRGImpReader = ::getTRGReader(fDAQReader->fEventReader);
  assert(fTRGImpReader);
}
StTRGReader::~StTRGReader() {
  close();
}
int StTRGReader::close() {
  return 0;
}
char StTRGReader::thereIsTriggerData() {
  if(fTRGImpReader) return 7; // TRUE
  return 0;                   // FALSE
}
