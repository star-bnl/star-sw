/***************************************************************************
 *
 * $Id: StTRGReader.cxx,v 1.4 2003/12/23 23:17:10 akio Exp $
 *
 * Author: Herbert Ward, Dec 28 1999, 13:10 EST.
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.cxx,v $
 * Revision 1.4  2003/12/23 23:17:10  akio
 * Update for Fy04 run
 *
 * Revision 1.3  2003/07/16 19:58:29  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 1.2  2000/06/12 15:04:02  perev
 * SVT + cleanup
 *
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

//_____________________________________________________________________________
StTRGReader::StTRGReader(StDAQReader *daqr) {
  fDAQReader = daqr;
  fTRGImpReader = 0;
  Update();
}
//_____________________________________________________________________________
void StTRGReader::Update() {
  delete fTRGImpReader;
  fTRGImpReader = ::getTRGReader(fDAQReader->getEventReader());
  assert(fTRGImpReader);
}
//_____________________________________________________________________________
StTRGReader::~StTRGReader() {
  close();
}
//_____________________________________________________________________________
int StTRGReader::close() {
  delete fTRGImpReader; fTRGImpReader=0;
  return 0;
}
//_____________________________________________________________________________
char StTRGReader::thereIsTriggerData() {
  if(fTRGImpReader) return 7; // TRUE
  return 0;                   // FALSE
}
//_____________________________________________________________________________
const char *StTRGReader::getData() const
{
  if(!fTRGImpReader) return 0;
  return (const char*)(fTRGImpReader->pBankTRGD)+40;

}
//_____________________________________________________________________________
int   StTRGReader::getYear() const
{
   char *dat = (char*)getData(); 
   if(!dat) return 0;
   return fTRGImpReader->YearOfData(dat);
}
//_____________________________________________________________________________
const TrgDataType2000 *StTRGReader::getDataType2000() const
{return 0;}
//_____________________________________________________________________________
const TrgDataType2003 *StTRGReader::getDataType2003() const
{
  if (getYear()!=2003) return 0;
  return (const TrgDataType2003*)getData();
}
//_____________________________________________________________________________
const TrgDataType2004 *StTRGReader::getDataType2004() const
{
  if (getYear()!=2004) return 0;
  return (const TrgDataType2004*)getData();
}
 	
