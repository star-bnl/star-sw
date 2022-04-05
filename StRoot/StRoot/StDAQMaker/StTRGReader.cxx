/***************************************************************************
 *
 * $Id: StTRGReader.cxx,v 1.11 2009/11/25 20:50:08 fisyak Exp $
 *
 * Author: Herbert Ward, Dec 28 1999, 13:10 EST.
 ***************************************************************************
 *
 * Description: Offline Wrapper for TRG reader class
 *
 ***************************************************************************
 *
 * $Log: StTRGReader.cxx,v $
 * Revision 1.11  2009/11/25 20:50:08  fisyak
 * Fix the broken logic for the DAQ file with NO trigger data
 *
 * Revision 1.10  2009/11/02 21:42:52  fine
 * allow the Akio DAT file to be read in 1999-2008 format
 *
 * Revision 1.9  2007/11/19 19:40:10  akio
 * Change fro run8
 *
 * Revision 1.8  2007/05/29 22:12:19  fine
 * Introduce logger-based output
 *
 * Revision 1.7  2007/02/22 22:18:41  akio
 * Update for 2007
 *
 * Revision 1.6  2004/11/03 17:55:28  akio
 * update for fy05 run
 *
 * Revision 1.5  2004/08/07 02:39:00  perev
 * Traditional Clear added
 *
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
#include "StStreamFile.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define PP LOG_INFO << 

//_____________________________________________________________________________
StTRGReader::StTRGReader(StDAQReader *daqr) {
  fDAQReader = daqr;
  fTRGImpReader = 0;
  Update();
}
//_____________________________________________________________________________
void StTRGReader::Update() {
  delete fTRGImpReader; fTRGImpReader = 0;
  fTRGImpReader = ::getTRGReader(fDAQReader->getEventReader());
//VP  assert(fTRGImpReader);
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
  if(fTRGImpReader ||  
  ( fDAQReader->getDatFileReader() && OldFormat(fDAQReader->getDatFileReader()->Version()) )) return 7; // TRUE
  return 0;                   // FALSE
}
//_____________________________________________________________________________
const char *StTRGReader::getData() const
{
   const char *data = 0;
   if(fTRGImpReader) {
      data = (const char*)(fTRGImpReader->pBankTRGD)+40;
   } else if ( fDAQReader->getDatFileReader() ) {
      data = fDAQReader->getDatFileReader()->Record(); 
   }
   return data;
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
//_____________________________________________________________________________
const TrgDataType2005 *StTRGReader::getDataType2005() const
{
  if (getYear()!=2005) return 0;
  return (const TrgDataType2005*)getData();
}
//_____________________________________________________________________________
const TrgDataType2007 *StTRGReader::getDataType2007() const
{
  if (getYear()!=2007) return 0;
  return (const TrgDataType2007*)getData();
}
//_____________________________________________________________________________
const TrgDataType2008 *StTRGReader::getDataType2008() const
{
  if (getYear()!=2008) return 0;
  return (const TrgDataType2008*)getData();
}
 	
