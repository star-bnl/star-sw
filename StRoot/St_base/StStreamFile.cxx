// $Id: StStreamFile.cxx,v 1.1 2009/10/12 04:21:06 fine Exp $
//
// $Log: StStreamFile.cxx,v $
// Revision 1.1  2009/10/12 04:21:06  fine
// Add abstract STAR iostream-base file interface
//
// 

/***************************************************************************
 * \Author: Valeri Fine (fine@bnl.gov)
   \date 10-Oct-2009
 * Description: Trigger Data file (run*.*.dat) reader
 **************************************************************************/

#include "StStreamFile.h"

//__________________________________________________________________________
StStreamFile::~StStreamFile() { close(); }
//__________________________________________________________________________
void StStreamFile::open(const char *fileName, ios_base::openmode mode)
{
   fStream.open(fileName,mode);
}

//__________________________________________________________________________
void StStreamFile::close()
{
   if (is_open()) fStream.close();
}
//__________________________________________________________________________
istream &StStreamFile::read(char *s, streamsize n)
{
   return fStream.read(s,n);
}
