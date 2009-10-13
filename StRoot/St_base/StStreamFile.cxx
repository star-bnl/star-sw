// $Id: StStreamFile.cxx,v 1.2 2009/10/13 15:44:59 fine Exp $
//
// $Log: StStreamFile.cxx,v $
// Revision 1.2  2009/10/13 15:44:59  fine
// add the method to provide the error message
//
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
#include <iostream>

//__________________________________________________________________________
StStreamFile::~StStreamFile() { close();  }
//__________________________________________________________________________
void StStreamFile::open(const char *fileName, ios_base::openmode mode)
{
   if (fFilename==fileName && is_open() ) {
      fStream.clear();   
      fStream.seekg (0, ios::beg);
   } else {
      close();
      fFilename=fileName;
      fStream.clear();   
      fStream.open(fFilename.c_str(),mode); 
   }
   if (Debug()) Perror("StStreamFile::open");
}

//__________________________________________________________________________
void StStreamFile::close()
{
   if (is_open()) { 
     fStream.clear();
     fStream.close();
   }
}
//__________________________________________________________________________
istream &StStreamFile::read(char *s, streamsize n)
{
   fStream.clear();   
   fStream.read(s,n);
   if (Debug()) Perror("StStreamFile::read");
   return fStream;
}
//__________________________________________________________________________
void StStreamFile::Perror(const char *header) const
{
   if (!good()) {
      if (header && header[0]) std::cerr << header << ": ";
      std::cerr << " StStreamFile I/O error: "   
         << " fail=" << fail()
         << "; bad="  << bad()
         << "; eof=" << eof() 
         << " for <" << fFilename <<">  " 
         << " open=" << ((StStreamFile *)this)->is_open() 
         <<  std::endl;
   } else if (Debug()) {
     if (header && header[0]) std::cerr << header << ": ";
     std::cerr << "There was no I/O Error" 
               << " for <" << fFilename <<">  " 
               << std::endl;
   }
}
