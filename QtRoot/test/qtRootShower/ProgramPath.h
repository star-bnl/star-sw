#ifndef ROOT_PROGRAMPATH
#define ROOT_PROGRAMPATH

#include "TSystem.h"
#include <qstring.h>

inline QString ProgramPath(const char *fileName="",const char *module="libRootShower") 
{  
  // figure out which directory RootShower.DLL (share library) came from
  // return the full path for the file fileName in respect of the directory 
  // that the RootShower share library (DLL) was loaded from

  QString fullName;
  char *path = gSystem->DynamicPathName(module);
  if (path && path[0]) {
	  const char *dirName = gSystem->DirName(path);
	  if (fileName && fileName[0] ) {
		  fullName = fileName;
		  if (dirName && dirName[0] ) {
			  char *fullFileName = gSystem->ConcatFileName(dirName,fileName);
			  fullName = fullFileName;
			  //fprintf(stderr," ProgramPath fullFileName=%s \n", fullFileName);
			  // delete [] fullFileName;
		  }
	  } else {
		  fullName = path;
	  }
  }
  // fprintf(stderr," ProgramPath path=%s \n", path);
  // delete [] path;
  return fullName;
}

#endif
