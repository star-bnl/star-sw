#include "StStreamFileFactory.h"
#include "StStreamFile.h"

StStreamFileFactory *StStreamFileFactory::fgStreamFactory=0;
//____________________________________________________________________________________
StStreamFileFactory::StStreamFileFactory()
{
	if (!Factory()) SetFactory(this);
}

//____________________________________________________________________________________
StStreamFileFactory::~StStreamFileFactory(){ if (Factory() == this) SetFactory(0); }

//____________________________________________________________________________________
StStreamFile *StStreamFileFactory::StreamFile()
{
	return Factory() ? Factory()->Create() :0;
}
//____________________________________________________________________________________
StStreamFile *StStreamFileFactory::StreamFile(const char *fileName, ios_base::openmode mode)
{
	return Factory() ? Factory()->Create(fileName,mode): 0;
}
