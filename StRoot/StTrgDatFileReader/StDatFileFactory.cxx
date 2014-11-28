#include "StDatFileFactory.h"
#include "StTrgDatReader.h"

namespace {
	StDatFileFactory *singletonFactory = StDatFileFactory::CreateDatFactory();
}
//____________________________________________________________________________________
StDatFileFactory *StDatFileFactory::CreateDatFactory()
{
	return Factory() ? 0 : new StDatFileFactory();
}

//____________________________________________________________________________________
StStreamFile *StDatFileFactory::Create()
{
   return new StTrgDatReader();
}
//____________________________________________________________________________________
StStreamFile *StDatFileFactory::Create(const char *fileName, ios_base::openmode mode)
{
   return new StTrgDatReader(fileName,mode);
}
