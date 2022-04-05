#ifndef  STAR_StDatFileFactory
#define  STAR_StDatFileFactory

#include "StStreamFileFactory.h"

class StDatFileFactory : public StStreamFileFactory {
protected:
	StDatFileFactory(){;}
public:
	virtual ~StDatFileFactory(){;}
	virtual StStreamFile *Create();
	virtual StStreamFile *Create(const char *fileName, ios_base::openmode mode = ios_base::in);
	static StDatFileFactory *CreateDatFactory();
};

#endif
