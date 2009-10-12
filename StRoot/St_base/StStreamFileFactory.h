#ifndef  STAR_StStreamFileFactory
#define  STAR_StStreamFileFactory

#include <iostream>

using namespace std;

class StStreamFile;

class StStreamFileFactory {
	static StStreamFileFactory *fgStreamFactory;
protected:
	StStreamFileFactory();
public:
   virtual ~StStreamFileFactory();
	virtual StStreamFile *Create() = 0;
	virtual StStreamFile *Create(const char *fileName, ios_base::openmode mode = ios_base::in) = 0;
	static  StStreamFile *StreamFile();
	static  StStreamFile *StreamFile(const char *fileName, ios_base::openmode mode = ios_base::in);
	static StStreamFileFactory *Factory() { return fgStreamFactory; }
	static void  SetFactory(StStreamFileFactory *factory) { fgStreamFactory=factory; }
};

#endif
