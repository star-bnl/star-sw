//StiIOBroker.cxx
//M.L. Miller (Yale Software)
//11/01

//std
#include <iostream.h>
using std::cout;
using std::endl;

//Sti
#include "StiIOBroker.h"
//StiMaker
#include "StiMaker/StiRootIOBroker.h"

StiIOBroker* StiIOBroker::sInstance=0;

StiIOBroker::StiIOBroker()
{
	cout <<"StiIOBroker::StiIOBroker()"<<endl;
}

StiIOBroker::~StiIOBroker()
{
	cout <<"StiIOBroker::~StiIOBroker()"<<endl;
}

StiIOBroker* StiIOBroker::instance()
{
    return (sInstance) ? sInstance : new StiRootIOBroker();
}

void StiIOBroker::kill()
{
	delete sInstance;
	sInstance = 0;
}
