//StiIOBroker.cxx
//M.L. Miller (Yale Software)
//11/01

//std
#include <iostream.h>
using std::cout;
using std::endl;

//StiMaker
#include "StiMaker/StiRootIOBroker.h"

//Sti
#include "StiIOBroker.h"

StiIOBroker* StiIOBroker::sInstance=0;

StiIOBroker::StiIOBroker()
{
    cout <<"StiIOBroker::StiIOBroker()"<<endl;
    sInstance=this;
}

StiIOBroker::~StiIOBroker()
{
    cout <<"StiIOBroker::~StiIOBroker()"<<endl;
}

StiIOBroker* StiIOBroker::instance()
{
    return (sInstance) ? sInstance : new StiRootIOBroker();
}
