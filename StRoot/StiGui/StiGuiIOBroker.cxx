//StiGuiIOBroker.cxx
//M.L. Miller (Yale Software)
//11/01

//std
#include <iostream.h>
using std::cout;
using std::endl;

//StiGui
#include "StiGuiIOBroker.h"

StiGuiIOBroker* StiGuiIOBroker::sInstance=0;

ClassImp(StiGuiIOBroker)
    
StiGuiIOBroker::StiGuiIOBroker()
    : mUnMarkedHitSize(.3), mUnMarkedHitColor(4), mUnMarkedHitStyle(8),
      mMarkedHitSize(.3), mMarkedHitColor(2), mMarkedHitStyle(8)
{
    cout <<"StiGuiIOBroker::StiGuiIOBroker()"<<endl;
    sInstance=this;
}

StiGuiIOBroker::~StiGuiIOBroker()
{
    cout <<"StiGuiIOBroker::~StiGuiIOBroker()"<<endl;
}
