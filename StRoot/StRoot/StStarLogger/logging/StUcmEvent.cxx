#include "StUcmEvent.h"
using namespace TxLogging;
StUcmEvent::StUcmEvent()  {;}
StUcmEvent::~StUcmEvent() {;}
const FieldList &StUcmEvent::getFields() const
{    return StRecord::getFields();   }
FieldList &StUcmEvent::getFields()
{   return StRecord::getFields();   }