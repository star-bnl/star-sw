#include "StiMaker/RootEditableParameter.h"

RootEditableParameter::RootEditableParameter()
  : EditableParameter(),
    _numberEntry(0),
    _checkButton(0)
{}

RootEditableParameter::RootEditableParameter(const string & name, 
					     const string & description,
					     double value, 
					     double defaultValue, 
					     double min, 
					     double max,
					     double increment,
					     int    type,
					     int    key)
  : EditableParameter(name,description,value,defaultValue,min,max,increment,type,key),
    _numberEntry(0),
    _checkButton(0)
{}

RootEditableParameter::~RootEditableParameter()
{
  delete _numberEntry;
  delete _checkButton;
}

RootEditableParameterFactory::RootEditableParameterFactory(const string& newName,
							   int original,
							   int incremental, 
							   int maxInc)
  : EditableParameterFactory(newName, 
			     original, 
			     incremental, 
			     maxInc)
{
  initialize();
}

RootEditableParameterFactory::~RootEditableParameterFactory()
{
  // cout <<"RootEditableParameterFactory::~RootEditableParameterFactory()"<<endl;
}
