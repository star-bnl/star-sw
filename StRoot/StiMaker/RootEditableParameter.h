#ifndef RootEditableParameter_H
#define RootEditableParameter_H
#include <TGNumberEntry.h>
#include <TGLabel.h>
#include "Sti/EditableParameter.h"

class RootEditableParameter : public EditableParameter
{
 public:

  RootEditableParameter();
  RootEditableParameter(const string & name, 
			const string & description,
			double value, 
			double defaultValue, 
			double min, 
			double max,
			double increment,
			int    type);
  RootEditableParameter(const string & name, const string & description);
  ~RootEditableParameter();
  
  TGCheckButton * getCheckButton() const;
  TGNumberEntry * getNumberEntry() const;
  void setNumberEntry(TGNumberEntry * numberEntry);
  void setCheckButton(TGCheckButton* checkButton);

 protected:
  
  TGNumberEntry * _numberEntry;
  TGCheckButton * _checkButton;
};

inline TGCheckButton * RootEditableParameter::getCheckButton() const
{
  return _checkButton;
}

inline TGNumberEntry * RootEditableParameter::getNumberEntry() const
{
  return _numberEntry;
}

inline void RootEditableParameter::setNumberEntry(TGNumberEntry * numberEntry)
{
  _numberEntry = numberEntry;
}

inline void RootEditableParameter::setCheckButton(TGCheckButton* checkButton)
{
  _checkButton = checkButton;
}

/*! ConstrainedParameter factory
 */
class RootEditableParameterFactory : public EditableParameterFactory
{
 public:
  ///This is the only constructor available.
  RootEditableParameterFactory(const string& newName, 
			int original=-1, int 
			incremental=-1, 
			int maxInc=-1);
  ///Default destructor.
  virtual ~RootEditableParameterFactory();
  
 protected:
  ///Return a pointer to a new Parameter object on the heap.
  virtual void* makeNewObject() const
    {
      return new RootEditableParameter();
    }
    
private:
    RootEditableParameterFactory(); // no imp
};

#endif
