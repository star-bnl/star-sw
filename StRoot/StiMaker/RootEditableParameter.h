#ifndef RootEditableParameter_H
#define RootEditableParameter_H
#include <TGNumberEntry.h>
#include <TGLabel.h>
#include "Sti/Base/EditableParameter.h"

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
			int    type,
			int    key);
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

#endif
