//StiDrawable.h
//M.L. Miller (Yale Software)
//04/01

//abstract base class for an sti displayable class

#ifndef StiDrawable_HH
#define StiDrawable_HH

#include <string>
using std::string;

#include "StThreeVector.hh"

class StiDrawable
{
public:
    StiDrawable();
    virtual ~StiDrawable();

    virtual void draw() = 0;
    virtual void update() = 0;
    virtual void setColor(int val) = 0;
    virtual void setVisibility(bool val) = 0;

    void setName(const string&);
    const string& name() const;

    ///Can it be removed from the DisplayManager each event?
    void setRemoved(bool);
    bool canBeRemoved() const;
    ///Has it been added to the DisplayManager for this event?
    bool isAdded() const;
    void setIsAdded(bool);
    
    const StThreeVector<double>& position() {return mposition;}
    
protected:
    StThreeVector<double> mposition; //Global position of center of volume
    bool mremoved_each_event;
    bool mIsAddedToDisplay;
    string mName;
};

inline bool StiDrawable::isAdded() const
{
    return mIsAddedToDisplay;
}

inline void StiDrawable::setIsAdded(bool val)
{
    mIsAddedToDisplay=val;
}

inline void StiDrawable::setName(const string& val)
{
    mName = val;
}

inline const string& StiDrawable::name() const
{
    return mName;
}

inline bool StiDrawable::canBeRemoved() const
{
    return mremoved_each_event;
}

inline void StiDrawable::setRemoved(bool val)
{
    mremoved_each_event = val;
}

#endif
