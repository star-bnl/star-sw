//StiGuiIOBroker.h
//M.L. Miller (Yale Software)
//11/01

/*!
  \class StiGuiIOBroker
  StiGuiIOBroker is a simple class that defines a central point for user
  access to get/set methods for RootDrawable objects. It is available
  at the cint level, so it provides a dynamic gateway to perform methods
  such as setting the point size, track line width, etc.

  \author M.L. Miller (Yale Software)

  \note Implemented as a singleton.
  
 */

#ifndef StiGuiIOBroker_HH
#define StiGuiIOBroker_HH

#include "TObject.h"

class StiGuiIOBroker
{
public:
    static StiGuiIOBroker* instance();
    // static void kill();

    //We'll list the get/sets grouped together by attribute

    ///UnMarked Hits are hits not assigned to tracks
    void setUnMarkedHitSize(double val);
    double unMarkedHitSize() const;
    
    void setUnMarkedHitColor(unsigned int val);
    unsigned int unMarkedHitColor() const;

    void setUnMarkedHitStyle(unsigned int);
    unsigned int unMarkedHitStyle() const;
    
    //Marked Hits are hits assigned to tracks
    void setMarkedHitSize(double val);
    double markedHitSize() const;
    
    void setMarkedHitColor(unsigned int val);
    unsigned int markedHitColor() const;
    
    void setMarkedHitStyle(unsigned int);
    unsigned int markedHitStyle() const;

    friend class nobody;
    
private:
    StiGuiIOBroker();
    virtual ~StiGuiIOBroker();

    static StiGuiIOBroker* sInstance;
    
private:

    double mUnMarkedHitSize;
    unsigned int mUnMarkedHitColor;
    unsigned int mUnMarkedHitStyle;
    
    double mMarkedHitSize;
    unsigned int mMarkedHitColor;
    unsigned int mMarkedHitStyle;
    
    ClassDef(StiGuiIOBroker, 1)
};

inline StiGuiIOBroker* StiGuiIOBroker::instance()
{
    return (sInstance) ? sInstance : new StiGuiIOBroker();
}

inline void StiGuiIOBroker::setUnMarkedHitSize(double val)
{
    mUnMarkedHitSize=val;
}

inline double StiGuiIOBroker::unMarkedHitSize() const
{
    return mUnMarkedHitSize;
}

inline void StiGuiIOBroker::setUnMarkedHitColor(unsigned int val)
{
    mUnMarkedHitColor=val;
}

inline unsigned int StiGuiIOBroker::unMarkedHitColor() const
{
    return mUnMarkedHitColor;
}

inline void StiGuiIOBroker::setUnMarkedHitStyle(unsigned int val)
{
    mUnMarkedHitStyle=val;
}

inline unsigned int StiGuiIOBroker::unMarkedHitStyle() const
{
    return mUnMarkedHitStyle;
}

inline void StiGuiIOBroker::setMarkedHitSize(double val)
{
    mMarkedHitSize=val;
}

inline double StiGuiIOBroker::markedHitSize() const
{
    return mMarkedHitSize;
}

inline void StiGuiIOBroker::setMarkedHitColor(unsigned int val)
{
    mMarkedHitColor=val;
}

inline unsigned int StiGuiIOBroker::markedHitColor() const
{
    return mMarkedHitColor;
}

inline void StiGuiIOBroker::setMarkedHitStyle(unsigned int val)
{
    mMarkedHitStyle=val;
}
    
inline unsigned int StiGuiIOBroker::markedHitStyle() const
{
    return mMarkedHitStyle;
}

#endif
