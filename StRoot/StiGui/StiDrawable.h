///\file StiDrawable.cxx
///\author M.L. Miller (Yale Software)
///\author Claude A Pruneau, Wayne State U.
///\date 04/2001
#ifndef StiDrawable_H_INCLUDED
#define StiDrawable_H_INCLUDED
///\class StiDrawable
///Class defining the notion of being drawbale for Sti purposes.
///Properties included a color, style, size, and visibility.
///Action are draw, update, and rest.
class StiDrawable
{
public:
    StiDrawable();
    virtual ~StiDrawable();
    virtual void draw() = 0;
    virtual void reset() = 0;
    virtual void setColor(int val) = 0;
    virtual void setStyle(int val) = 0;
    virtual void setSize(double val) = 0;
    virtual void setVisible(bool val) = 0;
};

#endif
