//StiDrawableTrack.h
/***********************************************************************
 *
 * $Id: StiDrawableTrack.h,v 2.2 2003/01/24 06:10:40 pruneau Exp $
 *
 * \class StiDrawableTrack
 *   Abstract base class used to define the interface to drawable tracks
 *
 * \author Claude A Pruneau
 *
 ***********************************************************************
 *
 * $Log: StiDrawableTrack.h,v $
 * Revision 2.2  2003/01/24 06:10:40  pruneau
 * removing centralized io
 *
 * Revision 2.1  2002/12/18 16:15:00  andrewar
 * Fixed reference to SubjectObserver.h to adhere to new directory structure.
 */

#ifndef StiDrawableTrack_HH
#define StiDrawableTrack_HH 1
class StiDrawableTrack 
{
public:
    StiDrawableTrack();
    virtual ~StiDrawableTrack();
    virtual void fillHitsForDrawing()=0;
    virtual void update();
    virtual void reset()=0;
};

inline void StiDrawableTrack::update()
{
	fillHitsForDrawing();
}
#endif
