/*!
 * \class StEstPrimaryTrack 
 * \author Thomas Ullrich, Mar 2002
 */
/***************************************************************************
 *
 * $Id: StEstPrimaryTrack.h,v 2.1 2002/04/19 13:28:34 jeromel Exp $
 *
 * Author: Thomas Ullrich, Mar 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEstPrimaryTrack.h,v $
 * Revision 2.1  2002/04/19 13:28:34  jeromel
 * New include for SVT 2 tables scheme support. Forgot about it yesterday ...
 *
 **************************************************************************/
#ifndef StEstPrimaryTrack_hh
#define StEstPrimaryTrack_hh

#include "StPrimaryTrack.h"
class StPrimaryVertex;

class StEstPrimaryTrack : public StPrimaryTrack {
public:
    StEstPrimaryTrack();
    StEstPrimaryTrack(const dst_track_st&);
    StEstPrimaryTrack(const StEstPrimaryTrack&);
    StEstPrimaryTrack& operator=(const StEstPrimaryTrack&);
    ~StEstPrimaryTrack();

    StTrackType type() const;

protected:
    StObject*   clone() const;
    
private:
//  StPrimaryVertex*         	mVertex; 	//$LINK
#ifdef __CINT__
    StObjLink  		     	mVertex; 	
#else
    StLink<StPrimaryVertex>  	mVertex; 	
#endif //__CINT__
    
    ClassDef(StEstPrimaryTrack,1)
};
#endif
