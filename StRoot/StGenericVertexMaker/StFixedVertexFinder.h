/*!
 *  \class StFixedVertexFinder
 *  \author Lee Barnby (University of Birmingham) May 2006.
 *  \brief StGenericVertexFinder implementation for fixing vertex.
 *
 *  $Id: StFixedVertexFinder.h,v 1.7 2018/03/24 20:10:41 jwebb Exp $
 *
 *  Modified J.Lauret for MC vertex
 *
 *  This vertex finder has two purposes
 *  - The first mode of operation, turned on by the VFFV chain option,
 *    allows to always set a vertex at a fixed position
 *  - The second mode of operation is turned on by the VFMCE chain option
 *    and allows instead to get the vertex from StMcEvent and not use a
 *    a fixed vertex
 *
 *  Within this "finder" (or faker really), the rank is set to -5 and
 *  the vertex finder Id is either respectively undefinedVertexFinder
 *  and mcEventVertexFFinder. Covariant matrix parameters are all set
 *  to zero for the lack of a better choice.
 */

#ifndef STAR_StFixedVertexFinder
#define STAR_StFixedVertexFinder


#include "StGenericVertexMaker/StGenericVertexFinder.h"

class StEvent;

class StFixedVertexFinder: public StGenericVertexFinder
{
public:
    StFixedVertexFinder();
    // should we have destructor too?
    
    // mandatory implementations
    int fit(StEvent*);
    void printInfo(ostream& = cout)const;

    
    // member not from base class
    void SetVertexPosition(double x, double y, double z);
    void SetVertexError( double ex, double ey, double ez );
    int  IsFixed() const 	{return 1;}
    
private:
    Double_t mFixedX; //!< X co-ordinate of vertex
    Double_t mFixedY; //!< Y co-ordinate of vertex
    Double_t mFixedZ; //!< Z co-ordinate of vertex

  double mFixedEx;
  double mFixedEy;
  double mFixedEz;

    /**
     * Vertex constraint not useful for this VF but is part of base class so implementation just
     * displays warning to this effect
     */
    virtual void UseVertexConstraint();

};

/***************************************************************************
*
* $Log: StFixedVertexFinder.h,v $
* Revision 1.7  2018/03/24 20:10:41  jwebb
*
* Added option for user to specify the uncertainties on the vertex.  Useful
* in embedding jobs in order to get the track association with primary
* vertex correct (especially when tracks are from precision tracking, eg
* HFT).
*
* Revision 1.6  2017/05/12 18:37:23  smirnovd
* Cosmetic changes
*
* Removed log messages from source files
* Prefixed included headers with paths to respective modules
*
* Revision 1.5  2016/08/18 17:46:14  smirnovd
* Squashed commit of the following refactoring changes:
*
* Date:   Wed Jul 27 18:31:18 2016 -0400
*
*     Removed unused arguments in UseVertexConstraint()
*
*     In StiPPVertexFinder and StvPPVertexFinder this method does nothing
*
* Date:   Wed Jul 27 16:47:58 2016 -0400
*
*     Make old UseVertexConstraint private virtual and call it from its public replacement in the base class
*
*     also mark methods as private explicitly
*
* Date:   Wed Jul 27 16:52:02 2016 -0400
*
*     Removed unused private data member mWeight
*
* Date:   Wed Jul 27 16:50:42 2016 -0400
*
*     Prefer base class static beamline parameters rather than this class private members
*
* Date:   Wed Jul 27 16:21:49 2016 -0400
*
*     StPPVertexFinder: Got rid of unused private beamline parameters
*
*     The equivalent measurements are available from the base class
*     StGenericVertexFinder
*
* Date:   Wed Jul 27 16:19:19 2016 -0400
*
*     StPPVertexFinder: For beamline position use equivalent static methods from parent class
*
* Date:   Wed Jul 27 16:05:50 2016 -0400
*
*     StGenericVertexMaker: Assigning once is enough
*
* Date:   Mon Aug 15 10:43:49 2016 -0400
*
*     StGenericVertexFinder: Print out beamline parameters
*
*     Print beamline values as extracted from the database before any modification.
*
* Date:   Wed Jul 6 15:33:02 2016 -0400
*
*     Stylistic changes and minor refactoring
*
*     Whitespace and comments for improved readability
*     s/track/stiKalmanTrack/
*
* Date:   Wed Jul 6 15:28:16 2016 -0400
*
*     StPPVertexFinder: Switched to cleaner c++11 range loop syntax
*
* Date:   Wed Jul 6 15:22:14 2016 -0400
*
*     StPPVertexFinder: Minor c++ refactoring
*
*     - Removed unused counter
*     - c-style array to std::array
*
* Date:   Wed Jul 6 15:20:11 2016 -0400
*
*     Deleted commented out code
*
*     Removed unused #include's StMinuitVertexFinder
*
* Revision 1.4  2015/11/13 04:11:40  perev
* Added metod IsFixed
*
* Revision 1.3  2006/05/18 19:14:24  lbarnby
* Added SetVertexPosition function. Tidied up comments/docs
*
* Revision 1.2  2006/05/10 14:35:00  jeromel
* Changed VertexId to new enum, added doxygen doc
*
* Revision 1.1  2006/05/03 22:11:10  lbarnby
* Initial version of fixed position vertex finder and option in maker to switch it on
*
*
**************************************************************************/
#endif
