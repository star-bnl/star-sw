/*!
 *  \class StFixedVertexFinder
 *  \author Lee Barnby (University of Birmingham) May 2006.
 *  \brief StGenericVertexFinder implementation for fixing vertex.
 *
 *  $Id: StFixedVertexFinder.h,v 1.4 2015/11/13 04:11:40 perev Exp $
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


#include "StGenericVertexFinder.h"

class StEvent;

class StFixedVertexFinder: public StGenericVertexFinder
{
public:
    StFixedVertexFinder();
    // should we have destructor too?
    
    // mandatory implementations
    int fit(StEvent*);
    void printInfo(ostream& = cout)const;
    /**
     * Vertex constraint not useful for this VF but is part of base class so implementation just
     * displays warning to this effect
     */
    void UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);

    
    // member not from base class
    void SetVertexPosition(double x, double y, double z);
    int  IsFixed() const 	{return 1;}
    
private:
    Double_t mFixedX; //!< X co-ordinate of vertex
    Double_t mFixedY; //!< Y co-ordinate of vertex
    Double_t mFixedZ; //!< Z co-ordinate of vertex

};

/***************************************************************************
*
* $Log: StFixedVertexFinder.h,v $
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
