/***************************************************************************
 *
 * $Id: StMcContainers.hh,v 2.4 2000/03/06 18:05:21 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Containers for StMcEvent objects
 *
 ***************************************************************************
 *
 * $Log: StMcContainers.hh,v $
 * Revision 2.4  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:00:58  calderon
 * Completely revised for new StEvent
 *
 **************************************************************************/
#ifndef StMcContainers_hh
#define StMcContainers_hh

#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif


class StMcHit;
class StMcVertex;
class StMcSvtHit;
class StMcTpcHit;
class StMcFtpcHit;
class StMcRichHit;
class StMcTrack;
   
#ifndef ST_NO_TEMPLATE_DEF_ARGS
// Owners
typedef vector<StMcHit*>     StSPtrVecMcHit;    //! 
typedef vector<StMcVertex*>  StSPtrVecMcVertex; //!
typedef vector<StMcSvtHit*>  StSPtrVecMcSvtHit; //!
typedef vector<StMcTpcHit*>  StSPtrVecMcTpcHit; //!
typedef vector<StMcFtpcHit*> StSPtrVecMcFtpcHit; //!
typedef vector<StMcRichHit*> StSPtrVecMcRichHit; //!
typedef vector<StMcTrack*>   StSPtrVecMcTrack;  //!
// Not owners
typedef vector<StMcVertex*>  StPtrVecMcVertex; //!
typedef vector<StMcSvtHit*>  StPtrVecMcSvtHit; //!
typedef vector<StMcTpcHit*>  StPtrVecMcTpcHit; //!
typedef vector<StMcFtpcHit*> StPtrVecMcFtpcHit; //!
typedef vector<StMcRichHit*> StPtrVecMcRichHit; //!
typedef vector<StMcTrack*>   StPtrVecMcTrack; //!
#else
//Owners
typedef vector<StMcHit*, allocator<StMcHit*> >          StSPtrVecMcHit; //!
typedef vector<StMcVertex*, allocator<StMcVertex*> >    StSPtrVecMcVertex; //!
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >    StSPtrVecMcSvtHit; //!
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >    StSPtrVecMcTpcHit; //!
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >  StSPtrVecMcFtpcHit; //!
typedef vector<StMcRichHit*, allocator<StMcRichHit*> >  StSPtrVecMcRichHit; //!
typedef vector<StMcTrack*, allocator<StMcTrack*> >      StSPtrVecMcTrack; //!
// Not owners
typedef vector<StMcVertex*, allocator<StMcVertex*> >    StPtrVecMcVertex; //!
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >    StPtrVecMcSvtHit; //!
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >    StPtrVecMcTpcHit; //!
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >  StPtrVecMcFtpcHit; //!
typedef vector<StMcRichHit*, allocator<StMcRichHit*> >  StPtrVecMcRichHit; //!
typedef vector<StMcTrack*, allocator<StMcTrack*> >      StPtrVecMcTrack; //!

#endif // no template default arguments
//Iterators
typedef StPtrVecMcVertex::iterator  StMcVertexIterator; //!
typedef StPtrVecMcSvtHit::iterator  StMcSvtHitIterator; //!
typedef StPtrVecMcTpcHit::iterator  StMcTpcHitIterator; //!
typedef StPtrVecMcFtpcHit::iterator StMcFtpcHitIterator; //!
typedef StPtrVecMcRichHit::iterator StMcRichHitIterator; //!
typedef StPtrVecMcTrack::iterator   StMcTrackIterator; //!
//Const Iterators
typedef StPtrVecMcVertex::const_iterator  StMcVertexConstIterator; //!
typedef StPtrVecMcSvtHit::const_iterator  StMcSvtHitConstIterator; //!
typedef StPtrVecMcTpcHit::const_iterator  StMcTpcHitConstIterator; //!
typedef StPtrVecMcFtpcHit::const_iterator StMcFtpcHitConstIterator; //!
typedef StPtrVecMcRichHit::const_iterator StMcRichHitConstIterator; //!
typedef StPtrVecMcTrack::const_iterator   StMcTrackConstIterator; //!

#endif //StMcContainers
