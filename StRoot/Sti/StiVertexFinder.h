#ifndef StiVertexFinder_H
#define StiVertexFinder_H 1

class StEvent;
class StiHit;

/*!
An abstract class defining  the interface to the vertex finder.
*/
class StiVertexFinder 
{
public:
  
  /// Find the vertex(es) associated with the given event
  virtual StiHit * findVertex(StEvent * event)=0;
};


#endif
