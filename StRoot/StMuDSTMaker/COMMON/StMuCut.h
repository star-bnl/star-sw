/***************************************************************************
 *
 * $Id: StMuCut.h,v 1.1 2002/03/08 17:04:17 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuCut_h
#define StMuCut_h

class StEvent;
class StTrack;
class StV0Vertex;
class StXiVertex;
class StKinkVertex;

class StMuCut{
 public:
  StMuCut() {};
  virtual ~StMuCut() {};
  
  bool pass( const StEvent*);
  bool pass( const StTrack*);
  bool pass( const StV0Vertex*);
  bool pass( const StXiVertex*);
  bool pass( const StKinkVertex*);
 private:
  virtual bool accept( const StEvent*) = 0;
  virtual bool accept( const StTrack*) = 0;
  virtual bool accept( const StV0Vertex*) = 0;
  virtual bool accept( const StXiVertex*) = 0;
  virtual bool accept( const StKinkVertex*) = 0;
  bool leave(bool);

  unsigned int mPass;
  unsigned int mFail;
};

inline bool StMuCut::leave(bool b) { (b) ? mPass++ : mFail++; return b; } 
inline bool StMuCut::pass( const StEvent* e) { return leave( accept(e) ); }
inline bool StMuCut::pass( const StTrack* t) { return leave( accept(t) ); }
inline bool StMuCut::pass( const StV0Vertex* v) { return leave( accept(v) ); }
inline bool StMuCut::pass( const StXiVertex* x) { return leave( accept(x) ); }
inline bool StMuCut::pass( const StKinkVertex* k) { return leave( accept(k) ); }



#endif

/***************************************************************************
 *
 * $Log: StMuCut.h,v $
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
