/***************************************************************************
 *
 * $Id: StarMuCut.h,v 1.1 2002/03/05 15:41:08 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuCut_h
#define StarMuCut_h

class StEvent;
class StTrack;
class StV0Vertex;
class StXiVertex;
class StKinkVertex;

class StarMuCut{
 public:
  StarMuCut() {};
  virtual ~StarMuCut() {};
  
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

inline bool StarMuCut::leave(bool b) { (b) ? mPass++ : mFail++; return b; } 
inline bool StarMuCut::pass( const StEvent* e) { return leave( accept(e) ); }
inline bool StarMuCut::pass( const StTrack* t) { return leave( accept(t) ); }
inline bool StarMuCut::pass( const StV0Vertex* v) { return leave( accept(v) ); }
inline bool StarMuCut::pass( const StXiVertex* x) { return leave( accept(x) ); }
inline bool StarMuCut::pass( const StKinkVertex* k) { return leave( accept(k) ); }



#endif

/***************************************************************************
 *
 * $Log: StarMuCut.h,v $
 * Revision 1.1  2002/03/05 15:41:08  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
