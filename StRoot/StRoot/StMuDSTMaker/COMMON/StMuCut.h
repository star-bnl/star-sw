/***************************************************************************
 *
 * $Id: StMuCut.h,v 1.6 2004/11/24 19:46:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
/**
   @class StMuCut
   Abstract base class for cut objects
   Implementations will have to overwrite the abstract virtual but protected accept() functions.
   User code, however, will call the non-virtual pass() funstions which will call() accept and keep track of
   counters.
*/

#ifndef StMuCut_h
#define StMuCut_h

#include "TObject.h"

class StEvent;
class StTrack;
class StV0Vertex;
class StXiVertex;
class StKinkVertex;
class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;

class StMuCut : public TObject {
 public:
  StMuCut();
  virtual ~StMuCut() {};
  
  bool pass( const StEvent*      );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StTrack*      );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StV0Vertex*   );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StXiVertex*   );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StKinkVertex* );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StV0MuDst*    );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StXiMuDst*    );///< called by user code,  returns true if argument passes cuts, else false
  bool pass( const StKinkMuDst*  );///< called by user code,  returns true if argument passes cuts, else false
 protected:
  virtual bool accept( const StEvent*      ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StTrack*      ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StV0Vertex*   ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StXiVertex*   ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StKinkVertex* ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StV0MuDst*    ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StXiMuDst*    ) = 0;///< abstract cut function, has to be overwritten by derived class
  virtual bool accept( const StKinkMuDst*  ) = 0;///< abstract cut function, has to be overwritten by derived class

  bool leave(bool b, unsigned int counter[2]); ///< increment pass/fail counter (2nd argument) depending on 1st argument and return 1st argument

  // counters for passed and failed calls to cut
  unsigned int mNStEvent[2];    
  unsigned int mNStTrack[2];    
  unsigned int mNStV0Vertex[2]; 
  unsigned int mNStXiVertex[2]; 
  unsigned int mNStKinkVertex[2];
  unsigned int mNStV0MuDst[2];  
  unsigned int mNStXiMuDst[2];  
  unsigned int mNStKinkMuDst[2];

  ClassDef(StMuCut,1)
};

inline bool StMuCut::leave(bool b, unsigned int counter[2]) { (b) ? counter[0]++ : counter[1]++; return b; } 
inline bool StMuCut::pass( const StEvent* e) {        return leave( accept(e), mNStEvent      ); }
inline bool StMuCut::pass( const StTrack* t) {        return leave( accept(t), mNStTrack      ); }
inline bool StMuCut::pass( const StV0Vertex* v) {     return leave( accept(v), mNStV0Vertex   ); }
inline bool StMuCut::pass( const StXiVertex* x) {     return leave( accept(x), mNStXiVertex   ); }
inline bool StMuCut::pass( const StKinkVertex* k) {   return leave( accept(k), mNStKinkVertex ); }
inline bool StMuCut::pass( const StV0MuDst* v) {      return leave( accept(v), mNStV0MuDst    ); }
inline bool StMuCut::pass( const StXiMuDst* x) {      return leave( accept(x), mNStXiMuDst    ); }
inline bool StMuCut::pass( const StKinkMuDst* k) {    return leave( accept(k), mNStKinkMuDst  ); }



#endif

/***************************************************************************
 *
 * $Log: StMuCut.h,v $
 * Revision 1.6  2004/11/24 19:46:09  jeromel
 * Forgot .h file commit
 *
 * Revision 1.5  2004/05/02 04:10:13  perev
 * private => protected
 *
 * Revision 1.4  2003/09/10 22:33:41  perev
 * Grid for MuDst corrections
 *
 * Revision 1.3  2002/09/11 21:02:41  laue
 * added cut on track encoded method for ITTF
 *
 * Revision 1.2  2002/05/04 23:56:29  laue
 * some documentation added
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
