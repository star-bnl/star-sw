//
//

//
//
/*!
 \author Anselm Vossen (avossen@indiana.edu) 
  \class StFgtIPointAlgo
Abstract base class for point algorithm implementation.
Provides prototype for:
 makePoints function, called to construct points from a given hit collection that are to be placed in the given point collectin
Init() which is called when the parent makers Init function is called



*/
#ifndef STAR_StFgtIPointAlgo_HH
#define STAR_StFgtIPointAlgo_HH

#include "Stypes.h"
#include "TNamed.h"

class StFgtHitCollection;
class StFgtPointCollection; 
class StFgtCollection;

class StFgtIPointAlgo: public TNamed
{
 public:
  //subclasses must implement this function that takes raw hits from StEvent and fills the Point collection
  //  virtual Int_t makePoints( const StFgtHitCollection&, StFgtPointCollection& )=0;
  StFgtIPointAlgo(const char* name, const char* title):TNamed(name, title)
    {

    }
  virtual Int_t makePoints( StFgtCollection&)=0;
  virtual Int_t Init()=0;
  
};

#endif

/*
 * $Id: StFgtIPointAlgo.h,v 1.1 2013/03/13 20:36:28 jeromel Exp $ 
 * $Log: StFgtIPointAlgo.h,v $
 * Revision 1.1  2013/03/13 20:36:28  jeromel
 * Initial revision, Anselm Vossen
 *
 * Revision 1.4  2011/11/01 18:48:34  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.3  2011/10/28 14:41:27  sgliske
 * Changed to get StFgtEvent from StEvent rather than another maker.
 * Also pPointrAlgo changed to mPointerAlgoPtr to conform with STAR guidelines.
 *
 */
