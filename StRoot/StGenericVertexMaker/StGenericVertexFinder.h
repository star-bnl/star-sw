/*!
 * \class StGenericVertexFinder
 *
 * \author Lee Barnby, April 2003
 *
 * (pseudo) Base class for vertex finders
 *
 *
 * $Id: StGenericVertexFinder.h,v 1.29 2016/04/20 22:03:45 smirnovd Exp $
 */

#ifndef STAR_StGenericVertexFinder
#define STAR_StGenericVertexFinder
#include "StEnumerations.h"
#include "Stiostream.h"
#include <assert.h>
#include "StPrimaryVertex.h"
#include "tables/St_vertexSeed_Table.h"

class StEvent;
class StPrimaryVertex;
class StGenericVertexFinder {
 public:

  /// Options used to define the type of vertex fit performed in a concrete
  /// implementation
  enum class VertexFit_t : int { Unspecified, NoBeamline, Beamline1D, Beamline3D };

  // virtual and '=0' ; those MUST be implemented
  virtual ~StGenericVertexFinder();                           // virtual destructor
  virtual int            fit(StEvent*)=0;                     // fit the vertex

  StPrimaryVertex*       getVertex(int idx) const;
  void                   addVertex(StPrimaryVertex*);
  int                    size() const;
  virtual void           UseVertexConstraint(double, double, double, double, double)=0;
          void           UseVertexConstraint(const vertexSeed_st& beamline);
          void           NoVertexConstraint();
          int            IsVertexConstraint() const {return mVertexConstrain;}
  virtual void           UsePCT(bool usePCT = true);
  virtual void           UseBTOF(bool useBTOF = true){mUseBtof=useBTOF;}
  virtual void           UseCTB (bool useCTB  = true){mUseCtb =useCTB ;}
          void           setMC  (bool x=true)        {mIsMC   = x     ;}
  virtual void           CalibBeamLine(){ /* noop */;} // overload if useful

  virtual void           printInfo(ostream& = cout) const=0;

  // General (default)
  virtual void           SetMode(Int_t mode=0 ) {mMode = mode;}
  virtual int            GetMode() const 	{return mMode;}
          void           SetDebugLevel(Int_t level) {mDebugLevel=level;}
  virtual void           Init(){ /* noop */;}
  virtual void           Finish(){ /* noop */;}
  virtual void           InitRun  (int runumber){ /* noop */;}
  virtual void           Clear();
  const std::vector<StPrimaryVertex> *result() {return &mVertexList;} 
 
  void                   FillStEvent(StEvent*);
  virtual void SetVertexPosition(double x,double y,double z){assert(0);}
  virtual int            IsFixed() const 	{return 0;}
 protected: //................................

  StGenericVertexFinder(VertexFit_t fitMode=VertexFit_t::Unspecified);

 private:
  vector<StPrimaryVertex> mVertexList;      // Holds all found prim veritcess

 protected: //................................
  StPrimaryVertexOrder   mVertexOrderMethod; // will default to 0 i.e. orderByNumberOfDaughters
  bool                   mVertexConstrain;   // Use vertex constraint from db
  int                    mMode;              // used for any Finder behavior change

  /// The type of vertex fit to use in derived concrete implementation
  VertexFit_t            mVertexFitMode;

  int                    mDebugLevel;
  bool   		 mIsMC;              // flag minor differences between Data & M-C
  bool                   mUseBtof;           // default use btof = false
  bool                   mUseCtb;            // default use ctb = false

  /// Caclulates chi2 for the beamline and a point
  static double CalcBeamlineChi2(const StThreeVectorD& point);

  /// All measured parameters of the beamline. Updated whenever
  /// UseVertexConstraint(const vertexSeed_st&) is called
  static vertexSeed_st  sBeamline;
};

#endif
