/*!
 * \class StGenericVertexFinder
 *
 * \author Lee Barnby, April 2003
 *
 * (pseudo) Base class for vertex finders
 *
 *
 * $Id: StGenericVertexFinder.h,v 1.44 2016/12/12 16:42:30 smirnovd Exp $
 */

#ifndef STAR_StGenericVertexFinder
#define STAR_StGenericVertexFinder

#include <vector>

//#include "StEventTypes.h"
#include "StPrimaryVertex.h"
#include "tables/St_vertexSeed_Table.h"

class StEvent;
class StDcaGeometry;


class StGenericVertexFinder
{
public:

  // Alias for shorthand
  using StDcaList = std::vector<const StDcaGeometry*>;

  /// Options used to define the type of vertex fit performed in a concrete
  /// implementation
  enum class VertexFit_t : int { Unspecified, NoBeamline, Beamline1D, Beamline3D };

  // virtual and '=0' ; those MUST be implemented
  virtual ~StGenericVertexFinder();                           // virtual destructor
  virtual int            fit(StEvent*)=0;                     // fit the vertex

  StPrimaryVertex*       getVertex(int idx) const;
  void                   addVertex(StPrimaryVertex*);
  int                    size() const;
          void           UseVertexConstraint(const vertexSeed_st& beamline);
          void           NoVertexConstraint();
          int            IsVertexConstraint() const {return mVertexConstrain;}
  virtual void           UsePCT(bool usePCT = true);
  virtual void           UseBTOF(bool useBTOF = true){mUseBtof=useBTOF;}
  virtual void           UseCTB (bool useCTB  = true){mUseCtb =useCTB ;}
  virtual void           CalibBeamLine(){ /* noop */;} // overload if useful

  virtual void           printInfo(ostream& = cout) const=0;

  // General (default)
  virtual void           SetMode(Int_t mode=0 ) {mMode = mode;}
  virtual int            GetMode() const        {return mMode;}
          void           SetDebugLevel(Int_t level) {mDebugLevel=level;}
  virtual void           Init(){ /* noop */;}
  virtual void           Finish(){ /* noop */;}
  virtual void           InitRun  (int runumber){ /* noop */;}
  virtual void           Clear();
  const std::vector<StPrimaryVertex> *result() {return &mVertexList;}

  void                   FillStEvent(StEvent*);
  virtual void SetVertexPosition(double x,double y,double z){assert(0);}
  virtual int            IsFixed() const        {return 0;}

protected:

  StGenericVertexFinder(VertexFit_t fitMode=VertexFit_t::Unspecified);

  StPrimaryVertexOrder   mVertexOrderMethod; // will default to 0 i.e. orderByNumberOfDaughters
  bool                   mVertexConstrain;   // Use vertex constraint from db
  int                    mMode;              // used for any Finder behavior change

  /// The type of vertex fit to use in derived concrete implementation
  VertexFit_t            mVertexFitMode;

  int                    mDebugLevel;
  bool                   mUseBtof;           // default use btof = false
  bool                   mUseCtb;            // default use ctb = false

  /// Returns x coordinate on the beamline (given by sBeamline) corresponding to
  /// the passed value of z.
  static double beamX(double z);

  /// Returns y coordinate on the beamline (given by sBeamline) corresponding to
  /// the passed value of z.
  static double beamY(double z);

  /// Caclulates chi2 for the beamline and a point
  static double CalcChi2Beamline(const StThreeVectorD& point);

  /// Recalculates the vertex position from DCA measurements in the input list
  /// of DCAs
  static StThreeVectorD CalcVertexSeed(const StDcaList &trackDcas);

  /// Caclulates total chi2 for the track DCAs stored in sDCAs and a point
  static double CalcChi2DCAs(const StThreeVectorD &point);

  /// Caclulates total chi2 for the beamline and track DCAs stored in sDCAs and a point
  static double CalcChi2DCAsBeamline(const StThreeVectorD &point);

  /// Just an interface to CalcChi2DCAsBeamline(...)
  static void fcnCalcChi2DCAsBeamline(int& npar, double* gin, double& f, double* par, int iflag)
  {
     f = CalcChi2DCAsBeamline( StThreeVectorD(par) );
  }

  /// A static container with pointers to DCA states to be used in a vertex fit.
  /// The DCAs are assumed to be calculated w.r.t. the z-axis, i.e. x = y = 0.
  static StDcaList&  sDCAs();

  /// All measured parameters of the beamline. Updated whenever
  /// UseVertexConstraint(const vertexSeed_st&) is called
  static vertexSeed_st  sBeamline;

private:

  std::vector<StPrimaryVertex> mVertexList;      // Holds all found prim veritcess

  virtual void  UseVertexConstraint() = 0;
};

#endif
