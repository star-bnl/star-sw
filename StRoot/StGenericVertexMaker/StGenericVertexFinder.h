/*!
 * \class StGenericVertexFinder
 *
 * \author Lee Barnby, April 2003
 *
 * (pseudo) Base class for vertex finders
 *
 *
 * $Id: StGenericVertexFinder.h,v 1.57 2017/05/10 23:16:40 smirnovd Exp $
 */

#ifndef STAR_StGenericVertexFinder
#define STAR_StGenericVertexFinder

#include <vector>

//#include "StEventTypes.h"
#include "TMinuit.h"

#include "StPrimaryVertex.h"
#include "tables/St_vertexSeed_Table.h"
#include "StGenericVertexMaker/VertexFinderOptions.h"

class StEvent;
class StMuDst;
class StDcaGeometry;
class TClonesArray;
class St_db_Maker;


class StGenericVertexFinder
{
public:

  // Alias for shorthand
  using StDcaList = std::vector<const StDcaGeometry*>;

  /// Options used to define the type of vertex fit performed in a concrete
  /// implementation
  using VertexFit_t = star_vertex::VertexFit_t;

  /// Options to select vertex seed finder
  using SeedFinder_t = star_vertex::SeedFinder_t;

  // virtual and '=0' ; those MUST be implemented
  virtual ~StGenericVertexFinder();                           // virtual destructor
  virtual int            fit(StEvent*)=0;                     // fit the vertex

  StPrimaryVertex*       getVertex(int idx) const;
  void                   addVertex(const StPrimaryVertex& vtx);
  int                    size() const;
          void           UseVertexConstraint(const vertexSeed_st& beamline);
          void           NoVertexConstraint();
          int            IsVertexConstraint() const {return mVertexConstrain;}
  virtual void           UsePCT(bool usePCT = true);
  virtual void           UseBTOF(bool useBTOF = true){mUseBtof=useBTOF;}
  virtual void           UseCTB (bool useCTB  = true){mUseCtb =useCTB ;}

  virtual void           printInfo(ostream& = cout) const=0;

  // General (default)
  virtual void           SetMode(Int_t mode=0 ) {mMode = mode;}
  virtual int            GetMode() const        {return mMode;}
          void           SetDebugLevel(Int_t level) {mDebugLevel=level;}
  virtual void           Init(){ /* noop */;}
  virtual void           Finish(){ /* noop */;}
  virtual void           InitRun(int run_number, const St_db_Maker* db_maker);
  virtual void           Clear();
  const std::vector<StPrimaryVertex> *result() {return &mVertexList;}

  void result(TClonesArray& stMuDstPrimaryVertices);

  void                   FillStEvent(StEvent*);
  virtual void SetVertexPosition(double x,double y,double z){assert(0);}
  virtual int            IsFixed() const        {return 0;}

  virtual int            fit(const StMuDst& muDst) { return -1; }

protected:

  /// Default initialization with unspecified seed finder and fitting mode
  StGenericVertexFinder();

  StGenericVertexFinder(SeedFinder_t seedFinder, VertexFit_t fitMode);

  StPrimaryVertexOrder   mVertexOrderMethod; // will default to 0 i.e. orderByNumberOfDaughters
  bool                   mVertexConstrain;   // Use vertex constraint from db
  int                    mMode;              // used for any Finder behavior change

  /// The type of vertex fit to use in derived concrete implementation
  VertexFit_t            mVertexFitMode;

  /// The type of vertex seed finder to use in derived concrete implementation
  SeedFinder_t           mSeedFinderType;

  int                    mDebugLevel;
  bool                   mUseBtof;           // default use btof = false
  bool                   mUseCtb;            // default use ctb = false

  /// All measured parameters of the beamline. Updated whenever
  /// UseVertexConstraint(const vertexSeed_st&) is called
  vertexSeed_st  mBeamline;

  /// A container with pointers to DCA states to be used in a vertex fit.
  /// The DCAs are assumed to be calculated w.r.t. the z-axis, i.e. x = y = 0.
  StDcaList  mDCAs;

  TMinuit*  mMinuit;

  /// Static pointer to this base class allowing access to concrete
  /// implementations from Minuit minimization function
  static StGenericVertexFinder* sSelf;

  /// Searches for vertex candidates and fills private `mVertexData` container
  /// using the ROOT's TSpectrum peak finder applied to the distribution of
  /// track DCAs along the `z` axis
  std::vector<double> FindSeeds_TSpectrum();

  /// Returns x coordinate on the beamline (given by mBeamline) corresponding to
  /// the passed value of z.
  double beamX(double z) const;

  /// Returns y coordinate on the beamline (given by mBeamline) corresponding to
  /// the passed value of z.
  double beamY(double z) const;

  /// Recalculates the vertex position from DCA measurements in the input list
  /// of DCAs
  StThreeVectorD CalcVertexSeed(const StDcaList &trackDcas);

  /// Caclulates total chi2 for the track DCAs stored in mDCAs and a point
  virtual double CalcChi2DCAs(const StThreeVectorD &point);

  /// Caclulates chi2 for the beamline and a point
  double CalcChi2Beamline(const StThreeVectorD& point);

  /// Caclulates total chi2 for the beamline and track DCAs stored in mDCAs and a point
  double CalcChi2DCAsBeamline(const StThreeVectorD &point);

  // A static interface to CalcChi2DCAs(...)
  static void fcnCalcChi2DCAs(int& npar, double* gin, double& f, double* par, Int_t iflag)
  {
     f = sSelf->CalcChi2DCAs( StThreeVectorD(par) );
  }

  /// A static interface to CalcChi2DCAs(...) with x and y fixed by beamline equation
  static void fcnCalcChi2DCAsBeamline1D(int& npar, double* gin, double& f, double* par, Int_t iflag)
  {
     double z = par[0], x = sSelf->beamX(z), y = sSelf->beamY(z);
     f = sSelf->CalcChi2DCAs( StThreeVectorD(x, y, z) );
  }

  /// A static interface to CalcChi2DCAsBeamline(...)
  static void fcnCalcChi2DCAsBeamline(int& npar, double* gin, double& f, double* par, int iflag)
  {
     f = sSelf->CalcChi2DCAsBeamline( StThreeVectorD(par) );
  }

private:

  /// Holds all found primary vertices
  std::vector<StPrimaryVertex> mVertexList;

  virtual void  UseVertexConstraint() = 0;
};

#endif
