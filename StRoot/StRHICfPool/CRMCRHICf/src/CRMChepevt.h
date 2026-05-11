// -*- mode: C++ -*-
/**
 * @file      src/CRMChepevt.h
 * @author    Christian Holm Christensen <cholm@nbi.dk>
 * @date      Feb 1, 2021
 * 
 * @brief Helper to fill in HepMC3::GenEvent from common block HEPEVT
 */
#ifndef CRMCHEPEVT_H_
#define CRMCHEPEVT_H_
#include <cmath>
#include <memory>
#include <iostream>
#include <iomanip>
#include "CRMCconfig.h"
#ifndef HepMC_HEPEVT_SIZE
#error HepMC_HEPEVT_SIZE is not defined!
#endif

//--------------------------------------------------------------------
/** 
 * The structure of the HEPEVT common block.  Note, we declare it as a
 * a template only to make life a little simpler.
 */
template <int N, typename T=double>
struct HepEvt { 
  int    nevhep;
  int    nhep;
  int    isthep[N];
  int    idhep [N];
  int    jmohep[N][2];
  int    jdahep[N][2];
  T      phep  [N][5];
  T      vhep  [N][4];

  using real=T;
  enum {
    size = N
  };
};

//--------------------------------------------------------------------
extern "C" {
  using HepEvtType=HepEvt<HepMC_HEPEVT_SIZE>;
  /** The HEPEVT common block */
  extern HepEvtType hepevt_;
}

//==================================================================
/** 
 * Check if two values are close, where close is defined by the
 * relative and absolute tolerances.
 *
 * @f$ |a - b| \le (\epsilon_a + \epsilon_r |b|) @f$ 
 *
 * Suppose the absolute tolerance is zero @f$\epsilon_a=0@f$ ,
 * then the above is equivalent to
 *
 * @f$ \frac{|a-b|}{|b|} \le \epsilon_r@f$ 
 *
 * If the relative tolerance is zero @f$\epsilon_r=0@f$, then we
 * have
 * 
 * @f$ |a - b| \leg \epsilon_a @f$ 
 *
 * If both are non-zero, the one takes care of small numbers
 * (absolute tolerance) while the other takes care of larger
 * numbers.
 *
 * @param a "new" value 
 * @param b "reference" value 
 * @param atol Absolute tolerrance @f$\epsilon_a@f$
 * @param rtol Absolute tolerrance @f$\epsilon_r@f$
 *
 * @return true if the condition is met.
 *
 * @ingroup utils 
 */
template <typename T>
bool isClose(T a, T b, T rtol=1e-5, T atol=1e-8) 
{
  return std::abs(a - b) <= (atol + rtol * std::abs(b));
}

//==================================================================
/** 
 * Implements boosts along rapidity axes.  
 *
 * Note, in some cases we perform calculations ourselves rather than
 * relying on the HepMC3::FourVector member functions.  This is
 * because we use this with HepMC::FourVector which often does not
 * have the needed functionality.y.
 *
 */
template <typename Vector>
struct Booster
{
  using FourVector=Vector;
  /** 
   * Constructor 
   * 
   * @param yCM centre of mass rapidity
   */
  Booster(double yCM=0) : _ycm(0) { setyCM(yCM); }

  /** 
   * Set the CM rapidity 
   *
   * @param ycm Rapidity 
   */
  void setyCM(double ycm)
  {
    _ycm = ycm;
    _pzcm = std::sinh(ycm);
    _ecm  = std::cosh(ycm);
  }
  /** 
   * Performs a Lorentz boost 
   *
   * If dir==+1, then boost the four-vector x to obtain
   * that four vector in th frame specfied by the 5-vector
   * p,m (4-vector and mass).
   *
   * If dir==-1, then if the four-vector x is given in some frame
   * characterized by p,m respect to some laboratory frame, then
   * this returns the 4-vector in the laboratory frame.
   * 
   * Inspired by utlob2 in CRMC
   */
  FourVector rapidityBoost(int dir, const FourVector& x) const
  {
    if (isClose(_ycm, 0., 1e-5, 1e-2)) return x;
    
    int        sig  = dir < 0 ? -1 : 1;
    double     xx0  = x.m2();
    FourVector z    = x;
    double     bp   = sig * z.z() * _pzcm;
    z.set(z.x(),z.y(),
	  z.z() + sig * _pzcm * (z.t() + bp / (_ecm + 1)),
	  z.t() * _ecm + bp);
      
    double x0123 = xx0 + z.x()*z.x() + z.y()*z.y() + z.z()*z.z();
    if (x0123 > 0)
      z.set(z.x(),z.y(),z.z(),std::copysign(std::sqrt(x0123),z.t()));
    else
      z.set(z.x(),z.y(),z.z(),0);
    
    return z;
  }
  /** 
   * Boost 4-vector with rapidity boost 
   * 
   * Inspired by utlob5dbl in CRMC
   */
  FourVector rapidityBoost(const FourVector& x, double mass) const
  {
    if (isClose(_ycm, 0., 1e-5, 1e-2)) return x;

    auto rap    = [](const FourVector& x) {
       return std::log((x.e() + x.pz()) / (x.e() - x.pz()))/2;
    };
    double mt   = std::sqrt(x.m2() + x.px()*x.px() + x.py()*x.py());
    double nxt  = rap(x) + _ycm;
    FourVector r(x.x(),x.y(),  mt * std::sinh(nxt), mt * std::cosh(nxt));
    return r;
  }
protected:
  /** Centre-of-mass rapidity to boost too */
  double _ycm;
  /** pz of CMS */
  double _pzcm;
  /** E of cms */
  double _ecm;
};
namespace {
  /** Helper structure to detect if type is a shared_ptr */
  template<typename T> struct is_shared_ptr : std::false_type {};
  /** Helper structure to detect if type is a shared_ptr */
  template<typename T> struct is_shared_ptr<std::shared_ptr<T>>
    : std::true_type
  {};

  /** 
   * Helper function to create a pointer-like object reference.  This
   * uses SFINAE to detect if we are trying to create a
   * std::shared_ptr<T> (as is the case here) or a bare pointer.
   */
  template <class T,typename ...Args>
  typename std::enable_if<is_shared_ptr<T>::value,T>::type
  make(Args...args)
  {
    return std::make_shared<typename T::element_type>(args...);
  }

  /** 
   * Helper function to create a pointer-like object reference.  This
   * uses SFINAE to detect if we are trying to create a
   * std::shared_ptr<T> or a bare pointer (as is the case here) .
   */
  template <class T,
	    typename ...Args>
  typename std::enable_if<!is_shared_ptr<T>::value, T>::type
  make(Args...args)
  {
    using Type=typename std::remove_pointer<T>::type;
    return new Type(args...);
  }
}

/** 
 * Converts particles from HEPEVT Fortran common block to
 * HepMC3::GenParticle's and HepMC3::GenVertex's.
 *
 * @par IMPORTANT 
 *
 * The preprocessor define @c HEPEVT_SIZE _must_ be set to the right
 * value used by the code that populates the common block. 
 *
 * @ingroup model 
 */
template<typename Particle,
	 typename Vertex,
	 typename Vector,
	 typename EventType>
struct CRMChepevt
{
  using ParticlePtr=Particle;
  using VertexPtr=Vertex;
  using Event=EventType;
  using FourVector=Vector;
  using ParticleVector=std::vector<ParticlePtr>;
  using VertexVector=std::vector<VertexPtr>;
  using Boost=Booster<Vector>;
  
  /** 
   * Constructor 
   * 
   * @param yCM  If non-zero, boost particles by this rapidity 
   * @param forceOnShell If true, force particles on shell
   */
  CRMChepevt(double yCM=0, bool forceOnShell=true)
    : _particles(),
      _booster(yCM),
      _putOnShell(forceOnShell)
  {
    setyCM(yCM);
  }
  /** 
   * Set the centre-of-mass rapidity 
   * 
   * @param yCM  If non-zero, boost particles by this rapidity 
   */
  void setyCM(double ycms) { _booster.setyCM(ycms); }
  /** 
   * Converts data in the common block HEPEVT to particles and vertices. 
   *
   * The converted particles are available via the member function
   * `particles`..  The beam particles are accessed via the member
   * function `beams`.  Use f.ex. `HepMC3::GenEvent::add_tree` to
   * add the full tree to the event.
   *
   * @param ip Interaction point vertex 
   *
   * @return true on success 
   */
  bool convert()
  {
    _offshell = 0;
    _beams.clear();
    _particles.clear();
    _particles.resize(hepevt_.nhep);
    _vertices.clear();
    for (int i = 0; i < hepevt_.nhep; i++) 
      genParticle(i);

    return true;
  }
  /** 
   * Converts data in the common block HEPEVT to HepMC event
   *
   * @param event Event to fill
   *
   * @return true on success 
   */
  bool convert(Event& event)
  {
    event.clear();
    
    if (!convert()) return false;
    
    event.reserve(particles().size()+
		  beams()    .size(),
		  vertices() .size());
    event.add_tree(beams());

    return true;
  }
      
  /** Get the particles */
  const ParticleVector& particles() const { return _particles; }
  /** Get the beam particles */
  const ParticleVector& beams() const { return _beams; }
  /** Get the beam vertices */
  const VertexVector& vertices() const { return _vertices; }
  /** Number of particles that were off-shell */
  int offshell() const { return _offshell; }
  /** Dump content of HEPEVT to stream */
  void dump(std::ostream& out=std::cout) const 
  {
    out << std::setw(4) << "#" << "  "
	<< std::setw(5) << "PDG" << " "
	<< std::setw(3) << "STS" << " "
	<< std::setw(51) << "--- 4-Momentum ---" << "   "
	<< std::setw(51) << "--- 4-vertex ---" << " "
	<< std::setw(12) << "--- Mothers ---"
	<< std::endl;
    for (int i = 0; i < hepevt_.nhep; i++) {
      double* pv   =  hepevt_.phep  [i];
      double* xv   =  hepevt_.vhep  [i];
      int     pdg  =  hepevt_.idhep [i];
      int     sts  =  hepevt_.isthep[i];
      int*    mth  =  hepevt_.jmohep[i];
      double  p2   =  pv[0] * pv[0] + pv[1] * pv[1] + pv[2] * pv[2];
      double  p    =  std::sqrt(p2);
      double  m2   =  (pv[3] + p) * (pv[3] - p);
      double  m    =  pv[5];
      bool    os   =  (m2 - m * m) / std::max(100.,p2) > 1e-4;

      out << std::setw(4)  << i      << ": "
	  << std::setw(5)  << pdg    << " "
	  << std::setw(3)  << sts    << " "
	  << std::setw(12) << pv[0]  << ","
	  << std::setw(12) << pv[1]  << ","
	  << std::setw(12) << pv[2]  << ","
	  << std::setw(12) << pv[3]  << " @ "
	  << std::setw(12) << xv[0]  << ","
	  << std::setw(12) << xv[1]  << ","
	  << std::setw(12) << xv[2]  << ","
	  << std::setw(12) << xv[3]  << " "
	  << std::setw(5)  << mth[0] << ","
	  << std::setw(6)  << mth[1]
	  << (os ? " off-shell" : "") << std::endl;
    }
  }
protected:
  /**
   * Get the end-point vertex of current particle.
   *
   * @param  i Current particle index 
   *
   * @return end-point vertex of current particle 
   */
  VertexPtr getVertex(int i) const
  {
    HepEvtType::real* xv = hepevt_.vhep[i];
    FourVector        pos(xv[0],xv[1],xv[2],xv[3]);
    pos = _booster.rapidityBoost(-1, pos);
    
    return make<VertexPtr>(pos);
  }
  /**
   * Get indexes of mother particles - if any. 
   *
   * @param i Current particle index
   *
   * @return mother ids 
   */
  std::pair<int,int> getMothers(int i) const
  {
    return std::make_pair(hepevt_.jmohep[i][0]-1,hepevt_.jmohep[i][1]-1);
  }      
  /** 
   * Get current particle 
   *
   * @param i  Current particle index  
   * @param offsh counter of off-shelf 
   * 
   * @return particle i 
   */
  ParticlePtr getParticle(int i, int& offsh) const
  {
    HepEvtType::real* pv   =  hepevt_.phep  [i];
    int               pdg  =  hepevt_.idhep [i];
    int               sts  =  hepevt_.isthep[i];
    double            mas  =  pv[4];
    FourVector        mom(pv[0], pv[1], pv[2], pv[3]);
    
    if (_putOnShell) {
      double p2   = mom.x()*mom.x()+mom.y()*mom.y()+mom.z()*mom.z();
      double p    = std::sqrt(p2);
      double m2   = (mom.e() + p) * (mom.e() - p);
      double mas2 = mas * mas;
      if (not isClose(m2, mas2, 1e-2)) offsh++;

      mom.set(mom.px(),mom.py(),mom.pz(),std::sqrt(p2 + mas * mas));
    }
    mom = _booster.rapidityBoost(mom, mas);

    ParticlePtr g = make<ParticlePtr>(mom,pdg,sts);
    g->set_generated_mass(mas);

    return g;
  }
  /** 
   * Generate particle i and any of it's mothers that are not done yet.  
   *
   * @param i  index into HEPEVT 
   */
  ParticlePtr genParticle(int i)
  {
    if (_particles[i]) return _particles[i];
    
    // Get mothers
    auto mth = getMothers(i);

    // Generate mothers if needed
    ParticlePtr m1 = 0;
    ParticlePtr m2 = 0;
    if (mth.first  >= 0) m1 = genParticle(mth.first);
    if (mth.second >= 0) m2 = genParticle(mth.second);

    // Generate the particle 
    _particles[i] = getParticle(i, _offshell);
    auto&  p      = _particles[i];
      
    // Find vertex, possibly creating it 
    VertexPtr            vout = 0;
    if (m1)              vout = m1->end_vertex();
    if (m2 and not vout) vout = m2->end_vertex();
    if (not vout and p->status() != 4 and (m1 or m2)) {
      // If no end vertex from mothers, and this is not beam, then
      // create production vertex
      vout = getVertex(i);
      _vertices.push_back(vout);
      
      // Set end-vertex on mothers 
      if (m1 and not m1->end_vertex()) vout->add_particle_in(m1);
      if (m2 and not m2->end_vertex()) vout->add_particle_in(m2);
    }
    // Add this to output vertex if it exists
    if (vout) vout->add_particle_out(p);
    
    // Set as beam if status tells us so
    if (p->status() == 4) _beams.push_back(p);

    return p;
  };
  /** Internal cache of particles */
  ParticleVector _particles;
  /** Beam particles */
  ParticleVector _beams;
  /** Internal cache of vertices */
  VertexVector   _vertices;
  /** Number of off-shell particles in event */
  int _offshell;
  /** Force particles on-shell */
  bool _putOnShell;
  /** Rapidity booster */
  Boost _booster;
};
#endif
//
// EOF
//

