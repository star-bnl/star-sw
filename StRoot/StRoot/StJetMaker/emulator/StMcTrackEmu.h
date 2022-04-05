// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 8 Dec 2009
//

#ifndef ST_MC_TRACK_EMU_H
#define ST_MC_TRACK_EMU_H

class StMcTrackEmu {
public:
  StMcTrackEmu()
    : _pt(0)
    , _eta(0)
    , _phi(0)
    , _m(0)
    , _e(0)
    , _id(0)
    , _pdg(0)
    , _status(0)
    , _firstMother(0)
    , _lastMother(0)
    , _firstDaughter(0)
    , _lastDaughter(0)
  {
  }

  double     pt() const { return _pt    ; }
  double    eta() const { return _eta   ; }
  double    phi() const { return _phi   ; }
  double      m() const { return _m     ; }
  double      e() const { return _e     ; }
  int        id() const { return _id    ; }
  int       pdg() const { return _pdg   ; }
  int    status() const { return _status; }
  int   firstMother() const { return _firstMother; }
  int    lastMother() const { return _lastMother; }
  int firstDaughter() const { return _firstDaughter; }
  int  lastDaughter() const { return _lastDaughter; }

private:

  friend class StjMCParticleToStMuTrackFourVec;
  
  double _pt;			// transverse momentum
  double _eta;			// pseudorapidity
  double _phi;			// azimuthal angle
  double _m;			// mass
  double _e;			// energy
  int    _id;			// line in Pythia record
  int    _pdg;			// PDG code
  int    _status;               // 1=stable, 2=unstable, 3=incoming and parton
  int    _firstMother;		// first mother
  int    _lastMother;		// last mother
  int    _firstDaughter;	// first daughter
  int    _lastDaughter;		// last daughter
};

#endif	// ST_MC_TRACK_EMU_H
