// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 2 September 2009
//

#ifndef ST_MU_TOWER_EMU_H
#define ST_MU_TOWER_EMU_H

class StMuTowerEmu {
public:
  StMuTowerEmu()
    : _px(0)
    , _py(0)
    , _pz(0)
    , _adc(0)
    , _pedestal(0)
    , _rms(0)
    , _status(0)
    , _id(0)
    , _detectorId(0)
  {
  }

  virtual ~StMuTowerEmu() {}

  double px        () const { return _px;         }
  double py        () const { return _py;         }
  double pz        () const { return _pz;         }
  int    adc       () const { return _adc;        }
  double pedestal  () const { return _pedestal;   }
  double rms       () const { return _rms;        }
  int    status    () const { return _status;     }
  int    id        () const { return _id;         }
  int    detectorId() const { return _detectorId; }

private:
  friend class StjeTowerEnergyListToStMuTrackFourVecList;

  double _px;
  double _py;
  double _pz;
  int    _adc;
  double _pedestal;
  double _rms;
  int    _status;
  int    _id;
  int    _detectorId;
};

#endif // ST_MU_TOWER_EMU_H
