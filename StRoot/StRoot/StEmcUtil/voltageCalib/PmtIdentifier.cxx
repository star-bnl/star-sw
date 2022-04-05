#include "PmtIdentifier.h"

PmtIdentifier::PmtIdentifier()
  : _serialPort(0),
    _softId(0),
    _module(0),
    _pmtBox(0),
    _position(0),
    _serial(0),
    _baseId(0)
{}

PmtIdentifier::PmtIdentifier(int serialPort,
			     int softId,
			     int module,
			     int pmtBox,
			     int position,
			     long int serial,
			     long int baseId)
  : _serialPort(serialPort),
    _softId(softId),
    _module(module),
    _pmtBox(pmtBox),
    _position(position),
    _serial(serial),
    _baseId(baseId)
{}

PmtIdentifier::~PmtIdentifier()
{}

PmtIdentifier::PmtIdentifier(const PmtIdentifier & pi) 
  : _serialPort(pi._serialPort),
    _softId(pi._softId),
    _module(pi._module),
    _pmtBox(pi._pmtBox),
    _position(pi._position),
    _serial(pi._serial),
    _baseId(pi._baseId)
{}

const PmtIdentifier& PmtIdentifier::operator=(const PmtIdentifier &pi)
{ 
  _serialPort = pi._serialPort;
  _softId = pi._softId;
  _module = pi._module;
  _pmtBox = pi._pmtBox;
  _position = pi._position;
  _serial = pi._serial;
  _baseId = pi._baseId;
  return *this;
}

ostream& operator<<(ostream& os, const PmtIdentifier& object)
{
  os << object._serialPort << "\t"
     << object._softId     << "\t"
     << object._module     << "\t"
     << object._pmtBox     << "\t"
     << object._position   << "\t"
     << object._serial     << "\t"
     << object._baseId;
 return os;
}

istream& operator>>(istream& is, PmtIdentifier& object)
{
  is >> object._serialPort 
     >> object._softId 
     >> object._module 
     >> object._pmtBox 
     >> object._position 
     >> object._serial 
     >> object._baseId;
 return is;
}
