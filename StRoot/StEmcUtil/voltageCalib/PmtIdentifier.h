#ifndef PmtIdentifier_H_INCLUDED
#define PmtIdentifier_H_INCLUDED

#include <Stiostream.h>

///Class used to encapsulate all data need to uniquely identify each PMT.
///
///When written/read to/from file, they shall be in the following other:
///Column A - The serial port number of each CW-Base 
///Column B - The software Id of the tower connect to the CW-Base 
///Column C - The module number 
///Column D - The PMT box number 
///Column E - The position of the CW-Base inside the PMT box 
///Column F - The serial number of the PMT connected to the CW-Base 
///Column G - The Id of the CW-Base 

class PmtIdentifier
{
 public:
  PmtIdentifier();
  PmtIdentifier(int serialPort,
		int softId,
		int module,
		int pmtBox,
		int position,
		long serial,
		long baseId);
  ~PmtIdentifier();
  PmtIdentifier(const PmtIdentifier &);
  const PmtIdentifier& operator=(const PmtIdentifier &);

  friend ostream& operator<<(ostream& os, const PmtIdentifier& object);
  friend istream& operator>>(istream& is, PmtIdentifier& object);


  int _serialPort;
  int _softId;
  int _module;
  int _pmtBox;
  int _position;
  long int _serial;
  long int _baseId;
};

#endif
