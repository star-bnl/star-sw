#ifndef ST_FSS_SECTOR_READER_HH
#define ST_FSS_SECTOR_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"


class StFssSectorReader
{
public:
  StFssSectorReader(int sector,
		    unsigned short *fcl_ftpcsqndx, int nSeq,
		    char *fcl_ftpcadc, int nAdc);
  ~StFssSectorReader();
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nSeq, Sequence **SeqData);
  int initialize();
private:
  int mSector;
  unsigned short *m_ftpcsqndx;
  char *m_ftpcadc;  
  int m_numSqndx, m_numAdc;
  struct Pad Pad_array[FTP_PADROWS][FTP_MAXPADS];
  struct PadRow Row_array[FTP_PADROWS];
  u_char padlist[FTP_PADROWS][FTP_MAXPADS];

};

#endif
