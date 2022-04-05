// $Id: StFtpcRawWriter.hh,v 1.2 2003/01/29 12:06:36 fsimon Exp $
// $Log: StFtpcRawWriter.hh,v $
// Revision 1.2  2003/01/29 12:06:36  fsimon
// Include switch to enable/disable turning of pad order for ASIC 2 in FTPC E
// Was an error on Y2001/2002 DAQ mapping
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcRawWriter
#define STAR_StFtpcRawWriter

#include "tables/St_fcl_ftpcndx_Table.h" 
#include "tables/St_fcl_ftpcsqndx_Table.h" 
#include "tables/St_fcl_ftpcadc_Table.h" 

class StFtpcRawWriter
{
public:
  StFtpcRawWriter(St_fcl_ftpcndx *ftpcndxIn,
		  St_fcl_ftpcsqndx *ftpcsqndxIn,
		  St_fcl_ftpcadc *ftpcadcIn,
		  const int inAsic2EastNotInverted);
  ~StFtpcRawWriter();
  int writeArray(float *array, 
		 int numberPadrows, 
		 int numberSectors, 
		 int numberPads, 
		 int numberTimebins,
		 int threshold);
private:
  int mAsic2EastNotInverted;
  FCL_FTPCNDX_ST *ndx; 
  int numNdx;
  int maxNdx; 
  FCL_FTPCSQNDX_ST *sqndx;
  int numSqndx;
  int maxSqndx; 
  FCL_FTPCADC_ST *adc;
  int numAdc;
  int maxAdc;
  St_fcl_ftpcndx *ftpcndx;
  St_fcl_ftpcsqndx *ftpcsqndx;
  St_fcl_ftpcadc *ftpcadc;
};
#endif

