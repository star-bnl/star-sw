//==================================================================================================
// Writes Embedded data into FTPC SlowSimulator Sequences
//
// Author: Frank Simon (fsimon@bnl.gov)
//==================================================================================================

#ifndef STAR_StFtpcSequencer
#define STAR_StFtpcSequencer

#include "tables/St_fcl_ftpcndx_Table.h" 
#include "tables/St_fcl_ftpcsqndx_Table.h" 
#include "tables/St_fcl_ftpcadc_Table.h" 

class StFtpcSequencer
{
public:
  StFtpcSequencer(St_fcl_ftpcndx *ftpcndxIn,
		  St_fcl_ftpcsqndx *ftpcsqndxIn,
		  St_fcl_ftpcadc *ftpcadcIn);
  ~StFtpcSequencer();
  int writeArray(const int *cArray, 
		 const int numberPadrows, 
		 const int numberSectors, 
		 const int numberPads, 
		 const int numberTimebins);
private:
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

 /***************************************************************************
 *
 * $Id: StFtpcSequencer.hh,v 1.1 2003/02/14 18:11:25 fsimon Exp $
 *
 * $Log: StFtpcSequencer.hh,v $
 * Revision 1.1  2003/02/14 18:11:25  fsimon
 * Initial commit of FTPC embedding code
 *
 *
 ***************************************************************************/
