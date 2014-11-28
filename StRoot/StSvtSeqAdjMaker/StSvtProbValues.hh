/***************************************************************************
 *
 * $Id: StSvtProbValues.hh,v 1.4 2001/05/01 00:23:58 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.hh,v $
 * Revision 1.4  2001/05/01 00:23:58  caines
 *  Update h files for use with zsp data
 *
 * Revision 1.3  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/


#ifndef STSVTPROBVALUES_HH
#define STSVTPROBVALUES_HH

#define MAX_ADC_COUNTS 14

class StSvtProbValues
{
public:
   StSvtProbValues();
   ~StSvtProbValues();
   
   void   SetProbValue(float sigma=0);
   double GetProbValue(int adc);
   double GetSigma();
private:
   double mSigma;
   double mProb[MAX_ADC_COUNTS];

 
};

inline double StSvtProbValues::GetProbValue(int adc){ return mProb[adc]; }
inline double StSvtProbValues::GetSigma( ){ return mSigma; }
#endif

