/***************************************************************************
 *
 * $Id: StSvtProbValues.hh,v 1.3 2000/11/30 20:45:56 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.hh,v $
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
   
   void  SetProbValue(float sigma=0);
   double GetProbValue(int adc);
   
private:
   double mSigma;
   double mProb[MAX_ADC_COUNTS];

  //ClassDef(StSvtProbValues,1)

};

inline double StSvtProbValues::GetProbValue(int adc){ return mProb[adc]; }

#endif

