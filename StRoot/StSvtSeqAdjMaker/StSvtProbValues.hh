/***************************************************************************
 *
 * $Id: StSvtProbValues.hh,v 1.1 2000/06/15 20:04:54 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.hh,v $
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/


#ifndef STSVTPROBVALUES_HH
#define STSVTPROBVALUES_HH


class StSvtProbValues
{
public:
   StSvtProbValues();
   ~StSvtProbValues();
   
   void WriteTable();
   void Prob();

private:
   int mAdcCounts;
   double mSigma;
   double mProb[20];
   double mSum[20];
   double mErrf[20];

  //ClassDef(StSvtProbValues,1)

};

#endif

