/***************************************************************************
 *
 * $Id: StSvtProbValues.hh,v 1.2 2000/10/02 13:48:10 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.hh,v $
 * Revision 1.2  2000/10/02 13:48:10  caines
 * Adjusting donw hybrid by hybrid
 *
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
   void Prob2(int Hybrid);
   void Prob1(int Hybrid);

private:
   int mAdcCounts;
   double mSigma[14];
   double mProb[20];
   double mSum[20];
   double mErrf[20];


};

#endif

