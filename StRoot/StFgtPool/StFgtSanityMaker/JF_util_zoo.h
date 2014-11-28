// *+****1****+****2****+****3****+****4****+****5****+****6****+****7****+****
// 
//  example algos fro FGT 
//
//*--  Author: Jan Balewski, MIT, January 2012
//
// $Id: JF_util_zoo.h,v 1.1 2012/02/21 21:21:25 balewski Exp $
//
// *+****1****+****2****+****3****+****4****+****5****+****6****+****7****+****


#ifndef JF_util_zoo_H
#define JF_util_zoo_H

//-------------------------------
//--------  pulse seed finder , based on expected pulse shape and sigPed
//-------------------------------
bool pluseSeedFinderA(float *adc, int N,  float sigPed,
		      float &peakAdc, int &leadEdgeBin, float &sumAdc) {
  // assumes adc[timeBin] are pedestal subtracted
  assert(N==7);// works only for 7 time-bin sampling
  peakAdc=leadEdgeBin=-9999;
  sumAdc=0;

  for(int i=0;i<N;i++) {
    sumAdc+=adc[i];
    if(leadEdgeBin<0 && adc[i]> 5*sigPed) leadEdgeBin=i;
    if(2<=i && i<=4 && peakAdc < adc[i] ) peakAdc =adc[i]; // snap to peak
  }
  // do sum first, it is used even if puls is not tagged as a seed

  if( adc[0]> 2*sigPed) return false; // start low 
  if( adc[2]< 5*sigPed) return false; // high is visible
  if( adc[3]< 5*sigPed) return false; // high is visible
  if( adc[4]< 5*sigPed) return false; // high is visible

  if(peakAdc < adc[6] ) return false; //trailing  edge 
  
  return true;
}



#endif

// $Log: JF_util_zoo.h,v $
// Revision 1.1  2012/02/21 21:21:25  balewski
// *** empty log message ***
//
