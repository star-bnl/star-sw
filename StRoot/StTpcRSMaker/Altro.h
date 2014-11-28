/**		@file Altro.h
	*	@brief This the header File for the Altro class
	*
	*	@author Roland Bramm
	*	@version $LastChangedRevision: 397 $
	*	@date    $LastChangedDate: 2004-05-11 10:11:49 +0200 (Tue, 11 May 2004) $
	*
	*	\verbinclude Altro/Altro.h.log
*/
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
g++ -c -o Altro.o Altro.C
g++  Altro.o -o Altromain.app Altromain.C
g++ -o Altromain.app Altro.C Altromain.C
*/
using namespace std;

class Altro{
	public:
	Altro(int timebins, short* Channel);
	~Altro();

	void ConfigAltro(int ONBaselineCorrection1, int ONTailcancellation, int ONBaselineCorrection2, int ONClipping, int ONZerosuppression);
	void ConfigBaselineCorrection_1(int mode, int ValuePeDestal, int *PedestalMem, int polarity);
	void ConfigTailCancellationFilter(int K1, int K2, int K3, int L1, int L2, int L3);
	void ConfigBaselineCorrection_2(int HighThreshold, int LowThreshold, int Offset, int Presamples, int Postsamples);
	void ConfigZerosuppression(int Threshold, int MinSamplesaboveThreshold, int Presamples, int Postsamples);
	void PrintParameters();
	void RunEmulation();
	float calculatecompression();

	enum {
		/**din - fpd*/					DIN_FPD,
		/**din - f(t)*/					DIN_FT,
		/**din - f(din)*/				DIN_FDIN,
		/**din - f(din-vpd)*/			DIN_FDIN_VPD,
		/**din - vpd - fpd*/			DIN_VPD_FPD,
		/**din - vpd - f(t)*/			DIN_VPD_FT,
		/**din - vpd - f(din)*/			DIN_VPD_FDIN,
		/**din - vpd - f(din - vpd)*/	DIN_VPD_FDIN_VPD,
		/**f(din) - fpd*/				FDIN_FPD,
		/**f(din - vpd) - fpd*/			FDIN_VPD_FPD,
		/**f(t) - fpd*/					FT_FPD,
		/**f(t) - f(t)*/				FT_FT,
		/**f(din) - f(din)*/			FDIN_FDIN,
		/**f(din - vpd) - f(din - vpd)*/FDIN_VPD_FDIN_VPD,
		/**din - fpd*/					DIN_FPD1,
		/**din - fpd*/					DIN_FPD2
	};
	private:
	int ftimebins;

	short *channelIn;
	short *channelShort;
 public:
	short *ADCkeep;
 private:
	int fOnBSL1;
	int fOnTCF;
	int fOnBSL2;
	int fOnClip;
	int fOnZSU;

	int fConfiguredAltro;
	int fConfiguredBSL1;
	int fConfiguredTCF;
	int fConfiguredBSL2;
	int fConfiguredZSU;

	int fBSL1mode;
	int fBSL1ValuePeDestal;
	int* fBSL1PedestalMem;
	int fBSL1polarity;

	float fTCFK1;
	float fTCFK2;
	float fTCFK3;
	float fTCFL1;
	float fTCFL2;
	float fTCFL3;

	int fTCFK1Int;
	int fTCFK2Int;
	int fTCFK3Int;
	int fTCFL1Int;
	int fTCFL2Int;
	int fTCFL3Int;

	int fBSL2HighThreshold;
	int fBSL2LowThreshold;
	int fBSL2Offset;
	int fBSL2Presamples;
	int fBSL2Postsamples;

	int fZSUThreshold;
	int fZSUMinSamplesaboveThreshold;
	int fZSUPresamples;
	int fZSUPostsamples;

	void BaselineCorrection_1(int mode, int FixedPeDestal, int *PedestalMem, int polarity);
	void TailCancellationFilter_FixedPoint(int K1, int K2, int K3, int L1, int L2, int L3);
	void BaselineCorrection_2_RTL(int HighThreshold, int LowThreshold, int Offset, int Presamples, int Postsamples);
	void Clipping();
	void Zerosuppression(int Threshold, int MinSamplesaboveThreshold, int Presamples, int Postsamples);

	short getElement(short* Array,int index);
	void setElement(short* Array,int index,short value);

	int inBand(int ADC,int bsl, int LowThreshold, int HighThreshold);
	int	inRange(int parameter,int Low,int High,const char *Module,const char *ParameterName);
	short GetShortChannel(int i);
	short GetKeepChannel(int i);
	int multiply36(int P, int N);
	long long mask(long long in, int left, int right);
	long long maskandshift(long long in, int left, int right);
};
