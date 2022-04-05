/**		@file Altro.C
	*	@brief The Altro class implements the Altro digital Chain in C++
	*
	*	This Class represents a C++ version of the ALTRO. For a complete Documentation of the Altro
	*	Look at : http://ep-ed-alice-tpc.web.cern.ch/ep-ed-alice-tpc/altro_chip.htm\n
	*	Due to the fact that the real ALTRO constantly samples in between the recorded events,
	*	it has the knowledge on what happened in the period. This affects the BSL1, TCF and BSL2 module.
	*	In the BSL1 the ALTRO follows slow baseline drifts e.g. temperature change, the TCF has a infinite
	*	(IIR Filter) memory of "old samples" i.e. a cluster at the start of a readout cycle will be treated
	*	differently, and the BSL2 has a 8 step pipeline. The ALTRO Class can't emulate this behavior
	*	because the data is not recorded.\n
	*
	*	@author Roland Bramm
	*	@version $LastChangedRevision: 467 $
	*	@date    $LastChangedDate: 2004-08-06 14:40:55 +0200 (Fri, 06 Aug 2004) $
	*
	*	\verbinclude Altro/Altro.C.log
	*/

#include "Altro.h"

/**	@brief Consturctor of Altro Class
	*
	*	Consturctor of Altro Class, some variables are set.\n
	*	The input Data is altered, so after running the complete emulation you have the
	*	Altro Processed Data in the Channel Pointer.\n
	*
	*	@param timebins an <tt> int </tt> sets the length of the input Data (Channel)
	*	@param Channel an <tt> short* </tt> Pointer to a 1d short Array with the input Data
	*/
Altro::Altro(int timebins, short* Channel){
	ftimebins = timebins;

	channelShort = Channel;

	fOnBSL1 = 0;
	fOnTCF = 0;
	fOnBSL2 = 0;
	fOnClip = 0;
	fOnZSU = 0;

	fConfiguredAltro = 0;
	fConfiguredBSL1 = 0;
	fConfiguredTCF = 0;
	fConfiguredBSL2 = 0;
	fConfiguredZSU = 0;
	ADCkeep = 0;
}

/**	@brief Destructor of Altro Class
	*
	*	Destructor of Altro Class\n
	*/
Altro::~Altro(){
	if(fConfiguredZSU == 1)
//VP		delete ADCkeep;
        free(ADCkeep);
}

/**  @brief Configures which modules of the Altro should be on.
	*
	*	Configures which modules of the Altro should be on. Each of the modules
	*	which are configured to be on, have to be configured later before running
	*	the emulation!\n
	*
	*	@param ONBaselineCorrection1 an <tt> int </tt> Switch (0,1) to turn on the Base Line Correction 1 (BSL1) Module
	*	@param ONTailcancellation an <tt> int </tt> Switch (0,1) to turn on the Tail Cancellation Filter (TCF) Module
	*	@param ONBaselineCorrection2 an <tt> int </tt> Switch (0,1) to turn on the Moving Average Filter (BSL2) Module
	*	@param ONClipping an <tt> int </tt> Switch (0,1) to turn on the Clipping Module. This is not possible in the real Altro, there it is always on.
	*	@param ONZerosuppression an <tt> int </tt> Switch (0,1) to turn on the Zero Suppression (ZSU) Module
	*/
void Altro::ConfigAltro(int ONBaselineCorrection1, int ONTailcancellation, int ONBaselineCorrection2, int ONClipping, int ONZerosuppression){
	fOnBSL1 = inRange(ONBaselineCorrection1,0,1,"Altro::ConfigAltro","ONBaselineCorrection1");
	fOnTCF  = inRange(ONTailcancellation,0,1,"Altro::ConfigAltro","ONTailcancellation");
	fOnBSL2 = inRange(ONBaselineCorrection2,0,1,"Altro::ConfigAltro","ONBaselineCorrection2");
	fOnClip = inRange(ONClipping,0,1,"Altro::ConfigAltro","ONClipping");
	fOnZSU = inRange(ONZerosuppression,0,1,"Altro::ConfigAltro","ONZerosuppression");
	fConfiguredAltro = 1;
}

/**  @brief Configures the Base Line Correction 1 (BSL1) Module
	*
	*	Configures the Base Line Correction 1 (BSL1) Module. You dont have to build a proper pedestalMemory
	*	array, a pointer of the correct type is enough, of course you are not allowed to use Basline
	*	Correction Modes which need then the array ...\n
	*	All configurable values are "Range checked" and if out of the Range set to the nearest extreme.
	*	So the Emulation will work, but the result is maybe not the expected one.
	*
	*	@param mode an <tt> int </tt> sets the mode of the Baseline Correction. See the Altro manual for a description
	*	@param ValuePeDestal an <tt> int </tt> this is the baseline of the Channel.
	*	@param PedestalMem an <tt> *int </tt> Pointer to a 1d short Array with the pedestal memory Data
	*	@param polarity an <tt> int </tt> Switch (0,1) for the polarity
	*/
void Altro::ConfigBaselineCorrection_1(int mode, int ValuePeDestal, int *PedestalMem, int polarity){
	fBSL1mode          = inRange(mode,0,10,"Altro::ConfigBaselineCorrection_1","mode");
	fBSL1ValuePeDestal = inRange(ValuePeDestal,0,1023,"Altro::BaselineCorrection_1","ValuePeDestal");
	fBSL1PedestalMem = PedestalMem;
	fBSL1polarity = inRange(polarity,0,1,"Altro::BaselineCorrection_1","polarity");
	fConfiguredBSL1 = 1;
}

/**  @brief Configures the Tail Cancellation Filter (TCF) Module
	*
	*	Configures the Tail Cancellation Filter (TCF) Module. You have to set the coefficients in the
	*	Integer version.\n
	*	To convert from int to float use (int)*(pow(2,-16)-1)
	*	To convert from float to int usw (float)*(pow(2,16)-1)
	*	All configurable values are "Range checked" and if out of the Range set to the nearest extreme.
	*	So the Emulation will work, but the result is maybe not the expected one.
	*
	*	@param K1 an <tt> int </tt> sets the K1 coeeficient of the TCF
	*	@param K2 an <tt> int </tt> sets the K2 coeeficient of the TCF
	*	@param K3 an <tt> int </tt> sets the K3 coeeficient of the TCF
	*	@param L1 an <tt> int </tt> sets the L1 coeeficient of the TCF
	*	@param L2 an <tt> int </tt> sets the L2 coeeficient of the TCF
	*	@param L3 an <tt> int </tt> sets the L3 coeeficient of the TCF
	*/
void Altro::ConfigTailCancellationFilter(int K1, int K2, int K3, int L1, int L2, int L3){
	// conf from int to fp:
	//(int)*(pow(2,-16)-1)
	//backway.
	//float*(pow(2,16)-1)
	fTCFK1Int = inRange(K1,0,65535,"Altro::ConfigTailCancellationFilter","K1");
	fTCFK2Int = inRange(K2,0,65535,"Altro::ConfigTailCancellationFilter","K2");
	fTCFK3Int = inRange(K3,0,65535,"Altro::ConfigTailCancellationFilter","K3");

	fTCFL1Int = inRange(L1,0,65535,"Altro::ConfigTailCancellationFilter","L1");
	fTCFL2Int = inRange(L2,0,65535,"Altro::ConfigTailCancellationFilter","L2");
	fTCFL3Int = inRange(L3,0,65535,"Altro::ConfigTailCancellationFilter","L3");
	fConfiguredTCF = 1;
}

/**  @brief Configures the Moving Average Filter (BSL2) Module
	*
	*	Configures the Moving Average Filter (BSL2) Module.
	*	All configurable values are "Range checked" and if out of the Range set to the nearest extreme.
	*	So the Emulation will work, but the result is maybe not the expected one.
	*
	*	@param HighThreshold an <tt> int </tt> sets the high Threshold
	*	@param LowThreshold an <tt> int </tt> sets the low Theshold
	*	@param Offset an <tt> int </tt> sets the the offset which is added to the Signal
	*	@param Presamples an <tt> int </tt> sets the number of pre samples excluded from the moving average caclulation
	*	@param Postsamples an <tt> int </tt> sets the number of post samples excluded from the moving average caclulation
	*/
void Altro::ConfigBaselineCorrection_2(int HighThreshold, int LowThreshold, int Offset, int Presamples, int Postsamples){
	fBSL2HighThreshold = inRange(HighThreshold,0,1023,"Altro::ConfigBaselineCorrection_2","HighThreshold");
	fBSL2LowThreshold  = inRange(LowThreshold,0,1023,"Altro::ConfigBaselineCorrection_2","LowThreshold");
	fBSL2Offset        = inRange(Offset,0,1023,"Altro::ConfigBaselineCorrection_2","Offset");
	fBSL2Presamples    = inRange(Presamples,0,3,"Altro::ConfigBaselineCorrection_2","Presamples");
	fBSL2Postsamples   = inRange(Postsamples,0,15,"Altro::ConfigBaselineCorrection_2","Postsamples");
	fConfiguredBSL2 = 1;
}

/**  @brief Configures the Zero Suppression Module (ZSU)
	*
	*	Configures the Zero Suppression Module (ZSU).
	*	All configurable values are "Range checked" and if out of the Range set to the nearest extreme.
	*	So the Emulation will work, but the result is maybe not the expected one.
	*
	*	@param Threshold an <tt> int </tt> sets the Threshold
	*	@param MinSamplesaboveThreshold an <tt> int </tt> sets the minimum number of samples which have to be greater than the threshold
	*	@param Presamples an <tt> int </tt> sets the number of pre samples which are kept
	*	@param Postsamples an <tt> int </tt> sets the number of post samples which are kept
	*/
void Altro::ConfigZerosuppression(int Threshold, int MinSamplesaboveThreshold, int Presamples, int Postsamples){
	fZSUThreshold                = inRange(Threshold,0,1023,"Altro::BaselineCorrection_1","Threshold");
	fZSUMinSamplesaboveThreshold = inRange(MinSamplesaboveThreshold,1,3,"Altro::BaselineCorrection_1","MinSamplesaboveThreshold");
	fZSUPresamples               = inRange(Presamples,0,3,"Altro::BaselineCorrection_1","Presamples");
	fZSUPostsamples              = inRange(Postsamples,0,7,"Altro::BaselineCorrection_1","Postsamples");
	ADCkeep = (short *)calloc(sizeof(short),ftimebins);

// 	for(int i = 0; i < ftimebins; i++){
// 		ADCkeep[i] = 0;
// 	}
	fConfiguredZSU = 1;
}

/**  @brief Prints the set Parameters, if module is configured
	*
	*	Prints the set Parameters, if module is configured.
	*/
void Altro::PrintParameters(){
	cout << "+-------------------------------------------+" << endl;
	cout << "| Configured Parameters of the Altro Module |" << endl;
	cout << "+-------------------------------------------+" << endl << endl;

	cout << "Parameters set in the Altro Modules:" << endl << endl;
	cout << "ONBaselineCorrection1: " << fOnBSL1 << endl;
	cout << "ONTailcancellation   : " << fOnTCF << endl;
	cout << "ONBaselineCorrection2: " << fOnBSL2 << endl;
	cout << "ONClipping           : " << fOnClip << endl;
	cout << "ONZerosuppression    : " << fOnZSU << endl << endl << endl;
	if(fConfiguredBSL1 == 1){
		cout << "Parameters set in the BSL1 (Baseline Correction 1) Module:" << endl << endl;
		cout << "mode                 : " << fBSL1mode << endl;
		cout << "ValuePeDestal        : " << fBSL1ValuePeDestal << endl;
		cout << "polarity             : " << fBSL1ValuePeDestal << endl << endl << endl;
	}else{
		cout << "BSL1 (Baseline Correction 1) Module not configured!" << endl << endl << endl;
	}
	if(fConfiguredTCF == 1){
		cout << "Parameters set in the TCF (TailCancellation Filter) Module:" << endl << endl;
		cout << "K1       (int|float) : " << fTCFK1Int << " | " << fTCFK1Int/(float)((1<<16)-1) << endl;
		cout << "K2       (int|float) : " << fTCFK2Int << " | " << fTCFK2Int/(float)((1<<16)-1) << endl;
		cout << "K3       (int|float) : " << fTCFK3Int << " | " << fTCFK3Int/(float)((1<<16)-1) << endl;
		cout << "L1       (int|float) : " << fTCFL1Int << " | " << fTCFL1Int/(float)((1<<16)-1) << endl;
		cout << "L2       (int|float) : " << fTCFL2Int << " | " << fTCFL2Int/(float)((1<<16)-1) << endl;
		cout << "L3       (int|float) : " << fTCFL3Int << " | " << fTCFL3Int/(float)((1<<16)-1) << endl << endl << endl;
	}else{
		cout << "TCF (TailCancellation Filter) Module not configured!" << endl << endl << endl;
	}
	if(fConfiguredBSL2 == 1){
		cout << "Parameters set in the BSL2 (Baseline Correction 2) Module:" << endl << endl;
		cout << "HighThreshold        : " << fBSL2HighThreshold << endl;
		cout << "LowThreshold         : " << fBSL2LowThreshold << endl;
		cout << "Offset               : " << fBSL2Offset << endl;
		cout << "Presamples           : " << fBSL2Presamples << endl;
		cout << "Postsamples          : " << fBSL2Postsamples << endl << endl << endl;
	}else{
		cout << "BSL2 (Baseline Correction 2) Module not configured!" << endl << endl << endl;
	}
	if(fConfiguredZSU == 1){
		cout << "Parameters set in the ZSU (Zero Suppression Unit) Module:" << endl << endl;
		cout << "Threshold            : " << fZSUThreshold << endl;
		cout << "MinSampaboveThreshold: " << fZSUMinSamplesaboveThreshold << endl;
		cout << "Presamples           : " << fZSUPresamples << endl;
		cout << "Postsamples          : " << fZSUPostsamples << endl << endl << endl;
	}else{
		cout << "ZSU (Zero Suppression Unit) Module not configured!" << endl << endl << endl;
	}
}

/**  @brief Runs the emulation of all configured Modules.
	*
	*	Runs the emulation of all configured Modules. This changes then the content of the
	*	input Array
	*/
void Altro::RunEmulation(){
	//cout << "Altro::RunEmulation | start" << endl;
	if(fConfiguredAltro == 0){
		cout << "ERROR cant run Altro Emulation because not configured" << endl;
		return;
	}
	for(int i = 0; i < ftimebins; i++){
		ADCkeep[i] = 0;
	}

	//cout << "Altro::RunEmulation | start BSL1 on: " << fOnBSL1 << " configures: " << fConfiguredBSL1 << endl;
	if(fOnBSL1 == 1){
		if(fConfiguredBSL1 == 1){
			BaselineCorrection_1(fBSL1mode, fBSL1ValuePeDestal, fBSL1PedestalMem, fBSL1polarity);
		}else{
			cout << "ERROR cant run Baseline Correction 1 because not configured" << endl;
			return;
		}
	}

	//cout << "Altro::RunEmulation | start TCF on: " << fOnTCF << " configures: " << fConfiguredTCF << endl;
	if(fOnTCF == 1){
		if(fConfiguredTCF == 1){
			TailCancellationFilter_FixedPoint(fTCFK1Int, fTCFK2Int, fTCFK3Int, fTCFL1Int, fTCFL2Int, fTCFL3Int);
		}else{
			cout << "ERROR cant run Tail Cancellation Filter because not configured" << endl;
			return;
		}
	}

	//cout << "Altro::RunEmulation | start BSL2 on: " << fOnBSL2 << " configures: " << fConfiguredBSL2 << endl;
	if(fOnBSL2 == 1){
		if(fConfiguredBSL2 == 1){
			BaselineCorrection_2_RTL(fBSL2HighThreshold, fBSL2LowThreshold, fBSL2Offset, fBSL2Presamples, fBSL2Postsamples);
		}else{
			cout << "ERROR cant run Baseline Correction 2 because not configured" << endl;
			return;
		}
	}
	//cout << "Altro::RunEmulation | start CLIP on: " << fOnClip << endl;
	if(fOnClip == 1){
		Clipping();
	}
	//cout << "Altro::RunEmulation | start ZSU on: " << fOnZSU << " configures: " << fConfiguredZSU << endl;
	if(fOnZSU == 1){
		if(fConfiguredZSU == 1){
			Zerosuppression(fZSUThreshold,fZSUMinSamplesaboveThreshold,fZSUPresamples,fZSUPostsamples);
		}else{
			cout << "ERROR cant run Zero Suppression Unit because not configured" << endl;
			return;
		}
	}
	for(int i = 0; i < ftimebins; i++){
	  if (! ADCkeep[i]) channelShort[i]  = 0;
	}
}

void Altro::BaselineCorrection_1(int mode, int ValuePeDestal, int *PedestalMem, int polarity){
	//VPD == 0 !!
	int FixedPeDestal = 0;

	if(polarity ==1){
		for(int i = 0; i < ftimebins; i++){
			channelShort[i]  = 1023 - channelShort[i];
		}
	}

	switch(mode) {
		case DIN_FPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - FixedPeDestal;
			break;
		case DIN_FT:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - PedestalMem[i];
			break;
		case DIN_FDIN:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - PedestalMem[ channelShort[i] ];
			break;
		case DIN_FDIN_VPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - PedestalMem[ channelShort[i] - ValuePeDestal];
			break;
		case DIN_VPD_FPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - ValuePeDestal - FixedPeDestal;
			break;
		case DIN_VPD_FT:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - ValuePeDestal - PedestalMem[i];
			break;
		case DIN_VPD_FDIN:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - ValuePeDestal - PedestalMem[ channelShort[i] ];
			break;
		case DIN_VPD_FDIN_VPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = channelShort[i]  - ValuePeDestal - PedestalMem[ channelShort[i] - ValuePeDestal ];
			break;
		case FDIN_FPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = PedestalMem[ channelShort[i] ] - FixedPeDestal;
			break;
		case FDIN_VPD_FPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = PedestalMem[ channelShort[i] - ValuePeDestal ] - FixedPeDestal;
			break;
		case FT_FPD:
			for(int i = 0; i < ftimebins; i++)
				channelShort[i]  = PedestalMem[i] - FixedPeDestal;
			break;
	}
}

int Altro::multiply36(int P, int N){
	//multiply function to emulate the 36 bit fixed point multiplication of the Altro.
	long long retval =0;
	long long temp = 0;
	long long AX = 0;
	temp = (long long)P*(long long)N;
	AX = (( mask(temp,35,18) + ((long long)(-P)<<18) ) + mask(temp,17,0));
	if ( maskandshift(N,17,17) == 1){
		retval = ((maskandshift(AX,35,35)<<17) + maskandshift(AX,32,16));
	}else{
		retval = maskandshift(temp,32,16);
	}
	return retval;
}
long long Altro::mask(long long in, int left, int right){
	long long retval;
	long long pattern;
	long long length = abs(left - right)+1;
	pattern = ((1<<length)-1)<<right;
	retval = in&pattern;
	return retval;
}

long long Altro::maskandshift(long long in, int left, int right){
	long long retval;
	long long pattern;
	long long length = abs(left - right)+1;
	pattern = ((1<<length)-1);
	retval = (in>>right)&pattern;
	return retval;
}

void Altro::TailCancellationFilter_FixedPoint(int K1, int K2, int K3, int L1, int L2, int L3){
	int c1n = 0, c2n = 0, c3n = 0;
	int c1o = 0, c2o = 0, c3o = 0;
	int d1  = 0, d2  = 0;
	int dout = 0;
	int din = 0;
	int bit = 0;
	for(int i = 0; i < ftimebins; i++){
		din = channelShort[i];

		din = (din<<2);
		c1n             = mask( (mask(din,17,0) + multiply36(K1,mask(c1o,17,0)) ) ,17,0);
		d1              = mask( (mask(c1n,17,0) - multiply36(L1,mask(c1o,17,0)) ) ,17,0);
		//d1              = mask( (mask(c1n,17,0) + mask(~multiply36(L1,mask(c1o,17,0))+1,17,0) ) ,17,0);

		c2n             = mask( (mask(d1 ,17,0) + multiply36(K2,mask(c2o,17,0)) ) ,17,0);
		d2              = mask( (mask(c2n,17,0) - multiply36(L2,mask(c2o,17,0)) ) ,17,0);
		//d2              = mask( (mask(c2n,17,0) + mask(~multiply36(L2,mask(c2o,17,0))+1,17,0) ) ,17,0);

		c3n             = mask( (mask(d2 ,17,0) + multiply36(K3,mask(c3o,17,0)) ) ,17,0);
		dout            = mask( (mask(c3n,17,0) - multiply36(L3,mask(c3o,17,0)) ) ,17,0);
		//dout            = mask( (mask(c3n,17,0) + mask(~multiply36(L3,mask(c3o,17,0))+1,17,0) ) ,17,0);

		if( (maskandshift(dout,2,2) == 1) || (maskandshift(dout,1,1) == 1)){
			bit = 1;
		}else{
			bit = 0;
		}

		dout = ((dout>>3)<<1) + bit;
		if(maskandshift(dout,15,15) == 1){
			//is needed to get the correct coding when getting negative results
			dout = -mask((-mask(dout,9,0)),9,0);
		}else{
			dout = mask(dout,9,0);
		}

		channelShort[i] = (short) dout;
		c1o = c1n;
		c2o = c2n;
		c3o = c3n;
	}
}

void Altro::BaselineCorrection_2_RTL(int HighThreshold, int LowThreshold, int Offset, int Presamples, int Postsamples){
	//cout << "Altro::BaselineCorrection_2_RTL | HighThreshold: " << HighThreshold << " LowThreshold: " << LowThreshold << " Offset: " << Offset << " Presamples: " << Presamples << " Postsamples: " << Postsamples << endl;
	//more or less direct "translation" of the hdl code.
	//Input signals
	int din;
	int dout;
	int edges[6]; // = Postsamples*4 + Presamples;
	int offset = Offset;
	int thrlo = LowThreshold;//called thr_mau[19] ...
	int thrhi = HighThreshold;

	// Variables
	int fOld[4]; //flag pipe
	int fNew[4]; //flag pipe
	int dOld[4]; //data pipe
	int dNew[4]; //data pipe
	int dxOld;
	int dxNew;
	int pstscnt; // Counter for Postsamples
	int zOld[9]; // Filter stages
	int zNew[9]; // Filter stages
	int zxOld; //Accumulator stage
	int zxNew; //Accumulator stage
	int valcntOld; //Valid sample counter
	int valcntNew = 0; //Valid sample counter

	int valid; //Valid flag
	int fx; //postsample flag
	//int s07; // differentiator result
	int s8; // Acc + Diff result
	int flag;
	//int bsth; //baseline threshold
	//int din_p; //Data input strictly positive
	int bsl;
	//int dx_bsls; // dx -bsl
	//int dx_clip; // dxbsl clipped
	//int bsl_of = 0;

	//initialisation
	for(int i = 0; i < 9 ; i++)
		zOld[i] = 0;
	for(int i = 0; i < 4 ; i++){
		fOld[i] = 0;
		dOld[i] = 0;
	}
	dxOld= 0;
	pstscnt = 0;
	zxOld = 0;
	valcntOld = 0;
	valid = 0;
	for(int i = 0; i < 2 ; i++){
		edges[i] = (Presamples&(1<<i))>>i;
	}
	for(int i = 0; i < 4 ; i++){
		edges[(3-i)+2] = (Postsamples&(1<<i))>>i;
	}
	/*cout << "edges :";
	for(int i = 0; i < 6 ; i++)
		cout << edges[i] << ":";
	cout << " Presamples: " << Presamples << " Postsamples: " << Postsamples << endl;*/

	//Loop
	//cout << "Altro::BaselineCorrection_2_RTL | starting Loop" << endl;
	for(int timebin = -12; timebin < ftimebins+10; timebin++){
		//cout << "Altro::BaselineCorrection_2_RTL | in Loop timebin: " << timebin << endl;
		din = getElement(channelShort,timebin);

		s8 = zxOld + (zOld[8] - zOld[0]);

		if(valid == 1)
			bsl = s8>>3;// ...
		else
			bsl = 0;

		//assign flag = (din_p > thrhi) | (thrlo > din_p);	// Signal samples between thresholds
		if( (din <= (bsl + thrhi)) && (din >= (bsl - thrlo)) )
			flag = 0;
		else
			flag = 1;

		if(pstscnt == 0)
			fx = 0;
		else
			fx = 1;

		if(valcntOld >= 12)
			valid = 1;
		else
			valid = 0;

		fNew[3] = flag;

		if( (fOld[3] == 1) || ( (flag == 1) && ( (edges[0] == 1) || (edges[1] == 1) ) ) ) //f[2] =  f[3] | (flag&(edges[0]|edges[1]));
			fNew[2] = 1;
		else
			fNew[2] = 0;

		if( (fOld[2] == 1) || ( (edges[1] == 1) && (flag == 1) ) ) //		f[1] =  f[2] | (edges[1] & flag);
			fNew[1] = 1;
		else
			fNew[1] = 0;

		if( ( (fOld[1] == 1) || ( (flag == 1) && (edges[0] == 1) && (edges[1] == 1) )  || (fx==1) ) && (valid==1) ) //		f[0] = (f[1] | (edges[1] & edges[0] & flag) | fx) & valid;
			fNew[0] = 1;
		else
			fNew[0] = 0;

		dxNew = dOld[0];
		for(int i = 0; i < 3; i++)
			dNew[i] = dOld[i+1];
		dNew[3] = din;

		if( (fOld[1]==1) && (fOld[2]==0) )
			pstscnt = Postsamples;
		else if(fx == 1)
			pstscnt--;

		if(fOld[0] == 0){
			if(valid == 0)
				valcntNew =  ++valcntOld;

			zxNew = s8;
			for(int i = 0; i < 8; i++)
				zNew[i] = zOld[i+1];
			zNew[8] = dOld[0];
		}else{
			zxNew = zxOld;
			for(int i = 0; i < 9; i++)
				zNew[i] = zOld[i];
		}
		dout = dxOld - (bsl - offset);
		//if(dout <0)
		//	dout = 0;

		setElement(channelShort,timebin-5,(short)dout);
	//sim clockschange
		for(int i = 0; i < 9 ; i++)
			zOld[i] = zNew[i];
		zxOld = zxNew;
		for(int i = 0; i < 4 ; i++){
			fOld[i] = fNew[i];
			dOld[i] = dNew[i];
		}
		dxOld = dxNew;
		valcntOld = valcntNew;
	}
}

void Altro::Clipping(){ // implement if no BC2 clipping has to run
	for(int i = 0; i < ftimebins; i++){
		if(channelShort[i] < 0)
			channelShort[i] = 0;
	}
}

void Altro::Zerosuppression(int Threshold, int MinSamplesaboveThreshold, int Presamples, int Postsamples){ // add again altro feature
	//TODO: Implement "Altro Bug"
	//int Postsamplecounter = 0;
	//int setPostsample = 0;
	for(int i = 0; i < ftimebins; i++){
		if(channelShort[i] >= Threshold){
			ADCkeep[i] = 1;
		}
	}

	int startofclustersequence = -1;
	int endofClustersInSequence = -1;

	for(int i = 0; i < ftimebins; i++){
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i-1) == 0) ){
			startofclustersequence = i;
		}
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i+1) == 0) ){
			endofClustersInSequence = i;
		}
		//cout << i << " startofclustersequence: " << startofclustersequence << " endofClustersInSequence: " << endofClustersInSequence;
		if( (startofclustersequence != -1) && (endofClustersInSequence != -1) ){
			//cout << " found! " <<  (endofClustersInSequence - startofclustersequence + 1);
			if ( (endofClustersInSequence - startofclustersequence + 1) < MinSamplesaboveThreshold ){
				for(int j = startofclustersequence; j <= endofClustersInSequence ; j++){
					ADCkeep[j] = 0;
				}
			}
			startofclustersequence = -1;
			endofClustersInSequence = -1;
		}
		//cout << endl;
	}

	/*for(int i = 0; i < ftimebins; i++){
		if( (getElement(ADCkeep,i-1) == 1) && (getElement(ADCkeep,i) == 0) && (getElement(ADCkeep,i+1) == 1) ){
			setElement(ADCkeep,i,1);
		}
	}*/

	for(int i = 0; i < ftimebins; i++){
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i-1) == 0) ){
			for(int j = i-Presamples ; j <= i; j++){
				setElement(ADCkeep,j,1);
			}
		}
	}
	for(int i = ftimebins-1; i >= 0; i--){
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i+1) == 0) ){
			for(int j = i ; j <= i+Postsamples; j++){
				setElement(ADCkeep,j,1);
			}
		}
	}
	/*cout << " Postsamplecounter: " << Postsamplecounter;
		for(int j = i+1 ; j <= i+Postsamples; j++){
				setElement(ADCkeep,j,1);
				i+=Postsamples;
			}
		cout << endl;
	}
		cout << i << " ADCK: " << getElement(ADCkeep,i);
		cout << " Postsam: " << Postsamplecounter << " ADCK: " << getElement(ADCkeep,i);*/

	for(int i = 0; i < ftimebins; i++){
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i+1) == 0) && ( (getElement(ADCkeep,i+3) == 1) || (getElement(ADCkeep,i+2) == 1) ) ){
			setElement(ADCkeep,i+1,1);
			setElement(ADCkeep,i+2,1);
		}
	}
}

/**  @brief calculates the compression out of the bitmask
	*
	*	calculates the compression out of the bitmask with the set adc values
	*
	* 	@return \c float consisting of the compression factor
	*/
float Altro::calculatecompression(){
	// calculation is based on altro 10 bit words ..
	int sample = 0;
	int cluster = 0;
	int data = 0;
	float retval = 0.0;

	for(int i = 0; i < ftimebins; i++){
		if(ADCkeep[i] == 1){
			sample++;
		}
		if( (ADCkeep[i] == 1) && (getElement(ADCkeep,i+1) == 0) ){
			cluster++;
		}
	}
	data = sample + cluster*2;
	data = data + data%4 + 4;
	if(data >0){
		retval = ftimebins / (float)data;//num of timebins is equal to max number of samples
	}else{
		retval = 1.0;
	}
	return retval;
}

short Altro::getElement(short* Array,int index){
	if (index < 0)
		return 0;
	else if(index >= ftimebins)
		return 0;
	else
		return Array[index];
}

void Altro::setElement(short* Array,int index,short value){
	if (index < 0)
		return;
	else if(index >= ftimebins)
		return;
	else
		Array[index] = value;
}

int Altro::inBand(int ADC,int bsl, int LowThreshold, int HighThreshold){
	int fLow = bsl - LowThreshold;
	int fHigh = bsl + HighThreshold;
	if( (ADC <= fHigh) && (ADC >= fLow) )
		return 1;
	else
		return 0;
}

int	Altro::inRange(int parameter,int Low,int High,const char *Module,const char *ParameterName){
	char out[255];
	int retval;
	if(parameter > High){
		sprintf(out,"Error | %s | Parameter %s is to big, has to be %d <= %s <= %d, is %d, now set to %d",Module,ParameterName,Low,ParameterName,High,parameter,High);
		cout << out << endl;
		retval = High;
	}else if(parameter < Low){
		sprintf(out,"Error | %s | Parameter %s is to small, has to be %d <= %s <= %d, is %d, now set to %d",Module,ParameterName,Low,ParameterName,High,parameter,Low);
		cout << out << endl;
		retval = Low;
	}else{
		retval = parameter;
	}
	return retval;
}

short Altro::GetShortChannel(int i){
	return getElement(channelShort,i);
}

short Altro::GetKeepChannel(int i){
	return getElement(ADCkeep,i);
}
