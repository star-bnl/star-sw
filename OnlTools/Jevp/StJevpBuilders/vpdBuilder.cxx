#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "vpdBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(vpdBuilder);
	
void vpdBuilder::initialize(int argc, char *argv[]) {

	

	char tmp[256];
	char tmp1[256];
	

	// VPD lo

	sprintf(tmp,"vpd_east_ADClo");
	sprintf(tmp1,"VPD-vtx east ADC");
	contents.cdb[0] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.cdb[0]->SetXTitle("Channel # (east)");
	contents.cdb[0]->SetYTitle("Low-Th ADC (east)");

	sprintf(tmp,"vpd_east_TAClo");
	sprintf(tmp1,"VPD-vtx east TAC");
	contents.cdb[1] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.cdb[1]->SetXTitle("Channel # (east)");
	contents.cdb[1]->SetYTitle("Low-Th TAC (east)");

	sprintf(tmp,"vpd_west_ADClo");
	sprintf(tmp1,"VPD-vtx west ADC");
	contents.cdb[2] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.cdb[2]->SetXTitle("Channel # (west)");
	contents.cdb[2]->SetYTitle("Low-Th ADC (west)");

	sprintf(tmp,"vpd_west_TAClo");
	sprintf(tmp1,"VPD-vtx west TAC");
	contents.cdb[3] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.cdb[3]->SetXTitle("Channel # (west)");
	contents.cdb[3]->SetYTitle("Low-Th TAC (west)");
	
	sprintf(tmp,"vpd_tac_east_vs_tac_west");
	contents.tac_east_vs_tac_west = new TH2D(tmp,"VPD-vtx TAC East vs. TAC West", 256, -1.5, 4094.5, 256, -1.5, 4094.5);
	contents.tac_east_vs_tac_west->SetXTitle("TAC West");
	contents.tac_east_vs_tac_west->SetYTitle("TAC East");
	

	//sprintf(tmp,"vpd_vertex_vs_l3_vertex");
	//contents.vertex_vs_l3_vertex = new TH2D(tmp,"VPDlo TAC Difference vs. L3 Vertex z-Position", 200, -200, 200, 200, 3600,4600);
	//contents.vertex_vs_l3_vertex->SetXTitle("L3 Vertex z-Position [cm]");
	//contents.vertex_vs_l3_vertex->SetYTitle("VPD TAC Difference");

	sprintf(tmp,"vpd_earliestTAC_vs_chan_east");
	contents.earliestTAC_vs_eastchan = new TH2D(tmp,"VPD-vtx EarliestTAC vs chan east", 16, -0.5, 15.5, 256, -1.5, 4094.5);
	contents.earliestTAC_vs_eastchan->SetXTitle("Chan#(east)");
	contents.earliestTAC_vs_eastchan->SetYTitle("Earliest TAC");


	sprintf(tmp,"vpd_earliestTAC_vs_chan_west");
	contents.earliestTAC_vs_westchan = new TH2D(tmp,"VPD-vtx EarliestTAC vs chan west", 16, -0.5, 15.5, 256, -1.5, 4094.5);
	contents.earliestTAC_vs_westchan->SetXTitle("Chan#(west)");
	contents.earliestTAC_vs_westchan->SetYTitle("Earliest TAC");
	

	// VPD Hi


	sprintf(tmp,"vpd_east_ADChi");
	sprintf(tmp1,"VPD-mtd east ADC");
	contents.hi_cdb[0] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.hi_cdb[0]->SetXTitle("Channel # (east)");
	contents.hi_cdb[0]->SetYTitle("High-Th ADC (east)");

	sprintf(tmp,"vpd_east_TAChi");
	sprintf(tmp1,"VPD-mtd east TAC");
	contents.hi_cdb[1] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.hi_cdb[1]->SetXTitle("Channel # (east)");
	contents.hi_cdb[1]->SetYTitle("High-Th TAC (east)");

	sprintf(tmp,"vpd_west_ADChi");
	sprintf(tmp1,"VPD-mtd west ADC");
	contents.hi_cdb[2] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.hi_cdb[2]->SetXTitle("Channel # (west)");
	contents.hi_cdb[2]->SetYTitle("High-Th ADC (west)");

	sprintf(tmp,"vpd_west_TAChi");
	sprintf(tmp1,"VPD-mtd west TAC");
	contents.hi_cdb[3] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
	contents.hi_cdb[3]->SetXTitle("Channel # (west)");
	contents.hi_cdb[3]->SetYTitle("High-Th TAC (west)");
	
	sprintf(tmp,"vpd_hi_tac_east_vs_tac_west");
	contents.hi_tac_east_vs_tac_west = new TH2D(tmp,"VPD-mtd TAC East vs. TAC West", 256, -1.5, 4094.5, 256, -1.5, 4094.5);
	contents.hi_tac_east_vs_tac_west->SetXTitle("TAC West");
	contents.hi_tac_east_vs_tac_west->SetYTitle("TAC East");
	


	sprintf(tmp,"vpd_tac_align_east");
	sprintf(tmp1,"VPD-vtx TAC Alignment East; Channel # ; TAC_{ch} - TAC_{ch=%i}", refChannelEast);
	contents.tac_align_east = new TH2D(tmp,tmp1, 16,-0.5,15.5, 100, -400, 400);
	sprintf(tmp,"vpd_tac_align_west");
	sprintf(tmp1,"VPD-vtx TAC Alignment West; Channel # ; TAC_{ch} - TAC_{ch=%i}", refChannelWest);
	contents.tac_align_west = new TH2D(tmp,tmp1, 16,-0.5,15.5, 100, -400, 400);

	// sprintf(tmp,"vtx_TAC_diff");
	// contents.vtx_TAC_diff = new TH1D( tmp, "TAC Diff; <West> - <East>", 200, -100, 100 );


	//sprintf(tmp,"vpd_hi_vertex_vs_l3_vertex");
	//contents.hi_vertex_vs_l3_vertex = new TH2D(tmp,"VPDhi TAC Difference vs. L3 Vertex z-Position", 200, -200, 200, 200, 3600,4600);
	//contents.hi_vertex_vs_l3_vertex->SetXTitle("L3 Vertex z-Position [cm]");
	//contents.hi_vertex_vs_l3_vertex->SetYTitle("VPD TAC Difference");

	sprintf(tmp,"vpd_hi_earliestTAC_vs_chan_east");
	contents.hi_earliestTAC_vs_eastchan = new TH2D(tmp,"VPD-mtd EarliestTAC vs chan east", 16, -0.5, 15.5, 256, -1.5, 4094.5);
	contents.hi_earliestTAC_vs_eastchan->SetXTitle("Chan#(east)");
	contents.hi_earliestTAC_vs_eastchan->SetYTitle("Earliest TAC");


	sprintf(tmp,"vpd_hi_earliestTAC_vs_chan_west");
	contents.hi_earliestTAC_vs_westchan = new TH2D(tmp,"VPD-mtd EarliestTAC vs chan west", 16, -0.5, 15.5, 256, -1.5, 4094.5);
	contents.hi_earliestTAC_vs_westchan->SetXTitle("Chan#(west)");
	contents.hi_earliestTAC_vs_westchan->SetYTitle("Earliest TAC");

	
	// Add root histograms to Plots
	int np = sizeof(contents) / sizeof(TH1 *);
	JevpPlot *plots[np];

	int n=0;
	plots[n] = new JevpPlot(contents.cdb[0]);
	plots[++n] = new JevpPlot(contents.cdb[1]);
	plots[++n] = new JevpPlot(contents.cdb[2]);
	plots[++n] = new JevpPlot(contents.cdb[3]);
	plots[++n] = new JevpPlot(contents.tac_east_vs_tac_west);
	plots[++n] = new JevpPlot(contents.tac_align_east);
	plots[++n] = new JevpPlot(contents.tac_align_west);
	//plots[++n] = new JevpPlot(contents.vertex_vs_l3_vertex);
	plots[++n] = new JevpPlot(contents.earliestTAC_vs_eastchan);
	plots[++n] = new JevpPlot(contents.earliestTAC_vs_westchan);  
	plots[++n] = new JevpPlot(contents.hi_cdb[0]);
	plots[++n] = new JevpPlot(contents.hi_cdb[1]);
	plots[++n] = new JevpPlot(contents.hi_cdb[2]);
	plots[++n] = new JevpPlot(contents.hi_cdb[3]);
	plots[++n] = new JevpPlot(contents.hi_tac_east_vs_tac_west);
	//plots[++n] = new JevpPlot(contents.hi_vertex_vs_l3_vertex);
	plots[++n] = new JevpPlot(contents.hi_earliestTAC_vs_eastchan);
	plots[++n] = new JevpPlot(contents.hi_earliestTAC_vs_westchan);  




	// Add Plots to plot set...
	for(int i=0;i<=n;i++) {
		LOG(DBG, "Adding plot %d",i);
		
		contents.array[i]->GetXaxis()->SetLabelSize(0.055);
		contents.array[i]->GetYaxis()->SetLabelSize(0.045);

		addPlot(plots[i]);
	}
}
	
void vpdBuilder::startrun(daqReader *rdr) {
	resetAllPlots();

	pulserSwitch = false;
	noiseCorr = false;
	refChannelEast = 1;
	refChannelWest = 1;

	ReadConfig();
	readParams();
}

void vpdBuilder::stoprun(daqReader *rdr) {
}

void vpdBuilder::event(daqReader *rdr) {
	LOG(DBG, "event #%d",rdr->seq);

	StTriggerData2016 *trgd = (StTriggerData2016*)getStTriggerData(rdr);
	if(!trgd) {
		LOG(DBG, "No trigger data");
		return;
	}

//  int maxTacEast = trgd->vpdEarliestTDC((StBeamDirection)0);
//  int maxTacWest = trgd->vpdEarliestTDC((StBeamDirection)1);

//  EAST = 0
//  WEST = 1
	int maxTacEast = -1;
	int maxTacWest = -1;
	
	int earliestchan_east=-1;
	int earliestchan_west=-1; 
	

	int sumTAC[2] = {0, 0};
	int sumADC[2] = {0, 0};
	int nHit[2] = {0, 0};
	int maxTAC[2] = {-1, -1};
	int earliestChan[2] = { -1, -1 };

	int nlitlo[2]={0};

	for(int side=0;side<2;side++) {   // East or West
		
		for(int ich=0;ich<16;ich++){ // Channel

			int adc_lo = trgd->vpdADC( (StBeamDirection)side, ich+1);
			int tdc_lo = trgd->vpdTDC( (StBeamDirection)side, ich+1);
			int tdc_corr = correctedTAC( trgd, (StBeamDirection)side, ich );

			if ( goodHit( adc_lo, tdc_corr ) ){
				sumTAC[ side ] += tdc_corr;
				sumADC[ side ] += adc_lo;
				nHit[ side ] += 1;
				if ( maxTAC[side] < tdc_corr ){
					earliestChan[side] = ich;
					maxTAC[side] = tdc_corr;
				}

			}
			if ( goodHit( adc_lo, tdc_lo ) ){
				contents.cdb[2*side+0]->Fill(ich, adc_lo);
				contents.cdb[2*side+1]->Fill(ich, tdc_lo);
			}
		}
	}

	contents.tac_east_vs_tac_west->Fill(sumTAC[0] / (float)nHit[0], sumTAC[1] / (float)nHit[1]);

	if (maxTAC[0]>200){
			contents.earliestTAC_vs_eastchan->Fill(earliestChan[0],maxTAC[0]);
	}
	if (maxTAC[1]>200){
			contents.earliestTAC_vs_westchan->Fill(earliestChan[1],maxTAC[1]);
	}
	
	

	int On_sumTacEast = trgd->bbcVP101( 5 );
	int On_sumAdcEast = (trgd->bbcVP101( 4 )&0xfff);
	int On_nHitsEast = (trgd->bbcVP101( 4 )>>12);
	int On_sumTacWest = trgd->bbcVP101( 7 );
	int On_sumAdcWest = (trgd->bbcVP101( 6 )&0xfff);
	int On_nHitsWest = (trgd->bbcVP101( 6 )>>12);
	
	// contents.vtx_TAC_diff->Fill( (sumTAC[1] / (float)nHit[1]) - (sumTAC[0] / (float)nHit[0]) );
	// if ( On_nHitsWest > 0 && On_nHitsEast > 0 )
	// 	contents.vtx_TAC_diff->Fill( (On_sumTacWest / On_nHitsWest) - (On_sumTacEast / On_nHitsEast) );

	// contents.vtx_east_tacsum_on_vs_off->Fill( On_sumTacEast, sumTAC[ 0 ] );
	// contents.vtx_west_tacsum_on_vs_off->Fill( On_sumTacWest, sumTAC[ 1 ] );

	// if ( sumADC[ 0 ] > 4095 )
	// 	sumADC[ 0 ] = 4095;
	// if ( sumADC[ 1 ] > 4095 )
	// 	sumADC[ 1 ] = 4095;
	// contents.vtx_east_adcsum_on_vs_off->Fill( On_sumAdcEast, sumADC[ 0 ] );
	// contents.vtx_west_adcsum_on_vs_off->Fill( On_sumAdcWest, sumADC[ 1 ] );


	// // pulsers
	// for(int ich=0;ich<16;ich++){
	// 	if ( ich != pulserCh( ich ) ) continue;
	// 	int adcE = trgd->vpdADC((StBeamDirection)east,ich+1);
	// 	int tdcE = trgd->vpdTDC((StBeamDirection)east,ich+1);
	// 	// int tdcE = correctedTAC( trgd, east, ich );

	// 	int adcW = trgd->vpdADC((StBeamDirection)1,ich+1);
	// 	int tdcW = trgd->vpdTDC((StBeamDirection)1,ich+1);
	// 	// int tdcW = correctedTAC( trgd, west, ich );

	// 	int pindex = ich / 4;
	// 	contents.pulser_west[pindex]->Fill( tdcW - expected_pulser_means_west[ pindex ] );
	// 	if ((int)contents.pulser_west[pindex]->GetEntries() % 100 == 0  )
	// 		contents.pulser_west[pindex]->Fit( "gaus", "QR", "", -25, 25 );
		
	// 	contents.pulser_east[pindex]->Fill( tdcE - expected_pulser_means_east[ pindex ] );
	// 	if ((int)contents.pulser_east[pindex]->GetEntries() % 100 == 0  )
	// 		contents.pulser_east[pindex]->Fit( "gaus", "QR", "", -25, 25 );
	// }

	// TAC alignment
	
	int refAdcW = trgd->vpdADC((StBeamDirection)west,refChannelWest+1);
	int refTdcW = trgd->vpdTDC((StBeamDirection)west,refChannelWest+1);

	if ( goodHit( refAdcW, refTdcW ) ){
		for(int ich=0;ich<16;ich++){
			if ( ich == refChannelWest || ich == pulserCh( ich )  ) continue;
			int adc = trgd->vpdADC((StBeamDirection)west,ich+1);
			int tdc = trgd->vpdTDC((StBeamDirection)west,ich+1);

			if ( goodHit( adc, tdc ) ){
				contents.tac_align_west->Fill( ich, tdc - refTdcW );
			}
		}	
	}

	// EAST
	int refAdcE = trgd->vpdADC(east,refChannelEast+1);
	int refTdcE = trgd->vpdTDC(east,refChannelEast+1);

	if ( goodHit( refAdcW, refTdcW ) ){
		for(int ich=0;ich<16;ich++){
			if ( ich == refChannelEast || (ich == pulserCh( ich ) && pulserSwitch )  ) continue;
			int adc = trgd->vpdADC(east,ich+1);
			int tdc = trgd->vpdTDC(east,ich+1);

			if ( goodHit( adc, tdc ) ){
				contents.tac_align_east->Fill( ich, tdc - refTdcE );
			}
		}	
	}
	




	// VPD-MXQ 
	// MTD
	// Still using the earliest TDC East and West method
	int maxTacEastHigh = -1;
	int maxTacWestHigh = -1;
	int earliestchan_east_hi=-1;
	int earliestchan_west_hi=-1; 
	int nlithi[2]={0};
	for(int i=0;i<2;i++) {   //
		for(int ich=0;ich<16;ich++){
			if ( pulserSwitch && ich == pulserCh( ich ) ) continue;
			int adc_hi = trgd->vpdADCHighThr((StBeamDirection)i,ich+1);
			int tdc_hi = trgd->vpdTDCHighThr((StBeamDirection)i,ich+1);
			if(tdc_hi>200 && i==0 && maxTacEastHigh<tdc_hi){ earliestchan_east_hi=ich; maxTacEastHigh=tdc_hi; }
			if(tdc_hi>200 && i==1 && maxTacWestHigh<tdc_hi){ earliestchan_west_hi=ich; maxTacWestHigh=tdc_hi; }
			if (tdc_hi>200){
					nlithi[i]++;
				contents.hi_cdb[2*i+0]->Fill(ich, adc_hi);
				contents.hi_cdb[2*i+1]->Fill(ich, tdc_hi);
		}
		}
	}
	contents.hi_tac_east_vs_tac_west->Fill(maxTacWestHigh, maxTacEastHigh);
	if (maxTacEastHigh>200){
			contents.hi_earliestTAC_vs_eastchan->Fill(earliestchan_east_hi,maxTacEastHigh);
	}
	if (maxTacWestHigh>200){
			contents.hi_earliestTAC_vs_westchan->Fill(earliestchan_west_hi,maxTacWestHigh);
	}

	if(trgd) delete trgd;
}

void vpdBuilder::main(int argc, char *argv[])
{
	vpdBuilder me;  
	me.Main(argc, argv);
}


vector<string> vpdBuilder::readTokenVector( TString &str ){
	str.ToLower();
	TObjArray* Strings = str.Tokenize(" ");

	TIter iString( Strings );
	TObjString* os=0;

	vector<string> strs;
	while ( (os=(TObjString*)iString()) ){

		strs.push_back( os->GetString().Data() );

	}

	return strs;
}

vector<int> vpdBuilder::readIntVector( TString &str, int start ){

	vector<string> tokens = readTokenVector( str );

	vector<int> itokens;
	if ( start >= tokens.size() )
		return itokens;

	for ( int i = start; i < tokens.size(); i++ ){
		itokens.push_back( atoi( tokens[i].c_str() ) );
	}
	return itokens;
}

void vpdBuilder::ReadConfig(){
	TString buffer;
	char mConfigFile[256];
	sprintf(mConfigFile, "%s/tof/%s",confdatadir,"VPD_Config.txt");
	ifstream filein(mConfigFile); 

	int count=0;
	vector<TString> cValues;
	if(filein){
		while(!filein.eof()){
			buffer.ReadLine(filein);
			if(buffer.BeginsWith("/")) continue;
			if(buffer.BeginsWith("#")) continue;
			cValues.push_back( buffer );
			float number=atof(buffer.Data());
			count++;
		}
	} else {
		LOG("====VPD====", "Can not open file: %s", mConfigFile);
	}


	// process the tokens
	for ( int i = 0; i < cValues.size(); i++ ){
		vector<string> tk = readTokenVector( cValues[ i ] );		
		if ( tk.size() <= 0  ) continue;

		if ( "maskedeast" == tk[0] )
			maskedChannelsEast = readIntVector( cValues[ i ], 1 );
		else if ( "maskedwest" == tk[0] )
			maskedChannelsWest = readIntVector( cValues[ i ], 1 );
		else if ( "pulseronoff" == tk[0] && tk.size() >= 2 ){
			pulserSwitch = (bool) atoi( tk[1].c_str() );
		}
		else if ( "noisecorr" == tk[0] && tk.size() >= 2 ){
			noiseCorr = (bool) atoi( tk[1].c_str() );
		}
		else if ( "pulsermeanseast" == tk[0] )
			expected_pulser_means_east = readIntVector( cValues[ i ], 1 );
		else if ( "pulsermeanswest" == tk[0] )
			expected_pulser_means_west = readIntVector( cValues[ i ], 1 );
		else if ( "refeast" == tk[0] && tk.size() >= 2 ) 
			refChannelEast = atoi( tk[1].c_str() );
		else if ( "refwest" == tk[0] && tk.size() >= 2 ) 
			refChannelWest = atoi( tk[1].c_str() );
	}

	char tmp[256];
	sprintf(tmp,"VPD-vtx TAC Alignment West; Channel # ; TAC_{ch} - TAC_{ch=%i}", refChannelWest);
	contents.tac_align_west->SetTitle( tmp );
	sprintf(tmp,"VPD-vtx TAC Alignment East; Channel # ; TAC_{ch} - TAC_{ch=%i}", refChannelEast);
	contents.tac_align_east->SetTitle( tmp );

	eastGoodCh.clear();
	westGoodCh.clear();
	// init the channel masks
	for ( int i = 0; i < 16; i++ ){
		eastGoodCh.push_back( true ); // default to good
		westGoodCh.push_back( true ); // default to good
	}




	bool debug = false;

	if ( debug  ){

		for ( int i = 0; i < maskedChannelsEast.size(); i++ ){
			LOG("====VPD====", "Masked Channel East : %i", maskedChannelsEast[ i ] );
			eastGoodCh[ maskedChannelsEast[ i ] ] = false;
		}
		for ( int i = 0; i < maskedChannelsWest.size(); i++ ){
			LOG("====VPD====", "Masked Channel West : %i", maskedChannelsWest[ i ] );
			westGoodCh[ maskedChannelsWest[ i ] ] = false;
		}
		for ( int i = 0; i < 16; i++ ){
			if ( eastGoodCh[ i ] ){
				LOG("====VPD====", "Include East Channel %i", i );
			}
		}
		for ( int i = 0; i < 16; i++ ){
			if ( westGoodCh[ i ] ){
				LOG("====VPD====", "Include West Channel ", i );
			}
		}


		for ( int i = 0; i < expected_pulser_means_east.size(); i++ ){
			cout << "Pulser Mean East [" << i << " ] = " << expected_pulser_means_east[ i ] << endl;
		}
		for ( int i = 0; i < expected_pulser_means_west.size(); i++ ){
			cout << "Pulser Mean West [" << i << " ] = " << expected_pulser_means_west[ i ] << endl;
		}

		cout << "Reference Channel East " << refChannelEast << endl;
		cout << "Reference Channel West " << refChannelWest << endl;


	}


	filein.close();
}


int vpdBuilder::correctedTAC( StTriggerData2016 * td, int side, int channel  ){



	int pChannel = pulserCh( channel );								// pulser channel corresponding to this channel
	int adc = td->vpdADC( (StBeamDirection) side, channel+1 );			// channel's ADC
	int rawTac = td->vpdTDC( (StBeamDirection) side, channel+1 );		// channel's TAC
	int pulserTac = td->vpdTDC( (StBeamDirection) side, pChannel+1 );	// pulser's TAC

	// pulsers dont get corrections
	if ( channel == pChannel )
		return rawTac;


	int slewTac = rawTac;
	if ( 0 == side ) // EAST
		slewTac = corrEast( channel, adc, rawTac );
	if ( 1 == side ) // WEST
		slewTac = corrWest( channel, adc, rawTac );

	// get the mean of the pulser (as set by user in trigger config)
	int pulserMean = 0;
	if ( 0 == side )
		pulserMean = expected_pulser_means_east[ pulserChToIndex( pChannel ) ];
	if ( 1 == side )
		pulserMean = expected_pulser_means_west[ pulserChToIndex( pChannel ) ];

	// Apply the jitter correction
	int jitterCorr = pulserTac - pulserMean;
	if ( !noiseCorr )
		jitterCorr = 0;

	int corrTac = slewTac - jitterCorr;


	return corrTac;

}


void vpdBuilder::readParams(){

	char mConfigFile[256];
	sprintf(mConfigFile, "%s/tof/%s",confdatadir,"Slewing.txt");
	ifstream inf(mConfigFile); 
	
	if(!inf.good()){
		LOG( "====VPD====", "BAD or NO SLEWING FILE" );
		return;
	}

	for ( int i = 0; i < 19; i ++ ){
		for ( int j = 0; j < 10; j++){
			eBinEdges[ i ][ j ] = 0;
			wBinEdges[ i ][ j ] = 0;
			eCorrs[ i ][ j ] = 0;
			wCorrs[ i ][ j ] = 0;
		}
	}


	if ( !inf.good() ){
		
		return;
	}

	int iEast = 0;
	int iWest = 0;

	string line;
	while( getline( inf, line ) ){

		
		stringstream ss( line );

		string boardId;

		ss >> boardId;

		if ( "0x16" == boardId || "0x18" == boardId ){

			int channel;
			int nBins;
			int opt;

			ss >> channel >> nBins >> opt;
			

			numBins = nBins;
			
			if ( 0 == opt ) {// bin edges
				for ( int i = 0; i < nBins; i++ ){
					int be;
					ss >> be;
					
					if ( "0x16" == boardId ) // east
						eBinEdges[iEast][i] = be;
					else if ("0x18" == boardId) // west
						wBinEdges[iWest][i] = be;
				}
			} else if ( 1 == opt ) {// bin corrs
				for ( int i = 0; i < nBins; i++ ){
					int bc;
					ss >> bc;
					
					if ( "0x16" == boardId ) // east
						eCorrs[iEast][i] = bc;
					else if ("0x18" == boardId) // west
						wCorrs[iWest][i] = bc;
				}
			}
			if ( "0x16" == boardId && 1 == opt ) // east
				iEast ++;
			else if ("0x18" == boardId && 1 == opt ) // west
				iWest ++;	
		}

	}

	inf.close();
}

int vpdBuilder::corrEast( int iCh, int adc, int tac ){
	int bin = corrBin( iCh, adc, eBinEdges[ iCh ] );
	return tac + eCorrs[ iCh ][ bin ];
}
int vpdBuilder::corrWest( int iCh, int adc, int tac ){
	int bin = corrBin( iCh, adc, wBinEdges[ iCh ] );
	return tac + wCorrs[ iCh ][ bin ];
}

int vpdBuilder::corrBin( int iCh, int adc, int *bins ){
	
	// find the bin
	for ( int iBin = 0; iBin < numBins; iBin++){

		if ( 0 == iBin && adc >= 0 && adc <= bins[ 0 ] ){
			return 0;
		} else {
			if ( adc > bins[ iBin - 1 ] && adc <= bins[ iBin ] )
				return iBin;
		}
	}
	return -1;
}