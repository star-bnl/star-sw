#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_STGC/daq_stgc.h>
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1D.h>
#include <TH2F.h>
#include <TString.h>

#include <math.h>
#include "fttBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>

#include <algorithm>    // std::find
#include <vector>       // std::vector


// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

/*********************************************************/
// class for mapping the TPX electronics

#include <map>
#include <string>



class Strip2CH
{
private:
    std :: map< int , int > StripGroupLast; 
    std :: map< int , int > FEEs2Slot;
    std :: map< int , int > ALTRO1_2_Strip;
    std :: map< int , int > ALTRO2_2_Strip;
public:
    Strip2CH(){}
    
    void init( string confdatadir, string filename, int n_FEEs){
        TString baseConfPath;
        char baseFile[256];
        sprintf(baseFile, "%s/ftt/",confdatadir.c_str() );
        baseConfPath = TString( baseFile );
        
        // initialize number of last strip of one strip group
        ifstream inFile;
        inFile.open( (baseConfPath + "LastStrip.dat").Data() );
        int LastStrip = 0;
        int nGroup = 1;
        while ( inFile >> LastStrip)
        {
            // cout << LastStrip << endl;
            StripGroupLast[nGroup] = LastStrip;
            nGroup++;
        }
        inFile.close();

        // initialize FEEs to Slots
        inFile.open( (baseConfPath + filename.c_str()).Data() );
        nGroup = 1;
        int FEEnumber = 0;
        while (inFile >> FEEnumber)
        {
            // cout << FEEnumber << endl;
            FEEs2Slot[ FEEnumber ] = nGroup;
            nGroup++;
            if (nGroup > n_FEEs) break;
        }
        inFile.close();

        // initialize FEE channel to Strip channel
        inFile.open( (baseConfPath + "ALTRO1_2_Strip.dat").Data() );
        int FEE_Channel;
        int Strip_Channel;
        while (inFile >> FEE_Channel >> Strip_Channel ) {
            // cout << "FEE channel is " << FEE_Channel << " Strip Channel is " << Strip_Channel << endl;
            ALTRO1_2_Strip[FEE_Channel] = Strip_Channel;
        }
        inFile.close();

        // initialize FEE channel to Strip channel
        inFile.open( (baseConfPath + "ALTRO2_2_Strip.dat").Data() );
        while (inFile >> FEE_Channel >> Strip_Channel ) {
            // cout << "FEE channel is " << FEE_Channel << " Strip Channel is " << Strip_Channel << endl;
            ALTRO2_2_Strip[FEE_Channel] = Strip_Channel;
        }
        inFile.close();

    }
    void initFEEs( int* FEEs, int n_FEEs ) {}

    // get the strip number from the FEE ALTRO and channel number from electronics
    // must follow this RULE : ALTRO number = FEE*2 or FEE*2+1
    int GetStripNumber( int FEE, int ALTRO, int Channel )
    {
        int GroupLastStrip = StripGroupLast[ FEEs2Slot[ FEE ] ];
        map < int , int > Maps;
        if ( ALTRO == FEE*2 ) Maps = ALTRO1_2_Strip;
        if ( ALTRO == FEE*2+1 ) Maps = ALTRO2_2_Strip;

        int Strip_number = GroupLastStrip-Maps[Channel]+1;
        return Strip_number;
    }

    // 60*60 prototype layer1, there is a mirror flip between the layer 1 and layer 2,
    // the map needs to be modified.
    // this function is used to correct the difference between the layer 1 and layer 2
    int GetStripNumber_MirrorFlip( int FEE, int ALTRO, int Channel )
    {
        int GroupLastStrip = StripGroupLast[ FEEs2Slot[ FEE ] ];
        map < int , int > Maps;
        if ( ALTRO == FEE*2 ) Maps = ALTRO2_2_Strip;
        if ( ALTRO == FEE*2+1 ) Maps = ALTRO1_2_Strip;

        int Strip_number;
        if ( Channel <= 7 ) Strip_number = GroupLastStrip-Maps[Channel+8]+1;
        if ( Channel >= 8 ) Strip_number = GroupLastStrip-Maps[Channel-8]+1;
        return Strip_number;
    }


    ~Strip2CH() {}
};


/*********************************************************/
// class for mapping the TPX electronics



ClassImp(fttBuilder);
	
void fttBuilder::initialize(int argc, char *argv[]) {
	

    
    // TPX
    contents.tpxADC              = new TH1D( "tpxADC", "sTGC TPX; ADC; counts", 400,-0.5,1023.5);
    contents.tpxADCZoom          = new TH1D( "tpxADCZoom", "sTGC TPX; ADC; counts", 200,-0.5,199.5);
    contents.tpxFEE              = new TH1D( "tpxFEE", "sTGC TPX; FEE; counts", 31,-0.5,30.5);
    contents.tpxALTRO            = new TH1D( "tpxALTRO", "sTGC TPX; ALTRO; counts", 61,-0.5,60.5);
    contents.tpxCHANNEL          = new TH1D( "tpxCHANNEL", "sTGC TPX; CHANNEL; counts", 16,-0.5,15.5);
    // contents.tpxFEEALTRO         = new TH2D( "tpxFEEALTRO", "sTGC TPX; ALTRO; FEE", 61, -0.5, 99.5, 31,-0.5,30.5);
    contents.tpxALTROCHANNEL     = new TH2D( "tpxALTROCHANNEL", "sTGC TPX; ALTRO; Channel; counts", 61,-0.5,60.5,16,-0.5,15.5);
    contents.tpxLayerRowStrip[0] = new TH2D( "tpxLayer1RowStrip", "sTGC TPX Layer1; Strip; Row; counts", 200,-0.5,199.5,4,-0.5,3.5);
    contents.tpxLayerRowStrip[1] = new TH2D( "tpxLayer2RowStrip", "sTGC TPX Layer2; Strip; Row; counts", 200,-0.5,199.5,4,-0.5,3.5);

    contents.tpxLayerRowStripADC[0] = new TH2D( "tpxLayer1RowStripADC", "sTGC TPX Layer1; Strip; Row; ADC", 200,-0.5,199.5,4,-0.5,3.5);
    contents.tpxLayerRowStripADC[1] = new TH2D( "tpxLayer2RowStripADC", "sTGC TPX Layer2; Strip; Row; ADC", 200,-0.5,199.5,4,-0.5,3.5);

    contents.tpxLayerRowTimebinStripADC[0] = new TH2D( "tpxLayer1Row1TimebinStripADC", "sTGC TPX Layer1 Row 1; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);
    contents.tpxLayerRowTimebinStripADC[1] = new TH2D( "tpxLayer1Row2TimebinStripADC", "sTGC TPX Layer1 Row 2; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);
    contents.tpxLayerRowTimebinStripADC[2] = new TH2D( "tpxLayer1Row3TimebinStripADC", "sTGC TPX Layer1 Row 3; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);
    contents.tpxLayerRowTimebinStripADC[3] = new TH2D( "tpxLayer2Row1TimebinStripADC", "sTGC TPX Layer2 Row 1; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);
    contents.tpxLayerRowTimebinStripADC[4] = new TH2D( "tpxLayer2Row2TimebinStripADC", "sTGC TPX Layer2 Row 2; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);
    contents.tpxLayerRowTimebinStripADC[5] = new TH2D( "tpxLayer2Row3TimebinStripADC", "sTGC TPX Layer2 Row 3; Strip; Timebin; ADC", 200,-0.5,199.5,60,-0.5,59.5);

    contents.tpxTimebinStrip = new TH2D( "tpxTimebinStrip", "sTGC TPX; Strip; Timebin; counts", 200,-0.5,199.5,100,-0.5,99.5);
    contents.tpxTimebinADC   = new TH2D( "tpxTimebinADC", "sTGC TPX;Timebin; ADC; counts", 60,-0.5,59.5,1024,-0.5,1023.5);
    contents.tpxNStripsFired = new TH1D( "tpxNStripsFired", "sTGC TPX; nStripsFired; nEvents", 200, -0.5, 199.5 );

    std::vector<std::string> setLogy   = {"tpxADC", "tpxADCZoom", "tpxFEE", "tpxALTRO", "tpxCHANNEL"};
    std::vector<std::string> setLogz   = {"tpxTimebinADC"};
    std::vector<std::string> hideStats = {  "tpxALTROCHANNEL", "tpxLayer1RowStrip", "tpxLayer2RowStrip", 
                                            "tpxLayer1Row1TimebinStripADC", "tpxLayer1Row2TimebinStripADC", 
                                            "tpxLayer1Row3TimebinStripADC", "tpxLayer2Row1TimebinStripADC", 
                                            "tpxLayer2Row2TimebinStripADC", "tpxLayer2Row3TimebinStripADC", 
                                            "tpxLayer1RowStripADC", "tpxLayer2RowStripADC"
                                         };

    // VMM
    contents.ADC              = new TH1D( "ADC", "sTGC; ADC; counts", 400,-0.5,1023.5);
    contents.ADCZoom          = new TH1D( "ADCZoom", "sTGC; ADC; counts", 200,-0.5,199.5);

	// Add root histograms to Plots
	int np = sizeof(contents) / sizeof(TH1 *);
	JevpPlot *plots[np];


    // Add all of the plots
    for ( int i = 0; i < np; i++ ){
        if ( !contents.array[i] ) continue;
        plots[i] = new JevpPlot( contents.array[i] );
        
        // set some special options logy, opt stats
        std::string name = contents.array[i]->GetName();
        if ( std::find( setLogy.begin(), setLogy.end(), name ) != setLogy.end() ) {
            plots[i]->logy=1;
        }
        if ( std::find( hideStats.begin(), hideStats.end(), name ) != hideStats.end() ) {
            plots[i]->optstat=0;
        }
        if ( contents.array[i]->GetZaxis())
            contents.array[i]->GetZaxis()->SetLabelSize(10.0 / 360.0);

        addPlot(plots[i]);
    }

    // Set up the mapping for the TPX electronics
    tpxMapLayer1 = std::make_shared<Strip2CH>();
    tpxMapLayer2 = std::make_shared<Strip2CH>();
}
	
void fttBuilder::startrun(daqReader *rdr) {
	resetAllPlots();

    // Read the mapping parameters
    if ( tpxMapLayer1 ){
        tpxMapLayer1->init( string(confdatadir), "FEE2Slot_Layer1.dat", /*nFEEs=*/15 );
    }
    if ( tpxMapLayer2 ){
        tpxMapLayer2->init( string(confdatadir), "FEE2Slot_Layer2.dat", /*nFEEs=*/15 );
    }

}

void fttBuilder::stoprun(daqReader *rdr) {
}

void fttBuilder::processTPX(daqReader *rdr) {

    bool do_print = true;
    daq_dta *dd ;
    dd = rdr->det("stgc")->get("altro") ;   

    int nStripsFired = 0;

    bool altro_found = false;
    while(dd && dd->iterate()) {    
        altro_found = 1 ;

        if(do_print) {
            // there is NO RDO in the bank
            int ALTRO = dd->row;
            int FEE = dd->row/2;
            int CHANNEL = dd->pad;
            // printf("STGC ALTRO: sec %d, ALTRO %2d(FEE%02d):%02d\n",dd->sec,dd->row,dd->row/2,dd->pad) ;

            contents.tpxFEE->Fill( FEE );
            
            contents.tpxALTRO->Fill( ALTRO );
            contents.tpxCHANNEL->Fill( CHANNEL );
            contents.tpxALTROCHANNEL->Fill( ALTRO, CHANNEL );

            int Layer = -1;
            int strip = -1;
            if ( FEE <= 15 ){
                Layer = 1;
                strip = tpxMapLayer1->GetStripNumber_MirrorFlip( FEE, ALTRO, CHANNEL );
            }
            else{
                Layer = 2;
                strip = tpxMapLayer2->GetStripNumber( FEE, ALTRO, CHANNEL );
            }
            int row = strip / 1000;
            int rstrip = strip - row*1000;
            nStripsFired++;

            // printf( "STGC Strip: %d, Row: %d, strip: %d\n\n", strip, row, rstrip );


            contents.tpxLayerRowStrip[Layer-1]->Fill( rstrip, row );

            for(u_int i=0;i<dd->ncontent;i++) {
                int TB = dd->adc[i].tb;
                int ADC = dd->adc[i].adc;
                // printf("    %3d %3d\n",dd->adc[i].tb,dd->adc[i].adc) ;
                contents.tpxADC->Fill( dd->adc[i].adc );
                contents.tpxADCZoom->Fill( dd->adc[i].adc );

                contents.tpxTimebinStrip->Fill( rstrip, TB );
                contents.tpxTimebinADC->Fill( TB, ADC );

                contents.tpxLayerRowStripADC[Layer-1]->Fill( rstrip, row, ADC );
                contents.tpxLayerRowTimebinStripADC[((Layer-1)*3 + row)-1]->Fill( rstrip, TB, ADC );
            }
        }
    }

    contents.tpxNStripsFired->Fill( nStripsFired );
} // processTPX

void fttBuilder::processVMM(daqReader *rdr) {

    bool do_print = true;
    daq_dta *dd ;
    dd = rdr->det("stgc")->get("vmm") ;

    bool vmm_found = false;
    while(dd && dd->iterate()) {    
        vmm_found = 1 ;

        if(do_print) {
            // there is NO RDO in the bank
            printf("STGC VMM: sec %d, RDO %d\n",dd->sec,dd->rdo) ;

            struct stgc_vmm_t *vmm = (stgc_vmm_t *)dd->Void ;
            for(u_int i=0;i<dd->ncontent;i++) {
                u_char feb = vmm[i].feb_vmm >> 2 ;  // feb [0..5]
                u_char vm = vmm[i].feb_vmm & 3 ;    // VMM [0..3]

                printf("  FEB %d:%d, ch %02d: ADC %d, BCID %d\n",feb,vm,vmm[i].ch,
                       vmm[i].adc,vmm[i].bcid) ;

                contents.ADC->Fill( vmm[i].adc );
                contents.ADCZoom->Fill( vmm[i].adc );
            }
        }
    }
} // processVMM

void fttBuilder::event(daqReader *rdr) {
	LOG(DBG, "event #%d",rdr->seq);

    processTPX(rdr);
    // processVMM(rdr);

}

void fttBuilder::main(int argc, char *argv[])
{
	fttBuilder me;  
	me.Main(argc, argv);
}