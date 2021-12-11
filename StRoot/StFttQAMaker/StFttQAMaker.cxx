#include "StFttQAMaker.h"

// #include "StFttRawHitMaker/StFttRawHitMaker.h"

#include "StEvent/StFttRawHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"

#include "StEvent/StFttCluster.h"

#include "StFttDbMaker/StFttDb.h"

#include "TFile.h"
#include "Parameters.h"
#include "TCanvas.h"

#include <set>
//_____________________________________________________________                                                       
StFttQAMaker::StFttQAMaker(const char *name):StMaker("fttQA",name)
{                                            
    LOG_DEBUG << "StFttQAMaker::ctor"  << endm;
}
//_____________________________________________________________                                                       
StFttQAMaker::~StFttQAMaker()
{ 
}

//_____________________________________________________________                                                       
Int_t StFttQAMaker::Init()
{
    LOG_INFO << "StFttQAMaker::Init" << endm;
    mFile = new TFile( "fttQA.root", "RECREATE" );
    mFile->cd();

    BookHistograms();
    BookTree();
    return kStOk;
}
//_____________________________________________________________                                                       
Int_t StFttQAMaker::InitRun(Int_t runnumber)
{ 
    return kStOk;
}

//_____________________________________________________________                                                       
Int_t StFttQAMaker::FinishRun(Int_t runnumber)
{ 
    return kStOk;
}

//-------------------------------------------------------------                                                       
Int_t StFttQAMaker::Finish()
{ 
    LOG_INFO << "StFttQAMaker::Finish()" << endm;
    mFile->cd();

    WriteHistograms();

    mFttTree->Write();

    LOG_INFO << "StFttQAMaker::Finish() - writing *.fttQA.root ..." << endm;

    mFile->Write();
    mFile->Close();
    return kStOk;
}

//_____________________________________________________________                                                       
Int_t StFttQAMaker::Make()
{ 
    LOG_INFO << "StFttQAMaker::Make()" << endm;

    mEvent = (StEvent*)GetInputDS("StEvent");
    if(mEvent) {
        LOG_DEBUG<<"Found StEvent"<<endm;
    } else {
        return kStOk;
    }
    mFttCollection=mEvent->fttCollection();
    if(!mFttCollection) {
        return kStOk;
    } else {
        LOG_DEBUG <<"Found StFttCollection"<<endm;
    }
    
    MakeRawHitQA();
    MakeClusterQA();
    
    return kStOk;
}

//_____________________________________________________________  
void
StFttQAMaker::MakeRawHitQA(){
    

    mFttData.N = 0;
    for ( auto rawHit : mFttCollection->rawHits() ) {
        
        mH1d[ "adc" ]->Fill( rawHit->adc() );
        mH1d[ "bcid" ]->Fill( rawHit->bcid() );
        // mH1d[ "febvmm" ]->Fill( rawHit->feb_vmm );
        mH1d[ "feb" ]->Fill( rawHit->feb() );
        mH1d[ "vmm" ]->Fill( rawHit->vmm() );
        mH1d[ "ch" ]->Fill( rawHit->channel() );
        mH1d[ "tb" ]->Fill( rawHit->tb() );


        mFttData.sec[mFttData.N]       = rawHit->sector();
        mFttData.rdo[mFttData.N]       = rawHit->rdo();
        mFttData.adc[mFttData.N]       = rawHit->adc();
        mFttData.feb[mFttData.N]       = rawHit->feb();
        // mFttData.febvmm[mFttData.N] = rawHit->feb_vmm;
        mFttData.vmm[mFttData.N]       = rawHit->vmm();
        mFttData.ch[mFttData.N]        = rawHit->channel();
        mFttData.bcid[mFttData.N]      = rawHit->bcid();
        mFttData.tb[mFttData.N]        = rawHit->tb();

        mFttData.plane[mFttData.N]     = rawHit->plane();
        mFttData.quad[mFttData.N]      = rawHit->quadrant();
        mFttData.row[mFttData.N]       = rawHit->row();
        mFttData.strip[mFttData.N]     = rawHit->strip();
        mFttData.dir[mFttData.N]       = rawHit->orientation();
        mFttData.N++;

        TString name;
        name = Form("rdovsfeb_Plane_%d",rawHit->sector());
        mH2d[ name.Data() ]->Fill(rawHit->feb()+1,rawHit->rdo());

        mFttData.N++;
        cout << " now at " << mFttData.N << "th event" << endl;
        if ( rawHit->rdo() == 1 || rawHit->rdo() == 3)
        {
            name = Form("Plane_%d_",rawHit->sector());
            cout << name.Data() << endl;
            if ( rawHit->feb() == 0 || rawHit->feb() == 4 ) {name = name + "h"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 1 || rawHit->feb() == 5 ) {name = name + "v"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 2 ) {name = name + "Diag_2"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 3 ) {name = name + "Diag_1"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
        }
        if ( rawHit->rdo() == 2 || rawHit->rdo() == 4)
        {
            TString name = Form("Plane_%d_",rawHit->sector());
            cout << name.Data() << endl;
            if ( rawHit->feb() == 1 || rawHit->feb() == 5 ) {name = name + "h"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 0 || rawHit->feb() == 4 ) {name = name + "v"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 2 ) {name = name + "Diag_1"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
            if ( rawHit->feb() == 3 ) {name = name + "Diag_2"; cout << name.Data() << endl; Fill_sTGC( rawHit->feb()+1, rawHit->rdo(), mH2p[ name.Data() ] ,1);}
        }
        

        // LOG_INFO << "n = " << mFttData.N << endm;
    } // rawHits

    mFttData.cN = 0;
    for ( auto clu : mFttCollection->clusters() ) {
        mFttData.cplane[mFttData.cN]   = clu->plane();
        mFttData.cquad[mFttData.cN]    = clu->quadrant();
        mFttData.cdir[mFttData.cN]     = clu->orientation();

        mFttData.crow[mFttData.cN]     = clu->row();
        mFttData.csumadc[mFttData.cN]  = clu->sumAdc();
        mFttData.cnstrips[mFttData.cN] = clu->nStrips();
        mFttData.cx[mFttData.cN]       = clu->x();
        mFttData.csigma[mFttData.cN]   = clu->sigma();
        mFttData.cN++;
    }

    mFttTree->Fill();
}

void StFttQAMaker::PlotClusterWithHits( vector<StFttRawHit*> hits ){
    if( histCounter > 100 ) return;
    LOG_INFO << "PlotClusterWithHits :: Event " << GetIventNumber() << endm;

    bool sort_and_dedup = true;

    string label = "(w/dups)";
    if ( sort_and_dedup ){
        auto cmp = [](StFttRawHit* a, StFttRawHit* b) { 
            
            return  a->plane() < b->plane() ||
                    a->quadrant() < b->quadrant() ||
                    a->row() < b->row() ||
                    a->strip() < b->strip(); 
        };
    
        // NOTE according to SO this is faster than using ctor
        set<StFttRawHit*, decltype(cmp)> s(cmp);
        unsigned size = hits.size();
        for( auto h : hits ) s.insert( h );
        hits.assign( s.begin(), s.end() );
    
        sort(hits.begin(), hits.end(), [](const StFttRawHit * a, const StFttRawHit * b) -> bool { 
                size_t indexA = a->strip() + a->row() * StFttDb::maxStripPerRow;
                size_t indexB = b->strip() + b->row() * StFttDb::maxStripPerRow;
                return indexA < indexB; 
            });
        string label = "(wo/dups)";
    }

    TCanvas *c = new TCanvas( "c", "c", 1200, 900 );
    string n = TString::Format( "h%lu", histCounter ).Data();
    auto h0 = hits[0];
    int indexHit = (int)h0->orientation() + 6 * ( h0->row() + 6 * ( h0->quadrant() + 5 * h0->plane() ));
    TH1 *h1 = new TH1F( n.c_str(), 
                        TString::Format( "[Event: %lu] Plane %d, Quadrant %d, Row %d, Dir %d %s", GetIventNumber(), h0->plane(), h0->quadrant(), h0->row(), h0->orientation(), label.c_str() ), 
                        200, -0.5, 200-0.5 );
    float minX = 640;
    float maxX = 0;
    for ( auto h : hits ){
        float x = h->strip();
        h1->Fill( x, h->adc() );

        if ( x < minX )
            minX = x;
        if ( x > maxX )
            maxX = x;
    }
    h1->GetXaxis()->SetRangeUser( minX - 10.0, maxX + 10.0 );
    h1->Draw();

    
    for ( auto clu : mFttCollection->clusters() ) {
        int indexClu = (int)clu->orientation() + 6 * ( clu->row() + 6 * ( clu->quadrant() + 5 * clu->plane() ));

        if ( indexHit != indexClu ) continue;

        float s = clu->sigma();
        if ( s != s || s < 1 )
            s = 1;
        TF1 * f1 = new TF1( TString::Format( "f1%lu", f1CluCounter ), "gaus" );
        f1->SetParameters( clu->sumAdc() / 3.2, (clu->x() + 1.6) / 3.2 , s / 3.2 );
        f1->SetRange( 0, 640 );
        f1->SetNpx( 1000 );
        f1->Draw("same");
        f1CluCounter++;

        LOG_INFO << "Cluster at (" << clu->sumAdc() << ", " << clu->x() << ", " << clu->sigma() << " )" << endm;
    }

    LOG_INFO << "Printing h" << histCounter << ".pdf" << endm;
    c->Print( TString::Format( "h%lu.pdf", histCounter ) );

    mH1d[n] = h1;
    LOG_INFO << "Adding Cluster+Hits Histogram: " << n << endm;
    histCounter++;

}


void StFttQAMaker::MakeClusterQA(){
    LOG_INFO << "MakeClusterQA :: Event " << GetIventNumber() << " has " << mFttCollection->rawHits().size() << " rawHits" << endm;




    map< int, vector<StFttRawHit*> > hitProj;
    for ( auto rawHit : mFttCollection->rawHits() ) {
        int index = (int)rawHit->orientation() + 6 * ( rawHit->row() + 6 * ( rawHit->quadrant() + 5 * rawHit->plane() ));

        if ( rawHit->tb() < -70 || rawHit->tb() > 20 ) continue;

        if ( 16 == GetIventNumber() ){
            LOG_INFO << "[index=" << index << "] ->" << *rawHit << endm;
        }

        hitProj[index].push_back( rawHit );
    }


    for ( auto kv : hitProj ){
        PlotClusterWithHits( kv.second );
    }

}



//_____________________________________________________________  
void
StFttQAMaker::WriteHistograms()
{
    LOG_DEBUG << "StFttQAMaker::WriteHistograms()" << endm;
    for( const auto& kv : mH1d ) {
        if( kv.second->GetEntries() > 0 ) kv.second->Write();
    }
    for( const auto& kv : mH2d ) {
        if( kv.second->GetEntries() > 0 ) kv.second->Write();
    }
    cout << "debug" << endl;
    for( const auto& kv : mH2p ) {
        kv.second->Print();
        if( kv.second->GetEntries() > 0 ) kv.second->Write();
        
    }

}
//_____________________________________________________________  
void
StFttQAMaker::BookHistograms()
{

    mH1d[ "adc" ]    = new TH1F( "adc", "adc", 1025, 0, 1025 );
    mH1d[ "bcid" ]   = new TH1F( "bcid", "bcid", 1024, 0, 4096 );
    mH1d[ "feb" ]    = new TH1F( "feb", "feb", 10, 0, 10 );
    mH1d[ "febvmm" ] = new TH1F( "febvmm", "febvmm", 4096, 0, 4096 );
    mH1d[ "vmm" ]    = new TH1F( "vmm", "vmm", 10, 0, 10 );
    mH1d[ "ch" ]     = new TH1F( "ch", "ch", 100, 0, 100 );
    mH1d[ "tb" ]     = new TH1F( "tb", "tb", 1000, -32000, 32000 );


    for ( int i = 1; i <= 4; i++)
    {
        //set TH2Poly for Geo plot check
        TString name = Form("Plane_%d_v",i); 
        TString title = Form("Plane_%d_v;X (mm); Y (mm)",i); 
        mH2p[ name.Data() ] = SetsTGC_v(name,title);
        name = Form("Plane_%d_h",i); 
        title = Form("Plane_%d_h;X (mm); Y (mm)",i); 
        mH2p[ name.Data() ] = SetsTGC_h(name,title);
        name = Form("Plane_%d_Diag_1",i); 
        title = Form("Plane_%d_Diag_1;X (mm); Y (mm)",i); 
        mH2p[ name.Data() ] = SetsTGc_Dig_1(name,title);
        name = Form("Plane_%d_Diag_2",i); 
        title = Form("Plane_%d_Diag_2;X (mm); Y (mm)",i); 
        mH2p[ name.Data() ] = SetsTGc_Dig_2(name,title);

        //set 2D plot to check feb
        name = Form("rdovsfeb_Plane_%d",i);
        mH2d[ name.Data() ] = new TH2F(name.Data(),";feb;rdo",6,0.5,6.5,4,0.5,4.5);

    }

    for( auto& kv : mH1d ) {
        kv.second->SetDirectory( 0 );
    }

    for( auto& kv : mH2d ) {
        kv.second->SetDirectory( 0 );
    }

    for( auto& kv : mH2p ) { 
        kv.second->SetDirectory( 0 );
    }
}

//_____________________________________________________________  
void
StFttQAMaker::BookTree()
{
    cout << " Booking the event tree " << endl;
    mFttTree = new TTree("ftt","ftt");
    mFttTree->SetAutoSave(1000); 

    // Event information
    mFttTree->Branch("EVT"      , &mFttData.EVT     , "EVT/I");
    mFttTree->Branch("N"        , &mFttData.N       , "N/I");

    // Channel information
    mFttTree->Branch("sec"      , mFttData.sec      , "sec[N]/I");
    mFttTree->Branch("rdo"      , mFttData.rdo      , "rdo[N]/I");
    mFttTree->Branch("plane"    , mFttData.plane    , "plane[N]/I");
    mFttTree->Branch("quad"     , mFttData.quad     , "quad[N]/I");
    mFttTree->Branch("feb"      , mFttData.feb      , "feb[N]/I");
    mFttTree->Branch("febvmm"   , mFttData.febvmm   , "febvmm[N]/I");
    mFttTree->Branch("vmm"      , mFttData.vmm      , "vmm[N]/I");
    mFttTree->Branch("ch"       , mFttData.ch       , "ch[N]/I");
    mFttTree->Branch("bcid"     , mFttData.bcid     , "bcid[N]/I");
    mFttTree->Branch("adc"      , mFttData.adc      , "adc[N]/I");
    mFttTree->Branch("tb"       , mFttData.tb       , "tb[N]/I");
    mFttTree->Branch("row"      , mFttData.row      , "row[N]/I");
    mFttTree->Branch("strip"    , mFttData.strip    , "strip[N]/I");
    mFttTree->Branch("dir"      , mFttData.dir      , "dir[N]/I");

    mFttTree->Branch("cN"       , &mFttData.cN      , "cN/I");
    mFttTree->Branch("cplane"   , mFttData.cplane   , "cplane[cN]/I");
    mFttTree->Branch("cquad"    , mFttData.cquad    , "cquad[cN]/I");
    mFttTree->Branch("crow"     , mFttData.crow     , "crow[cN]/I");
    mFttTree->Branch("cdir"     , mFttData.cdir     , "cdir[cN]/I");
    mFttTree->Branch("cx"       , mFttData.cx       , "cx[cN]/F");
    mFttTree->Branch("csigma"   , mFttData.csigma   , "csigma[cN]/F");
    mFttTree->Branch("cnstrips" , mFttData.cnstrips , "cnstrips[cN]/F");
    mFttTree->Branch("csumadc"  , mFttData.csumadc  , "csumadc[cN]/I");
}
