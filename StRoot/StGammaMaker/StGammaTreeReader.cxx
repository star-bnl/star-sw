#include "StGammaTreeReader.h"

#include "TString.h"
#include <algorithm>

ClassImp(StGammaTreeReader);

struct ID {
  int run;
  int evt;
};


// ----------------------------------------------------------------------------
StGammaTreeReader::StGammaTreeReader(const Char_t *name,const Char_t *bname):StMaker(name)
{
  mChain=new TChain(name,"gamma");
  mBranchName=bname;
  index=0;
  mNumberOfFiles=0;
  mIndexed=0;
  mFirst=false;
}

// ----------------------------------------------------------------------------
Int_t StGammaTreeReader::Init()
{ 
  mEvent=new StGammaEvent();
  mChain-> SetBranchAddress(mBranchName,&mEvent);
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StGammaTreeReader::Make()
{
  // when run as a maker, retrieves each event in sequence
  Int_t stat= getEvent(index++);
  if ( !stat ) return kStEOF;
  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StGammaTreeReader::getEvent(Long64_t i)
{
  if ( mFirst ) treeDetails();
  Int_t stat=mChain->GetEntry(i);
  return stat;
}

Int_t StGammaTreeReader::getEvent(Int_t run, Int_t myevent)
{

  if ( !mIndexed ) buildIndex();
  Long64_t mykey = key(run,myevent);
  Long64_t entry = mIndex[ mykey ];
  if ( !entry ) {
    mEvent->Clear();
    assert(myevent);
    return 0;
  }
  return getEvent( entry );
}

void StGammaTreeReader::treeDetails()
{
  mFirst=false;
  std::cout << GetCVS() << std::endl;
  std::cout << mChain->GetTitle() << std::endl; 

}
// ----------------------------------------------------------------------------
void StGammaTreeReader::Clear(Option_t *opts)
{
}

// ----------------------------------------------------------------------------
void StGammaTreeReader::chainFile( const Char_t *file, const Char_t *matches )
{
  
  TString fname=file;
  if ( fname.Contains(matches) ) // must be a root file
    {
      std::cout << Form("[%02i] add %s",mNumberOfFiles++,file ) << std::endl;
      mChain->Add(fname);
    }

}

// ----------------------------------------------------------------------------
void StGammaTreeReader::buildIndex()
{

  // loop over all entries in TTree
  Long64_t N = getNumberOfEntries();
  std::cout << GetName() << "::Indexing " << N << " entries" << std::endl;

  for ( Long64_t i=0;i<N;i++ )
    {
      if ( !getEvent(i) ) continue;          // read in event
      Int_t run   = mEvent->runNumber();
      Int_t event = mEvent->eventNumber();
      Long64_t mykey = key(run,event);       // generate key
      mIndex[ mykey ] = i;                   // add event to index
    }
  mIndexed=1;

}

// ----------------------------------------------------------------------------
void StGammaTreeReader::Test()
{

  static Int_t nmiss=0;
  static Int_t ngood=0;

 Long64_t N = getNumberOfEntries();

 std::vector<ID> ids;
 for ( Long64_t i=0;i<N;i++ )
   {
     ID myid ;
     if ( !getEvent(i) ) continue;
     myid.run = mEvent->runNumber();
     myid.evt = mEvent->eventNumber();
     ids.push_back(myid); // add this ID to list of IDs
   }
 
 // Shuffle events randomly
 std::random_shuffle( ids.begin(), ids.end() );

 std::cout << Form("Verifying indexing scheme for %g events",(double)N) 
	   << std::endl;

 for ( Long64_t i=0;i<N;i++ )
   {

     if (!getEvent( ids[i].run, ids[i].evt ))continue;


     Bool_t state=true;
     if ( mEvent->runNumber() != ids[i].run ||
	  mEvent->eventNumber() != ids[i].evt ) {
       mEvent->Print();
       state=false;
     }
     else
       ngood++;

     if ( !state )
       std::cout << Form("run=%i event=%i state=%s",ids[i].run,ids[i].evt,"bad")
		 << std::endl;


     assert( mEvent->runNumber() == ids[i].run );   // Run mismatch
     assert( mEvent->eventNumber() == ids[i].evt ); // Event mismatch

   }
 std::cout << Form("Verified %g of %g events were properly indexed",(double)ngood,(double)N) << std::endl;

}

Long64_t StGammaTreeReader::key( Int_t run, Int_t event )
{
 
  Long64_t mykey=(Long64_t)run;
  mykey  = ( mykey << 31 );
  mykey += event;
  
  //std::cout << Form("run=%i event=%i",run,event,mykey) << std::endl;
  return mykey;

}
