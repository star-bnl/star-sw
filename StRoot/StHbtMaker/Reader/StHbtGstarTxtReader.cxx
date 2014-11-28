/***************************************************************************
 *
 * $Id: 
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading
 *       event-generator (e.g. mevsim) files in GSTAR text format
 *
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/
#define HBT_BFIELD 0.5*tesla
#include <Stiostream.h>
#include "Stiostream.h"
#include "StHbtMaker/Reader/StHbtGstarTxtReader.h"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
// hbt stuff
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "phys_constants.h"
#include "StPhysicalHelix.hh"

#ifdef __ROOT__
ClassImp(StHbtGstarTxtReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif

/* =========== some useful functions in parsing the strings ============== */
int get_next_int(string strline,int start_at,int& wordends){
  int wordstarts = strline.find_first_not_of(" ",start_at);
  wordends   = strline.find_first_of(" ",wordstarts);
  int ians = atoi((strline.substr(wordstarts,wordends-1)).c_str());
  return ians;
}
float get_next_float(string strline,int start_at, int& wordends){
  int wordstarts = strline.find_first_not_of(" ",start_at);
  wordends   = strline.find_first_of(" ",wordstarts);
  float ans = atof((strline.substr(wordstarts,wordends-1)).c_str());
  return ans;
}
double dedxMean_geantTxt(double mass, double momentum){
  double dedxMean;
  double tpcDedxGain = 0.174325e-06;
  double tpcDedxOffset = -2.71889; 
  double tpcDedxRise = 776.626;
  
  double gamma = ::sqrt(::pow(momentum/mass,2)+1.);
  double beta = ::sqrt(1. - 1./::pow(gamma,2));
  double rise = tpcDedxRise*::pow(beta*gamma,2);      
  if ( beta > 0)
    dedxMean = tpcDedxGain/::pow(beta,2) * (0.5*::log(rise)-::pow(beta,2)- tpcDedxOffset);
  else
    dedxMean = 1000.;
  return dedxMean;
}
/* ======================================================================== */


//_______________________________
StHbtGstarTxtReader::StHbtGstarTxtReader() : mInputStream(0){
  mFileName = "GstarTextFile";  // default name
  mReaderStatus = 0;           // means "good"
}

//_______________________________
StHbtGstarTxtReader::StHbtGstarTxtReader(char* file) : mInputStream(0), mFileName(file)
{
  mReaderStatus = 0;           // means "good"
}

//_______________________________
StHbtGstarTxtReader::~StHbtGstarTxtReader(){
  if (!mInputStream){
    delete mInputStream;
    mInputStream = 0;
  }
}

//_______________________________
StHbtEvent* StHbtGstarTxtReader::ReturnHbtEvent(){
  if (!mInputStream){
    cout << "StHbtGstarTxtReader::ReturnHbtEvent() - there is no input stream!";
    mReaderStatus = 1;           // 0 means "good"
    return (0);
  }
  if (!(*mInputStream)){
    cout << "StHbtGstarTxtReader::ReturnHbtEvent() - input stream in bad state!" << endl;
    cout << "State is " << mInputStream->rdstate() << endl;
    mReaderStatus = 1;           // 0 means "good"
    return (0);
  }
  // ok find the next EVENT keyword...
  string strline;
  char line[100];
  string keyword;
  int stringposition;

  //vvvvvvvvvvvvvvvvvvvv find and decode EVENT line vvvvvvvvvvvvvvvvvvv
  keyword = "EVENT:";

  cout << "StHbtGstarTxtReader::ReturnHbtEvent() -- find and decode EVENT line..." << endl;
  while (strline.substr(0,keyword.size()) != keyword){
    (*mInputStream).getline(line,100);
    strline = line;
    if (!(mInputStream->good())){
      cout << "StHbtGstarTxtReader::ReturnHbtEvent() - input stream in bad state!" <<endl;
      cout << "State is " << mInputStream->rdstate() << endl;
      mReaderStatus = 1;           // 0 means "good"
      return (0);
      //mInputStream->clear();
    }
  }


  // event number...
  int ievent = get_next_int(strline,keyword.size()+1,stringposition);
  int ntracks = get_next_int(strline,stringposition,stringposition);
  int nvertices = get_next_int(strline,stringposition,stringposition);
  cout << "Event number is " << ievent << endl;
  cout << "Number of tracks is " << ntracks << endl;
  cout << "Number of vertices is " << nvertices << endl;

  if (ntracks <= 0){   //
    cout << "StHbtGstarTxtReader::ReturnHbtEvent() - hit end-of-file " << endl;
    cout << "State is " << mInputStream->rdstate() << endl;
    mReaderStatus = 1;           // 0 means "good"
    return (0);
  }
  //^^^^^^^^^^^^^^^^^^^^^^^^ done finding and decoding EVENT line ^^^^^^^^^^^^^^^^

  StHbtEvent* event = new StHbtEvent;
  const StHbtThreeVector vertexPos(0.0,0.0,0.0);

  event->SetEventNumber(ievent);
  event->SetCtbMult(0);
  event->SetZdcAdcEast(0);
  event->SetZdcAdcWest(0);
  event->SetNumberOfTpcHits(0);
  event->SetNumberOfTracks(ntracks);
  event->SetNumberOfGoodTracks(ntracks);  // same for now
  event->SetReactionPlane(0.);
  event->SetReactionPlaneSubEventDifference(0.);
  event->SetPrimVertPos(vertexPos); 


  // now decode TRACK lines...
  int pid;
  float px,py,pz;
  StHbtThreeVector p;
  keyword = "TRACK:";
  for (int itrack=0; itrack<ntracks; itrack++){
    strline = " ";
    while (strline.substr(0,keyword.size()) != keyword){
      (*mInputStream).getline(line,100);
      strline = line;
      if (!(mInputStream->good())){
	cout << "StHbtGstarTxtReader::ReturnHbtEvent() - input stream in bad state!" <<endl;
	cout << "State is " << mInputStream->rdstate() << endl;
	mReaderStatus = 1;           // 0 means "good"
	return (0);
	//mInputStream->clear();
      }
    }


    pid = get_next_int(strline,keyword.size()+1,stringposition);
    px = get_next_float(strline,stringposition,stringposition);
    py = get_next_float(strline,stringposition,stringposition);
    pz = get_next_float(strline,stringposition,stringposition);

    int charge = -1;
    float mass=0.0;

    StHbtTrack* hbtTrack = new StHbtTrack;

    //    cout << "Pid is " << pid;
    hbtTrack->SetTrackId(itrack);

    switch (pid){
    case 2:  // intentional fall-through
      charge = 1;
    case 3:  // pion
      hbtTrack->SetNSigmaElectron(0.);
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      mass = 5.1099905E-04;
      break;
    case 8:  // intentional fall-through
      charge = 1;
    case 9:  // pion
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(0.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      mass = 0.139567;
      break;
    case 11:  // intensional fall-thru
      charge = 1;
    case 12:
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.0);
      hbtTrack->SetNSigmaKaon(0.);
      hbtTrack->SetNSigmaProton(-999.);
      mass = 0.493667;
      break;
    case 14:  // proton
      charge = 1;
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(999.);
      hbtTrack->SetNSigmaProton(0.);
      mass = 0.93828;
      break;
    default:
      //      cout << "Non-recognized track -- pid = " << pid << endl;
      charge = -99;
      mass = 0.0;
      break;
    }

    if (charge == -99){
      delete hbtTrack;
      //      cout << "Removing StHbtTrack" << endl;
      continue;  // do NOT make a StHbtTrack-- continue with loop...
    }


    StHbtThreeVector tmpP;
    tmpP.setX(px);
    tmpP.setY(py);
    tmpP.setZ(pz);
    hbtTrack->SetP( tmpP );

    //    cout << "Pid is " << pid << " and momentum is " << tmpP << endl;

    // place-holder
    hbtTrack->SetNHits(45);
    hbtTrack->SetNHitsPossible(45);
    hbtTrack->SetdEdx( dedxMean_geantTxt( mass, tmpP.mag() ) );
    hbtTrack->SetPt( tmpP.perp());
    hbtTrack->SetCharge(charge);

    StPhysicalHelixD helix = StPhysicalHelixD( hbtTrack->P(), vertexPos, HBT_BFIELD, hbtTrack->Charge() ); 
    hbtTrack->SetHelix(helix);

    hbtTrack->SetDCAxy(0.001);
    hbtTrack->SetDCAz(0.001);
    hbtTrack->SetChiSquaredXY( 0.); 
    hbtTrack->SetChiSquaredZ( 0.); 

    event->TrackCollection()->push_back(hbtTrack);
  }
  cout << event->TrackCollection()->size() << " tracks pushed to collection" << endl;

  return event;
}

//_______________________________
StHbtString StHbtGstarTxtReader::Report(){
  StHbtString temp = "\n This is the StHbtGstarTxtReader - no Early Cuts applied\n";
  temp += "  *** NOTE I am kinda stupid-- I do NOT handle vertices, and I ONLY\n";
  temp += "  *** know about pions protons and kaons\n";
  return temp;
}


//_______________________________
int StHbtGstarTxtReader::Init(const char* ReadWrite, StHbtString& Message){
  cout << " *\n *\n *\n StHbtGstarTxtReader::Init() being called*\n *\n";
  mReaderStatus = 0;           // means "good"
  //  if ((ReadWrite=="r")|| (ReadWrite=="R")){  // this object will be a reader
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    mInputStream = new ifstream;
    mInputStream->open(mFileName);
    if (!(*mInputStream)){
      cout << "StHbtGstarTxtReader::Init() - Cannot open input file! " << endl;
      return (1);
    }
    cout << "StHbtGstarTxtReader::Init() - being configured as a Reader - " << ReadWrite << endl;
  }
  else{                                      // this object will be a writer
    cout << " CANNOT BE CONFIGURED AS A WRITER" << endl;
    return (1);
  }
  return (0);
}

//_______________________________
void StHbtGstarTxtReader::Finish(){
  if (mInputStream) mInputStream->close();
}

