// $Id $
/// \File StiMaker.cxx
/// \author M.L. Miller 5/00
/// \author C Pruneau 3/02
// $Log: StiMaker.cxx,v $
// Revision 1.142  2004/04/15 00:43:22  pruneau
// Added Ssd to the list of possible detectors...
//
// Revision 1.141  2004/03/26 15:30:06  andrewar
// bug in field reset
//
// Revision 1.140  2004/03/26 14:52:43  calderon
// Print out the magnetic field read from StEvent::eventSummary()
//
// Revision 1.139  2004/03/25 22:42:44  andrewar
// temp mag field fix; cache filed value and reset if it goes to zero. This
// protects against corrupt event headers...
//
// Revision 1.138  2004/02/24 01:59:46  jeromel
// Commented out include of disappeared .h
//
// Revision 1.137  2004/02/21 18:28:31  pruneau
// Updates to comply to changes in interfaces
//
// Revision 1.136  2004/02/19 22:18:07  pruneau
// Modified call to StMcEventMaker structure
//
// Revision 1.135  2004/02/13 17:36:24  andrewar
// Changed name of StMcEventMaker to StMcEvent... this allows me to run
// simulation. It doesn't seem like this follows the Maker name scheme, though...
//
// Revision 1.134  2004/02/03 18:10:10  pruneau
// Changed name of StMcEventMaker to McEvent in GetMaker call
//
// Revision 1.133  2004/01/30 21:47:23  pruneau
// Changed organization so detector geometris are loaded and build in InitRun
// rather than Make.
// Added accesses to db
//
// Revision 1.132  2003/10/28 16:01:15  andrewar
// Passing tracking parameter file to detector Builders.
//
// Revision 1.131  2003/09/02 17:59:59  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.130  2003/08/05 18:20:33  andrewar
// Changed default parameters to apply eta filters.
//
// Revision 1.129  2003/07/30 20:12:31  pruneau
// Added new histo group
//
// Revision 1.128  2003/06/10 18:47:28  andrewar
// Changed StiResiduaCalc calls to conform to modified class.
//
// Revision 1.127  2003/05/07 03:06:34  pruneau
// *** empty log message ***
//
// Revision 1.126  2003/05/06 16:48:10  mmiller
// Incorporated StiPixel.  usePixel==false by default.
//
// Revision 1.125  2003/05/06 15:36:36  mmiller
// Committing changes to turn on multiple regions (StiPlacement::StiRegion -> kMidRapidity, kForwardRapidity, etc).
// Also added a point to StiToolkit for StiMaker.  This allows for the req. GetDataSet calls in the FTPC code.
// Not so elegant...
//
// Revision 1.124  2003/04/30 15:39:33  pruneau
// Integrating StiResidual in main stream Sti
//
// Revision 1.123  2003/04/29 18:48:50  pruneau
// *** empty log message ***
//
// Revision 1.122  2003/04/13 02:16:13  pruneau
// *** empty log message ***
//
// Revision 1.121  2003/04/11 18:56:14  pruneau
// Pulling the B field from StEventSummary
//
// Revision 1.120  2003/04/11 16:51:57  pruneau
// various fixes
//
// Revision 1.119  2003/04/10 14:53:06  pruneau
// removing obsolete files and classes
//
// Revision 1.118  2003/04/10 12:10:09  pruneau
// Changed StiMaker and Default Toolkit to accomodate the new Event Display
//
// Revision 1.117  2003/03/31 17:19:27  pruneau
// various
//
// Revision 1.116  2003/03/17 17:44:49  pruneau
// *** empty log message ***
//
// Revision 1.115  2003/03/13 18:59:42  pruneau
// various updates
//
// Revision 1.114  2003/03/13 16:30:59  andrewar
// Added plotting package
//
// Revision 1.113  2003/03/13 15:15:51  pruneau
// various
//
// Revision 1.112  2003/03/12 17:58:04  pruneau
// fixing stuff
//
// Revision 1.111  2003/02/25 14:21:06  pruneau
// *** empty log message ***
//
// Revision 1.110  2003/01/24 06:12:28  pruneau
// removing centralized io
//
// Revision 1.109  2003/01/22 20:06:26  andrewar
// Changed includes to point to new libraries (StiTpc, StiSvt, etc)
//
// Revision 1.108  2002/12/19 19:29:42  pruneau
// *** empty log message ***
//
// Revision 1.106  2002/10/04 01:54:48  pruneau
// DefaultToolkit now uses the StiHitLoader scheme rahter than the StiHitFiller.
//
// Revision 1.105  2002/09/27 19:19:01  mmiller
// Changed program flow to once again allow for track by track gui.
//
// Revision 1.104  2002/09/10 18:42:40  pruneau
// Fixed bug in the call sequence of the association maker
// introduced in the previous release.
//
// Revision 1.103  2002/09/05 21:27:10  pruneau
// Fixed problem with StiRootSimpleTrackFilter::makeNewObject
//
// Revision 1.102  2002/09/05 05:47:30  pruneau
// Adding Editable Parameters and dynamic StiOptionFrame
//
// Revision 1.101  2002/08/28 17:14:18  pruneau
// Simplified the interface of StiKalmanTrackFinder and the calls
// required in StiMaker.
//
// Revision 1.100  2002/08/23 18:16:50  pruneau
// Added StiSimpleTrackFilter to StiMaker to enable simple and
// fast track finding diagnostics.
//
// Revision 1.99  2002/08/19 19:32:59  pruneau
// eliminated cout when unnecessary, made helix member of the EventFiller
//
// Revision 1.98  2002/06/26 23:05:31  pruneau
// changed macro
//
// Revision 1.97  2002/06/18 18:08:34  pruneau
// some cout statements removed/added
//
// Revision 1.96  2002/06/04 19:45:31  pruneau
// including changes for inside out tracking
//
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "Sti/Base/EditableFilter.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiMasterDetectorBuilder.h"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "StiFtpc/StiFtpcDetectorGroup.h"
#include "StiTpc/StiTpcDetectorGroup.h"
#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiSsd/StiSsdDetectorGroup.h"
#include "StiEmc/StiEmcDetectorGroup.h"
#include "StiPixel/StiPixelDetectorGroup.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiHitLoader.h"
//#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiVertexFinder.h"
#include "Sti/StiResidualCalculator.h"
#include "Sti/StiDetectorContainer.h"
#include "StiMaker/StiMakerParameters.h"
#include "StiMaker/StiStEventFiller.h"
#include "StiGui/EventDisplay.h"
#include "StiDefaultToolkit.h"
#include "StiMaker.h"
#include "TFile.h"
#include "TCanvas.h"
#include "Sti/Html/HistoDocument.h"
#include "Sti/StiTrackingPlots.h"
#include "Sti/RadLengthPlots.h"
#include "Sti/StiTrackingParameters.h"
#include "Sti/StiKalmanTrackFinderParameters.h"
#include "Sti/StiKalmanTrackFitterParameters.h"
#include "StiTpc/StiTpcDetectorBuilder.h"
#include "StiSvt/StiSvtDetectorBuilder.h"
#include "Sti/StiHitErrorCalculator.h"
#include "TDataSet.h"
#include "tables/St_TrackingParameters_Table.h"
#include "tables/St_KalmanTrackFinderParameters_Table.h"
#include "tables/St_KalmanTrackFitterParameters_Table.h"
#include "tables/St_HitError_Table.h"


ClassImp(StiMaker)
  
  StiMaker::StiMaker(const Char_t *name) : 
    StMaker(name),
    _pars(0),
    _initialized(false),
    _toolkit(StiToolkit::instance() ),
    _hitLoader(0),
    _seedFinder(0),
    _tracker(0),
		_fitter(0),
    _eventFiller(0),
    _trackContainer(0),
    _vertexFinder(0),
    mMcEventMaker(0),
    mAssociationMaker(0),
    _recPlotter(0),
    _mcPlotter(0),
    _radLength(0),
    _residualCalculator(0),
    _loaderTrackFilter(0),
    _loaderHitFilter(0)

{
    cout <<"StiMaker::StiMaker() -I- Starting"<<endl;
    _toolkit->setStiMaker(this);
}

StiMaker::~StiMaker() 
{
  cout <<"StiMaker::~StiMaker() -I- Started/Done"<<endl;
}

void StiMaker::Clear(const char*)
{
  if (_initialized) 
      _tracker->clear();
  StMaker::Clear();
}

Int_t StiMaker::Finish()
{
  if (_pars->doPlots)
    {
      TCanvas * canvas = new TCanvas();
      if (_radLength)
	{
	  _radLength->write("StiMakerHistograms.root");
	  //HistoDocument histoDocumentRec("html/radLength","RadLength","Radiation Length Plots",canvas);
	  //histoDocumentRec.generateWebPage(_radLength);
	}
      if (_recPlotter) 
	{
	  _recPlotter->write("StiMakerHistograms.root","UPDATE");
	  //HistoDocument histoDocumentRec("html/rec","ReconstructedData","Reconstructed Data",canvas);
	  //histoDocumentRec.generateWebPage(_recPlotter);
	}
      if (_mcPlotter)
	{
	  _mcPlotter->write("StiMakerHistograms.root","UPDATE");
	  //HistoDocument histoDocumentMc("html/mc","McData","McData",canvas);
	  //histoDocumentMc.generateWebPage(_mcPlotter);
	}
      if (_residualCalculator)
	_residualCalculator->write("StiMakerHistograms.root", "UPDATE"); 
      delete canvas;
    }
  return StMaker::Finish();
}

Int_t StiMaker::Init()
{

  runField =0.;

  _loaderHitFilter = 0; // not using this yet.
  _loaderTrackFilter = new StiDefaultTrackFilter("LoaderTrackFilter","MC Tracks Filter"); 
  _loaderTrackFilter->add(new EditableParameter("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi));
  _loaderTrackFilter->add(new EditableParameter("PhiMin",   "Minimum Phi", 0.,   0.,  0., 6.3,2,Parameter::Double, StiTrack::kPhi));
  _loaderTrackFilter->add(new EditableParameter("PhiMax",   "Maximum Phi", 6.3, 6.3, 0., 6.3,2,Parameter::Double, StiTrack::kPhi));
  _loaderTrackFilter->add(new EditableParameter("PtUsed",   "Use Pt",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPt));
  _loaderTrackFilter->add(new EditableParameter("PtMin",    "Minimum Pt", 0., 0.1, 0., 100.,2,Parameter::Double, StiTrack::kPt));
  _loaderTrackFilter->add(new EditableParameter("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt));
  _loaderTrackFilter->add(new EditableParameter("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP));
  _loaderTrackFilter->add(new EditableParameter("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP));
  _loaderTrackFilter->add(new EditableParameter("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP));
  _loaderTrackFilter->add(new EditableParameter("EtaUsed",  "Use Eta",   true, true, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity));
  _loaderTrackFilter->add(new EditableParameter("EtaMin",   "Min Eta", -1.5, -1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity));
  _loaderTrackFilter->add(new EditableParameter("EtaMax",   "Max Eta",  1.5,  1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity));
  _loaderTrackFilter->add(new EditableParameter("nPtsUsed", "Use nPts",     true , true, 0,1,1,Parameter::Boolean, StiTrack::kPointCount));
  _loaderTrackFilter->add(new EditableParameter("nPtsMin",  "Minimum nPts", 10., 10., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount));
  _loaderTrackFilter->add(new EditableParameter("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount));
  _loaderTrackFilter->add(new EditableParameter("chargeUsed","Use Charge",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kCharge));
  _loaderTrackFilter->add(new EditableParameter("chargeMin", "Min Charge", -1., -1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge));
  _loaderTrackFilter->add(new EditableParameter("chargeMax", "Max Charge",  1.,  1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge));
  _toolkit->setLoaderHitFilter(_loaderHitFilter);
  _toolkit->setLoaderTrackFilter(_loaderTrackFilter);
  InitDetectors();
  return kStOk;
}

Int_t StiMaker::InitDetectors()
{
  StiDetectorGroup<StEvent,StMcEvent> * group;
  cout<<"StiMaker::InitDetectors() -I- Adding detector group:Star"<<endl;
  _toolkit->add(new StiStarDetectorGroup(false,"none"));
  if (_pars->useTpc)
    {
      cout<<"StiMaker::InitDetectors() -I- Adding detector group:TPC"<<endl;
      _toolkit->add(group = new StiTpcDetectorGroup(_pars->activeTpc,_pars->tpcInputFile));
      group->setGroupId(kTpcId);
    }
  if (_pars->useSvt)
    {
    cout<<"StiMaker::Init() -I- Adding detector group:SVT"<<endl;
    _toolkit->add(group = new StiSvtDetectorGroup(_pars->activeSvt,_pars->svtInputFile));
    group->setGroupId(kSvtId);
    }
  if (_pars->useSsd)
      {
	  cout<<"StiMaker::Init() -I- Adding detector group:Ssd"<<endl;
	  _toolkit->add(group = new StiSsdDetectorGroup(_pars->activeSsd,_pars->ssdInputFile));
	  group->setGroupId(kSsdId);
      }
  if (_pars->useFtpc)
    {
      cout<<"StiMaker::Init() -I- Adding detector group:FTPC"<<endl;
      _toolkit->add(group = new StiFtpcDetectorGroup(_pars->activeFtpc,_pars->ftpcInputFile));
      group->setGroupId(kFtpcWestId);
    }
  if (_pars->usePixel)
    {
      cout<<"StiMaker::Init() -I- Adding detector group:PIXEL"<<endl;
      _toolkit->add(group = new StiPixelDetectorGroup(_pars->activePixel,_pars->pixelInputFile));
      group->setGroupId(9999);
    }
  return kStOk;
}

Int_t StiMaker::InitRun(int run)
{
  if (!_initialized)
    {
      cout <<"StiMaker::InitRun() -I- Initialization Segment Started"<<endl;


			// Load Detector related parameters
			StiMasterDetectorBuilder * masterBuilder = _toolkit->getDetectorBuilder();
			masterBuilder->build(*this);
      StiDetectorContainer * detectorContainer = _toolkit->getDetectorContainer(); 
      detectorContainer->initialize();//build(masterBuilder);
      detectorContainer->reset();
			if (_pars->useResidualCalculator)
				{
					_residualCalculator = _toolkit->getResidualCalculator();
					_residualCalculator->initialize(_toolkit->getDetectorBuilder());
				}
			_seedFinder = _toolkit->getTrackSeedFinder();
      _seedFinder->initialize();
      _hitLoader  = _toolkit->getHitLoader();
      _hitLoader->setUseMcAsRec(_pars->useMcAsRec);
      _tracker = dynamic_cast<StiKalmanTrackFinder *>(_toolkit->getTrackFinder());
      _fitter  = dynamic_cast<StiKalmanTrackFitter *>(_toolkit->getTrackFitter());
			_tracker->load("trackFinderPars.dat",*this);
			_fitter->load("trackFitterPars.dat",*this);
      _eventFiller =  new StiStEventFiller();
      _trackContainer = _toolkit->getTrackContainer();
      _vertexFinder   = _toolkit->getVertexFinder();
      if (!_tracker)
				throw runtime_error("StiMaker::Make() -F- tracker is not a StiKalmanTrackFinder");
      _tracker->initialize();
      _tracker->clear();
      if (_toolkit->isGuiEnabled())
				{
					_eventDisplay->initialize();
					_eventDisplay->draw();
				}
      /*if (_pars->doPlots)
				{
					_recPlotter = new StiTrackingPlots("R","Reconstructed");
					if (_pars->doSimulation) _mcPlotter = new StiTrackingPlots("MC","MC");
					_radLength = new RadLengthPlots("R","Radiation Length Plots");
					}*/
      _initialized=true;
      cout <<"StiMaker::InitRun() -I- Initialization Segment Completed"<<endl;
    }
  return StMaker::InitRun(run);
}

Int_t StiMaker::Make()
{
  cout <<"StiMaker::Make() -I- Starting on new event"<<endl;

  eventIsFinished = false;
  StMcEvent * mcEvent;
  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );

  if (!event)
    throw runtime_error("StiMaker::Make() - ERROR - event == 0");

  // Retrieve bfield in Tesla
  double field = event->summary()->magneticField()/10.;

  if (runField==0) runField=field;
  if (field==0 && field != runField) field=runField;

  cout << "StiMaker::Make() -I- Reading eventSummary()->magneticField() " << field << endl; 
  if (fabs(field)<2.)
    static_cast<StiKalmanTrackFinderParameters&>(_tracker->getParameters()).setField(field);
  else
    {
      cout <<"StiMaker::Make() -E- field:"<<field<<endl;
      return -1;
    }

  
  if (_toolkit->isMcEnabled() )
    {
      if (!mMcEventMaker)
	mMcEventMaker = dynamic_cast<StMcEventMaker*>(GetMaker("StMcEvent"));
      if (mMcEventMaker)
	{
	  mcEvent= mMcEventMaker->currentMcEvent();
	  if (!mcEvent) 
	    throw runtime_error("StiMaker::Make() -E- mcEvent == 0");
	}
      else
	throw runtime_error("StiMaker::Make() -E- mMcEventMaker == 0");
    }
  else 
    mcEvent = 0;
  
  _tracker->clear();
  _hitLoader->loadEvent(event,mcEvent,_loaderTrackFilter,_loaderHitFilter);
  _seedFinder->reset();
  if (_toolkit->isGuiEnabled())
    _eventDisplay->draw();
  else
    {
      _tracker->findTracks();
      try
	{
	  if (_eventFiller && !_pars->useMcAsRec)
	    _eventFiller->fillEvent(event, _trackContainer);
	}
      catch (runtime_error & rte)
	{
	  cout << "StiMaker::Make() - Run Time Error :" << rte.what() << endl;
	}
      if (_vertexFinder)
	{
	  StiHit *vertex=0;
	  //cout << "StiMaker::Maker() -I- Will Find Vertex"<<endl;
	  vertex = _vertexFinder->findVertex(event);
	  if (vertex)
	    {
	      //cout << "StiMaker::Make() -I- Got Vertex; extend Tracks"<<endl;
	      _tracker->extendTracksToVertex(vertex);
	      //cout << "StiMaker::Make() -I- Primary Filling"<<endl; 
	      try
		{
		  if (_eventFiller && !_pars->useMcAsRec)
		    _eventFiller->fillEventPrimaries(event, _trackContainer);
		}  
	      catch (runtime_error & rte)
		{
		  cout<< "StiMaker::Make() - Run Time Error :" << rte.what() << endl;
		}						
	    }
	}
      if (_recPlotter) _recPlotter->fill(_toolkit->getTrackContainer());
      if (_mcPlotter ) _mcPlotter->fill(_toolkit->getMcTrackContainer());  
      if (_radLength)  _radLength->fill(_toolkit->getTrackContainer());
      if (_residualCalculator) _residualCalculator->calcResiduals(_toolkit->getTrackContainer() );
    }
  //cout<< "StiMaker::Make() -I- Done"<<endl;
  return kStOK;
}

 void StiMaker::setParameters(StiMakerParameters * pars)
{
  _pars = pars;
}

StiMakerParameters * StiMaker::getParameters()
{
  return _pars;
}
