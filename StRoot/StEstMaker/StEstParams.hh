/***************************************************************************
 *
 *  
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: header file for StEstParams
 *
 ***************************************************************************
 *
 * 
 **************************************************************************/
class StEstParams{  
public:
  int	onoff[4]; 	//tracking for layer 1= tracking on for layer 0=off
  int	nneighbours[4];	//number of neighbours
  int	nbranch[4];	//number of branches for layer 0=no limit ??? 1=one branch ...
  int	ntotbranch[4];	//number of branches for track after layer ...
  int   maxtpchits;     //number of TPC hits taken to St_TPCTrack object
  int   maxsvthits;     //max number of SVT hits in the track
  int   maxbranches;    //number of branches for the track
  int   maxhitsproj;    //max hits taken in one projection
  int	share[4];	//number of branches for one hit 0=no limit
  double geomcutl[4];	//cut for length in [cm]
  double geomcutw[4];	//cut for width in [cm]
  double ptmin;
  double ptmax;
  double lrad[4][2];       //radii of the cylinders

  StEstParams::StEstParams() {} 
  StEstParams::~StEstParams() {}
};

class StEstSegments{
public: 
  int    slay[4];       // 2=required 1=allowed 0=forbiden hit on super layer
  double chisqcut;      // cut on chi square
  int    minhits;       // minimal number of hits in segment
  double rminTPC;       // minimal r of 1st hit in TPC
  int    minTPChits;    // minimal number of TPC hits
  StEstSegments::StEstSegments() {} 
  StEstSegments::~StEstSegments() {}
};
