#include "StarGenStats.h"
ClassImp(StarGenStats);

// --------------------------------------------------------------------------------------------------------------
StarGenStats::StarGenStats(const Char_t *name, const Char_t *title ) : TNamed(name,title),
								       nTried(0),
								       nSelected(0),
								       nAccepted(0),
								       sigmaGen(0.0),
								       sigmaErr(0.0),
								       sumWeightGen(0.0),
								       nFilterSeen(0),
								       nFilterAccept(0)
{
  /* nada */ 
}

/* / --------------------------------------------------------------------------------------------------------------
StarGenStats::StarGenStats( const StarGenStats &other ) : TNamed( other ),
							  nTried( other.nTried ),
							  nSelected( other.nSelected ),
							  nAccepted( other.nAccepted ),
							  sigmaGen( other.sigmaGen ),
							  sigmaErr( other.sigmaErr ),
							  sumWeightGen( other.sumWeightGen ),
							  nFilterSeen( other.nFilterSeen ),
							  nFilterAccept( other.nFilterAccept )
{

}
*/
