#ifndef StHbtTypes_HH

/*------------------------------------
 * Histograms
 */
#ifndef ROOT_TH1
#include "TH1.h"
#endif
#ifndef ROOT_TH2
#include "TH2.h"
#endif
#ifndef ROOT_TH3
#include "TH3.h"
#endif

typedef TH1D StHbt1DHisto;
typedef TH2D StHbt2DHisto;
typedef TH3D StHbt3DHisto;




/*----------------------------------------------------------------
 * Strings
 * This insane little bit is because Cint, which is supposed to be
 * so much like C++, doesn't even recognize "string" as a type!
 */


#ifndef __CINT__
#include <string>
typedef string StHbtString; //!
#else
class StHbtString; //!
#endif

/*----------------------------------------------------------------
 * Vectors:
 * Even the STAR Class Library keeps changing in terms of what will
 * work!  Unfortunately, we have to abstract even the ThreeVectors
 * and LorentzVectors to here, so that we can roll with the punches
 * from sofi more easily.
 */

//#include "StThreeVector.hh"
//typedef StThreeVector<double> StHbtThreeVector;//!
#include "StThreeVectorD.hh"
typedef StThreeVectorD StHbtThreeVector;//!
//#include "StLorentzVector.hh"
//typedef StLorentzVector<double> StHbtLorentzVector;//!
#include "StLorentzVectorD.hh"
typedef StLorentzVectorD StHbtLorentzVector;//!

#endif
