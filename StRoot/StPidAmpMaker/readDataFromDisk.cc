/***************************************************************************
 *
 * $Id: readDataFromDisk.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             read StPidAmpTrks from disk
 *             for quick tune purpose
 ***************************************************************************
 *
 * $Log: readDataFromDisk.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include <fstream.h>
#include "TH3.h"

#include "StPidAmpMaker/StPidAmpTrkVector.h"
#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"

void readDataFromDisk(StPidAmpTrkVector* trks,TH3D* histo){

         ifstream f;
       f.open("/star/u2e/aihong/newtpcpid/80evtsTrks.dat");

    if (!f) { cout<<" open file trks.dat error!! "<<endl;
          return; }

     double rig; 
     double dedx;
     int    charge;
     double pt;
     int    nhits;
     double dca;

     int NTrks=0;
  
 theLoop: if (!(f.eof())) {

       f>>rig; f>>dedx; f>>charge; f>>pt; f>>nhits; f>>dca;

      histo->Fill3(nhits,pt,1,1);

  StPidAmpTrk* theAmpTrk=new StPidAmpTrk(rig, dedx, charge,pt, nhits, dca);

 
    trks->push_back(theAmpTrk);
    NTrks++;
    goto theLoop;

     
     }


    f.close();

}
