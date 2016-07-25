#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "StMaker.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#else
class StMaker;
class St_g2t_tpc_hit;
class g2t_tpc_hit_st;
#endif
struct Hit_t {
  double x, y, z, charge ; 
};
static Hit_t Hit;
//________________________________________________________________________________
Hit_t *getHitInfo(int sector=1, int row=1, int track_id=1) {
  StMaker *mk = StMaker::GetChain();
  if (mk) {
    StMaker *geant = mk->Maker("geant");
    if (geant) {
      St_g2t_tpc_hit *hit = (St_g2t_tpc_hit *) geant->DataSet("g2t_tpc_hit");
      if (hit) {
	Int_t N = hit->GetNRows();
	g2t_tpc_hit_st *tpc_hit = hit->GetTable();
	for (int i = 0; i < N; i++, tpc_hit++) {
	  Int_t volId = tpc_hit->volume_id;
	  Int_t isDet  = (volId/100000);
	  if (isDet) continue; // pseudo pad row
	  volId  -= (isDet)*100000;
	  Int_t iSector = volId/100;
	  if (iSector != sector) continue;
	  volId  -= (iSector)*100;
	  Int_t iPadrow = volId;
	  if (iPadrow != row) continue;
	  if (tpc_hit->track_p != track_id) continue;
	  Hit.x = tpc_hit->x[0];
	  Hit.y = tpc_hit->x[1];
	  Hit.z = tpc_hit->x[2];
	  Hit.charge = tpc_hit->de;
	  cout << "Hit s/r/i " << sector << "/" << row << "/" << track_id
	       << "\t x/y/z/charge " <<  Hit.x << "/" << Hit.y << "/" << Hit.z 
	       << "/" << Hit.charge << endl;
	  return &Hit;
	}
      }
    }
  } 
  return 0;
}
