{
// #include "St_mevsim_Module.h"
// void mevsimtest()
   //  Delete the "old" log file
   gSystem.Exec("del *.log");

//   Read XDF file
   St_XDFFile  xdf("/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular/auau_ce_b0-2_4801_5000.xdf");
   St_Event *event = xdf.NextEvent();
   event->ls("*");
   St_DataSet *set=0;
   TList *list = event->GetListOfDataset();
   TIter next(list);
   set = (St_DataSet *)(next());
   set->ls("*");
   printf(" Getting the tables \n");
   St_particle *pa=(St_particle *)(set->GetStafTable());

// printf(" Calling the 'mevsim' module \n");

//   mevsim(pa);

printf(" Checking the results \n");

   table_head_st *t1_h = pa.GetHeader();

printf(" Name=%s type=%s t1_h.nok = %i \n", t1_h->name, t1_h->type, t1_h->nok);
printf(" t1_h.rbytes = %i \n", t1_h.rbytes);

   hfile = new TFile("mevsim.root","RECREATE","mevsim simulation");
   particle_st *particle = pa.GetTable();

// Create "ntuple"

   TNtuple *ntuple = new TNtuple("ntuple","Demo ntuple","idhep:p1:p2:p3:p4:p5");

// Fill "Ntuple"
printf(" Filling ntuple \n");

   for ( Int_t l=0; l<t1_h.nok; l++) 
   {
      ntuple->Fill(particle[l]->idhep
                  ,particle[l]->phep[0]
                  ,particle[l]->phep[1]
                  ,particle[l]->phep[2]
                  ,particle[l]->phep[3]
                  ,particle[l]->phep[4]
                  );
   }

// Show some results

printf(" some KUMACS like results to show \n");

//*-*
//*-* nt/plot 666.phep(3) \n");
//
     TCanvas cp3("cp3");
     ntuple->Draw("p3");

//*-*
//*-*  nt/plot 666.sqrt(phep(1)**2+phep(2)**2) (0.ne.phep(1))
//
     TCanvas cp4("cp4");
     ntuple->Draw("sqrt(p1**2+p2**2)", "p1 != 0");

//*-*
//*-* nt/plot 666.log(tan(0.5*atan(phep(3)/sqrt(phep(1)**2+phep(2)**2))+.7854)) (0.ne.phep(1))
//
     TCanvas cp5("cp5");
     ntuple->Draw("log(tan(0.5*atan(p3/sqrt(p1**2+p2**2))+.7854))", "p1 != 0");

//*-*
//*-* nt/plot 666.log(tan(0.5*atan(phep(3)/sqrt(phep(1)**2+phep(2)**2))+.7854)) (idhep.eq.211.or.idhep.eq.-211)\n");
//
     TCanvas cp6("cp6");
     ntuple->Draw("log(tan(0.5*atan(p3/sqrt(p1**2+p2**2))+.7854))", "idhep == 211 || idhep==-211");
     
hfile->Write();

}
