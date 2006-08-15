// $Id: MakeHists.cxx,v 1.14 2006/08/15 21:42:59 jeromel Exp $
// $Log: MakeHists.cxx,v $
// Revision 1.14  2006/08/15 21:42:59  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.13  2003/09/02 18:00:19  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.12  2000/01/19 21:00:40  kathy
// update macros to use standard default xdf files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.11  1999/10/11 17:18:00  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
// Revision 1.10  1999/06/03 16:40:36  kathy
// put back old dedx.C and restore original macros that use non-existent input file
//
// Revision 1.8  1999/05/21 15:33:51  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
//*-- Author :    Valery Fine   21/07/98  (E-mail:fine@bnl.gov)
// #include "St_mevsim_Module.h"
void MakeHists(Char_t *xdffilename=
  "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.xdf")
{

  gROOT->Reset();
  Bool_t NT = kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT")==0){
    NT = kTRUE;
    if (gSystem.Load("St_base.dll")) printf(" Loading DLL \"St_base.dll\" failed \n");
    if (gSystem.Load("St_Tables.dll")) printf(" Loading DLL \"St_Tables.dll\" failed \n");
  }
  else{
    if (gSystem.Load("St_base.so"))      printf(" Loading DLL \"St_base.so\" failed \n");
    if (gSystem.Load("xdf2root.so"))      printf(" Loading DLL \"xdf2root.so\" failed \n");
    if (gSystem.Load("St_Tables.so"))    printf(" Loading DLL \"St_Tables.so\" failed \n");
  }

//   Read XDF file
   St_XDFFile  xdf(xdffilename);
   St_DataSet *event = xdf.NextEventGet();
   if (!event) { printf(" NO events \n"); return;}
   event->ls("*");
   St_DataSet *set=0;
   St_DataSetIter root(event);
   set = root.Cd("/evgen/particle");
   //   set = root.Cd("/Event/hepe_gent");
   if (set && set->HasData()) { 
     St_particle *pa = (St_particle *)set;
     pa->ls("*");
     
     printf(" Checking the results \n");
     
     table_head_st *t1_h = pa->GetHeader();

     printf(" Name=%s type=%s t1_h.nok = %i \n", t1_h->name, t1_h->type, t1_h->nok);
     printf(" t1_h.rbytes = %i \n", t1_h.rbytes);

     particle_st *particle = pa->GetTable();

     // Create "histograms"

//*-*  nt/plot 666.phep(3)

     TH1F *h1 = new TH1F("h1","phep(3)",100);
     h1->SetFillColor(16);

//*-*  nt/plot 666.::sqrt(phep(1)**2+phep(2)**2) (0.ne.phep(1))

     TH1F *h1Sqrt=new TH1F("h1Sqrt","666.sqrt",100);


//*-* nt/plot 666.::log(tan(0.5*atan(phep(3)/::sqrt(phep(1)**2+phep(2)**2))+.7854)) (0.ne.phep(1))

     TH1F *h2Log1= new TH1F("h2Log1","666.log1",100);
     h2Log1->SetFillColor(42);

//*-* nt/plot 666.::log(tan(0.5*atan(phep(3)/::sqrt(phep(1)**2+phep(2)**2))+.7854)) (idhep.eq.211.or.idhep.eq.-211)\n");

     TH1F *h3Log2=new TH1F("h3Log2","666.log2",100);
     h3Log2->SetFillColor(46);

     TSlider *slider = 0;

     Int_t kUPDATE = t1_h.nok/20;

//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )

     c1 = new TCanvas("c1","The reading STAF table: \"particle.h\"",200,10,600,800);
     c1->SetGrid();
     gBenchmark->Start("hsum");

     Int_t l = 0;
     Int_t nev =0;
     for (l=0; l < pa->GetNRows(); l++) 
     {
       nev++;
       particle_st *p = particle[l];

    //*-* Fill a'la nt/plot 666.phep(3)

        h1->Fill(p->phep[2]);
    //
    //
    // Fill a'la nt/plot 666.::sqrt(phep(1)**2+phep(2)**2) (0.ne.phep(1))
    //                         and 
    //           nt/plot 666.::log(tan(0.5*atan(phep(3)/::sqrt(phep(1)**2+phep(2)**2))+.7854)) (0.ne.phep(1))
    //
    // in a single step because of the common condition:  (0.ne.phep(1))
    //

    // ******  be aware CINT feature -> Non standard expressions "**" follow

        if ( p->phep[0] != 0 ) 
        {
          h1Sqrt->Fill(::sqrt(p->phep[0]**2+p->phep[1]**2)); 
          h2Log1->Fill(::sqrt(tan(0.5*atan(p->phep[2]/::sqrt(p->phep[0]**2+p->phep[1]**2))+.7854)));
        }

    //*-*
    //*-* Fill a'la nt/plot 666.::log(tan(0.5*atan(phep(3)/::sqrt(phep(1)**2+phep(2)**2))+.7854)) (idhep->eq.211.or.idhep->eq.-211)\n");
    //
        if ( p->idhep ==211 || p->idhep == -211) 
          h3Log2->Fill(::log(tan(0.5*atan(p->phep[2]/::sqrt(p->phep[0]**2+p->phep[1]**2))+.7854)));

    // Update the view of these histograms (just for fun)

        if (l && (l%kUPDATE) == 0) {
          if (l == kUPDATE) {
             h1Sqrt->Draw("e1p");
             h2Log1->Draw("same");
             h1->Draw("same");
             h3Log2->Draw("same");
             c1->Update();
             slider = new TSlider("slider","test",1.05,0,1.1,h1Sqrt->GetMaximum()*1.3,38);
             slider->SetFillColor(46);
          }
          if (slider) slider->SetRange(0,Float_t(l)/t1_h->nok);
          c1->Modified();
          c1->Update();
        }
     }

     slider->SetRange(0,1);
     c1->Modified();
     gBenchmark->Show("hsum");
  }
  printf(" This is a finish \n");
  printf(" You may pick up this example from /afs/rhic.bnl.gov/star/packages/dev/StRoot/macros/MakeHists.cxx\n");
}

