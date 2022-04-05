
#include <iostream>

struct Pyjets_t {
    int    N;
    int    NPAD;
    int    K[5][4000];
    double P[5][4000];
    double V[5][4000];
};

struct pythia_particle {
    Int_t status;        // status of particle          ( LUJETS K[1] )
    Int_t pdg_id;        // flavour code                ( LUJETS K[2] )
    Int_t parent;        // parrent's id                ( LUJETS K[3] )
    Int_t firstChild;    // id of first child           ( LUJETS K[4] )
    Int_t lastChild;     // id of last  child           ( LUJETS K[5] )
    Float_t momentum[4]; // X.Y,Z,energy momenta [GeV/c]( LUJETS P[1]=P[4] )
    Float_t mass;        // Mass      [Gev/c^2]         ( LUJETS P[5] )
    Float_t vertex[4];   // X,Y,Z vertex  [mm]; time of procuction [mm/c]( LUJETS V[1]-V[4] )
    Float_t lifetime;    // proper lifetime [mm/c]      ( LUJETS V[5] )

    void print() {
	cout <<" --- New Particle --- "<<endl;
	cout <<"status: "<<status<<" pdg_id: "<<pdg_id<<" parent: "
	     <<parent<<" fChild: "<<firstChild<<" lChild: "<<lastChild<<endl;
	cout <<"px: "<<momentum[0]<<" py: "<<momentum[1]<<" pz: "<<momentum[2]<<" e: "
	     <<momentum[3]<<" mass: "<<mass<<endl;
	cout <<"vX: "<<vertex[0]<<" vY: "<<vertex[1]<<" vZ: "<<vertex[2]<<" t: "
	     <<vertex[3]<<" tau: "<<lifetime<<endl;
    }
	
};

void PythiaTest(int nEvents=1)
{
    gSystem->Load("$PYTHIA/libPythia6.so");
    gSystem->Load("libEG");
    gSystem->Load("libEGPythia6.so");

    TPythia6* p6 = new TPythia6();
    //p6->Dump();

    p6->SetMSEL(0);               //set subprocesses by hand
    p6->SetMSTP(48, 1);           //t->W+b
    p6->SetMSTJ(11, 3);           //select fragmentation function "Bowler"
    p6->SetPARJ(50+4, -0.07);     //peterson parameter for charm
    p6->SetPARJ(50+5, -0.006);    //peterson parameter for bottom
    p6->SetPARJ(50+5, -0.000001); //peterson parameter for top
    p6->SetPMAS(6, 1, 174.3);     //top mass
    p6->SetPMAS(25, 1, 120.0);    //higgs mass
    p6->SetMSTP(61, 1);           //ISR "on"
    p6->SetMSTP(71, 1);           //FSR "on"
    p6->SetMSTP(81, 1);           //multiple interactions "on"
    p6->SetMSTP(111, 1);          //fragmentation "on"
    p6->SetMSTP(82, 3);           //multiple interaction "vary impact param"
    p6->SetPARP(82, 2.41);        //cut-off pt for multiple interaction
    p6->SetMRPY(1, 88158204);     //random seed number
  
    p6->SetMSUB(26, 1);           //set subprocess WH
    p6->SetMDCY(24, 1, 1);        //W decaying
    p6->SetMDCY(25, 1, 1);        //H decaying
  
    //close all W decay channels
    int startdecay = p6->GetMDCY(24, 2);
    int enddecay   = p6->GetMDCY(24, 2) + p6->GetMDCY(24, 3);  
    for ( i=startdecay; i<enddecay; i++ ) p6->SetMDME(i, 1, 0);
  
    //close all H decay channels

    startdecay = p6->GetMDCY(25, 2);
    enddecay   = p6->GetMDCY(25, 2) + p6->GetMDCY(25, 3);  
    for ( i=startdecay; i<enddecay; i++ ) p6->SetMDME(i, 1, 0);

    //open wanted W decay channels
  
    p6->SetMDME(206, 1, 1);       //W->e+nu
    p6->SetMDME(207, 1, 1);       //W->mu+nu
    p6->SetMDME(208, 1, 1);       //W->tau+nu

    //open wanted W decay channels
  
    p6->SetMDME(214, 1, 1);       //H->b+bbar
  
    //initialize Pythia for D0

    p6->Initialize("CMS", "p", "pbar", 2000.0);

    for ( i=0; i<nEvents; i++ ) {
	
	if ( i%10 == 0 ) cout << "Event No.: " << i << endl;
	
	p6->GenerateEvent();
	
	Pyjets_t* pythiaParticle = p6->GetPyjets();
	
	Int_t nParticle = p6->GetN();
	
	for (int i=0;i<nParticle;i++) {
	    pythia_particle particle;
	    memset(&particle,0,sizeof(particle));
	    
	    particle.momentum[0] = pythiaParticle->P[0][i]; // px
	    particle.momentum[1] = pythiaParticle->P[1][i]; // px
	    particle.momentum[2] = pythiaParticle->P[2][i]; // px
	    particle.momentum[3] = pythiaParticle->P[3][i]; // energy
	    particle.mass        = pythiaParticle->P[4][i]; // mass
     
	    particle.status      = pythiaParticle->K[0][i]; // kS
	    particle.pdg_id      = pythiaParticle->K[1][i]; // kF
	    particle.parent      = pythiaParticle->K[2][i]; // parent
	    particle.firstChild  = pythiaParticle->K[3][i]; // firstchild
	    particle.lastChild   = pythiaParticle->K[4][i]; // lastchild

	    particle.vertex[0]   = pythiaParticle->V[0][i]; // vx
	    particle.vertex[1]   = pythiaParticle->V[1][i]; // vy
	    particle.vertex[2]   = pythiaParticle->V[2][i]; // vz
	    particle.vertex[3]   = pythiaParticle->V[3][i]; // time

	    particle.lifetime    = pythiaParticle->V[4][i]; // lifetime

	    particle.print();
	}

    }
    
    
}

