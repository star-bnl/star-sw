{
  #include <fstream.h>
  gROOT.Reset();

  //
  //  A macro for calculating CLT-based pt fluctuation measures 
  //   (Delta sigma [sigtom], Phi pt [sigphi], and s2d [sigvol])
  //   using ascii data files extracted from STAR EbyE DSTs
  //
  //  by Jeff Reid 
  //  with substantial input from Tom Trainor and Lanny Ray
  //
 
  ////////////////////////////////////////////////////////////////
  // Declare and initialize variables
  //
  Int_t i = 0; Int_t j = 0;

  Float_t Npb = 0; Float_t Nmb = 0;
  Float_t mppb = 0; Float_t mpmb = 0;
  Float_t Np2b = 0; Float_t Nm2b = 0;
  Float_t Ppb = 0; Float_t Pmb = 0;
  Float_t Pp2b = 0; Float_t Pm2b = 0;
  Float_t Np22b = 0; Float_t Nm22b = 0;

  Float_t sigtomp = 0;
  Float_t sigtomm = 0;
  Float_t sigtomc = 0;

  Float_t sigphip = 0;
  Float_t sigphim = 0;
  Float_t sigphic = 0;

  Float_t sigvolp = 0;
  Float_t sigvolm = 0;
  Float_t sigvolc = 0;

  Int_t eventPool = 300000;

  Float_t Np[eventPool],pp[eventPool],p2p[eventPool];
  Float_t Nm[eventPool],pm[eventPool],p2m[eventPool];

  Float_t PPP,PP2,PPPp,PPPm,PP2p,PP2m;

  // Open input streams
  //   two separate files, separated by charge species [+,-]
  //   format should be ascii: ( N <p_t> <p_t^2> )
  //                           one event per line
  // 
  ifstream InPlus("C:/root/data/STAR/minbias/qm02.mb7.+",ios::in);
  ifstream InMinus("C:/root/data/STAR/minbias/qm02.mb7.-",ios::in);


  ///////////////////////////////////////////////////////////////////
  // read data from input streams and calculate relevant statistics
  //
  while (InPlus >> Np[i] >> pp[i] >> p2p[i]) {
    InMinus >> Nm[i] >> pm[i] >> p2m[i];

    // as long as we have one particle of each charge species add this
    //  event to the analysis
    if ((Np[i] > 1) && (Nm[i] > 1)) {

      // calculate event-ensemble average quantities...

      // ...N+/-
      Npb += Np[i];
      Nmb += Nm[i];

      // ...<p+/-> 
      mppb += pp[i];
      mpmb += pm[i];

      // ...N^2+/-
      Np2b += Np[i]*Np[i];
      Nm2b += Nm[i]*Nm[i];

      // ...total pt+/-
      Ppb += pp[i]*Np[i];
      Pmb += pm[i]*Nm[i];

      // ...(total pt+/-)^2
      Pp2b += (pp[i]*Np[i])*(pp[i]*Np[i]);
      Pm2b += (pm[i]*Nm[i])*(pm[i]*Nm[i]);

      // ...total pt^2+/-
      Np22b += Np[i]*p2p[i];
      Nm22b += Nm[i]*p2m[i];

      // increment event counter
      i++;
    }

  }

  // Calculate (charge-dependent [CD]) inclusive mean p+/-...
  PPPp = Ppb/Npb;
  PPPm = Pmb/Nmb;

  // ...and (charge-independent [CI]) inclusive mean p.
  PPP = (Ppb+Pmb)/(Npb+Nmb);
  Float_t inclpBar = PPP;

  // Calculate (CD) inclusive mean p^2+/-...
  PP2p = Np22b/Npb;
  PP2m = Nm22b/Nmb;

  // ...and (CI) inclusive mean p^2.
  PP2 = (Np22b+Nm22b)/(Npb+Nmb);

  // Complete average value calculations by dividing 'totals' by number of events.
  Npb /= i; Nmb /= i;
  mppb /= i; mpmb /= i;
  Np2b /= i; Nm2b /= i;
  Ppb /= i; Pmb /= i;
  Pp2b /= i; Pm2b /= i;
  Np22b /= i; Nm22b /= i;

  // calculate width of (CD) inclusive pt+/- distributions
  Float_t inclSigPp = sqrt(PP2p-(PPPp*PPPp));
  Float_t inclSigPm = sqrt(PP2m-(PPPm*PPPm));

  // calculate width of (CI) inclusive pt distribution
  Float_t inclSig = sqrt(PP2-PPP*PPP);


  ///////////////////////////////////////////////////////////////////
  // loop over data and calculate fluctuation measures
  //
  for (j = 0 ; j < i ; j++) {
 
    // calculate plus, minus, and covariant [p,m,c] terms 
    //   for the three CLT fluctuation measures...
    
    // Delta sigma
    sigtomp += Np[j]*(pp[j]-PPPp)*(pp[j]-PPPp)-(inclSigPp*inclSigPp);
    sigtomm += Nm[j]*(pm[j]-PPPm)*(pm[j]-PPPm)-(inclSigPm*inclSigPm);
    sigtomc += sqrt(Np[j]*Nm[j])*(pp[j]-PPPp)*(pm[j]-PPPm);

    // Phi pt
    sigphip += Np[j]*(Np[j]*(pp[j]-PPPp)*(pp[j]-PPPp)-(inclSigPp*inclSigPp));
    sigphim += Nm[j]*(Nm[j]*(pm[j]-PPPm)*(pm[j]-PPPm)-(inclSigPm*inclSigPm));
    sigphic += Np[j]*Nm[j]*(pp[j]-PPPp)*(pm[j]-PPPm);

    // s2d (sort of)
    sigvolp += (Np[j]*(pp[j]-PPPp)*(pp[j]-PPPp)-(inclSigPp*inclSigPp))/(Np[j]-1);
    sigvolm += (Nm[j]*(pm[j]-PPPm)*(pm[j]-PPPm)-(inclSigPm*inclSigPm))/(Nm[j]-1);
    sigvolc += (sqrt(Np[j]*Nm[j])*(pp[j]-PPPp)*(pm[j]-PPPm))/(sqrt(Np[j]*Nm[j])-1);

  } 
  printf("%i events.\n\n",i);

  // complete calculations by dividing by number of events
  sigtomp /= i; sigtomm /= i; sigtomc /= i;
  sigphip /= i; sigphim /= i; sigphic /= i;
  sigvolp /= i; sigvolm /= i; sigvolc /= i;

  // calculate individual CLT terms in units of MeV
  //  (++ [pa], -- [ma], +- [pm])
  Float_t dspa1 = 1000*sigtomp/(2*inclSigPp);
  Float_t dsma1 = 1000*sigtomm/(2*inclSigPm);
  Float_t dspm1 = 1000*sigtomc/(2*sqrt(inclSigPp*inclSigPm));

  Float_t dspa2 = 1000*sigphip/((2*inclSigPp)*Npb);
  Float_t dsma2 = 1000*sigphim/((2*inclSigPm)*Nmb);
  Float_t dspm2 = 1000*sigphic/((2*sqrt(inclSigPp*inclSigPm))*sqrt(Npb*Nmb));

  Float_t dspa3 = 1000*(Npb-1)*sigvolp/(2*inclSigPp);
  Float_t dsma3 = 1000*(Nmb-1)*sigvolm/(2*inclSigPm);
  Float_t dspm3 = 1000*(sqrt(Npb*Nmb)-1)*sigvolc/(2*sqrt(inclSigPp*inclSigPm));

  // calculate final numbers (sigma [CI], delta [CD]) for CLT measures
  Float_t sigma1 = 0.5*(dspa1+dsma1+2*dspm1);
  Float_t delta1 = 0.5*(dspa1+dsma1-2*dspm1);

  Float_t sigma2 = 0.5*(dspa2+dsma2+2*dspm2);
  Float_t delta2 = 0.5*(dspa2+dsma2-2*dspm2);

  Float_t sigma3 = 0.5*(dspa3+dsma3+2*dspm3);
  Float_t delta3 = 0.5*(dspa3+dsma3-2*dspm3);


  //////////////////////////////////////////////////////////////////////
  // print all possibly relevant variables (units of MeV)
  //   (tom [Delta sigma], phi [Phi pt], vol [s2d-like CLT measure])
  //
  printf("      Ds++     Ds--     Ds+-     Sig      Del       Nbar   inclSig   inclpBar\n");
  printf("tom:%-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f\n"
         ,dspa1,dsma1,dspm1,sigma1,delta1,(Npb+Nmb),1000*inclSig,1000*inclpBar);
  printf("phi:%-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f\n"
         ,dspa2,dsma2,dspm2,sigma2,delta2,(Npb+Nmb),1000*inclSig,1000*inclpBar);
  printf("vol:%-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f %-7.5f\n"
         ,dspa3,dsma3,dspm3,sigma3,delta3,(Npb+Nmb),1000*inclSig,1000*inclpBar);

}

