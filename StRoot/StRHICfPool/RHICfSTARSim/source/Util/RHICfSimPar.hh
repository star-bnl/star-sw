#ifndef RHICfSimPar_hh
#define RHICfSimPar_hh

static const int ntower=2;
static const int nplate=16;
static const int nbelt=4;
static const int nxy=2;
static const int nbar[ntower]={20,40};
static const int nbarTS=20;
static const int nbarTL=40;
static const int nunused=4;
static const double weight[nplate+1]={1.,1.,1.,1.,1.,1.,1.,1.,
				      1.,1.,1.,2.,2.,2.,2.,2.,1.};

static const int nzdc=3;
static const int nsmd[nxy]={8,7};
static const int nscin=2;

static const int nbbc=36;
static const int nside=2;

static const int nstation=4;
static const double pstation[nstation]={15.8e3,17.1e3,55.5e3,58.5e3};

#endif
