#ifndef G3BRIDGE_H
#define G3BRIDGE_H

typedef void  (*Sub0)();
typedef void  (*Sub1)(void *);
typedef void  (*Sub2)(void *,void *);
typedef void  (*Sub3)(void *,void *,void *);
typedef void  (*Sub4)(void *,void *,void *,void *);
typedef void  (*Sub5)(void *,void *,void *,void *,void *);
typedef void  (*Sub6)(void *,void *,void *,void *,void *,void *);
typedef void  (*Sub7)(void *,void *,void *,void *,void *,void *,void *);

typedef float (*Fun1)(void *);
typedef float (*Fun2)(void *,void *);
typedef float (*Fun3)(void *,void *,void *);
typedef float (*Fun4)(void *,void *,void *,void *);


class G3Bridge {
public:
   G3Bridge();
static G3Bridge *Instance();

char *fBeg;

void *m_ggperp;
void *m_ginvol;
void *m_glvolu;
void *m_gmedia;
void *m_grndm ;
void *m_grndmq;
void *m_gtmany;
void *m_gtmedi;
void *m_gtnext;
void *m_gtonly;
void *m_gudcay;
void *m_gudigi;
void *m_gudtim;
void *m_gufld ;
void *m_guhadr;
void *m_guiget;
void *m_guinme;
void *m_guinti;
void *m_gukine;
void *m_gunear;
void *m_guout ;
void *m_gupara;
void *m_guphad;
void *m_guplsh;
void *m_guskip;
void *m_gustep;
void *m_guswim;
void *m_gutrak;
void *m_gutrev;
void *m_guview;
void *m_guxcs ;
void *m_rxgtrak;
void *m_rxinh ;
void *m_rxouth;

char *fEnd;

static G3Bridge *fBridge;
};
#endif
