struct fcfHit {
	u_short pad ;
	u_short tm ;
	u_short f ;
	u_short c ;
	u_short p1,p2,t1,t2 ;
} ;

class fcfAfterburner {
public:
        fcfAfterburner() { last_n = last_count = last_i = last_stage = 0; do_merge = do_cuts = 1 ; verbose = true; };
	~fcfAfterburner() { ; } ;

	int burn(u_int *ptr_res[3]) ;
	int next(fcfHit *out) ;
	int compare(u_int *p1[3], u_int *p2[3]) ;

	void decode(u_int *data, fcfHit *h) ;	// utility: from FCF to local
	void print_hit(char *, fcfHit *hit) ;	// utility

	u_int do_swap ;		// 0=no, 1=yes => set by "burn"
	u_int do_merge ;	// merge broken rows
	u_int do_cuts ;		// apply additional cuts
	u_int row ;		// picked up from the data...

        void setVerbose(bool v) { verbose = v; };
private :
        bool verbose;

	u_int last_n, last_i, last_count, last_stage ;
	u_int **ptr ;	// storage for the burn arg...

	u_int edge[4] ;
	struct fcfHit broken[4][100] ;
	u_int cou_broken[4] ;

	int output(fcfHit *l, char *any) ;	// the cut function; return TRUE if accepted
	int check_merge(fcfHit *l, fcfHit *r) ;	// merges r into l and returns TRUE 
} ;

