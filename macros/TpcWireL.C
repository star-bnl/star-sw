void TpcWireL() {
  Double_t R[8][2] = {
    { 53.2  ,  68.8  }, // 40
    { 69.2  ,  84.8  }, // 40
    { 85.2  , 100.8  }, // 40
    {101.2  , 120.8  }, // 50
    {122.795, 141.195}, // 47
    {141.595, 157.195}, // 40
    {157.595, 173.195}, // 40
    {173.595, 191.195}  // 45
  };
  Int_t NoWires[8] = {40, 40, 40, 50,
		      47, 40, 40, 45};
  Double_t Linner = 0, Louter = 0;
  for (Int_t i = 0; i < 8; i++) {
    Double_t rho = (R[i][0] + R[i][1])/2;
    Double_t L = rho*2*TMath::Tan(TMath::DegToRad()*15.)*NoWires[i];
    if (i < 4) Linner += L;
    else       Louter += L;
    cout << "Channel " << i+1 << " L = " << L << endl;
  }
  cout << "L Inner = " << 24*Linner << " L Outer = " << 24*Louter << endl;
}
/*
20 wires per channel / card
Subsector  Row  Radius   Pads  Pad 1    Pad 2    Anode wires  Anode-wire    Notes (Lecroy Card slot#.channel# => (l.c)) 
                   (cm)        fraction fraction              termination
Inner        1   60.000   88   0.778    1.000       17 -  19  Card 1        1  (0.0)                         sector 1 + 2*l + c/4
"            2   64.800   96   0.652    "           29 -  31  Card 2        2
"            3   69.600  104   0.499    "           41 -  43  Card 3        "  (0.1)
"            4   74.400  112   0.347    0.992       53 -  54  "             "   
"            5   79.200  118   0.957    1.000       65 -  67  Card 4        "   
"            6   84.000  126   0.893    "           77 -  79  "             "   
"            7   88.800  134   0.802    "           89 -  91  Card 5        "  (0.2)
"            8   93.600  142   0.683    "          101 - 103  Card 6        "
"            9   98.800  150   0.801    "          114 - 116  "             "   
"           10  104.000  158   0.892    "          127 - 129  Card 7        "  (0.3)
"           11  109.200  166   0.955    "          140 - 142  Cards 7, 8    " 
"           12  114.400  174   0.991    "          153 - 155  Card 8        "     
"           13  119.600  182   1.000    "          166 - 168  LOAB/ISOR     
 
Outer       14  127.195   98   0.764    1.000       10 -  14  Card 9        2  (0.0)              
"           15  129.195  100   0.583    "           15 -  19  "  "                        
"           16  131.195  102   0.383    "           20 -  24  "  "                        
"           17  133.195  104   0.210    0.973       25 -  29  Cards 9, 10   "          
"           18  135.195  106   0.089    0.894       30 -  34  Card 10       "             
"           19  137.195  "     0.764    1.000       35 -  39  "             "                     
"           20  139.195  108   0.582    "           40 -  44  "             "             
"           21  141.195  110   0.382    "           45 -  49  Cards 10, 11  "         
"           22  143.195  112   0.210    0.972       50 -  54  Card 11       "  (0.1)              
"           23  145.195  "     0.893    1.000       55 -  59  "             "                     
"           24  147.195  114   0.763    "           60 -  64  "             "             
"           25  149.195  116   0.582    "           65 -  69  Cards 11, 12  "         
"           26  151.195  118   0.382    "           70 -  74  Card 12       "             
"           27  153.195  120   0.209    0.972       75 -  79  "             "                     
"           28  155.195  122   0.088    0.893       80 -  84  "             "             
"           29  157.195  "     0.762    1.000       85 -  89  Cards 12, 13  "         
"           30  159.195  124   0.581    "           90 -  94  Card 13       "  (0.2)              
"           31  161.195  126   0.381    "           95 -  99  "             "                     
"           32  163.195  128   0.209    0.972      100 - 104  "             "                     
"           33  165.195  "     0.893    1.000      105 - 109  Cards 13, 14  "         
"           34  167.195  130   0.762    "          110 - 114  Card 14       "             
"           35  169.195  132   0.580    "          115 - 119  "             "                     
"           36  171.195  134   0.380    "          120 - 124  "             "                     
"  "        37  173.195  136   0.208    0.972      125 - 129  Cards 14, 15  "     
"           38  175.195  138   0.088    0.892      130 - 134  Card 15       "  (0.3)              
"           39  177.195  "     0.761    1.000      135 - 139  "             "                     
"           40  179.195  140   0.579    "          140 - 144  "             "                     
"           41  181.195  142   0.379    "          145 - 149  Cards 15, 16  "             
"           42  183.195  144   0.208    0.972      150 - 154  Card 16       "             
"           43  185.195  "     0.892    1.000      155 - 159  "             "                     
"           44  187.195  "     1.000    "          160 - 164  "             "                     
"           45  189.195  "      "       "          165 - 169  Card 16, LOAB/OSOR  2, 3

Notes:

1. MWPC socket 1 was unfilled during Summer 1999 running. In Fall 1999, a (modified FEE) 
grounding card will be installed, but the socket will eventually be instrumented.
2. The MWPC cards were not powered during the Summer 1999 test run, so their input 
impedance was not well-defined: the inputs were, effectively, partway between floating and ground.
3. Over row 45, the three innermost anode wires feed MWPC 16, while the outer two go to the 
LOAB. The next three wires also go to the LOAB, making edge effects, per se, negligible at 
the outer edge of the sector.

ISOR N = 10 (Inner sector, outer radius)
OSIR N =  7 (Outer sector, inner radius)
OSOR N = 5  (Outer sector, outer radisu)



Inner sector anode-wire information and MWC-socket connections:
Wire range    Radius to    Radius to    MWC    Notes
              first wire   last wire	socket      
              (cm)	   (cm)     
  1 -  20      53.200       60.800      1      1, 2
 21 -  40      61.200       68.800      2 
 41 -  60      69.200       76.800      3
 61 -  80      77.200       84.800      4
 81 - 100      85.200       92.800      5
101 - 120      93.200      100.800      6
121 - 140     101.200      108.800      7
141 - 160     109.200      116.800      8
161 - 170     117.200      120.800      ISOR   1

Outer sector anode-wire information and MWC-socket connections:
Wire range    Radius to    Radius to    MWC    Notes
	      first wire   last wire	socket      
	      (cm)         (cm)     
  1 -   7     122.795      125.195      OSIR    3
  8 -  27     125.595      133.195      9
 28 -  47     133.595      141.195      10
 48 -  67     141.595      149.195      11
 68 -  87     149.595      157.195      12
 88 - 107     157.595      165.195      13
108 - 127     165.595      173.195      14
128 - 147     173.595      181.195      15
148 - 167     181.595      189.195      16
168 - 172     189.595      191.195      OSOR    3

Channels

  1 -  40      53.200       68.800     
 41 -  80      69.200       84.800      4
 81 - 120      85.200      100.800      6
121 - 170     101.200      120.800      ISOR   1

Outer sector anode-wire information and MWC-socket connections:
Wire range    Radius to    Radius to    MWC    Notes
	      first wire   last wire	socket      
	      (cm)         (cm)     
  1 -  47     122.795      141.195      10
 48 -  87     141.595      157.195      12
 88 - 127     157.595      173.195      14
128 - 172     173.595      191.195      OSOR    3


 */
