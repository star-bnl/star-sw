void Matrix() {
  // C = (I - K*H)*C*(I - K*H)T + K*V*KT
  // P = (I - K*H) => p_ij = delta_ij - k_ik*h_kj
  TString P[5][5];
  for (int i = 0; i<5; i++) for (int j = 0; j < 5; j++) P[i][j] = "";
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      TString term(Form("\tp%i%i=",i,j));
      TString t("");
      int l = 0;
      if (i == j) {term += "1."; l++;P[i][j]="1";}
      for (int k = 0; k < 2; k++) {
	if (k == j) t += Form("k%i%i",i,k);
      }
      if (t != "") {term += "-"; term += t; l++;P[i][j]+="-";P[i][j]+=t;}
      if (l) {term += ";"; cout << term;}
    }
  cout << endl;
  }
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      cout << "\t" << P[i][j];
    }
    cout << endl;
  }
  // W = P*C*PT; w_ij = p_ik*c_kl*p_jl;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j <= i; j++) {
      TString term("");
      for (int k = 0; k < 5; k++) {
	for (int l = 0; l < 5; l++) {
	  TString Pik = P[i][k];
	  TString Pjl = P[j][l];
	  TString sign = "+";
	  if (Pik != "" && Pjl != "") {
	    if (Pik.BeginsWith("-") && Pjl.BeginsWith("-")) {
	      Pik.Replace(0,1,0,0);
	      Pjl.Replace(0,1,0,0);
	    }
	    else 
	    if (Pik.BeginsWith("-") || Pjl.BeginsWith("-")) {
	      if (Pik.BeginsWith("-")) Pik.Replace(0,1,0,0);
	      if (Pjl.BeginsWith("-")) Pjl.Replace(0,1,0,0);
	      sign = "-";
	    }
	    Char_t *p = Pik.Data();
	    if (term == "") term = Form("_c%i%i=",i,j);
	    else if (p[0] != '-') term += sign;
	    if (p[0] == '1') {
	      if (Pik != "1") {
		term += "(";
		term += Pik;
		term += ")*";
	      } 
	    }
	    else {
	      term += Pik;
	      term += "*";
	    }
	    if (k >= l)	term += Form("c%i%i",k,l);
	    else	term += Form("c%i%i",l,k);
	    p = Pjl.Data();
	    if (p[0] == '1') {
	      if (Pjl != "1") {
		term += "*(";
		term += Pjl;
		term += ")";
	      } 
	    }
	    else {
	      term += "*";
	      term += Pjl;
	    }
	  }
	}
      }
      //      +K*V*KT => k_ik*v_kl*k_jl
      for (int k = 0; k < 2; k++) {
	for (int l = 0; l < 2; l++) {
	  term += "+";
	  term += Form("k%i%i",i,k);
	  if (k >= l) term += Form("*v%i%i*",k,l);
	  else        term += Form("*v%i%i*",l,k);
	  term += Form("k%i%i",j,l);
	}
      }
      term += ";";
      cout << "\t" << term << endl;
    }
    cout << endl;
  }
}
