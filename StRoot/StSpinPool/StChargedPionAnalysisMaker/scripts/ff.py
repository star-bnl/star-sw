import os
from scipy.special import beta as Beta
mu0 = 1
## mu0charm = 1.43
## mu0bottom = 4.3

## this is for pi+
N = {
    'u+ubar':   0.345,
    'd+dbar':   0.380,
    'ubar'  :   0.115,
    's+sbar':   0.190,
    'c+cbar':   0.271,
    'b+bbar':   0.501,
    'g':        0.279
}

alpha = {
    'u+ubar':   -0.015,
    'd+dbar':   -0.015,
    'ubar'  :    0.520,
    's+sbar':    0.520,
    'c+cbar':   -0.905,
    'b+bbar':   -1.305,
    'g':         0.899
}

beta = {
    'u+ubar':   1.20,
    'd+dbar':   1.20,
    'ubar'  :   3.27,
    's+sbar':   3.27,
    'c+cbar':   3.23,
    'b+bbar':   5.67,
    'g':        1.57
}

gamma = {
    'u+ubar':   11.06,
    'd+dbar':   11.06,
    'ubar'  :   16.26,
    's+sbar':   16.26,
    'c+cbar':   0.00,
    'b+bbar':   0.00,
    'g':        20.00
}

delta = {
    'u+ubar':   4.23,
    'd+dbar':   4.23,
    'ubar'  :   8.46,
    's+sbar':   8.46,
    'c+cbar':   0.00,
    'b+bbar':   0.00,
    'g':        4.91
}

def dss(z, flavor, charge):
    i = flavor
    num = N[i] * pow(z,alpha[i]) * pow(1-z,beta[i]) * \
        (1+gamma[i]*pow(1-z, delta[i]))
    denom = Beta(2+alpha[i], beta[i]+1) + \
        gamma[i]*Beta(2+alpha[i], beta[i]+delta[i]+1)
    return num/denom


def pythia(ckMin, ckMax, nevents):
    """
    runs standalone Pythia to measure charged pion FFs
    """
    import ROOT
    pythia = ROOT.TPythia6()
    
    ## CDF Tune A for STAR
    pythia.SetMSEL(1)
    pythia.SetMSTP(51, 7)
    pythia.SetMSTP(81, 1)
    pythia.SetMSTP(82, 4)
    pythia.SetPARP(67, 4.0)
    pythia.SetPARP(83, 0.5)
    pythia.SetPARP(84, 0.4)
    pythia.SetPARP(85, 0.9)
    pythia.SetPARP(86, 0.95)
    pythia.SetPARP(89, 1800)
    pythia.SetPARP(90, 0.25)
    pythia.SetPARP(91, 1.0)
    
    ## some customizations of the gluon fragmentation
    # pythia.SetMSTJ(1, 2)
    # pythia.SetMSTJ(2, 4)
    # pythia.SetMSTJ(3, 0)
    # pythia.SetMSTJ(11, 1)
    # pythia.SetPARJ(22, 1.0)
    # pythia.SetPARJ(41, 0.3)
    # pythia.SetPARJ(42, 0.58)
    # pythia.SetPARJ(43, 0.5)
    # pythia.SetPARJ(44, 0.9)
    
    pythia.SetCKIN(3, ckMin)
    pythia.SetCKIN(4, ckMax)
    
    pythia.Initialize('CMS', 'p', 'p', 200)
    
    hz = {
        211: {
            1:  ROOT.TH1D('z_d_plus', '', 200, 0., 1.),
            2:  ROOT.TH1D('z_u_plus', '', 200, 0., 1.),
            3:  ROOT.TH1D('z_s_plus', '', 200, 0., 1.),
            4:  ROOT.TH1D('z_c_plus', '', 200, 0., 1.),
            5:  ROOT.TH1D('z_b_plus', '', 200, 0., 1.),
            -1: ROOT.TH1D('z_dbar_plus', '', 200, 0., 1.),
            -2: ROOT.TH1D('z_ubar_plus', '', 200, 0., 1.),
            -3: ROOT.TH1D('z_sbar_plus', '', 200, 0., 1.),
            -4: ROOT.TH1D('z_cbar_plus', '', 200, 0., 1.),
            -5: ROOT.TH1D('z_bbar_plus', '', 200, 0., 1.),
            21: ROOT.TH1D('z_g_plus', '', 200, 0., 1.)
        },
        -211: {
            1:  ROOT.TH1D('z_d_minus', '', 200, 0., 1.),
            2:  ROOT.TH1D('z_u_minus', '', 200, 0., 1.),
            3:  ROOT.TH1D('z_s_minus', '', 200, 0., 1.),
            4:  ROOT.TH1D('z_c_minus', '', 200, 0., 1.),
            5:  ROOT.TH1D('z_b_minus', '', 200, 0., 1.),
            -1: ROOT.TH1D('z_dbar_minus', '', 200, 0., 1.),
            -2: ROOT.TH1D('z_ubar_minus', '', 200, 0., 1.),
            -3: ROOT.TH1D('z_sbar_minus', '', 200, 0., 1.),
            -4: ROOT.TH1D('z_cbar_minus', '', 200, 0., 1.),
            -5: ROOT.TH1D('z_bbar_minus', '', 200, 0., 1.),
            21: ROOT.TH1D('z_g_minus', '', 200, 0., 1.)
        }
    }
    
    hpt = {}
    ptbins = (40, 0., 20.)
    cname = {211: 'plus', -211: 'minus'}
    fname = {
        1:'d', 2:'u', 3:'s', 4:'c', 5:'b', 
        -1:'ubar', -2:'dbar', -3:'sbar', -4:'cbar', -5:'bbar', 
        21:'g', 'any':'any'
    }
    for process in (11,12,13,28,53,68):
        tmp = hpt.setdefault(process, {})
        for charge in (-211,211):
            tmp2 = tmp.setdefault(charge, {})
            for flavor in (1,2,3,4,5,-1,-2,-3,-4,-5,21,'any'):
                name = 'pt_%d_%s_%s' % (process, fname[flavor], cname[charge])
                hpt[process][charge][flavor] = ROOT.TH1D(name, '', *ptbins)
    
    vectorize = lambda pythia, i: ROOT.TLorentzVector( pythia.GetP(i,1), 
        pythia.GetP(i,2), pythia.GetP(i,3), pythia.GetP(i,4) )
    
    ## Particle numbers are as follows:
    ## 1,2 protons
    ## 3,4 incoming partons before ISR
    ## 5,6 incoming partons after ISR
    ## 7,8 outgoing partons  ... use these?
    for i in range(nevents):
        if i % 1000 == 0: print 'generating event', i
        pythia.GenerateEvent()
        nparticles = pythia.GetN()
        processid = pythia.GetMSTI(1)
        for i in range(9,nparticles+1):
            pid = pythia.GetK(i, 2)
            
            ## select only charged pions
            if abs(pid) != 211: continue
            
            status = pythia.GetK(i, 1)
            assert(status == 1)
            
            parent = pythia.GetK(i, 3)            
            while parent > 8:
                parent = pythia.GetK(parent, 3)
            
            ## skip pions from beam remnants, ISR
            if parent not in (7,8): continue
            
            mom = vectorize(pythia, i)
            
            parton = vectorize(pythia, parent)
            flavor = pythia.GetK(parent, 2)
            
            z = mom.Vect().Dot(parton.Vect()) / parton.P()**2
            
            hz[pid][flavor].Fill(z)
            hpt[processid][pid][flavor].Fill(mom.Pt())
            hpt[processid][pid]['any'].Fill(mom.Pt())
        
    pythia.Pystat(1)
    
    f = ROOT.TFile('pythia_%(ckMin)d_%(ckMax)d_%(nevents)d.root' % locals(), 
        'recreate')
    for charge in (-211, 211):
        [ h.Write() for h in hz[charge].values() ]
        for process in (11,12,13,28,53,68):
            [ h.Write() for h in hpt[process][charge].values() ]
    f.Close()
    
    return (hz, hpt)

def condor_ff(nevents=1E5, ckin=[2, 3, 4, 5, 7, 9, 11, 15, 25, 35, 100]):
    try:
        os.mkdir('out')
        os.mkdir('err')
        os.mkdir('log')
    except OSError: 
        pass
    
    ## build the script that we will run -- note trick with sys.argv
    f = open('job.py', 'w')
    f.write('from sys import argv\n')
    f.write('import analysis\n')
    f.write('analysis.ff.pythia(int(argv[1]), int(argv[2]), %d)' % nevents)
    
    ## build the submit.condor file
    f = open('submit.condor', 'w')
    f.write('executable = /usr/bin/python\n')
    f.write('getenv = True\n')
    f.write('notification = Error\n')
    f.write('notify_user = kocolosk@rcf.rhic.bnl.gov\n')
    f.write('universe = vanilla\n')
    f.write('stream_output = True\n')
    f.write('stream_error = True\n')
    f.write('transfer_executable = False\n\n')
    
    ## add jobs for each ckin bin
    for i in range(len(ckin)-1):
        sample = '%d_%d' % (ckin[i], ckin[i+1])
        f.write('output = out/pythia_%s.out\n' % (sample,))
        f.write('error = err/pythia_%s.err\n' % (sample,))
        f.write('log = log/pythia_%s.condor.log\n' % (sample,))
        f.write('arguments = %s/job.py %d %d -b\n' % (os.getcwd(), ckin[i], 
            ckin[i+1]))
        f.write('queue\n\n')
    
    f.close()
    
    ## and off we go
    os.system('condor_submit submit.condor')

    
