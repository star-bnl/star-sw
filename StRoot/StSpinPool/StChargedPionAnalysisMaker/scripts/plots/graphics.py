import ROOT
from uuid import uuid1 as uuid

def canvas1(name=None):
    """
    creates a new TCanvas 750px wide, usual aspect ratio
    """
    title = name or str(uuid())
    c =  ROOT.TCanvas(title, title, 750, 535)
    return c

def canvas1e(name=None):
    """
    750px wide, lower panel for datamc ratio
    """
    title = name or str(uuid())
    c = ROOT.TCanvas(title, title, 750, 535)
    pads = [
        # name, title, x1, y1, x2, y2, color, bordersize, bordermode
        ROOT.TPad(c.GetName()+'_pad1', '', 0.0, 0.3, 1.0, 1.0, 10, 5, 0),
        ROOT.TPad(c.GetName()+'_pad2', '', 0.0, 0.0, 1.0, 0.3, 10, 5, 0),
    ]
    for i,p in enumerate(pads):
        p.SetNumber(i+1)
        p.SetCanvas(c)
        p.Draw()
        ROOT.SetOwnership( p, False )
    return c

def canvas2(name=None):
    """
    creates a TCanvas with 2 pads for -/+ histograms
    """
    title = name or str(uuid())
    c = ROOT.TCanvas(title, title, 750, 350)
    c.Divide(2,1)
    return c


def canvas3(name=None):
    """
    TCanvas with 4 pads -- top 2 for histograms, bottom 2 for errors
    """
    title = name or str(uuid())
    c = ROOT.TCanvas(title, title, 750, 400)
    pads = [
        # name, title, x1, y1, x2, y2, color, bordersize, bordermode
        ROOT.TPad(c.GetName()+'_pad1', '', 0.0, 0.3, 0.5, 1.0, 10, 5, 0),
        ROOT.TPad(c.GetName()+'_pad2', '', 0.5, 0.3, 1.0, 1.0, 10, 5, 0),
        ROOT.TPad(c.GetName()+'_pad3', '', 0.0, 0.0, 0.5, 0.3, 10, 5, 0),
        ROOT.TPad(c.GetName()+'_pad4', '', 0.5, 0.0, 1.0, 0.3, 10, 5, 0)
    ]
    for i,p in enumerate(pads):
        p.SetNumber(i+1)
        p.SetCanvas(c)
        p.Draw()
        ROOT.SetOwnership( p, False )
    return c

def ps_canvas(name=None, x=3, y=4):
    title = name or str(uuid())
    c = ROOT.TCanvas(title,title,100,100,600,800)
    c.Divide(x,y)
    return c

def maybe_save(c=None):
    if not c:
        c = ROOT.gPad
    fname = raw_input('Save As? ')
    if fname:
        c.Print(fname)

        