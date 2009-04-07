import os

files = os.listdir(os.path.dirname(__file__))
modules = map(lambda m: os.path.splitext(m)[0], 
    filter(lambda f: f.endswith('.py') and not f=='__init__.py', files))
modules.append('ptprofiles')
modules.append('mcasym')
modules.append('away2')
modules.append('hardP')
__import__(__name__, {}, {}, modules)

def all_modules(mod=None):
    """
    return all histogram configuration modules starting at mod
    """
    if not mod:
        import analysis.config as mod
    ret = list()
    if hasattr(mod, 'modules'):
        [ ret.extend(all_modules(getattr(mod, m))) for m in mod.modules ]
    else:
        ret.append(mod)
    return ret

def run5_simu():
    """
    return all config modules appropriate for Run 5 simulations
    """
    ret = all_modules()
    ret.remove(bx7)
    ret.remove(spinBit)
    [ ret.remove(m) for m in all_modules(mcasym.z) ]
    return ret

def run6_simu():
    """
    return all config modules appropriate for Run 6simulations
    """
    ret = all_modules()
    ret.remove(bx7)
    ret.remove(spinBit)
    [ ret.remove(m) for m in all_modules(mcasym.pt) ]
    return ret

def run5():
    """
    return all config modules appropriate for Run 5 data
    """
    ret = all_modules()
    [ ret.remove(m) for m in (cosTheta, hardP, ptMc_ptPr, x1, x1_x2, x2) ]
    [ ret.remove(m) for m in all_modules(mcasym) ]
    ret.remove(ptprofiles.mcjetpt)
    return ret

def run6():
    """
    return all config modules appropriate for Run 6 data
    """
    ret = all_modules()
    [ ret.remove(m) for m in (cosTheta, hardP, ptMc_ptPr, x1, x1_x2, x2) ]
    [ ret.remove(m) for m in all_modules(mcasym) ]
    ret.remove(ptprofiles.mcjetpt)
    return ret
