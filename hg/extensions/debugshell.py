# debugshell extension
"""a python shell with repo, changelog & manifest objects"""

from __future__ import absolute_import
import code
import mercurial
import sys
from mercurial import (
    demandimport,
    pycompat,
    registrar,
)

cmdtable = {}
command = registrar.command(cmdtable)

def pdb(ui, repo, msg, **opts):
    objects = {
        'mercurial': mercurial,
        'repo': repo,
        'cl': repo.changelog,
        'mf': repo.manifestlog,
    }

    code.interact(msg, local=objects)

def ipdb(ui, repo, msg, **opts):
    import IPython

    cl = repo.changelog
    mf = repo.manifestlog
    cl, mf # use variables to appease pyflakes

    IPython.embed()

@command(b'debugshell|dbsh', [])
def debugshell(ui, repo, **opts):
    bannermsg = ("loaded repo : %s\n"
                 "using source: %s" % (pycompat.sysstr(repo.root),
                                       mercurial.__path__[0]))

    pdbmap = {
        'pdb'  : 'code',
        'ipdb' : 'IPython'
    }

    debugger = ui.config(b"ui", b"debugger")
    if not debugger:
        debugger = 'pdb'
    else:
        debugger = pycompat.sysstr(debugger)

    # if IPython doesn't exist, fallback to code.interact
    try:
        with demandimport.deactivated():
            __import__(pdbmap[debugger])
    except ImportError:
        ui.warn((b"%s debugger specified but %s module was not found\n")
                % (debugger, pdbmap[debugger]))
        debugger = b'pdb'

    getattr(sys.modules[__name__], debugger)(ui, repo, bannermsg, **opts)
