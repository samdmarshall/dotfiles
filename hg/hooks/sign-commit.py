#!/usr/bin/env python3
import os

from mercurial import (
  cmdutil,
  commands,
  fancyopts,
  node as hgnode,
  pycompat,
  ui
)

def authenticate_commit(ui, repo, node, **kwargs):
  revision = repo[node]

  has_gpg_extension = 'gpg' in [name for name, _ in ui.configitems('extensions')]
  if not has_gpg_extension: return
  from hgext import gpg

  is_signed = ('.hgsigs' in revision.files())
  if not is_signed: pass
  alias, (func, opts, _) = cmdutil.findcmd("sign", commands.table)
  arguments = [(arg[0], arg[1], arg[2] if arg[1] != 'no-commit' else True) for arg in opts]
  print(gpg.sign(ui, repo, revs=node, opts='--no-commit'))
  abort()
