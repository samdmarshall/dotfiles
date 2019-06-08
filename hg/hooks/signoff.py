#!/usr/bin/env python
#
# Copyright (C) 2015 Lenz Grimmer <lenz@grimmer.com>
#
# signoff.py: A simple hook for Mercurial to automatically add a
# "Sign-off-by:" tag line below your commit messages (regular commits only,
# merge commits are excluded).
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License version 2 as published by the Free
# Software Foundation.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# This hook expects that your username and email address are configured correctly
# in the "username" setting in the [ui] section of your hgrc configuration file.
#
# To enable it, copy this file as signoff.py into the .hg directory of your
# repository and add the following to .hg/hgrc:
#
#   [hooks]
#   precommit = python:.hg/signoff.py:sign_commit_message
#
# TODO:
# * convert this hook into a proper Mercurial extension that extends the
#   built-in "hg commit" command with an option, e.g. "--signoff"

import re,os,mercurial

def sign_commit_message(ui, repo, **kwargs):
  commitctx = repo.commitctx

  def sign_ctx(ctx, error):
    commit_message_annotation = "Signed-off-by: "+ctx.user()
    is_merge_commit = (len(ctx.parents()) == 1)
    if not is_merge_commit:
      was_signed = '.hgsigs' in ctx.files()
      message_already_signedoff = re.search('^'+commit_message_annotation, ctx._text, re.MULTILINE)
      if (not message_already_signedoff) and was_signed:
        ui.write("Signing commit ("") ...")
        ctx._text += os.linesep + os.linesep + commit_message_annotation
    return commitctx(ctx, error)

  repo.commitctx = sign_ctx
